# IN THIS SCRIPT WE WANT TO IMPORT THE PREVIOUS SP DATAFRAME AND JOIN IT WITH IRIS/COMMUNES/DEPARTEMENTS/REGIONS SHAPEFILES FROM INSEE
# Install packages
library(raster)
library(sp)
library(tmap)
library(sf)
library(dplyr)
library(here)
library(readxl)

# Import our Spatial Point Data Frame and convert into Simple Feature (sf) object for easier manipulation
path = here("Indicators/Tree Cover/Workflow/tree_cover_sp_data.rds")
sp_points = readRDS(path)
sp_points = st_as_sf(sp_points)

########################################################################################################################################################

# IRIS SCALE WORKFLOW
# Read the IRIS shapefile from the INSEE
path_iris = here("Insee Tables/1- Iris INSEE/CONTOURS-IRIS/1_DONNEES_LIVRAISON_2023-09-00134/CONTOURS-IRIS_3-0_SHP_LAMB93_FXX-2023/CONTOURS-IRIS.shp")
iris = st_read(path_iris)

# Aligning coordinate systems
sp_points = st_transform(sp_points, st_crs(iris))

# We're joining our spatial data frame with the iris shapefile
data_joined_iris <- st_join(sp_points, iris)%>%
  summarize(nom_commune = NOM_COM, nom_iris = NOM_IRIS, code_commune = INSEE_COM, code_iris = CODE_IRIS, type_iris = TYP_IRIS, tree_cover = value, geometry)%>%
  filter(type_iris=='H') # filtrant pour seulement les iris (omettant communes non-irisés)

# Remove the NA's (usually pixels not in FRANCE from the sf object)
sum(is.na(data_joined_iris$code_iris)) # number of NAs
data_joined_iris=na.omit(data_joined_iris) # remove the NAs

# Aggregating data per Iris (taking the mean value for each Iris)
data_aggregated_iris <- data_joined_iris %>%
  group_by(code_iris) %>% 
  reframe(nom_commune, nom_iris, code_commune, code_iris, tree_cover_mean = round(mean(tree_cover)))

# Remove all the implied duplicated to keep one row per Iris
data_aggregated_iris = data_aggregated_iris[!duplicated(data_aggregated_iris),]

# Re aggregation of average iris data with polygons from INSEE
data_aggregated_iris = data_aggregated_iris%>%
  left_join(iris, by=c('code_iris' = 'CODE_IRIS'))%>%
  summarize(nom_commune, nom_iris, code_commune, code_iris, tree_cover_mean, geometry)

########################################################################################################################################################

# COMMUNES SCALE WORKFLOW
# Read the Communes shapefile from the INSEE
path_communes = here("Insee Tables/2- Communes INSEE/communes-20220101.shp")
communes = st_read(path_communes)

# Aligning coordinate systems
sp_points = st_transform(sp_points, st_crs(communes))

# We're joining our spatial data frame with the communes shapefile
data_joined_communes <- st_join(sp_points, communes)%>%
  select(nom_commune = nom, code_commune = insee, tree_cover = value, geometry)

# Remove the NA's (usually pixels not in FRANCE from the sf object)
sum(is.na(data_joined_communes$code_commune)) # number of NAs
data_joined_communes=na.omit(data_joined_communes) # remove the NAs

# Aggregating data per commune (taking the mean value for each commune)
data_aggregated_communes <- data_joined_communes %>%
  group_by(code_commune) %>% 
  reframe(nom_commune, code_commune, tree_cover_mean = round(mean(tree_cover)))

# Remove all the implied duplicated to keep one row per commune
data_aggregated_communes = data_aggregated_communes[!duplicated(data_aggregated_communes),]

# Re aggregation of average commune data with polygons from INSEE
data_aggregated_communes = data_aggregated_communes%>%
  left_join(communes, by=c('code_commune' = 'insee'))%>%
  select(nom_commune, code_commune, tree_cover_mean, geometry)

########################################################################################################################################################

# DEPARTEMENT & REGION SCALE WORKFLOW
# Read the Departements shapefile from the INSEE
path_departements = here("Insee Tables/3- Départements INSEE/New Version/departement_shapefile.rds")
departements = readRDS(path_departements)

# Read the Regions shapefile from the INSEE
path_regions = here("Insee Tables/4- Régions INSEE/regions-20180101.shp")
regions = st_read(path_regions)

# Read the 'Table de Passage' excel from the INSEE
path_tdp = here("Insee Tables/Tables de passage/Table de passage Communes.xlsx")
tdp = read_xlsx(path_tdp, skip=5)

# Joining our dataset with insee code for departments and regions
data_aggregated_dep_reg = data_aggregated_communes%>% # Using the previously computed sf data frame for communes
  left_join(tdp, by=c('code_commune'='CODGEO'))%>%
  select(insee_region = REG, insee_departement = DEP, insee_communes = code_commune, nom_commune, geometry_communes = geometry, tree_cover_mean)

# Joining our department polygon sf object with our datset by department insee code
data_aggregated_dep_reg = data_aggregated_dep_reg%>%
  left_join(departements, by=c('insee_departement'='code_insee'))%>%
  select(insee_region, insee_departement, insee_communes, nom_departement = nom, nom_commune, geometry_communes, geometry_departement = geometry, tree_cover_mean)

# Joining our regions polygon sf object with our datset by region insee code
data_aggregated_dep_reg = data_aggregated_dep_reg%>%
  left_join(regions, by=c('insee_region'='code_insee'))%>%
  select(insee_region, insee_departement, insee_communes, nom_region = nom, nom_departement, nom_commune, geometry_region = geometry, geometry_departement, geometry_communes, tree_cover_mean)

## Saving our spatial object into R object for future use
# save_path_1 = here("Indicators/Tree Cover/Workflow/treecover_iris_indicator.rds")
# saveRDS(object = data_aggregated_iris, file = save_path_1)

## Saving our spatial object into R object for future use
# save_path_2 = here("Indicators/Tree Cover/Workflow/treecover_comm_dep_reg_indicator.rds")
# saveRDS(object = data_aggregated_dep_reg, file = save_path_2)

#################################################################################################################################################################################################################################################################################################
# Plot By Iris
# Average of cover tree per iris & plot
data_iris = data_aggregated_iris%>%
  group_by(code_iris)%>%
  summarize(tree_cover_mean, geometry_iris=geometry)

data_iris = st_as_sf(data_iris) # Reconvert to sf object for visualization

# Plot the map with added labels and titles
breaks <- seq(0, 100, by = 10)
colors <- c("#f5f5dc", "#006400")

tm_shape(data_iris) +
  tm_polygons(col = "tree_cover_mean", alpha=0.9, style = "fixed", breaks = breaks, palette = colors,
              border.col = NA, lwd = 0, title = "Tree Cover (in %)") +
  tm_layout(frame = FALSE,
            legend.position = c("left", "bottom"),
            title = "Forest Tree Cover per Iris (2018)")

#################################################################################################################################################################################################################################################################################################
# Plot By Commune
# Average of radiance per commune & plot
data_comm = data_aggregated_dep_reg%>%
  group_by(insee_communes)%>%
  summarize(tree_cover_mean = mean(tree_cover_mean), geometry_communes)

# Remove all the implied duplicated to keep one row per commune
data_comm = data_comm[!duplicated(data_comm),]
data_comm = na.omit(data_comm)
data_comm = st_as_sf(data_comm) # Reconvert to sf object for visualization

# Plot the map with added labels and titles
breaks <- seq(0, 100, by = 10)
colors <- c("#f5f5dc", "#006400")

tm_shape(data_comm) +
  tm_polygons(col = "tree_cover_mean", alpha=0.9, style = "fixed", breaks = breaks, palette = colors,
              border.col = NA, lwd = 0, title = "Tree Cover (in %)") +
  tm_layout(frame = FALSE,
            legend.position = c("left", "bottom"),
            title = "Forest Tree Cover per Commune (2018)")
########################################################################################################################################################################################################################
# Plot By Department
# Average of radiance per department & plot
data_dep = data_aggregated_dep_reg %>%
  group_by(insee_departement)%>%
  summarize(tree_cover_mean = mean(tree_cover_mean), geometry_departement)

# Remove all the implied duplicated to keep one row per department
data_dep = data_dep[!duplicated(data_dep),]
data_dep = na.omit(data_dep)
data_dep = st_as_sf(data_dep) # Reconvert to sf object for visualization

# Plot the map with added labels and titles
breaks <- seq(0, 100, by = 10)
colors <- c("#f5f5dc", "#006400")

tm_shape(data_dep) +
  tm_polygons(col = "tree_cover_mean", alpha=0.9, style = "fixed", breaks = breaks, palette = colors,
              border.col = 'black', lwd = 1, title = "Tree Cover (in %)") +
  tm_layout(frame = FALSE,
            legend.position = c("left", "bottom"),
            title = "Forest Tree Cover per Department (2018)")
########################################################################################################################################################################################################################
# Plot By Region
# Average of cover tree per region & plot
data_reg = data_aggregated_dep_reg%>%
  group_by(insee_region)%>%
  summarize(tree_cover_mean = mean(tree_cover_mean), geometry_region)

# Remove all the implied duplicated to keep one row per region
data_reg = data_reg[!duplicated(data_reg),]
data_reg = na.omit(data_reg)
data_reg = st_as_sf(data_reg ) # Reconvert to sf object for visualization

# Plot the map with added labels and titles
breaks <- seq(0, 100, by = 10)
colors <- c("#f5f5dc", "#006400")

tm_shape(data_reg) +
  tm_polygons(col = "tree_cover_mean", alpha=0.9, style = "fixed", breaks = breaks, palette = colors,
              border.col = 'black', lwd = 1, title = "Tree Cover (in %)") +
  tm_layout(frame = FALSE,
            legend.position = c("left", "bottom"),
            title = "Forest Tree Cover per Region (2018)")
########################################################################################################################################################################################################################
# Plotting a subregion of France

# Subset all the communes from a region & clean
comm_subset = data_aggregated_dep_reg[data_aggregated_dep_reg$nom_region == 'Île-de-France',]%>%
  reframe(tree_cover_mean, geometry_communes)

comm_subset = comm_subset[!duplicated(comm_subset),]
comm_subset = na.omit(comm_subset)
comm_subset = st_as_sf(comm_subset)

# Subset all the departments from a region & clean
dep_subset = data_aggregated_dep_reg[data_aggregated_dep_reg$nom_region == 'Île-de-France',]%>%
  group_by(insee_departement)%>%
  reframe(tree_cover_mean=mean(tree_cover_mean), geometry_departement)

dep_subset = dep_subset[!duplicated(dep_subset),]
dep_subset = na.omit(dep_subset)
dep_subset = st_as_sf(dep_subset)

# Plot the subsetted communes and departments
breaks <- seq(0, 100, by = 10)
colors <- c("#f5f5dc", "#006400")

tm_shape(comm_subset) +
  tm_polygons(col = "tree_cover_mean", alpha=0.9, style = "fixed", breaks = breaks, palette = colors, title = "Tree Cover (in %)",
              border.col = 'black', lwd = 0.5) +
  tm_shape(dep_subset) +
  tm_borders(lwd = 2, col = "black") +
  tm_layout(frame = FALSE,
            legend.position = c("left", "bottom"),
            title = "Forest Tree Cover per Commune in Ile-de-France (2018)",
            title.position = c("center", "TOP"))



