#----------------------------------- INFO -------------------------------------#
# Author : Alfonso AWADALLA 
# Date	: March 2024
# Descr	: Some Data Processing + Shiny App
#------------------------------------------------------------------------------#


#----------------------------- Import Libraries -------------------------------#
library(raster)
library(sp)
library(tmap)
library(sf)
library(dplyr)
library(here)
library(readr)
library(readxl)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(rmapshaper)
library(shinythemes)

################################################################################
#                         Import Necessary Data                               #
################################################################################

# Import shapefiles
path_regions = here("Insee Tables/4- Régions INSEE/regions-20180101.shp")
regions = st_read(path_regions)%>%
  st_simplify(dTolerance = 0.7)

path_departements = here("Insee Tables/3- Départements INSEE/New Version/departement_shapefile.rds")
departements = readRDS(path_departements)%>%
  st_transform(st_crs(regions))%>%
  st_simplify(dTolerance = 0.7)

path_communes = here("Insee Tables/2- Communes INSEE/communes-20220101.shp")
communes = st_read(path_communes)%>%
  st_transform(st_crs(regions))%>%
  st_simplify(dTolerance = 0.7)

path_iris = here("Insee Tables/1- Iris INSEE/CONTOURS-IRIS/1_DONNEES_LIVRAISON_2023-09-00134/CONTOURS-IRIS_3-0_SHP_LAMB93_FXX-2023/CONTOURS-IRIS.shp")
iris = st_read(path_iris)%>%
  st_transform(st_crs(regions))%>%
  st_simplify(dTolerance = 0.7)

# Import final indicator + cleaning
path = here("indicateur final.xlsx")
final_indic = read_xlsx(path, col_types=c('numeric', 'text', rep('numeric', 27)))

# Import correspondance indicateor with determinant
indicateurs_determinants = read_xlsx(path, sheet=2)

# Cleaning et modifications
# select code and name column of department shapefile to join with iris and communes (for visualisation per departement)
departements_v2 = as.data.frame(departements)%>%
  select(code_dep = code_insee, nom_dep=nom)

# Cleaning on final indicator
final_indic <- final_indic%>%
  mutate(code = if_else(nchar(as.character(code))==8 & niveau=='Iris', paste0("0", code), as.character(code)), # format issue
         code = if_else(nchar(as.character(code))==4 & niveau=='Communes', paste0("0", code), as.character(code)), # format issue
         code = if_else(nchar(as.character(code))==1 & niveau=='Departement', paste0("0", code), as.character(code)))%>% # format issue
  filter(!(niveau=='Regions' & code%in%c('01','02','03','04','06')))%>% # Withdraw non metropolitan region 
  mutate(code_dep = if_else(niveau %in% c('Iris', 'Communes'), substr(code, 1, 2), NA))%>%
  left_join(departements_v2, by='code_dep')%>%
  select(-code_dep)


# write_csv(final_indic, here('indicateur_final_projet.csv'))

################################################################################
#                         Final Pre-Processing                                 #
################################################################################

# Workflow on Iris
iris_indic = final_indic%>%
  filter(niveau=='Iris')%>%
  left_join(iris, by=c('code' = 'CODE_IRIS'))%>%
  select(-ID, -INSEE_COM, -NOM_COM, -IRIS, nom=NOM_IRIS, -TYP_IRIS)


# Workflow on Communes
communes_indic = final_indic%>%
  filter(niveau=='Communes')%>%
  left_join(communes, by=c('code' = 'insee'))%>%
  filter(!is.na(nom))%>%
  select(-wikipedia, -surf_ha)

# Workflow on Departement
depart_indic = final_indic%>%
  filter(niveau=='Departement')%>%
  left_join(departements, by=c('code' = 'code_insee'))%>%
  select(-nuts3, -wikipedia, -surf_km2)

# Workflow on Regions
regions_indic = final_indic%>%
  filter(niveau=='Regions')%>%
  left_join(regions, by=c('code' = 'code_insee'))%>%
  select(-nuts2, -wikipedia, -surf_km2)

# Recompiler nos 4 échelles afin d'obtenir le ready_to_use indicateur
indicateur_final = rbind(iris_indic, communes_indic, depart_indic, regions_indic)%>%
  st_as_sf() # Reconvert to sf object for visualization
  
################################################################################
#                               Shiny APP                                     #
################################################################################

ui <- fluidPage(
  titlePanel("Visualisation des déterminants de santé des territoires"),
  theme = shinytheme("journal"),
  sidebarLayout(
    sidebarPanel(
      selectInput("determinant", "Choisir un déterminant", choices = unique(indicateurs_determinants$Determinant), selected = "Déterminant n°1 : Qualité de l'air"),
      selectInput("indicateur", "Choisir un indicateur:", choices = NULL),
      selectInput("echelle", "Choisir une échelle:", choices = NULL),
      uiOutput("dynamicScale"),
      selectizeInput('localisation', "Choisir localisation:", choices = NULL),
      actionButton("btnInfo", "Informations")
    ),
    mainPanel(
      # Main panel content here
      leafletOutput("map")
    )
  )
)


server <- function(input, output, session) {
  # Reactive expression for filtered indicators by determinant
  filteredIndicators <- reactive({
    indicateurs_determinants %>%
      filter(Determinant == input$determinant)})
  
  # Update available indicators based on chosen determinant
  observe({
    items <- filteredIndicators()
    updateSelectInput(session, "indicateur", choices = unique(items$Indicateur))})
  
  # Reactive expression for echelle filtering
  filteredEchelle <- reactive({
    # Ensure that input$indicateur is not empty and exists as a column in indicateur_final
    req(input$indicateur, !is.na(input$indicateur), input$indicateur %in% names(indicateur_final))
    # Proceed with filtering and selection
    indicateur_final %>%
      filter(!is.na(.data[[input$indicateur]])) %>%
      select(code, niveau, nom_dep, nom, geometry, .data[[input$indicateur]])%>%
      mutate(across(.cols = all_of(input$indicateur), .fns = ~round(., digits = 2)))
  })
  
  # Update available echelles based on chosen indicateur
  observe({
    echelle <- filteredEchelle()
    if (nrow(echelle) > 0) {
      updateSelectInput(session, "echelle", choices = unique(echelle$niveau))}})
  
  # Dynamic input based on scale (iris & communes)
  output$dynamicScale <- renderUI({
    if(input$echelle %in% c("Communes", "Iris")) {
      selectInput("department", "Choisir département à visualiser:", choices = unique(filteredEchelle()$nom_dep))}})
  
  # Determine if the current scale requires a department filter
  # Make requiresDept a reactive expression
  requiresDept <- reactive({
    input$echelle %in% c("Communes", "Iris")
  })
  

  # Filter data based on current inputs, considering whether department is relevant
  filteredData <- reactive({
    baseData <- filteredEchelle() %>% filter(niveau == input$echelle)
    if (requiresDept() && !is.null(input$department) && input$department != "") {
      baseData %>% filter(nom_dep == input$department)
    } else {
      baseData
    }
  })
  
  # Update available locations based on indicateur and echelle
  observe({
    lieux <- filteredData()%>%
      filter(niveau == input$echelle)
    updateSelectizeInput(session, "localisation", choices = unique(lieux$nom), selected=NA, server = TRUE)})
  
  
  #Map render
  output$map <- renderLeaflet({
    
    # Data from reactive
    data_map <- filteredData()
    
    # Check if the data is in a valid state for rendering
    if (is.null(data_map) || nrow(data_map) == 0 || is.list(data_map[[input$indicateur]])) {
      return(NULL)  # Do not render the map if data is not ready
    }
    
    # Prepare color palette
    color_pal <- colorNumeric(palette = "viridis", domain = data_map[[input$indicateur]])
    
    # Render the map
    map <- leaflet(data = data_map) %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 2.2137, lat = 46.2276, zoom = 5) %>%
      addPolygons(
        fillColor = ~color_pal(as.numeric(data_map[[input$indicateur]])),
        color = "#BDBDC3", weight = 1, opacity = 1,
        fillOpacity = 0.7, smoothFactor = 0.5,
        highlightOptions = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.7, bringToFront = TRUE),
        label = ~as.character(nom),
        popup = ~paste("Zone: ", nom, "<br>", "Value: ", as.numeric(data_map[[input$indicateur]])),
        group = "polygons"
      ) %>%
      addLegend(pal = color_pal, values = ~as.numeric(data_map[[input$indicateur]]), title = "Légende", position = "bottomright")
    
    map
  })

  # Update map zoom on department chosen if iris or communes
  observeEvent(input$department, {
    # Ensure a location has been selected; otherwise, do nothing
    if(is.null(input$department) || input$department == "") {
      return()
    }
    
    # Assuming filteredData() properly filters the spatial data
    selectedDepartment <- filteredData()
    
    # Check if selectedDepartment has more than one row
    if(nrow(selectedDepartment) > 1) {
      bbox <- st_bbox(selectedDepartment)
      min_long <- as.numeric(bbox["xmin"])
      max_long <- as.numeric(bbox["xmax"])
      min_lat <- as.numeric(bbox["ymin"])
      max_lat <- as.numeric(bbox["ymax"])
      
      # You might want to use these variables inside fitBounds
      leafletProxy("map", session) %>%
        fitBounds(lng1 = min_long, lat1 = min_lat, lng2 = max_long, lat2 = max_lat)
    }
  })
  
  
  # Update map zoom on localisation chosen
  observeEvent(input$localisation, {
    # Ensure a location has been selected; otherwise, do nothing
    if(is.null(input$localisation) || input$localisation == "") {
      return()
    }
    
    # Assuming filteredData() properly filters the spatial data
    selectedLocation <- filteredData() %>%
      filter(nom == input$localisation)
    
    # Check if selectedDepartment has more than one row
    if(nrow(selectedLocation) > 0) {
      bbox <- st_bbox(selectedLocation)
      min_long <- as.numeric(bbox["xmin"])
      max_long <- as.numeric(bbox["xmax"])
      min_lat <- as.numeric(bbox["ymin"])
      max_lat <- as.numeric(bbox["ymax"])
      
      # You might want to use these variables inside fitBounds
      leafletProxy("map", session) %>%
        fitBounds(lng1 = min_long, lat1 = min_lat, lng2 = max_long, lat2 = max_lat)
    }
  })

  
  observeEvent(input$btnInfo, {
    # Filter the dataset for the selected indicateur
    selectedIndicateurInfo <- filteredIndicators() %>%
      filter(Indicateur == input$indicateur) %>%
      select(Description) %>%
      .[[1]] # Assuming 'Description' is the column with the information text
    # Use showModal to display the information
    if (!is.null(selectedIndicateurInfo) && selectedIndicateurInfo != "") {
      showModal(modalDialog(
        title = "Informations sur l'indicateur",
        selectedIndicateurInfo, # This displays the description text
        easyClose = TRUE,
        footer = modalButton("Fermer")
      ))
    } else {
      showModal(modalDialog(
        title = "Information non disponible",
        "Aucune information disponible pour cet indicateur.",
        easyClose = TRUE,
        footer = modalButton("Fermer")
      ))
    }
  })
  
}

shinyApp(ui, server)
