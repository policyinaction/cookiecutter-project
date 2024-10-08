# IN THIS SCRIPT WE WANT TO GENERATE A SPATIAL POINT DATA FRAME FROM ONE OR MORE RASTER FILES
# Install packages
library(raster)
library(sp)
library(tmap)
library(sf)
library(dplyr)
library(here)

# Read all Raster Files
path <- here("Indicators/Tree Cover/Copernicus Satellite Data/DATA")
files <- list.files(path, pattern="*.tif$", full.names=TRUE)

############################################################################################################################################

# EXTRACTING EACH PIXEL OF THE RASTER FILE AND MERGING SOME OF THEM FOR SMALLER RESOLUTION AND LESS COMPUTING POWER

# Initialize an empty list to store points
all_points <- list()


# Process Each Raster File
for (file in files) {
  r <- raster(file)
  
  # Calculate New Resolution and Resample
  target_factor = 5000
  new_res = sqrt(target_factor) * res(r)[1]  # Assuming square pixels
  resampled_raster <- aggregate(r, fact = sqrt(target_factor), fun = mean)
  
  # Filter out values above 100 (which correspond to 'sea' pixels we don't care about) - this line may depend on raster type and value label
  resampled_raster[resampled_raster > 100] <- NA 
  
  # Convert Resampled Raster to Points
  points <- rasterToPoints(resampled_raster, na.rm = TRUE)
  
  # Store the points
  all_points[[file]] <- points
}


# Combine all points into one data frame
combined_points <- do.call(rbind, all_points)


# Create a Spatial Points Data Frame
sp_points <- SpatialPointsDataFrame(combined_points, data.frame(value = combined_points[, 3]),
                                    proj4string = crs(r))


# Plotting (if one wishes to check for the final spatial object visualisation)
tm <- tm_shape(sp_points) +
  tm_dots(size = 0.1, col = "value", palette = c("#ffffff", "#006400"), style="fixed", breaks=seq(0, 100, by=10)) +
  tm_layout(frame = FALSE, bg.color = "#add8e6")
print(tm)


# # Save our spatial point data frame in our directory
# save_path = here("Indicators/Tree Cover/Workflow/tree_cover_sp_data.rds")
# saveRDS(object = sp_points, file = save_path)

############################################################################################################################################

