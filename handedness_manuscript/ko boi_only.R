###############################################################
# KO BOI CROP PLAYGROUND
#
# Change only the four coordinates in Section 3.
# Run the complete script to create a new satellite preview.
###############################################################


###############################################################
# PACKAGE LIBRARY
###############################################################

user_library <- "C:/Users/rohit_negi/R/win-library/4.3"

.libPaths(
  c(
    user_library,
    .libPaths()
  )
)


###############################################################
# LOAD THE PACKAGES
###############################################################

library(
  terra,
  lib.loc = user_library
)

library(
  sf,
  lib.loc = user_library
)


###############################################################
# 2. FILE LOCATIONS
###############################################################

mosaic_file <- paste0(
  "C:/Users/rohit_negi/Desktop/New folder/",
  "Ko_Boi_complete_Sentinel2_mosaic.tif"
)

output_folder <- "C:/Users/rohit_negi/Desktop/New folder"


if (!file.exists(mosaic_file)) {
  stop(
    paste(
      "The complete mosaic could not be found at:",
      mosaic_file
    )
  )
}


###############################################################
# 3. ENTER THE CROP COORDINATES HERE
###############################################################

# This is the only section you need to change.
#
# xmin = western longitude
# ymin = southern latitude
# xmax = eastern longitude
# ymax = northern latitude

crop_coordinates <- c(
  xmin = 98.509342,
  ymin = 8.067190,
  xmax = 98.629417,
  ymax = 8.161000
)


###############################################################
# 4. CHECK THAT THE COORDINATES ARE LOGICAL
###############################################################

if (
  crop_coordinates["xmin"] >= crop_coordinates["xmax"]
) {
  stop(
    "xmin must be smaller than xmax."
  )
}

if (
  crop_coordinates["ymin"] >= crop_coordinates["ymax"]
) {
  stop(
    "ymin must be smaller than ymax."
  )
}


###############################################################
# 5. LOAD THE COMPLETE SATELLITE MOSAIC
###############################################################

sentinel_mosaic <- terra::rast(
  mosaic_file
)

names(sentinel_mosaic) <- c(
  "red",
  "green",
  "blue"
)

print(sentinel_mosaic)


###############################################################
# 6. CREATE THE GEOGRAPHIC CROP BOUNDARY
###############################################################

crop_boundary_wgs84 <- sf::st_as_sfc(
  sf::st_bbox(
    crop_coordinates,
    crs = 4326
  )
)


# Transform the crop boundary into the mosaic CRS

crop_boundary_utm <- sf::st_transform(
  crop_boundary_wgs84,
  terra::crs(sentinel_mosaic)
)


###############################################################
# 7. CROP THE MOSAIC
###############################################################

satellite_crop <- terra::crop(
  sentinel_mosaic,
  terra::vect(crop_boundary_utm)
)


if (terra::ncell(satellite_crop) == 0) {
  stop(
    "The selected coordinates do not overlap the satellite mosaic."
  )
}


###############################################################
# 8. REMOVE INVALID REFLECTANCE VALUES
###############################################################

satellite_crop[
  satellite_crop < 0
] <- NA


###############################################################
# 9. IMPROVE THE NATURAL-COLOUR DISPLAY
###############################################################

satellite_crop_display <- terra::stretch(
  satellite_crop,
  minq = 0.01,
  maxq = 0.995,
  minv = 0,
  maxv = 255
)


###############################################################
# 10. DISPLAY THE CROP IN R
###############################################################

terra::plotRGB(
  satellite_crop_display,
  r = 1,
  g = 2,
  b = 3,
  scale = 255,
  axes = TRUE
)


###############################################################
# 11. CALCULATE PREVIEW DIMENSIONS
###############################################################

# Preserve the natural shape of the selected crop.

maximum_preview_dimension <- 2400

preview_factor <- maximum_preview_dimension / max(
  terra::ncol(satellite_crop_display),
  terra::nrow(satellite_crop_display)
)

preview_width <- round(
  terra::ncol(satellite_crop_display) *
    preview_factor
)

preview_height <- round(
  terra::nrow(satellite_crop_display) *
    preview_factor
)


message(
  paste(
    "Preview dimensions:",
    preview_width,
    "x",
    preview_height,
    "pixels"
  )
)


###############################################################
# 12. CREATE A NAME FOR THE PREVIEW
###############################################################

preview_file <- file.path(
  output_folder,
  "Ko_Boi_crop_preview.png"
)


###############################################################
# 13. SAVE THE CLEAN PREVIEW
###############################################################

png(
  filename = preview_file,
  width = preview_width,
  height = preview_height,
  units = "px",
  bg = "white"
)

par(
  mar = c(
    0,
    0,
    0,
    0
  )
)

terra::plotRGB(
  satellite_crop_display,
  r = 1,
  g = 2,
  b = 3,
  scale = 255,
  axes = FALSE,
  box = FALSE
)

dev.off()


###############################################################
# 14. SAVE THE GEOGRAPHIC CROP
###############################################################

crop_geotiff_file <- file.path(
  output_folder,
  "Ko_Boi_crop_current.tif"
)

terra::writeRaster(
  satellite_crop,
  crop_geotiff_file,
  overwrite = TRUE,
  gdal = c(
    "COMPRESS=LZW",
    "TILED=YES"
  )
)


###############################################################
# 15. PRINT THE RESULTS
###############################################################

message(
  paste(
    "Preview saved at:",
    preview_file
  )
)

message(
  paste(
    "Geographic crop saved at:",
    crop_geotiff_file
  )
)

print(crop_coordinates)