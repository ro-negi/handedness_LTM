###############################################################
# KO BOI CROP PLAYGROUND
#
# Change only the four coordinates in Section 3.
# Run the complete script to create a new satellite preview.
###############################################################

library(
  ggplot2,
  lib.loc = user_library
)

library(
  ggspatial,
  lib.loc = user_library
)

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
  "C:/Users/rohit_negi/Desktop/field_map/",
  "Ko_Boi_multidate_median_mosaic.tif"
)

output_folder <- "C:/Users/rohit_negi/Desktop/field_map"


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
  xmin = 98.524100,
  ymin = 8.085000,
  xmax = 98.596000,
  ymax = 8.178500
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
# 10. CONVERT THE SATELLITE CROP FOR GGPLOT2
###############################################################

satellite_df <- as.data.frame(
  satellite_crop_display,
  xy = TRUE,
  na.rm = FALSE
)

names(satellite_df) <- c(
  "x",
  "y",
  "red",
  "green",
  "blue"
)


# Keep colour values within the valid RGB range.

satellite_df$red <- pmax(
  0,
  pmin(
    255,
    satellite_df$red
  )
)

satellite_df$green <- pmax(
  0,
  pmin(
    255,
    satellite_df$green
  )
)

satellite_df$blue <- pmax(
  0,
  pmin(
    255,
    satellite_df$blue
  )
)


# Convert the red, green and blue bands into display colours.

valid_pixels <- complete.cases(
  satellite_df[
    c(
      "red",
      "green",
      "blue"
    )
  ]
)

satellite_df$colour <- NA_character_

satellite_df$colour[valid_pixels] <- rgb(
  red = satellite_df$red[valid_pixels],
  green = satellite_df$green[valid_pixels],
  blue = satellite_df$blue[valid_pixels],
  maxColorValue = 255
)


###############################################################
# 11. CREATE A CLEAN MAP WITH A 1000-METRE SCALE
###############################################################

map_extent <- terra::ext(
  satellite_crop_display
)

map_xmin <- terra::xmin(map_extent)
map_xmax <- terra::xmax(map_extent)
map_ymin <- terra::ymin(map_extent)
map_ymax <- terra::ymax(map_extent)

map_width <- map_xmax - map_xmin
map_height <- map_ymax - map_ymin


###############################################################
# DEFINE THE MANUAL 1000-METRE SCALE
###############################################################

scale_length <- 1000

scale_x_end <- map_xmax - 0.09 * map_width
scale_x_start <- scale_x_end - scale_length
scale_x_middle <- scale_x_start + scale_length / 2

scale_y <- map_ymin + 0.035 * map_height
scale_tick_height <- 0.012 * map_height
scale_label_y <- scale_y + 0.028 * map_height


###############################################################
# CREATE THE MAP
###############################################################

crop_map <- ggplot() +
  
  geom_raster(
    data = satellite_df,
    aes(
      x = x,
      y = y,
      fill = colour
    )
  ) +
  
  scale_fill_identity() +
  
  # Black 1000-m scale line
  
  annotate(
    "segment",
    x = scale_x_start,
    xend = scale_x_end,
    y = scale_y,
    yend = scale_y,
    colour = "black",
    linewidth = 1.2
  ) +
  
  # Black end ticks
  
  annotate(
    "segment",
    x = c(
      scale_x_start,
      scale_x_end
    ),
    xend = c(
      scale_x_start,
      scale_x_end
    ),
    y = scale_y - scale_tick_height,
    yend = scale_y + scale_tick_height,
    colour = "black",
    linewidth = 1.2
  ) +
  
  # Single scale label
  
  annotate(
    "text",
    x = scale_x_middle,
    y = scale_label_y,
    label = "1000 m",
    colour = "black",
    fontface = "bold",
    size = 4
  ) +
  
  # Full north arrow without a background panel
  
  ggspatial::annotation_north_arrow(
    location = "tl",
    which_north = "true",
    height = grid::unit(
      1.6,
      "cm"
    ),
    width = grid::unit(
      1.6,
      "cm"
    ),
    pad_x = grid::unit(
      0.31,
      "cm"
    ),
    pad_y = grid::unit(
      0.38,
      "cm"
    ),
    style = ggspatial::north_arrow_fancy_orienteering
  ) +
  
  coord_sf(
    crs = sf::st_crs(
      terra::crs(satellite_crop_display)
    ),
    expand = FALSE
  ) +
  
  theme_void() +
  
  theme(
    plot.margin = margin(
      t = 0,
      r = 0,
      b = 0,
      l = 0
    ),
    
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.8
    )
  )

print(crop_map)


###############################################################
# 12. CREATE THE PREVIEW FILENAME
###############################################################

preview_file <- file.path(
  output_folder,
  "Ko_Boi_crop_preview_with_scale_and_north_arrow.png"
)


###############################################################
# 13. SAVE THE MAP PREVIEW
###############################################################

ggsave(
  filename = preview_file,
  plot = crop_map,
  width = 16,
  height = 20,
  units = "cm",
  dpi = 300,
  bg = "white"
)

message(
  paste(
    "Map preview saved at:",
    preview_file
  )
)

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


--------------
  
 