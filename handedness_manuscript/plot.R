###############################################################
# PUBLICATION MAP OF KO BOI FIELD SITE
#
# Panel A: Existing map showing Thailand
# Panel B: Sentinel-2 close-up of Ko Boi Island
#
# Output:
#   1. TIFF at 600 dpi
#   2. PNG at 600 dpi
#   3. Vector PDF with raster satellite background
###############################################################



###############################################################
# 1. SET THE PERSONAL R PACKAGE LIBRARY
###############################################################

user_library <- "C:/Users/rohit_negi/R/win-library/4.3"

.libPaths(
  c(
    user_library,
    .libPaths()
  )
)


###############################################################
# 2. LOAD PACKAGES
###############################################################

library(rstac)
library(terra)
library(sf)
library(ggplot2)
library(ggspatial)
library(cowplot)
library(magick)


###############################################################
# 3. DEFINE FILE LOCATIONS
###############################################################

# Your existing Thailand map

thailand_map_file <-
  "C:/Users/rohit_negi/Downloads/field site map.png"


# Folder where all new files will be saved

output_folder <-
  "C:/Users/rohit_negi/Desktop/New folder"


# Create the output folder if it does not already exist

dir.create(
  output_folder,
  recursive = TRUE,
  showWarnings = FALSE
)


# Check that the Thailand map exists

if (!file.exists(thailand_map_file)) {
  stop(
    paste(
      "The Thailand map could not be found at:",
      thailand_map_file
    )
  )
}


###############################################################
# 4 COPERNICUS MAP EXTENT
###############################################################

ko_boi_bbox <- c(
  xmin = 98.425827,
  ymin = 8.012316,
  xmax = 98.662720,
  ymax = 8.271971
)

ko_boi_aoi <- sf::st_as_sfc(
  sf::st_bbox(
    ko_boi_bbox,
    crs = 4326
  )
)

print(ko_boi_bbox)

plot(
  ko_boi_aoi,
  col = "lightblue",
  border = "red",
  lwd = 2
)

###############################################################
# 5. DEFINE THE SATELLITE-IMAGE SEARCH PERIOD
###############################################################

search_start_date <- "2023-11-01"
search_end_date   <- "2026-04-30"


# The STAC catalogue requires RFC3339 date-time formatting

search_period <- paste0(
  search_start_date,
  "T00:00:00Z/",
  search_end_date,
  "T23:59:59Z"
)


print(search_period)


# Maximum reported cloud cover over the complete satellite tile

maximum_cloud_cover <- 20


###############################################################
# 6. CONNECT TO THE SENTINEL-2 CATALOGUE
###############################################################

sentinel_catalogue <- stac(
  "https://earth-search.aws.element84.com/v1"
)


###############################################################
# 7. SEARCH FOR SENTINEL-2 IMAGES
###############################################################

sentinel_search <- sentinel_catalogue |>
  stac_search(
    collections = "sentinel-2-c1-l2a",
    bbox = unname(ko_boi_bbox),
    datetime = search_period,
    limit = 100
  ) |>
  ext_query(
    "eo:cloud_cover" < maximum_cloud_cover
  ) |>
  post_request()


# Check how many scenes were found

number_of_scenes <- length(
  sentinel_search$features
)

message(
  paste(
    "Number of Sentinel-2 scenes found:",
    number_of_scenes
  )
)


if (number_of_scenes == 0) {
  stop(
    paste(
      "No Sentinel-2 scenes were found.",
      "Increase maximum_cloud_cover or change the dates."
    )
  )
}


###############################################################
# 8. CREATE A TABLE OF AVAILABLE SCENES
###############################################################

scene_information <- data.frame(
  scene_number = seq_along(
    sentinel_search$features
  ),
  
  acquisition_datetime = sapply(
    sentinel_search$features,
    function(scene) {
      scene$properties$datetime
    }
  ),
  
  cloud_cover = sapply(
    sentinel_search$features,
    function(scene) {
      scene$properties[["eo:cloud_cover"]]
    }
  ),
  
  stringsAsFactors = FALSE
)


# Extract the date

scene_information$acquisition_date <- as.Date(
  substr(
    scene_information$acquisition_datetime,
    1,
    10
  )
)


# Sort scenes from lowest to highest cloud cover

scene_information <- scene_information[
  order(
    scene_information$cloud_cover,
    scene_information$acquisition_date
  ),
]


rownames(scene_information) <- NULL


# Display the 20 apparently clearest scenes

print(
  head(
    scene_information,
    20
  )
)


# Save the complete scene table

write.csv(
  scene_information,
  file = file.path(
    output_folder,
    "Ko_Boi_available_Sentinel2_scenes.csv"
  ),
  row.names = FALSE
)

###############################################################
# 8B. CHECK COVERAGE AND MISSING DATA BY DATE
###############################################################

coverage_information <- data.frame(
  scene_number = seq_along(
    sentinel_search$features
  ),
  
  date = as.Date(
    substr(
      sapply(
        sentinel_search$features,
        function(scene) {
          scene$properties$datetime
        }
      ),
      1,
      10
    )
  ),
  
  cloud_cover = sapply(
    sentinel_search$features,
    function(scene) {
      scene$properties[["eo:cloud_cover"]]
    }
  ),
  
  missing_data = sapply(
    sentinel_search$features,
    function(scene) {
      
      value <- getElement(
        scene$properties,
        "s2:nodata_pixel_percentage"
      )
      
      if (
        is.null(value) ||
        length(value) == 0
      ) {
        NA_real_
      } else {
        as.numeric(value)
      }
    }
  )
)


###############################################################
# CHECK THE EXTRACTED INFORMATION
###############################################################

print(
  head(
    coverage_information,
    20
  )
)


###############################################################
# SUMMARIZE EACH ACQUISITION DATE
###############################################################

available_dates <- sort(
  unique(
    coverage_information$date
  )
)


date_coverage_summary <- do.call(
  rbind,
  lapply(
    available_dates,
    function(selected_date) {
      
      selected_rows <- (
        coverage_information$date == selected_date
      )
      
      selected_cloud <- coverage_information$cloud_cover[
        selected_rows
      ]
      
      selected_missing <- coverage_information$missing_data[
        selected_rows
      ]
      
      
      # Calculate the smallest cloud-cover value
      
      minimum_cloud <- if (
        all(
          is.na(selected_cloud)
        )
      ) {
        NA_real_
      } else {
        min(
          selected_cloud,
          na.rm = TRUE
        )
      }
      
      
      # Calculate the smallest missing-data value
      
      minimum_missing <- if (
        all(
          is.na(selected_missing)
        )
      ) {
        NA_real_
      } else {
        min(
          selected_missing,
          na.rm = TRUE
        )
      }
      
      
      data.frame(
        date = selected_date,
        cloud_cover = minimum_cloud,
        missing_data = minimum_missing,
        number_of_tiles = sum(
          selected_rows
        )
      )
    }
  )
)


###############################################################
# RETAIN DATES WITH AT LEAST TWO TILES
###############################################################

date_coverage_summary <- date_coverage_summary[
  date_coverage_summary$number_of_tiles >= 2,
]


###############################################################
# SORT THE DATES
###############################################################

# Dates with less missing data are placed first.
# For equal missing-data values, lower cloud cover comes first.
# Dates without missing-data information are placed last.

date_coverage_summary <- date_coverage_summary[
  order(
    is.na(
      date_coverage_summary$missing_data
    ),
    date_coverage_summary$missing_data,
    date_coverage_summary$cloud_cover
  ),
]


rownames(
  date_coverage_summary
) <- NULL


###############################################################
# DISPLAY AND SAVE THE RESULTS
###############################################################

print(
  date_coverage_summary,
  row.names = FALSE
)


write.csv(
  date_coverage_summary,
  file = file.path(
    output_folder,
    "Ko_Boi_date_coverage_summary.csv"
  ),
  row.names = FALSE
)

###############################################################
# 9. SELECT A SPECIFIC FULLER-COVERAGE DATE
###############################################################

selected_imagery_date <- as.Date(
  "2026-03-21"
)


# Find all catalogue scenes from this date

selected_date_rows <- which(
  coverage_information$date ==
    selected_imagery_date
)


if (length(selected_date_rows) == 0) {
  stop(
    paste(
      "No scenes were found for",
      selected_imagery_date
    )
  )
}


# Identify the clearest scene on this date for metadata

clearest_position <- which.min(
  coverage_information$cloud_cover[
    selected_date_rows
  ]
)


selected_scene_number <-
  coverage_information$scene_number[
    selected_date_rows[
      clearest_position
    ]
  ]


selected_scene <- sentinel_search$features[[selected_scene_number]]


selected_cloud_cover <- min(
  coverage_information$cloud_cover[
    selected_date_rows
  ],
  na.rm = TRUE
)


message(
  paste(
    "Selected acquisition date:",
    selected_imagery_date
  )
)


message(
  paste(
    "Number of available tiles:",
    length(selected_date_rows)
  )
)


message(
  paste(
    "Lowest reported cloud cover:",
    round(
      selected_cloud_cover,
      3
    ),
    "%"
  )
)

###############################################################
# 10. GET BOTH TILES FROM THE SELECTED DATE
###############################################################

same_date_indices <- selected_date_rows

same_date_scenes <- sentinel_search$features[
  same_date_indices
]

message(
  paste(
    "Number of tiles being combined:",
    length(same_date_scenes)
  )
)

if (length(same_date_scenes) < 2) {
  stop(
    "Fewer than two tiles were found for the selected date."
  )
}


###############################################################
# 11. READ THE RED BANDS FIRST
###############################################################

red_tiles <- lapply(
  same_date_scenes,
  function(scene) {
    terra::rast(
      scene$assets$red$href
    )
  }
)


# Check whether both tiles use the same CRS

tile_crs_values <- sapply(
  red_tiles,
  terra::crs
)

print(
  unique(tile_crs_values)
)


###############################################################
# 12. CROP AND MOSAIC THE TWO TILES
###############################################################

# Transform the map boundary to the CRS of the first tile

ko_boi_aoi_utm <- sf::st_transform(
  ko_boi_aoi,
  terra::crs(red_tiles[[1]])
)


# Crop each red tile before mosaicking.
# This avoids processing the complete 100 km satellite tiles.

red_tiles_cropped <- lapply(
  red_tiles,
  function(tile) {
    terra::crop(
      tile,
      terra::vect(ko_boi_aoi_utm)
    )
  }
)


# Read and crop the green tiles

green_tiles_cropped <- lapply(
  same_date_scenes,
  function(scene) {
    
    tile <- terra::rast(
      scene$assets$green$href
    )
    
    terra::crop(
      tile,
      terra::vect(ko_boi_aoi_utm)
    )
  }
)


# Read and crop the blue tiles

blue_tiles_cropped <- lapply(
  same_date_scenes,
  function(scene) {
    
    tile <- terra::rast(
      scene$assets$blue$href
    )
    
    terra::crop(
      tile,
      terra::vect(ko_boi_aoi_utm)
    )
  }
)


# Combine the two red tiles

red_band <- do.call(
  terra::mosaic,
  c(
    red_tiles_cropped,
    list(
      fun = "mean"
    )
  )
)


# Combine the two green tiles

green_band <- do.call(
  terra::mosaic,
  c(
    green_tiles_cropped,
    list(
      fun = "mean"
    )
  )
)


# Combine the two blue tiles

blue_band <- do.call(
  terra::mosaic,
  c(
    blue_tiles_cropped,
    list(
      fun = "mean"
    )
  )
)


# Combine the mosaicked colour bands

sentinel_rgb <- c(
  red_band,
  green_band,
  blue_band
)

names(sentinel_rgb) <- c(
  "red",
  "green",
  "blue"
)

print(sentinel_rgb)

###############################################################
# SAVE THE COMPLETE TWO-TILE MOSAIC
###############################################################

full_mosaic_file <- file.path(
  output_folder,
  "Ko_Boi_complete_Sentinel2_mosaic.tif"
)

terra::writeRaster(
  sentinel_rgb,
  full_mosaic_file,
  overwrite = TRUE,
  gdal = c(
    "COMPRESS=LZW",
    "TILED=YES"
  )
)

message(
  paste(
    "Complete mosaic saved at:",
    full_mosaic_file
  )
)

###############################################################
# 14. CROP THE SATELLITE IMAGE
###############################################################

ko_boi_rgb <- crop(
  sentinel_rgb,
  vect(ko_boi_aoi_utm)
)


if (ncell(ko_boi_rgb) == 0) {
  stop(
    "The cropped satellite image contains no cells."
  )
}


###############################################################
# 15. REMOVE INVALID VALUES
###############################################################

# Negative surface-reflectance values are not useful for
# this natural-colour visualization.

ko_boi_rgb[ko_boi_rgb < 0] <- NA


###############################################################
# 16. APPLY A NATURAL-COLOUR CONTRAST STRETCH
###############################################################

# This converts the reflectance values into a display range
# between 0 and 255.

ko_boi_rgb_stretched <- stretch(
  ko_boi_rgb,
  minq = 0.01,
  maxq = 0.995,
  minv = 0,
  maxv = 255
)


###############################################################
# 17. DISPLAY A QUALITY-CONTROL PREVIEW
###############################################################

plotRGB(
  ko_boi_rgb_stretched,
  r = 1,
  g = 2,
  b = 3,
  scale = 255,
  axes = TRUE
)


# This is only a preliminary preview.
#
# It will not yet contain the scale bar, compass or Thailand inset.
#
# If the island is covered by cloud, change selected_rank in
# Section 9 and rerun from Section 9 onward.


###############################################################
# 18. CONVERT THE RASTER FOR GGPLOT2
###############################################################

satellite_df <- as.data.frame(
  ko_boi_rgb_stretched,
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


# Keep all colour values within the permitted RGB range

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


# Convert the three bands into display colours

# Identify pixels containing valid values in all three bands

valid_pixels <- complete.cases(
  satellite_df[
    c(
      "red",
      "green",
      "blue"
    )
  ]
)


# Create an empty colour column

satellite_df$colour <- NA_character_


# Convert only valid pixels into RGB colours

satellite_df$colour[valid_pixels] <- rgb(
  red = satellite_df$red[valid_pixels],
  green = satellite_df$green[valid_pixels],
  blue = satellite_df$blue[valid_pixels],
  maxColorValue = 255
)

###############################################################
# 19. CREATE THE KO BOI MAP
###############################################################

ko_boi_map <- ggplot() +
  
  geom_raster(
    data = satellite_df,
    aes(
      x = x,
      y = y,
      fill = colour
    )
  ) +
  
  scale_fill_identity() +
  
  annotation_scale(
    location = "bl",
    width_hint = 0.15,
    bar_cols = c(
      "black",
      "white"
    ),
    text_cex = 0.7,
    line_width = 0.5,
    height = grid::unit(
      0.16,
      "cm"
    ),
    pad_x = grid::unit(
      0.25,
      "cm"
    ),
    pad_y = grid::unit(
      0.25,
      "cm"
    )
  ) +
  
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    height = grid::unit(
      0.9,
      "cm"
    ),
    width = grid::unit(
      0.9,
      "cm"
    ),
    pad_x = grid::unit(
      0.25,
      "cm"
    ),
    pad_y = grid::unit(
      0.25,
      "cm"
    ),
    style = north_arrow_fancy_orienteering
  ) +
  
  coord_sf(
    crs = st_crs(
      crs(ko_boi_rgb)
    ),
    expand = FALSE
  ) +
  
  labs(
    title = NULL,
    subtitle = NULL
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
      linewidth = 0.6
    )
  )


print(ko_boi_map)


###############################################################
# 20. CREATE A SQUARE THAILAND INSET
###############################################################

thailand_map_image <- image_read(
  thailand_map_file
)


# Resize and crop the regional map to a square.
# The map is not stretched.

thailand_map_square <- thailand_map_image |>
  image_resize(
    "1000x1000^"
  ) |>
  image_crop(
    geometry = "1000x1000+0+0",
    gravity = "center"
  )


###############################################################
# 21. ADD THE SQUARE INSET TO THE KO BOI MAP
###############################################################

combined_map <- ggdraw() +
  
  # Main Ko Boi map
  
  draw_plot(
    ko_boi_map,
    x = 0,
    y = 0,
    width = 1,
    height = 1
  ) +
  
  # Square Thailand inset in the upper-right
  
  draw_image(
    thailand_map_square,
    x = 0.74,
    y = 0.74,
    width = 0.23,
    height = 0.23
  ) +
  
  # Border around the inset
  
  draw_grob(
    grid::rectGrob(
      gp = grid::gpar(
        col = "black",
        fill = NA,
        lwd = 1
      )
    ),
    x = 0.74,
    y = 0.74,
    width = 0.23,
    height = 0.23
  )


print(combined_map)

###############################################################
# 22. EXPORT A 600-DPI TIFF
###############################################################

tiff_output <- file.path(
  output_folder,
  "Ko_Boi_publication_map_600dpi.tiff"
)


ggsave(
  filename = tiff_output,
  plot = combined_map,
  width = 160,
  height = 160,
  units = "mm",
  dpi = 600,
  compression = "lzw",
  bg = "white",
  limitsize = FALSE
)


###############################################################
# 23. EXPORT A 600-DPI PNG
###############################################################

png_output <- file.path(
  output_folder,
  "Ko_Boi_publication_map_600dpi.png"
)


ggsave(
  filename = png_output,
  plot = combined_map,
  width = 180,
  height = 160,
  units = "mm",
  dpi = 600,
  bg = "white",
  limitsize = FALSE
)


###############################################################
# 24. EXPORT A PDF
###############################################################

pdf_output <- file.path(
  output_folder,
  "Ko_Boi_publication_map.pdf"
)


ggsave(
  filename = pdf_output,
  plot = combined_map,
  width = 180,
  height = 160,
  units = "mm",
  device = cairo_pdf,
  bg = "white",
  limitsize = FALSE
)


###############################################################
# 25. SAVE THE SATELLITE-IMAGE INFORMATION
###############################################################

imagery_information <- data.frame(
  satellite = "Sentinel-2",
  product = "Collection 1 Level-2A surface reflectance",
  acquisition_date = selected_imagery_date,
  reported_tile_cloud_cover = selected_cloud_cover,
  western_boundary = ko_boi_bbox["xmin"],
  southern_boundary = ko_boi_bbox["ymin"],
  eastern_boundary = ko_boi_bbox["xmax"],
  northern_boundary = ko_boi_bbox["ymax"]
)


write.csv(
  imagery_information,
  file = file.path(
    output_folder,
    "Ko_Boi_selected_imagery_information.csv"
  ),
  row.names = FALSE
)


###############################################################
# 26. LIST THE CREATED FILES
###############################################################

created_files <- list.files(
  output_folder,
  pattern = "Ko_Boi",
  full.names = TRUE
)


print(created_files)


message(
  paste(
    "TIFF saved at:",
    tiff_output
  )
)


message(
  paste(
    "PNG saved at:",
    png_output
  )
)


message(
  paste(
    "PDF saved at:",
    pdf_output
  )
)


###############################################################
# END OF SCRIPT
###############################################################
