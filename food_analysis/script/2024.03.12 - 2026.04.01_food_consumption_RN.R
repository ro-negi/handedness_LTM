install.packages(c(
  "dplyr",
  "tidyr",
  "readr",
  "stringr",
  "lubridate",
  "ggplot2",
  "scales"
))

# ============================================================
# FOOD-RELATED TIME BUDGET PIE CHART
# ============================================================
#
# This script:
#   1. calculates the duration of each behavioural record;
#   2. links consecutive Search records to the next feeding record;
#   3. excludes Drink and artificial NOT VISIBLE padding time;
#   4. classifies foods into eight initial categories;
#   5. calculates food-time percentages separately for each monkey;
#   6. averages percentages across confirmed monkeys;
#   7. combines categories below 10% into Other foods; and
#   8. creates one journal-style pie chart.
#
# Install packages once if needed:
#
# install.packages(c(
#   "dplyr",
#   "tidyr",
#   "readr",
#   "stringr",
#   "lubridate",
#   "scales"
# ))


# Required libraries ----------------------------------------------------------

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(scales)


# File locations --------------------------------------------------------------

project_root <- "C:/Users/rohit_negi/Desktop/Nutrition_RN"


input_file <- file.path(
  project_root,
  "analysis",
  "2025.12.01 - 2026.04.01_focal_data_cleaned_final_RN.csv"
)


output_dir <- file.path(
  project_root,
  "analysis",
  "food_time_budget"
)


if (!dir.exists(output_dir)) {
  
  dir.create(
    output_dir,
    recursive = TRUE
  )
}


# Read data -------------------------------------------------------------------

df <- read_csv(
  input_file,
  
  show_col_types = FALSE,
  
  progress = FALSE,
  
  na = c(
    "",
    "NA",
    "N/A",
    "NaN",
    "nan",
    "None",
    "NULL",
    "."
  ),
  
  locale = locale(
    encoding = "Windows-1252"
  )
)


names(df) <- str_trim(
  names(df)
)


required_columns <- c(
  "Focal ID",
  "focal_id",
  "Date (Capture local)",
  "Time (Capture local)",
  "Behavior category",
  "Foraging behaviours",
  "Food with tools",
  "Shellfish species with tool",
  "Food",
  "Fruit/nut species",
  "Insect species",
  "Shellfish species"
)


missing_columns <- setdiff(
  required_columns,
  names(df)
)


if (length(missing_columns) > 0) {
  
  stop(
    paste0(
      "The following required columns are missing: ",
      
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  )
}


# Build timestamps and calculate row durations --------------------------------

parse_focal_datetime <- function(
    date_value,
    time_value
) {
  
  suppressWarnings(
    
    parse_date_time(
      paste(
        as.character(date_value),
        as.character(time_value)
      ),
      
      orders = c(
        "mdy HMS",
        "mdy HM",
        "ymd HMS",
        "ymd HM",
        "dmy HMS",
        "dmy HM"
      ),
      
      tz = "UTC",
      
      quiet = TRUE
    )
  )
}


df <- df %>%
  mutate(
    .original_row = row_number(),
    
    .capture_dt = parse_focal_datetime(
      `Date (Capture local)`,
      `Time (Capture local)`
    )
  ) %>%
  
  arrange(
    focal_id,
    .capture_dt,
    .original_row
  ) %>%
  
  group_by(focal_id) %>%
  
  mutate(
    .next_dt = lead(
      .capture_dt
    ),
    
    .next_behavior_category = lead(
      `Behavior category`
    ),
    
    duration_seconds = as.numeric(
      difftime(
        .next_dt,
        .capture_dt,
        units = "secs"
      )
    )
  ) %>%
  
  ungroup()


if (any(is.na(df$.capture_dt))) {
  
  warning(
    sum(is.na(df$.capture_dt)),
    " rows have an invalid date-time value."
  )
}


# Prepare food variables ------------------------------------------------------

optional_food_columns <- c(
  "Other food with tools",
  "Fruit/nut species with tool",
  "Other fruits/nuts with tool",
  "Other shellfish with tool",
  "Other food",
  "Other fruits/nuts",
  "Other insects",
  "Other shellfish",
  "Leaves licking"
)


for (column_name in optional_food_columns) {
  
  if (!column_name %in% names(df)) {
    
    df[[column_name]] <- NA_character_
  }
}


clean_text <- function(x) {
  
  str_to_lower(
    str_trim(
      coalesce(
        as.character(x),
        ""
      )
    )
  )
}


df <- df %>%
  mutate(
    .food_with_tools = clean_text(
      `Food with tools`
    ),
    
    .other_food_with_tools = clean_text(
      `Other food with tools`
    ),
    
    .fruit_tool = clean_text(
      `Fruit/nut species with tool`
    ),
    
    .other_fruit_tool = clean_text(
      `Other fruits/nuts with tool`
    ),
    
    .shellfish_tool = clean_text(
      `Shellfish species with tool`
    ),
    
    .other_shellfish_tool = clean_text(
      `Other shellfish with tool`
    ),
    
    .food = clean_text(
      Food
    ),
    
    .other_food = clean_text(
      `Other food`
    ),
    
    .fruit = clean_text(
      `Fruit/nut species`
    ),
    
    .other_fruit = clean_text(
      `Other fruits/nuts`
    ),
    
    .insect = clean_text(
      `Insect species`
    ),
    
    .other_insect = clean_text(
      `Other insects`
    ),
    
    .shellfish = clean_text(
      `Shellfish species`
    ),
    
    .other_shellfish = clean_text(
      `Other shellfish`
    ),
    
    .leaves_licking = clean_text(
      `Leaves licking`
    ),
    
    .all_food_text = paste(
      .food_with_tools,
      .other_food_with_tools,
      .fruit_tool,
      .other_fruit_tool,
      .shellfish_tool,
      .other_shellfish_tool,
      .food,
      .other_food,
      .fruit,
      .other_fruit,
      .insect,
      .other_insect,
      .shellfish,
      .other_shellfish,
      
      sep = " | "
    )
  )


# Food-classification patterns ------------------------------------------------

plant_part_pattern <- paste0(
  "\\b(",
  "leaves|leaf|root|shoot|flower|",
  "tree bark|bark|twig|seed|seeds|",
  "spiny plant shoot",
  ")\\b"
)


insect_pattern <- paste0(
  "mealy|insect|caterpillar|",
  "ant|termite|mantis|larv"
)


crustacean_pattern <- paste0(
  "crab|crustacean|sea slater|",
  "isopod|shrimp"
)


other_shellfish_pattern <- paste0(
  "snail|clam|nerita|atrina|",
  "periwinkle|lunella|mollusc|",
  "polinices|chicoreus|clypeomorus|",
  "thais|laevistrombus|acantho|",
  "cylindrical long transparent|",
  "big black periwinkle"
)


# Classify food in feeding rows -----------------------------------------------

df <- df %>%
  mutate(
    food_category = case_when(
      
      # Oyster includes:
      # Oyster sessile (attached)
      # Oyster sessile
      # Oyster non-sessile
      # Oyster
      # Oyster debris
      
      str_detect(
        .all_food_text,
        "oyster"
      ) ~ "Oyster",
      
      
      # Insects, including mealybugs.
      
      .food == "insects" |
        
        .insect != "" |
        
        .other_insect != "" |
        
        (
          .leaves_licking == "yes" &
            
            str_detect(
              .all_food_text,
              insect_pattern
            )
        ) ~ "Insects",
      
      
      # Crabs and other crustaceans.
      
      str_detect(
        paste(
          .food_with_tools,
          .food,
          .shellfish,
          .other_food,
          
          sep = " | "
        ),
        
        crustacean_pattern
      ) ~ "Crabs and other crustaceans",
      
      
      # Leaves and plant parts.
      
      str_detect(
        .food,
        plant_part_pattern
      ) ~ "Leaves and plant parts",
      
      
      # Fruits and nuts.
      
      .food == "fruit/nut species" |
        
        .food_with_tools ==
        "fruit/nut species" ~ "Fruits and nuts",
      
      
      # Other shellfish.
      
      .food_with_tools %in% c(
        "shellfish species",
        "snail"
      ) |
        
        .food %in% c(
          "shellfish species",
          "clam"
        ) |
        
        .shellfish_tool != "" |
        
        .other_shellfish_tool != "" |
        
        .shellfish != "" |
        
        .other_shellfish != "" |
        
        str_detect(
          .all_food_text,
          other_shellfish_pattern
        ) ~ "Other shellfish",
      
      
      # Unknown food.
      
      str_detect(
        .all_food_text,
        "unknown"
      ) |
        
        str_remove_all(
          .all_food_text,
          "[ |]"
        ) == "" ~ "Unknown",
      
      
      # Remaining uncommon foods.
      
      TRUE ~ "Other food"
    ),
    
    
    # Initially retain classifications only for feeding or
    # food-processing records.
    
    food_category = if_else(
      `Foraging behaviours` %in% c(
        "Tool assisted",
        "Without tools"
      ),
      
      food_category,
      
      NA_character_
    )
  )


# Assign Search time to the next feeding row ----------------------------------

# Example:
#
# Search -> Search -> Tool assisted: Oyster
#
# becomes:
#
# Oyster -> Oyster -> Oyster
#
# Drink and any behavioural interruption break the sequence.


df$episode_food_category <-
  df$food_category


focal_groups <- split(
  seq_len(nrow(df)),
  df$focal_id
)


for (indices in focal_groups) {
  
  future_food <- NA_character_
  
  for (i in rev(indices)) {
    
    current_foraging <-
      df$`Foraging behaviours`[i]
    
    
    if (
      !is.na(current_foraging) &&
      
      current_foraging %in% c(
        "Tool assisted",
        "Without tools"
      )
    ) {
      
      future_food <-
        df$food_category[i]
      
      
    } else if (
      !is.na(current_foraging) &&
      
      current_foraging == "Search"
    ) {
      
      if (!is.na(future_food)) {
        
        df$episode_food_category[i] <-
          future_food
      }
      
      
    } else {
      
      future_food <- NA_character_
    }
  }
}


# Retain valid food-related time ----------------------------------------------

analysis_rows <- df %>%
  filter(
    
    # Retain feeding records and resolvable Search records.
    
    !is.na(episode_food_category),
    
    
    # Exclude the generated NOT VISIBLE row.
    
    is.na(`Behavior category`) |
      `Behavior category` != "NOT VISIBLE",
    
    
    # Exclude the duration immediately before a generated
    # NOT VISIBLE endpoint.
    
    is.na(.next_behavior_category) |
      .next_behavior_category != "NOT VISIBLE",
    
    
    # Retain only valid positive durations.
    
    !is.na(duration_seconds),
    
    duration_seconds > 0
  )


# Exclude uncertain identities ------------------------------------------------

# Do not merge identities such as Yaiko? with Yaiko.
# Exclude question-mark identities from equal-weight averages.

analysis_rows_confirmed <- analysis_rows %>%
  filter(
    !str_detect(
      `Focal ID`,
      fixed("?")
    )
  )


# Calculate individual food-time percentages ---------------------------------

food_category_order <- c(
  "Oyster",
  "Fruits and nuts",
  "Insects",
  "Other shellfish",
  "Crabs and other crustaceans",
  "Leaves and plant parts",
  "Unknown",
  "Other food"
)


individual_food_time <- analysis_rows_confirmed %>%
  group_by(
    `Focal ID`,
    episode_food_category
  ) %>%
  
  summarise(
    time_seconds = sum(
      duration_seconds
    ),
    
    .groups = "drop"
  ) %>%
  
  complete(
    `Focal ID`,
    
    episode_food_category =
      food_category_order,
    
    fill = list(
      time_seconds = 0
    )
  ) %>%
  
  group_by(`Focal ID`) %>%
  
  mutate(
    total_food_time_seconds = sum(
      time_seconds
    ),
    
    individual_percent =
      100 *
      time_seconds /
      total_food_time_seconds
  ) %>%
  
  ungroup()


number_of_monkeys <- n_distinct(
  individual_food_time$`Focal ID`
)


# Calculate mean percentages across individuals -------------------------------

food_time_summary <- individual_food_time %>%
  group_by(
    episode_food_category
  ) %>%
  
  summarise(
    mean_individual_percent = mean(
      individual_percent
    ),
    
    sd = sd(
      individual_percent
    ),
    
    se = sd / sqrt(n()),
    
    .groups = "drop"
  ) %>%
  
  left_join(
    analysis_rows_confirmed %>%
      group_by(
        episode_food_category
      ) %>%
      
      summarise(
        pooled_hours =
          sum(duration_seconds) / 3600,
        
        .groups = "drop"
      ),
    
    by = "episode_food_category"
  ) %>%
  
  mutate(
    pooled_percent =
      100 *
      pooled_hours /
      sum(pooled_hours),
    
    episode_food_category = factor(
      episode_food_category,
      levels = food_category_order
    )
  ) %>%
  
  arrange(
    episode_food_category
  )


# Save analysis tables --------------------------------------------------------

write_csv(
  individual_food_time,
  
  file.path(
    output_dir,
    "individual_food_time_percentages.csv"
  )
)


write_csv(
  food_time_summary,
  
  file.path(
    output_dir,
    "food_time_budget_summary.csv"
  )
)


message(
  "Confirmed monkeys included: ",
  number_of_monkeys
)


message(
  "Usable classified time: ",
  
  round(
    sum(
      analysis_rows_confirmed$duration_seconds
    ) / 3600,
    
    1
  ),
  
  " hours"
)


print(food_time_summary)


# -------------------------------------------------------------------
# PLOT SETTINGS
# -------------------------------------------------------------------

# Combine every original category below this percentage into
# one category called Other foods.

combine_below_percent <- 10


# Labels printed outside the pie.

pie_labels <- c(
  "Oyster" = "Oyster",
  "Fruits and nuts" = "Fruits and nuts",
  "Insects" = "Insects",
  "Other shellfish" = "Other shellfish",
  "Other foods" = "Other foods"
)


# Colourblind-safe blue palette based on the Okabe–Ito blue
# used in the handedness plot.

pie_colors <- c(
  "Oyster" = "#0072B2",
  "Fruits and nuts" = "#2D8CC4",
  "Insects" = "#57A6D4",
  "Other shellfish" = "#84BFE1",
  "Other foods" = "#B9DAEE"
)


# Panel label.
# Set to "" to remove it.

panel_label <- ""


# Pie orientation.

pie_start_angle <- 90
pie_clockwise <- TRUE


# Print a percentage inside every slice.

minimum_label_percent <- 0
percentage_decimal_places <- 1


# Match dimensions and resolution of the handedness plot.

figure_width <- 10
figure_height <- 7
figure_dpi <- 300


# Match global text size and transparent background.

base_size_global <- 20
figure_background <- "transparent"


# Font sizes relative to the global base size.

outside_label_cex <- 1.00
percent_label_cex <- 0.85
panel_label_cex <- 1.00


# Label positions.

outside_label_radius <- 1.10
percent_label_radius <- 0.63


# Slice borders.

slice_border_colour <- "white"
slice_border_width <- 1


# Output filename.

figure_name <-
  "food_time_budget_detailed.png"


# Combine categories below 10% ------------------------------------------------

retained_categories <- food_time_summary %>%
  filter(
    mean_individual_percent >=
      combine_below_percent
  ) %>%
  
  pull(
    episode_food_category
  ) %>%
  
  as.character()


plot_category_order <- c(
  food_category_order[
    food_category_order %in%
      retained_categories
  ],
  
  "Other foods"
)


plot_data <- food_time_summary %>%
  mutate(
    category = if_else(
      mean_individual_percent <
        combine_below_percent,
      
      "Other foods",
      
      as.character(
        episode_food_category
      )
    )
  ) %>%
  
  group_by(category) %>%
  
  summarise(
    mean_individual_percent = sum(
      mean_individual_percent
    ),
    
    .groups = "drop"
  ) %>%
  
  mutate(
    category = factor(
      category,
      levels = plot_category_order
    ),
    
    outside_text = unname(
      pie_labels[
        as.character(category)
      ]
    ),
    
    percent_text = if_else(
      mean_individual_percent >=
        minimum_label_percent,
      
      paste0(
        number(
          mean_individual_percent,
          
          accuracy =
            10^(-percentage_decimal_places)
        ),
        
        "%"
      ),
      
      ""
    )
  ) %>%
  
  arrange(category)


message(
  "Combined pie percentages sum to: ",
  
  round(
    sum(
      plot_data$mean_individual_percent
    ),
    
    6
  ),
  
  "%"
)


print(plot_data)


# Save the five-category data used in the figure.

write_csv(
  plot_data %>%
    mutate(
      category = as.character(category)
    ),
  
  file.path(
    output_dir,
    "food_time_budget_combined_for_plot.csv"
  )
)


# Calculate label positions ---------------------------------------------------

pie_values <-
  plot_data$mean_individual_percent


pie_names <-
  plot_data$outside_text


pie_slice_colours <- unname(
  pie_colors[
    as.character(
      plot_data$category
    )
  ]
)


pie_fractions <-
  pie_values /
  sum(pie_values)


pie_mid_fraction <-
  cumsum(pie_fractions) -
  pie_fractions / 2


if (pie_clockwise) {
  
  pie_mid_angle <-
    pie_start_angle *
    pi / 180 -
    2 *
    pi *
    pie_mid_fraction
  
} else {
  
  pie_mid_angle <-
    pie_start_angle *
    pi / 180 +
    2 *
    pi *
    pie_mid_fraction
}


# Coordinates for percentages inside the pie.

percent_x <-
  percent_label_radius *
  cos(pie_mid_angle)

percent_y <-
  percent_label_radius *
  sin(pie_mid_angle)


# Coordinates for category labels outside the pie.

outside_x <-
  outside_label_radius *
  cos(pie_mid_angle)

outside_y <-
  outside_label_radius *
  sin(pie_mid_angle)


# Text positions:
#
# 2 = left
# 3 = above
# 4 = right

outside_position <- ifelse(
  cos(pie_mid_angle) > 0.05,
  
  4,
  
  ifelse(
    cos(pie_mid_angle) < -0.05,
    
    2,
    
    3
  )
)


# Function for drawing the pie -----------------------------------------------

draw_food_pie <- function() {
  
  par(
    mar = c(
      1.5,
      3.5,
      1.5,
      3.5
    ),
    
    family = "sans",
    
    ps = base_size_global,
    
    fg = "black",
    
    col = "black",
    
    xpd = TRUE
  )
  
  
  pie(
    x = pie_values,
    
    labels = NA,
    
    col = pie_slice_colours,
    
    border = slice_border_colour,
    
    lwd = slice_border_width,
    
    clockwise = pie_clockwise,
    
    init.angle = pie_start_angle,
    
    radius = 1
  )
  
  
  # Category labels outside the pie.
  
  text(
    x = outside_x,
    
    y = outside_y,
    
    labels = pie_names,
    
    cex = outside_label_cex,
    
    pos = outside_position,
    
    offset = 0.2,
    
    family = "sans",
    
    col = "black"
  )
  
  
  # Percentages inside the pie.
  
  text(
    x = percent_x,
    
    y = percent_y,
    
    labels = plot_data$percent_text,
    
    cex = percent_label_cex,
    
    family = "sans",
    
    col = "black"
  )
  
  
  # Panel label.
  
  if (panel_label != "") {
    
    text(
      x = 0,
      
      y = 1.19,
      
      labels = panel_label,
      
      cex = panel_label_cex,
      
      family = "sans",
      
      col = "black"
    )
  }
}


# Save plot -------------------------------------------------------------------

png(
  filename = file.path(
    output_dir,
    figure_name
  ),
  
  width = figure_width,
  
  height = figure_height,
  
  units = "in",
  
  res = figure_dpi,
  
  pointsize = base_size_global,
  
  bg = figure_background
)


draw_food_pie()


dev.off()


# Display the same figure in RStudio.

draw_food_pie()
