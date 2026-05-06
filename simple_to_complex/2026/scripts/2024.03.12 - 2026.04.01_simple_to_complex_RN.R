###
# Project - Handedness in long-tailed macaques
# Analysis - Simple to complex task handedness strength
# Author - Rohit Negi
###

# Required libraries ----------------------------------------------------------
library(tidyverse)
library(rmcorr)
library(ggplot2)
library(dplyr)
library(grid)

# Working directory -----------------------------------------------------------
setwd("C:/Users/rohit_negi/Desktop/New folder (2)/simple_to_complex/data/2026")

# Create output folder if needed ---------------------------------------------
if (!dir.exists("clean_data")) {
  dir.create("clean_data", recursive = TRUE)
}

# Read data -------------------------------------------------------------------
hand_sum <- readr::read_csv("2024.03.12 - 2026.04.01_simple_to_complex_RN.csv")

str(hand_sum)

# Prepare data ----------------------------------------------------------------
hand_sum <- hand_sum %>%
  mutate(
    subj.id = as.factor(subj.id),
    behavior = as.factor(behavior),
    gutt.score = as.numeric(gutt.score),
    hand.score.abs = as.numeric(hand.score.abs),
    sex = as.factor(sex)
  )

# Remove missing values -------------------------------------------------------
hand_sum_rm <- hand_sum %>%
  filter(
    !is.na(subj.id),
    !is.na(gutt.score),
    !is.na(hand.score.abs)
  )

# Keep only subjects with at least 2 task-complexity levels -------------------
subject_task_counts <- hand_sum_rm %>%
  group_by(subj.id) %>%
  summarise(
    n_tasks = n_distinct(gutt.score),
    .groups = "drop"
  )

valid_subjects <- subject_task_counts %>%
  filter(n_tasks >= 2) %>%
  pull(subj.id)

hand_sum_rm <- hand_sum_rm %>%
  filter(subj.id %in% valid_subjects)

# Save filtered dataset -------------------------------------------------------
write_csv(
  hand_sum_rm,
  "clean_data/hand_sum_rmcorr_filtered.csv"
)

# Repeated-measures correlation -----------------------------------------------
rm_out <- rmcorr(
  participant = subj.id,
  measure1   = gutt.score,
  measure2   = hand.score.abs,
  dataset    = hand_sum_rm
)

rm_out

# Save rmcorr result ----------------------------------------------------------
rm_summary <- data.frame(
  r = rm_out$r,
  df = rm_out$df,
  p = rm_out$p,
  CI.lower = rm_out$CI[1],
  CI.upper = rm_out$CI[2],
  N.subjects = length(unique(hand_sum_rm$subj.id)),
  N.rows = nrow(hand_sum_rm)
)

write_csv(
  rm_summary,
  "clean_data/hand_sum_rmcorr_result.csv"
)

# -------------------------------------------------------------------
# PLOT
# -------------------------------------------------------------------

base_size_global <- 20

theme_ro <- theme_classic(base_size = base_size_global) +
  theme(
    text = element_text(family = "sans", colour = "black"),
    
    axis.title.x = element_text(size = 20, margin = margin(t = 8)),
    axis.title.y = element_text(size = 20, margin = margin(r = 8)),
    
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    
    legend.position = "right",
    legend.title = element_text(size = 18),
    legend.text  = element_text(size = 16),
    
    legend.key.height = unit(1.5, "lines"),
    legend.key.width  = unit(1.5, "lines"),
    
    plot.background  = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    
    plot.margin = margin(10, 10, 10, 10)
  )

# Create dynamic annotation text ----------------------------------------------
r_txt <- round(rm_out$r, 2)
p_txt <- signif(rm_out$p, 2)
n_txt <- length(unique(hand_sum_rm$subj.id))

annotation_label <- paste0(
  "italic('rm corr') == ", r_txt,
  " * ',' ~ italic(p) == ", p_txt,
  " * ';' ~ italic(N) == ", n_txt
)

# Build plot ------------------------------------------------------------------

point_fill  <- "#0072B2"   # colorblind-safe blue (Okabe-Ito)
point_edge  <- "#003B5C"   # darker edge for contrast
line_col    <- "#000000"
fill_col    <- "#BDBDBD"

p <- ggplot(hand_sum_rm, aes(x = gutt.score, y = hand.score.abs)) +
  
  geom_jitter(
    width  = 0.015,
    height = 0.005,
    size   = 6,
    alpha  = 0.35,
    shape  = 21,                 # allows fill + border
    fill   = point_fill,
    colour = point_edge,
    stroke = 0.6                 # outline thickness
  ) +
  
  geom_smooth(
    method = "lm",
    se     = TRUE,
    colour = line_col,
    fill   = fill_col,
    linewidth = 1.2,
    alpha  = 0.25
  ) +
  
  coord_cartesian(ylim = c(0, 1)) +
  
  scale_x_continuous(
    breaks = c(0, 0.2, 0.4, 0.6, 0.8),
    labels = c("Rubbing", "Pounding", "Washing", "Nut-cracking", "Oysters")
  ) +
  
  labs(
    x = "Task complexity",
    y = "Handedness index"
  ) +
  
#  annotate(
#    "text",
#    x = 0.78,
#    y = 0.10,
#    label = annotation_label,
#    parse = TRUE,
#    hjust = 1,
#    vjust = 0,
#    size = 9,
#    family = "sans",
#    colour = "black"
#  ) +
  
  theme_ro

print(p)

# Save plot -------------------------------------------------------------------
ggsave(
  filename = "handedness_vs_task_complexity_abs_HI.png",
  plot     = p,
  width    = 10,
  height   = 7,
  dpi      = 300,
  bg       = "transparent"
)