###
# Project - Handedness in long-tailed macaques
# Author - Rohit Negi
###

# Required libraries ----------------------------------------------------------
library(tidyverse)
library(rmcorr)

# Read the file ---------------------------------------------------------------
# Original cleaned file (if needed later)
# ydata <- readr::read_csv("clean_data/2024.03.12 - 2025.03.27_simple_to_complex_clean_RN.csv")

# Working dataset for simple-to-complex analysis
xdata <- readr::read_csv(
  "clean_data/2024.03.12 - 2025.03.27_simple_to_complex_RN_2025.12.09.csv"
)

str(xdata)

# -------------------------------------------------------------------
# STEP 1: Arrange data chronologically BEFORE assigning uni.id
# This ensures uni.id = 1 is the earliest trial in the entire study.
# We sort by date, then time, then subj.id as a tie-breaker.
# -------------------------------------------------------------------

xdata <- xdata %>% 
  arrange(date, time, subj.id)

# -------------------------------------------------------------------
# STEP 2: Create a new column 'uni.id' that identifies unique trials.
# We generate it row-by-row using dplyr::lag(). A new uni.id starts
# when ANY required condition fails. If focal.id.old is NA, then we
# ignore it and compare only subj.id, gutt.score, date, and time gap.
# -------------------------------------------------------------------

xdata <- xdata %>% 
  mutate(
    # Calculate time difference from previous row (in minutes)
    time_diff = as.numeric(difftime(time, lag(time), units = "mins")),
    
    # TRUE when focal.id.old is missing in either current or previous row
    focal_missing = is.na(focal.id.old) | is.na(lag(focal.id.old)),
    
    # Condition for staying in the SAME trial when focal.id.old is present
    same_trial_with_focal =
      !focal_missing &
      subj.id == lag(subj.id) &
      focal.id.old == lag(focal.id.old) &
      gutt.score == lag(gutt.score) &
      date == lag(date) &
      time_diff < 60,
    
    # Condition for staying in SAME trial when focal.id.old is missing
    same_trial_without_focal =
      focal_missing &
      subj.id == lag(subj.id) &
      gutt.score == lag(gutt.score) &
      date == lag(date) &
      time_diff < 60,
    
    # We stay in same uni.id if EITHER condition is TRUE
    same_trial = same_trial_with_focal | same_trial_without_focal,
    
    # Create uni.id by cumulative sum of when new trials start
    uni.id = cumsum(if_else(is.na(same_trial) | !same_trial, 1, 0))
  )

# -------------------------------------------------------------------
# STEP 3: Collapse rows belonging to the same uni.id
# For every trial (uni.id), sum the hand-use columns while keeping
# the identifying variables from the first row of each trial.
# xdata now becomes a trial-level dataset (one row per uni.id).
# -------------------------------------------------------------------

xdata <- xdata %>% 
  group_by(uni.id) %>% 
  summarise(
    subj.id       = first(subj.id),
    focal.id.old  = first(focal.id.old),
    date          = first(date),
    time          = first(time),
    gutt.score    = first(gutt.score),
    
    # Hand-use totals across all rows within a trial
    right.hand    = sum(right.hand, na.rm = TRUE),
    left.hand     = sum(left.hand,  na.rm = TRUE),
    both.hands    = sum(both.hands, na.rm = TRUE),
    total         = sum(total, na.rm = TRUE),
    
    .groups = "drop"
  )

# -------------------------------------------------------------------
# STEP 4: Move uni.id so it appears immediately after focal.id.old
# -------------------------------------------------------------------

xdata <- xdata %>% 
  relocate(uni.id, .after = focal.id.old)

# -------------------------------------------------------------------
# STEP 5: Distribute 'both.hands' counts into right.hand and left.hand
# -------------------------------------------------------------------
# For each trial, if the monkey used both hands, we add that count to
# both right.hand and left.hand so that two-handed uses contribute to
# both hand totals. After this adjustment, we remove both.hands.
# -------------------------------------------------------------------

xdata <- xdata %>% 
  mutate(
    right.hand = right.hand + both.hands,
    left.hand  = left.hand  + both.hands
  ) %>% 
  select(-both.hands)   # Drop the both.hands column


# Optional: code for dropping extra columns if needed later
# xdata <- xdata %>%
#   select(-c(focal.id.old, comments))


# -------------------------------------------------------------------
# STEP 6: Summaries of subjects and trial replicates per task
# -------------------------------------------------------------------
# Goal:
# 1) For each task (gutt.score), how many UNIQUE subjects (subj.id)?
# 2) For each task x subject, how many trials (uni.id)?
# 3) For each task, summary of replicates per subject (mean/median/min/max).
# -------------------------------------------------------------------

# 6.1 Number of unique subjects per task ------------------------------------
unique_subjects_per_task <- xdata %>%
  group_by(gutt.score) %>% 
  summarise(
    n_unique_subjects = n_distinct(subj.id),
    .groups = "drop"
  )

# Inspect in console or viewer if needed
unique_subjects_per_task
# View(unique_subjects_per_task)


# 6.2 Number of replicates (trials) per subject per task --------------------
# Here we count how many distinct uni.id each subject has for each task.
replicates_per_subject <- xdata %>%
  group_by(gutt.score, subj.id) %>%
  summarise(
    n_trials = n_distinct(uni.id),
    .groups = "drop"
  )

# Inspect
replicates_per_subject
# View(replicates_per_subject)


# -------------------------------------------------------------------
# STEP 7: Create CSV with per-task subject participation and replicates
# -------------------------------------------------------------------
# Goal: For each task (gutt.score), list every subject (subj.id) and
# the number of trials (n_trials = distinct uni.id). Sort by task, and
# within each task order subjects from most to least trials.
# -------------------------------------------------------------------

# 7.1 Compute number of trials per subject per task -------------------
replicates_per_subject <- xdata %>%
  group_by(gutt.score, subj.id) %>%
  summarise(
    n_trials = n_distinct(uni.id),
    .groups = "drop"
  )

# 7.2 Sort by task, then by number of trials (descending), then by subj.id
replicates_per_subject_sorted <- replicates_per_subject %>%
  arrange(gutt.score, desc(n_trials), subj.id)

# 7.3 Save to CSV in clean_data/ --------------------------------------
write_csv(
  replicates_per_subject_sorted,
  "clean_data/2025.12.09_simple_to_complex_replicates_per_subject_RN.csv"
)


# -------------------------------------------------------------------
# STEP 8: Remove subject-task combinations with only ONE trial
# -------------------------------------------------------------------
# rmcorr requires at least 2 observations per subject for the variable
# being analysed. Here we count how many trials each subject has within
# each task (gutt.score). If a subject has only 1 trial in a given task,
# we remove only that specific row, but we keep their other trials in 
# other tasks.
# -------------------------------------------------------------------

# 8.1 Count number of trials per subject per task
trial_counts <- xdata %>%
  group_by(gutt.score, subj.id) %>%
  summarise(
    n_trials = n_distinct(uni.id),
    .groups = "drop"
  )

# 8.2 Identify valid subject-task combinations (≥ 2 trials)
valid_combinations <- trial_counts %>%
  filter(n_trials >= 2)

# 8.3 Keep only rows belonging to valid subject-task pairs
xdata_filtered <- xdata %>%
  inner_join(valid_combinations,
             by = c("gutt.score", "subj.id"))

# Inspect

xdata_filtered

# View(xdata_filtered)
write_csv(
  xdata_filtered,
  "clean_data/2025.12.09_simple_to_complex_filtered_min2trials_RN.csv"
)

# -------------------------------------------------------------------
# STEP 9: Create table of subjects × tasks with trial counts
# -------------------------------------------------------------------
# This table will show for each subject how many trials they have in 
# each gutt.score (task). It will help evaluate how much data is lost 
# by filtering for rmcorr.
# -------------------------------------------------------------------

# Count trials per subject per task
subject_task_counts <- xdata %>% 
  group_by(subj.id, gutt.score) %>% 
  summarise(
    n_trials = n_distinct(uni.id),
    .groups = "drop"
  )

# Convert to wide format: one row per subject, columns = tasks
subject_task_wide <- subject_task_counts %>%
  pivot_wider(
    names_from = gutt.score,
    names_prefix = "task_",
    values_from = n_trials,
    values_fill = 0    # if no trials for a task, show 0
  ) %>%
  # Count number of tasks with ≥1 trial
  mutate(
    total_tasks_present = rowSums(across(starts_with("task_")) > 0)
  )

# Save CSV so you can inspect manually
write_csv(
  subject_task_wide,
  "clean_data/2025.12.09_simple_to_complex_subject_task_counts_RN.csv"
)
# -------------------------------------------------------------------
# STEP X: Create table of subjects × tasks with trial counts
# -------------------------------------------------------------------
# This table will show for each subject how many trials they have in 
# each gutt.score (task). It will help evaluate how much data is lost 
# by filtering for rmcorr.
# -------------------------------------------------------------------

# Count trials per subject per task
subject_task_counts <- xdata %>% 
  group_by(subj.id, gutt.score) %>% 
  summarise(
    n_trials = n_distinct(uni.id),
    .groups = "drop"
  )

# Convert to wide format: one row per subject, columns = tasks
subject_task_wide <- subject_task_counts %>%
  pivot_wider(
    names_from = gutt.score,
    names_prefix = "task_",
    values_from = n_trials,
    values_fill = 0    # if no trials for a task, show 0
  ) %>%
  # Count number of tasks with ≥1 trial
  mutate(
    total_tasks_present = rowSums(across(starts_with("task_")) > 0)
  )

# Save CSV so you can inspect manually
write_csv(
  subject_task_wide,
  "clean_data/2025.12.09_simple_to_complex_subject_task_counts_RN.csv"
)

# -------------------------------------------------------------------
# STEP X: Create table of subjects × tasks with trial counts
# -------------------------------------------------------------------
# This table will show for each subject how many trials they have in 
# each gutt.score (task). It will help evaluate how much data is lost 
# by filtering for rmcorr.
# -------------------------------------------------------------------

# Count trials per subject per task
subject_task_counts <- xdata %>% 
  group_by(subj.id, gutt.score) %>% 
  summarise(
    n_trials = n_distinct(uni.id),
    .groups = "drop"
  )

# Convert to wide format: one row per subject, columns = tasks
subject_task_wide <- subject_task_counts %>%
  pivot_wider(
    names_from = gutt.score,
    names_prefix = "task_",
    values_from = n_trials,
    values_fill = 0    # if no trials for a task, show 0
  ) %>%
  # Count number of tasks with ≥1 trial
  mutate(
    total_tasks_present = rowSums(across(starts_with("task_")) > 0)
  )

# Save CSV so you can inspect manually
write_csv(
  subject_task_wide,
  "clean_data/2025.12.09_simple_to_complex_subject_task_counts_RN.csv"
)

# -------------------------------------------------------------------
# STEP 10: Remove subjects who appear in ONLY ONE task
# -------------------------------------------------------------------
# For rmcorr between gutt.score (task complexity) and CV, subjects must 
# have variation in task complexity. Subjects who appear in only ONE 
# gutt.score level cannot contribute to within-subject correlation and 
# must therefore be removed.
# -------------------------------------------------------------------

# 10.1 Count number of distinct tasks per subject
task_counts <- xdata_filtered %>%
  group_by(subj.id) %>%
  summarise(
    n_tasks = n_distinct(gutt.score),
    .groups = "drop"
  )

# 10.2 Identify subjects who appear in TWO OR MORE tasks
valid_subjects_rmcorr <- task_counts %>%
  filter(n_tasks >= 2) %>%
  pull(subj.id)

# 10.3 Keep only these subjects in the dataset
xdata_rmcorr <- xdata_filtered %>%
  filter(subj.id %in% valid_subjects_rmcorr)


# -------------------------------------------------------------------
# STEP: Recompute subject × task trial counts AFTER filtering
# -------------------------------------------------------------------
# This table lets us verify how many tasks each subject still participates in
# after removing subjects with only 1 trial in a task.
# -------------------------------------------------------------------

# Count trials per subject per task again using xdata_filtered
#subject_task_counts2 <- xdata_filtered %>% 
#  group_by(subj.id, gutt.score) %>% 
#  summarise(
#    n_trials = n_distinct(uni.id),
#    .groups = "drop"
#  )

# Convert to wide format
#subject_task_wide2 <- subject_task_counts2 %>%
#  pivot_wider(
#    names_from = gutt.score,
#    names_prefix = "task_",
#    values_from = n_trials,
#    values_fill = 0
#  ) %>%
#  mutate(
#    total_tasks_present = rowSums(across(starts_with("task_")) > 0)
#  )

# Save CSV
#write_csv(
#  subject_task_wide2,
#  "clean_data/2025.12.09_simple_to_complex_subject_task_counts_RN.csv"
#)


# -------------------------------------------------------------------
# STEP 11: Collapse to one row per subject × task (for CV / HI work)
# -------------------------------------------------------------------
# Goal:
# From the trial-level dataset xdata_rmcorr (one row per uni.id),
# create a task-level dataset with ONE row per subj.id × gutt.score.
# For each subject and task, we pool all trials by summing right.hand
# and left.hand across trials.
#
# Resulting data frame: xdata_cv
# Columns kept:
#   - subj.id
#   - gutt.score
#   - right.hand  (total across all trials in that task)
#   - left.hand   (total across all trials in that task)
# -------------------------------------------------------------------

xdata_cv <- xdata_rmcorr %>%
  group_by(subj.id, gutt.score) %>%
  summarise(
    right.hand = sum(right.hand, na.rm = TRUE),
    left.hand  = sum(left.hand,  na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  arrange(subj.id, gutt.score)


# -------------------------------------------------------------------
# STEP 11: Compute mean, std, cv, and cv_abs (absolute cv)
# Input: xdata_cv with: subj.id, gutt.score, right.hand, left.hand
# Output: xdata_cv with new columns:
#   mean_HI, std_prop, cv, cv_abs (old handedness.index)
# -------------------------------------------------------------------

xdata_cv <- xdata_cv %>%
  mutate(
    # Total events
    n_total = right.hand + left.hand,
    
    # Proportions
    p_right = right.hand / n_total,
    p_left  = left.hand / n_total,
    
    # 1. Mean handedness index
    mean_HI = (right.hand - left.hand) / n_total,
    
    # 2. Standard deviation of proportions
    variance_prop = (p_right * p_left) / n_total,
    std_prop = sqrt(variance_prop),
    
    # 3. CV = std / mean
    cv = std_prop / mean_HI,
    
    # 4. Absolute CV (no direction) – keep as cv_abs
    cv_abs = abs(cv)
  )

# -------------------------------------------------------------------
# STEP 12: Handle Inf in cv_abs by replacing with next rounded integer
# above the largest finite value
# -------------------------------------------------------------------

# find the largest finite cv_abs value
max_finite <- xdata_cv %>%
  filter(!is.infinite(cv_abs)) %>%
  summarise(max_val = max(cv_abs, na.rm = TRUE)) %>%
  pull(max_val)

# define replacement value = next rounded integer above max_finite
replacement_val <- ceiling(max_finite)

# replace Inf ONLY in cv_abs
xdata_cv <- xdata_cv %>%
  mutate(
    cv_abs = ifelse(is.infinite(cv_abs),
                    replacement_val,
                    cv_abs)
  )

# (optional) check what replacement value was used
replacement_val

# -------------------------------------------------------------------
# STEP 13: Rescale cv_abs to 0–1 handedness.index
#  - smallest cv_abs  -> handedness.index = 1 (strongest handedness)
#  - largest  cv_abs  -> handedness.index = 0 (weakest handedness)
# -------------------------------------------------------------------

cv_min <- min(xdata_cv$cv_abs, na.rm = TRUE)
cv_max <- max(xdata_cv$cv_abs, na.rm = TRUE)

xdata_cv <- xdata_cv %>%
  mutate(
    cv_norm = (cv_abs - cv_min) / (cv_max - cv_min),   # 0 = strongest, 1 = weakest
    handedness.index = 1 - cv_norm                    # 1 = strongest, 0 = weakest
  )

# Save a copy so you can inspect in Excel / CSV viewer if you want
write_csv(
  xdata_cv,
  "clean_data/2025.12.10_simple_to_complex_xdata_cv_subj_task_RN.csv"
)

## Repeated Measures Correlation (rm corr)


# Check missing values 
colSums(is.na(xdata_cv))

# rm corr using the 0–1 scaled handedness.index
rm_out <- rmcorr(
  participant = subj.id,
  measure1   = gutt.score,
  measure2   = handedness.index,
  dataset    = xdata_cv
)

rm_out









