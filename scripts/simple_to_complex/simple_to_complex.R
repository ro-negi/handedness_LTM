### 
#Project - Handedness in long-tailed macaques
# Author - Rohit Negi
###

# Required libraries 

library(tidyverse)

# Read the file - 

xdata <- readr::read_csv("clean_data/2024.03.12 - 2025.03.27_simple_to_complex_clean_RN.csv")

str(xdata)

# Drop column 

xdata <- xdata |>
  select(-c (focal.id.old, comments, time))
