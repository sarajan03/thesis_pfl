library(dplyr)
library(tidyr)
library(readr)

# 1. Read the raw data 
# (Using text = ... for this example, replace with your file path)
all_students_raw_r <- read.csv(
  "/Users/sushmitarajan/Documents/GitHub/thesis_pfl/data/all_students_r.csv",
  stringsAsFactors = FALSE,
  skip = 8 # Adjust 'skip' based on where the actual headers start in your CSV
)

# 2. Select and rename columns
# Note: Based on your snippet, the headers are Year, Jurisdiction, and Average scale score
all_students_clean_r <- all_students_raw_r %>%
  select(
    year = Year,
    jurisdiction = Jurisdiction,
    scale_score = Average.scale.score
  )

# 3. Clean numeric scores and create the final column name
all_students_wide_r <- all_students_clean_r %>%
  mutate(
    # Convert scores to numeric (handles NA if the cell contains symbols like "‡")
    scale_score = as.numeric(as.character(scale_score))
  ) %>%
  filter(!is.na(scale_score)) %>%
  # Rename to match your master panel naming convention
  rename(score_all_students = scale_score)
all_students_wide_r