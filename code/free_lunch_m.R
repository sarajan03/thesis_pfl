library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# Read CSV
free_lunch_m.raw <- read.csv(
  "/Users/sushmitarajan/Documents/GitHub/thesis_pfl/data/free_lunch_m.csv",
  stringsAsFactors = FALSE,
  skip = 8
)

# Select and rename columns
free_lunch_m_clean <- free_lunch_m.raw %>%
  select(
    Year,
    Jurisdiction,
    National.School.Lunch.Program.eligibility..3.categories,
    Average.scale.score
  ) %>%
  rename(
    year = Year,
    jurisdiction = Jurisdiction,
    lunch_status = National.School.Lunch.Program.eligibility..3.categories,
    scale_score = Average.scale.score
  )

# Convert to consistent categories
free_lunch_m_clean <- free_lunch_m_clean %>%
  mutate(
    lunch_status = case_when(
      lunch_status == "Eligible" ~ "free_lunch",
      lunch_status == "Not eligible" ~ "not_free_lunch",
      TRUE ~ "information_not_available"
    ),
    scale_score = as.numeric(scale_score)  # convert scores to numeric
  ) %>%
  filter(!is.na(scale_score))  # remove rows without numeric scores

# Pivot wider: one column per category
free_lunch_wide <- free_lunch_m_clean %>%
  pivot_wider(
    id_cols = c(year, jurisdiction),
    names_from = lunch_status,
    values_from = scale_score,
    names_prefix = "score_"
  )

# View cleaned wide data
head(free_lunch_wide)
