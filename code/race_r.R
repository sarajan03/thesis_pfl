# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyr, dplyr, stringr, readr)

# Load the raw data
race_r.raw <- read.csv("/Users/sushmitarajan/Documents/GitHub/thesis_pfl/data/race_r.csv",
                       stringsAsFactors = FALSE)
# Remove completely empty columns
race_r.raw <- race_r.raw[, colSums(!is.na(race_r.raw) & race_r.raw != "") > 0]

# Assign clean column names manually
colnames(race_r.raw) <- c("year", "jurisdiction", "race", "scale_score")

# Remove empty rows
race_r_clean <- race_r.raw %>%
  filter(!is.na(year) & year != "", !is.na(jurisdiction) & jurisdiction != "")

# Clean race text and convert scores
race_r_clean <- race_r_clean %>%
  mutate(
    race = str_trim(race),
    scale_score = as.numeric(gsub("[^0-9\\.]", "", scale_score))
  ) %>%
  filter(!is.na(scale_score))

# Pivot to wide
race_wide_r <- race_r_clean %>%
  pivot_wider(
    id_cols = c(year, jurisdiction),
    names_from = race,
    values_from = scale_score,
    names_prefix = "score_"
  )

head(race_wide_r)