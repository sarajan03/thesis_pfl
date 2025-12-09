# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyr, dplyr, stringr, readr)

# Load the raw data
race_m.raw <- read.csv("/Users/sushmitarajan/Documents/GitHub/thesis_pfl/data/race_m.csv",
                       stringsAsFactors = FALSE)
# Remove completely empty columns
race_m.raw <- race_m.raw[, colSums(!is.na(race_m.raw) & race_m.raw != "") > 0]

# Assign clean column names manually
colnames(race_m.raw) <- c("year", "jurisdiction", "race", "scale_score")

# Remove empty rows
race_m_clean <- race_m.raw %>%
  filter(!is.na(year) & year != "", !is.na(jurisdiction) & jurisdiction != "")

# Clean race text and convert scores
race_m_clean <- race_m_clean %>%
  mutate(
    race = str_trim(race),
    scale_score = as.numeric(gsub("[^0-9\\.]", "", scale_score))
  ) %>%
  filter(!is.na(scale_score))

# Pivot to wide
race_wide <- race_m_clean %>%
  pivot_wider(
    id_cols = c(year, jurisdiction),
    names_from = race,
    values_from = scale_score,
    names_prefix = "score_"
  )

head(race_wide)