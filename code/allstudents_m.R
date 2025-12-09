# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyr, dplyr, stringr, readr)

# Read the CSV
all_students_m.raw <- read.csv(
  "/Users/sushmitarajan/Documents/GitHub/thesis_pfl/data/all_students_m.csv",
  stringsAsFactors = FALSE,
  skip = 8
)

# Select relevant columns and rename
all_students_m_clean <- all_students_m.raw %>%
  select(Year, Jurisdiction, `Average.scale.score`) %>%
  rename(
    year = Year,
    jurisdiction = Jurisdiction,
    score_all_students = `Average.scale.score`
  )

# Convert scores to numeric
all_students_m_clean <- all_students_m_clean %>%
  mutate(score_all_students = as.numeric(score_all_students))

# Remove rows with missing scores
all_students_m_clean <- all_students_m_clean %>%
  filter(!is.na(score_all_students))

# Keep only year, jurisdiction, and score
all_students_m_clean <- all_students_m_clean %>%
  select(year, jurisdiction, score_all_students)

