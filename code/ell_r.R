if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyr, dplyr, stringr, readr)

# Read raw data
ell_r.raw <- read.csv(
  "/Users/sushmitarajan/Documents/GitHub/thesis_pfl/data/ell_r.csv",
  stringsAsFactors = FALSE,
  skip = 8
)

# Select and rename columns
ell_r_clean <- ell_r.raw %>%
  select(
    Year,
    Jurisdiction,
    Status.as.English.learner..2.categories,
    Average.scale.score
  ) %>%
  rename(
    year = Year,
    jurisdiction = Jurisdiction,
    ell_status = Status.as.English.learner..2.categories,
    ell_scale_score = Average.scale.score
  )

# Recode ELL categories
ell_r_clean <- ell_r_clean %>%
  mutate(
    ell_status = case_when(
      ell_status == "ELL" ~ "ell",
      ell_status == "Not ELL" ~ "not_ell",
      TRUE ~ NA_character_
    ),
    ell_scale_score = as.numeric(ell_scale_score)
  )

# Remove rows with missing scores
ell_r_clean <- ell_r_clean %>%
  filter(!is.na(ell_scale_score))

# Pivot wider
ell_wide_r <- ell_r_clean %>%
  pivot_wider(
    id_cols = c(year, jurisdiction),
    names_from = ell_status,
    values_from = ell_scale_score,
    names_prefix = "score_"
  )

# View result
ell_wide_r
