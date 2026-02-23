if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyr, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata,readr)

econ_disadv_r.raw <- read.csv("/Users/sushmitarajan/Documents/GitHub/thesis_pfl/data/economically_disadvantaged_r.csv", stringsAsFactors = FALSE, skip = 8)

# Select and rename columns
econ_disadv_r_clean <- econ_disadv_r.raw %>%
  select(Year, Jurisdiction, Economically.disadvantaged.status, Average.scale.score) %>%
  rename(
    year = Year,
    jurisdiction = Jurisdiction,
    econ_disadv_r = Economically.disadvantaged.status,
    econ_disadv_scale_score = Average.scale.score
  )

econ_disadv_r_clean <- econ_disadv_r_clean %>%
  mutate(
    econ_disadv_r = case_when(
      econ_disadv_r == "Economically disadvantaged" ~ "econ_disadv",
      econ_disadv_r == "Not economically disadvantaged" ~ "not_econ_disadv",
      TRUE ~ "information_not_available"
    )
  )
# Clean text and convert scores to numeric
econ_disadv_r_clean <- econ_disadv_r_clean %>%
  mutate(
    econ_disadv_r = str_trim(econ_disadv_r),                        # remove leading/trailing spaces
    econ_disadv_scale_score = as.numeric(econ_disadv_scale_score)    # convert to numeric
  )

# Remove rows with missing scores
econ_disadv_r_clean <- econ_disadv_r_clean %>%
  filter(!is.na(econ_disadv_scale_score))

# Pivot data wider: one column per category
econ_disadv_wide_r <- econ_disadv_r_clean %>%
  pivot_wider(
    id_cols = c(year, jurisdiction),
    names_from = econ_disadv_r,
    values_from = econ_disadv_scale_score,
    names_prefix = "score_"
  )

econ_disadv_wide_r