if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyr, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata,readr)

econ_disadv_m.raw <- read.csv("/Users/sushmitarajan/Documents/GitHub/thesis_pfl/data/economically_disadvantaged_m.csv", stringsAsFactors = FALSE, skip = 8)

# Select and rename columns
econ_disadv_m_clean <- econ_disadv_m.raw %>%
  select(Year, Jurisdiction, Economically.disadvantaged.status, Average.scale.score) %>%
  rename(
    year = Year,
    jurisdiction = Jurisdiction,
    econ_disadv_m = Economically.disadvantaged.status,
    econ_disadv_scale_score = Average.scale.score
  )

econ_disadv_m_clean <- econ_disadv_m_clean %>%
  mutate(
    econ_disadv_m = case_when(
      econ_disadv_m == "Economically disadvantaged" ~ "econ_disadv",
      econ_disadv_m == "Not economically disadvantaged" ~ "not_econ_disadv",
      TRUE ~ "information_not_available"
    )
  )
# Clean text and convert scores to numeric
econ_disadv_m_clean <- econ_disadv_m_clean %>%
  mutate(
    econ_disadv_m = str_trim(econ_disadv_m),                        # remove leading/trailing spaces
    econ_disadv_scale_score = as.numeric(econ_disadv_scale_score)    # convert to numeric
  )

# Remove rows with missing scores
econ_disadv_m_clean <- econ_disadv_m_clean %>%
  filter(!is.na(econ_disadv_scale_score))

# Pivot data wider: one column per category
econ_disadv_wide <- econ_disadv_m_clean %>%
  pivot_wider(
    id_cols = c(year, jurisdiction),
    names_from = econ_disadv_m,
    values_from = econ_disadv_scale_score,
    names_prefix = "score_"
  )

