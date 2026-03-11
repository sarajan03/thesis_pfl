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

# Read SD file
econ_disadv_sd <- read.csv(
  "/Users/sushmitarajan/Documents/GitHub/thesis_pfl/data/econ_disadv_sd_r.csv",
  stringsAsFactors = FALSE,
  skip = 8
)

# Select and rename columns
econ_disadv_sd <- econ_disadv_sd %>%
  select(Year, Jurisdiction, Economically.disadvantaged.status, Standard.deviation) %>%
  rename(
    year = Year,
    jurisdiction = Jurisdiction,
    econ_disadv_m = Economically.disadvantaged.status,
    sd_value = Standard.deviation
  )

# Recode categories the same way as the mean dataset
econ_disadv_sd <- econ_disadv_sd %>%
  mutate(
    econ_disadv_m = case_when(
      econ_disadv_m == "Economically disadvantaged" ~ "econ_disadv",
      econ_disadv_m == "Not economically disadvantaged" ~ "not_econ_disadv",
      TRUE ~ "information_not_available"
    ),
    econ_disadv_m = str_trim(econ_disadv_m),
    sd_value = as.numeric(sd_value)
  ) %>%
  filter(!is.na(sd_value))

# Pivot SD wide
econ_disadv_sd_wide <- econ_disadv_sd %>%
  pivot_wider(
    id_cols = c(year, jurisdiction),
    names_from = econ_disadv_m,
    values_from = sd_value,
    names_prefix = "sd_"
  )

# Merge means and SD
econ_disadv_wide_r <- econ_disadv_wide_r %>%
  left_join(econ_disadv_sd_wide, by = c("year", "jurisdiction"))

# View result
head(econ_disadv_wide_r)