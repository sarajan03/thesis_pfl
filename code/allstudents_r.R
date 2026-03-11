library(dplyr)
library(tidyr)
library(readr)

# 1. Read the means CSV
all_students_means <- read.csv(
  "/Users/sushmitarajan/Documents/GitHub/thesis_pfl/data/all_students_r.csv",
  stringsAsFactors = FALSE,
  skip = 8
) %>%
  select(
    year = Year,
    jurisdiction = Jurisdiction,
    score_all_students = Average.scale.score
  ) %>%
  mutate(score_all_students = as.numeric(score_all_students)) %>%
  filter(!is.na(score_all_students))

# 2. Read the SD CSV
all_students_sd <- read.csv(
  "/Users/sushmitarajan/Documents/GitHub/thesis_pfl/data/all_students_sd_r.csv",
  stringsAsFactors = FALSE,
  skip = 8
) %>%
  select(
    year = Year,
    jurisdiction = Jurisdiction,
    sd_all_students = `Standard.deviation`
  ) %>%
  filter(year %in% c("2003","2005","2007","2009","2011","2013","2015","2017","2019","2022")) %>%
  mutate(sd_all_students = as.numeric(sd_all_students)) %>%
  filter(!is.na(sd_all_students))

# 3. Merge means and SD into one dataset
all_students_wide_r <- all_students_means %>%
  left_join(all_students_sd, by = c("year", "jurisdiction"))

# 4. View result
head(all_students_wide_r)