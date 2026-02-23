library(dplyr)
library(tidyr)
library(knitr)

# Start with all students as the base
master_panel_r <- all_students_wide_r %>%
  left_join(econ_disadv_wide_r, by = c("year", "jurisdiction")) %>%
  left_join(race_wide_r, by = c("year", "jurisdiction")) %>%
  left_join(ell_wide_r, by = c("year", "jurisdiction"))


#clean to remove info not available students
master_panel_clean_r <- master_panel_r %>%
  # Remove columns that are placeholders / unknown
  select(-starts_with("score_information_not_available")) %>%
  # If you want, you can also drop the old 'all students' if redundant
  select(year, jurisdiction, everything())



master_panel_clean_r <- master_panel_clean_r %>%
  # Remove national aggregate and non-states
  filter(!jurisdiction %in% c("DoDEA", "Puerto Rico","New Jersey", "New York", "Rhode Island", "Washington")) %>%
  
  # Make sure year is numeric
  mutate(year = as.numeric(year)) %>%
  
  # Clean up treatment indicators
  mutate(
    treated = ifelse(jurisdiction == "California", "1", "0"),
    post = ifelse(year >= 2014, 1, 0)
  )

#Rename columns 
master_panel_clean_r <- rename(master_panel_clean_r,
                             score_asian_pi = `score_Asian/Pacific Islander`,
                             score_ai_an = `score_American Indian/Alaska Native`,
                             score_two_plus = `score_Two or more races`)
master_panel_clean_r <- master_panel_clean_r %>%
  mutate(
    treated = as.numeric(treated),
    post = as.numeric(post),
    DiD = treated * post
  )

