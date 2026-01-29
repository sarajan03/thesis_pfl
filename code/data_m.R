library(dplyr)
library(tidyr)
library(knitr)

# Start with all students as the base
master_panel <- all_students_wide %>%
  left_join(econ_disadv_wide, by = c("year", "jurisdiction")) %>%
  left_join(free_lunch_wide, by = c("year", "jurisdiction")) %>%
  left_join(race_wide, by = c("year", "jurisdiction")) %>%
  left_join(ell_wide, by = c("year", "jurisdiction"))


#clean to remove info not available students
master_panel_clean <- master_panel %>%
  # Remove columns that are placeholders / unknown
  select(-starts_with("score_information_not_available")) %>%
  # If you want, you can also drop the old 'all students' if redundant
  select(year, jurisdiction, everything())



master_panel_clean <- master_panel_clean %>%
  # Remove national aggregate and non-states
  filter(!jurisdiction %in% c("National", "DoDEA", "Puerto Rico")) %>%
  
  # Make sure year is numeric
  mutate(year = as.numeric(year)) %>%
  
  # Clean up treatment indicators
  mutate(
    treated = ifelse(jurisdiction == "California", "1", "0"),
    post = ifelse(year >= 2013, 1, 0)
  )

  #Rename columns 
  master_panel_clean <- rename(master_panel_clean,
                             score_asian_pi = `score_Asian/Pacific Islander`,
                             score_ai_an = `score_American Indian/Alaska Native`,
                             score_two_plus = `score_Two or more races`)
  master_panel_clean <- master_panel_clean %>%
    mutate(
      treated = as.numeric(treated),
      post = as.numeric(post),
      DiD = treated * post
    )

  