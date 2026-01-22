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

