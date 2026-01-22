

library(dplyr)

# treated: identifies California (policy state)
# post: defines post-policy period based on
# first 4th-grade cohort exposed at birth (2013+)

all_students_wide <- all_students_wide %>%
  mutate(
    treated = ifelse(jurisdiction == "California", "California", "Other States"),
    post    = ifelse(year >= 2013, "Post-2013", "Pre-2013")
  )
summary_overall <- all_students_wide %>%
  summarise(
    mean_score = mean(score_all_students, na.rm = TRUE),  # Average score
    sd_score   = sd(score_all_students, na.rm = TRUE),    # Std. dev. across state-years
    min_score  = min(score_all_students, na.rm = TRUE),   # Minimum observed score
    max_score  = max(score_all_students, na.rm = TRUE),   # Maximum observed score
    n_obs      = n()                                      # Number of state-year observations
  )
summary_overall

# Compares outcomes before and after the
# first treated birth cohorts reach 4th grade
summary_pre_post <- all_students_wide %>%
  group_by(post) %>%                                     # Group by pre/post period
  summarise(
    mean_score = mean(score_all_students, na.rm = TRUE), # Mean score in each period
    sd_score   = sd(score_all_students, na.rm = TRUE),   # SD across states in each period
    min_score  = min(score_all_students, na.rm = TRUE),  # Min score in each period
    max_score  = max(score_all_students, na.rm = TRUE),  # Max score in each period
    n_obs      = n(),                                     # Number of observations
    .groups = "drop"
  )
summary_pre_post

# Compares California to all other states,
# pooling across years
summary_treated_control <- all_students_wide %>%
  group_by(treated) %>%                                  # Group by treatment status
  summarise(
    mean_score = mean(score_all_students, na.rm = TRUE), # Mean score by group
    sd_score   = sd(score_all_students, na.rm = TRUE),   # SD by group
    min_score  = min(score_all_students, na.rm = TRUE),  # Minimum score
    max_score  = max(score_all_students, na.rm = TRUE),  # Maximum score
    n_obs      = n(),                                     # Number of observations
    .groups = "drop"
  )
summary_treated_control 

summary_did_2x2 <- all_students_wide %>%
  group_by(treated, post) %>%                             # CA vs others x pre/post
  summarise(
    mean_score = mean(score_all_students, na.rm = TRUE), # Cell mean
    sd_score   = sd(score_all_students, na.rm = TRUE),   # Cell SD
    n_obs      = n(),                                     # Observations per cell
    .groups = "drop"
  )
summary_did_2x2 