library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

#1.
master_panel_final_r <- master_panel_clean_r %>%
  select(-score_ai_an, -score_two_plus) %>%
  mutate(
    is_california = jurisdiction == "California",
    is_large_state = jurisdiction %in% c("Florida","Pennsylvania","Texas"),
    is_neighbor = jurisdiction %in% c("Arizona","Oregon","Nevada"),
    is_nonPFL = jurisdiction != "California"  # all states except CA
  )

# Now calculate summary stats for each group
summary_stats_r <- bind_rows(
  master_panel_final_r %>% filter(is_california) %>% mutate(control_group = "California"),
  master_panel_final_r %>% filter(is_large_state) %>% mutate(control_group = "Large States"),
  master_panel_final_r %>% filter(is_neighbor) %>% mutate(control_group = "Neighbors"),
  master_panel_final_r %>% filter(is_nonPFL) %>% mutate(control_group = "Non-PFL States")
) %>%
  group_by(control_group, post) %>%
  summarise(
    across(all_of(score_cols),
           list(
             mean = ~mean(.x, na.rm = TRUE),
             sd   = ~sd(.x, na.rm = TRUE),
             min  = ~min(.x, na.rm = TRUE),
             max  = ~max(.x, na.rm = TRUE),
             n    = ~sum(!is.na(.x))
           ),
           .names = "{.col}__{.fn}"
    ),
    .groups = "drop"
  ) %>%
  mutate(period = ifelse(post == 1, "Post", "Pre"))
# 3. Create the Main Table (Mean, SD, Min, Max)
main_table_r <- summary_stats_r %>%
  select(-post) %>%
  pivot_longer(cols = starts_with("score_"), names_to = c("Variable", "Stat"), names_sep = "__") %>%
  filter(Stat != "n") %>% # Remove N from the main metrics section
  mutate(Variable = case_when(
    Variable == "score_all_students" ~ "All Students",
    Variable == "score_White" ~ "White",
    Variable == "score_Black" ~ "Black",
    Variable == "score_Hispanic" ~ "Hispanic",
    Variable == "score_asian_pi" ~ "Asian / NHPI",
    Variable == "score_ell" ~ "ELL",
    Variable == "score_not_ell" ~ "Not ELL",
    Variable == "score_econ_disadv" ~ "Econ Disadv",
    Variable == "score_not_econ_disadv" ~ "Not Econ Disadv",
    TRUE ~ Variable
  )) %>%
  pivot_wider(names_from = c(control_group, period, Stat), values_from = value)

# 4. Create the Observations (N) Row
# We use 'score_all_students__n' to represent the sample size for that group/year
n_row_r <- summary_stats_r %>%
  select(control_group, period, score_all_students__n) %>%
  mutate(
    Stat = "mean",
    value = score_all_students__n
  ) %>%
  select(-score_all_students__n) %>%
  pivot_wider(
    names_from  = c(control_group, period, Stat),
    values_from = value,
    values_fill = NA
  ) %>%
  mutate(
    Variable = "Observations (n)"
  )


#5. 
first_output_r <- bind_rows(main_table_r, n_row_r) %>%
  select(Variable, 
         matches("California_Pre"), matches("California_Post"),
         matches("Non-PFL States_Pre"), matches("Non-PFL States_Post"))

first_output_r<- first_output_r %>%
  mutate(across(where(is.numeric), ~round(., 0))) %>%
  mutate(across(everything(), ~as.character(.))) %>%
  mutate(across(everything(), ~replace_na(., "")))


first_output_r %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    digits = 0,
    col.names = c("Student Group", rep(c("Mean", "SD", "Min", "Max"), 4)),
    caption = "Performance Summary and Observations by Group and Period"
  ) %>%
  kable_styling(latex_options = c("scale_down", "hold_position")) %>%
  add_header_above(c(" " = 1, "Pre CA-PFL" = 4, "Post CA-PFL" = 4, "Pre CA-PFL" = 4, "Post CA-PFL" = 4)) %>%
  add_header_above(c(" " = 1, "California" = 8, "Control Group:Non-PFL States" = 8)) %>%
  row_spec(nrow(main_table), extra_css = "border-bottom: 2px solid black;") %>%
  row_spec(nrow(final_output), bold = TRUE, background = "#eeeeee")


first_table_r <- first_output_r %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    digits = 0,
    col.names = c("Student Group", rep(c("Mean", "SD", "Min", "Max"), 4)),
    caption = "Summary Statistics Treated and Non-PFL Control",
    escape = TRUE
  ) %>%
  add_header_above(c(" " = 1,
                     "Pre CA-PFL Cohort" = 4, "Post CA-PFL Cohort" = 4,
                     "Pre CA-PFL Cohort" = 4, "Post CA-PFL Cohort" = 4)) %>%
  add_header_above(c(" " = 1,
                     "California" = 8,
                     "Control Group: Non-PFL States" = 8)) %>%
  kable_styling(latex_options = "hold_position")  # ← removed scale_down

# 2. Save it to a .tex file
save_kable(first_table_r, "first_table_r.tex")
writeLines(first_table_r, "first_table_r.tex")



second_output_r <- bind_rows(main_table_r, n_row_r) %>%
  select(Variable, 
         matches("Large States_Pre"), matches("Large States_Post"),
         matches("Neighbors_Pre"), matches("Neighbors_Post"))

second_output_r<- second_output_r %>%
  mutate(across(where(is.numeric), ~round(., 0))) %>%
  mutate(across(everything(), ~as.character(.))) %>%
  mutate(across(everything(), ~replace_na(., "")))

second_output_r %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    digits = 0,
    col.names = c("Student Group", rep(c("Mean", "SD", "Min", "Max"), 4)),
    caption = "Performance Summary and Observations by Group and Period"
  ) %>%
  kable_styling(latex_options = c("scale_down", "hold_position")) %>%
  add_header_above(c(" " = 1, "Pre CA-PFL" = 4, "Post CA-PFL" = 4, "Pre CA-PFL" = 4, "Post CA-PFL" = 4)) %>%
  add_header_above(c(" " = 1, "Control Group: Large States" = 8, "Control Group: Neighboring States" = 8)) %>%
  row_spec(nrow(main_table), extra_css = "border-bottom: 2px solid black;") %>%
  row_spec(nrow(final_output), bold = TRUE, background = "#eeeeee")

second_table_r <- second_output_r %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    digits = 0,
    col.names = c("Student Group", rep(c("Mean", "SD", "Min", "Max"), 4)),
    caption = "Summary Statistics Large States and Neighboring States Control",
    escape = TRUE
  ) %>%
  add_header_above(c(" " = 1,
                     "Pre CA-PFL Cohort" = 4, "Post CA-PFL Cohort" = 4,
                     "Pre CA-PFL Cohort" = 4, "Post CA-PFL Cohort" = 4)) %>%
  add_header_above(c(" " = 1,
                     "Control Group: Large States" = 8,
                     "Control Group: Neighbor States" = 8)) %>%
  kable_styling(latex_options = "hold_position")  # ← removed scale_down

save_kable(second_table_r, "second_table_r.tex")
writeLines(second_table_r, "second_table_r.tex")
