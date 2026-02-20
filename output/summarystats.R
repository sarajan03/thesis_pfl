library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

#1.

master_panel_final <- master_panel_clean %>%
  select(-score_ai_an, -score_two_plus) %>%
  mutate(
    is_california = jurisdiction == "California",
    
    is_large_state = jurisdiction %in% c(
      "Florida", "New York", "Pennsylvania", "Texas"
    ),
    
    is_neighbor = jurisdiction %in% c(
      "Arizona", "Oregon", "Nevada", "Washington"
    ),
    
    is_other_state = jurisdiction != "California"
  ) %>%
  pivot_longer(
    cols = starts_with("is_"),
    names_to = "control_group",
    values_to = "in_group"
  ) %>%
  filter(in_group) %>%
  mutate(
    control_group = recode(
      control_group,
      is_california  = "California",
      is_large_state = "Large States",
      is_neighbor    = "Neighbors",
      is_other_state = "Non-PFL States"
    )
  )
summary_stats <- master_panel_final %>%
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
main_table <- summary_stats %>%
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
n_row <- summary_stats %>%
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
final_output <- bind_rows(main_table, n_row) %>%
  select(Variable, 
         matches("California_Pre"), matches("California_Post"),
         matches("Non-PFL States_Pre"), matches("Non-PFL States_Post"),
         matches("Large States_Pre"), matches("Large States_Post"),
         matches("Neighbors_Pre"), matches("Neighbors_Post"))

final_output<- final_output %>%
  mutate(across(where(is.numeric), ~round(., 0))) %>%
  mutate(across(everything(), ~as.character(.))) %>%
  mutate(across(everything(), ~replace_na(., "")))


final_output%>%
  kable(
    format = "latex",
    booktabs = TRUE,
    digits = 0,
    col.names = c("Student Group", rep(c("Mean", "SD", "Min", "Max"), 8)),
    caption = "Performance Summary and Observations by Group and Period"
  ) %>%
  kable_styling(latex_options = c("scale_down", "hold_position")) %>%
  add_header_above(c(" " = 1, "Pre CA-PFL" = 4, "Post CA-PFL" = 4, "Pre CA-PFL" = 4, "Post CA-PFL" = 4, "Pre CA-PFL" = 4, "Post CA-PFL" = 4, "Pre CA-PFL" = 4, "Post CA-PFL" = 4)) %>%
  add_header_above(c(" " = 1, "California" = 8, "Non-PFL States" = 8, "Large States" = 8, "Neighboring States" = 8 )) %>%
  row_spec(nrow(main_table), extra_css = "border-bottom: 2px solid black;") %>%
  row_spec(nrow(final_output), bold = TRUE, background = "#eeeeee")

library(kableExtra)


latex_table <- final_output %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    digits = 0,
    col.names = c("Student Group", rep(c("Mean", "SD", "Min", "Max"), 8)),
    caption = "Performance Summary and Observations by Group and Period",
    escape = TRUE
  ) %>%
  kable_styling(latex_options = c("scale_down", "hold_position")) %>%
  add_header_above(c(" " = 1,
                     "Pre CA-PFL Cohort" = 4, "Post CA-PFL Cohort" = 4,   # California
                     "Pre CA-PFL Cohort" = 4, "Post CA-PFL Cohort" = 4,   # Other States
                     "Pre CA-PFL Cohort" = 4, "Post CA-PFL Cohort" = 4,   # Large States
                     "Pre CA-PFL Cohort" = 4, "Post CA-PFL Cohort" = 4)) %>% # Neighboring States
  add_header_above(c(" " = 1, 
                     "California" = 8, 
                     "Non-PFL States" = 8, 
                     "Large States" = 8, 
                     "Neighboring States" = 8)) 

# 2. Save it to a .tex file
save_kable(latex_table, "summary_table.tex")
writeLines(latex_table, "summary_table.tex")



