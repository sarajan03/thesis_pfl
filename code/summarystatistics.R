
library(dplyr)
library(tidyr)
library(knitr)
library(ggplot2)
library(dplyr)


table_all_students <- master_panel_clean %>%
  group_by(jurisdiction) %>%
  summarise(
    mean_score = mean(score_all_students, na.rm = TRUE),
    sd_score = sd(score_all_students, na.rm = TRUE),
    min_score = min(score_all_students, na.rm = TRUE),
    max_score = max(score_all_students, na.rm = TRUE),
    .groups = "drop"
  )
#Relative California Mathematics Scores
ggplot(table_all_students, aes(x = mean_score)) +
  geom_histogram(bins = 15, fill = "gray80", color = "white") +
  geom_vline(
    data = table_all_students %>% filter(jurisdiction == "California"),
    aes(xintercept = mean_score),
    color = "red",
    linewidth = 1
  ) +
  labs(
    title = "Distribution of Mean NAEP Scores Across Jurisdictions",
    subtitle = "Red line indicates California",
    x = "Mean Score",
    y = "Number of Jurisdictions"
  ) +
  theme_minimal()

# summary of each dataset 
summary_panel <- master_panel %>%
  group_by(jurisdiction, year) %>%
  summarise(
    mean_all = mean(score_all_students, na.rm = TRUE),
    sd_all   = sd(score_all_students, na.rm = TRUE),
    min_all  = min(score_all_students, na.rm = TRUE),
    max_all  = max(score_all_students, na.rm = TRUE),
    
    mean_econ   = mean(score_econ_disadv, na.rm = TRUE),
    mean_not_econ = mean(score_not_econ_disadv, na.rm = TRUE),
    
    mean_free   = mean(score_free_lunch, na.rm = TRUE),
    mean_not_free = mean(score_not_free_lunch, na.rm = TRUE),
    
    mean_ell   = mean(score_ell, na.rm = TRUE),
    mean_not_ell = mean(score_not_ell, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  # Add simple achievement gaps for descriptive purposes
  mutate(
    gap_econ = mean_not_econ - mean_econ,
    gap_free = mean_not_free - mean_free,
    gap_ell  = mean_not_ell - mean_ell
  )

summary_panel

ca_2000 <- master_panel %>%
  filter(jurisdiction == "California", year == 2000) %>%
  pull(score_all_students)

similar_states <- master_panel %>%
  filter(year == 2000, jurisdiction != "California") %>%
  filter(abs(score_all_students - ca_2000) <= 5) %>%
  pull(jurisdiction
# Identify treatment and control states
panel_plot <- master_panel_clean %>%
  filter(jurisdiction %in% c("California", "Alabama","Alaska","Arizona","Arkansas","Florida","Georgia","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Pennsylvania","Puerto Rico","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","West Virginia","Wisconsin","Wyoming")) %>%
  mutate(group = ifelse(jurisdiction == "California", "California", "Control States"))

ggplot(panel_plot, aes(x = year, y = score_all_students, color = group)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  labs(title = "4th-Grade Mathematics Scores Over Time",
       x = "Year", y = "Average Score") +
  theme_minimal()
