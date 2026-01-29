# ===============================
# Load libraries
# ===============================
library(dplyr)
library(tidyr)
library(ggplot2)
library(fixest)
library(kableExtra) # optional for nicer tables

# ===============================
# Data prep
# ===============================

# Ensure numeric variables are numeric
master_panel_clean <- master_panel_clean %>%
  mutate(
    year = as.numeric(year),
    treated = as.numeric(treated),
    post = as.numeric(post),
    DiD = treated * post  # interaction for diff-in-diff
  )

# List of subgroups
subgroups <- c(
  "score_all_students", "score_White", "score_Black", "score_Hispanic",
  "score_asian_pi", "score_ai_an", "score_two_plus",
  "score_econ_disadv", "score_not_econ_disadv",
  "score_free_lunch", "score_not_free_lunch",
  "score_ell", "score_not_ell"
)

# ===============================
# Summary statistics for all students
# ===============================
plot_data <- master_panel_clean %>%
  group_by(year, treated) %>%
  summarise(mean_score = mean(score_all_students, na.rm = TRUE), .groups = "drop")

# Print table
plot_data %>%
  mutate(treated = ifelse(treated == 1, "California", "No Policy Change States")) %>%
  kable(caption = "Average Test Scores Over Time: California vs Untreated States") %>%
  kable_styling(full_width = FALSE)

# ===============================
# Plot: all students over time
# ===============================
ggplot(plot_data, aes(x = year, y = mean_score, color = factor(treated), group = factor(treated))) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2013, linetype = "dashed", color = "red", size = 1) +
  scale_color_manual(
    breaks = c(0,1),
    values = c("gray", "blue"),
    labels = c("No Policy Change States", "California")
  ) +
  labs(
    title = "Average Test Score Over Time: California vs Untreated States",
    x = "Year",
    y = "Average Score (All Students)",
    color = "State Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10)
  )

# ===============================
# Summary stats for all subgroups
# ===============================
plot_data_sub <- master_panel_clean %>%
  pivot_longer(
    cols = all_of(subgroups),
    names_to = "subgroup",
    values_to = "score"
  ) %>%
  group_by(year, treated, subgroup) %>%
  summarise(mean_score = mean(score, na.rm = TRUE), .groups = "drop")

# Example: plot for subgroups (optional: can facet_wrap)
ggplot(plot_data_sub, aes(x = year, y = mean_score, color = factor(treated), group = factor(treated))) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ subgroup, scales = "free_y") +
  geom_vline(xintercept = 2013, linetype = "dashed", color = "red") +
  scale_color_manual(
    breaks = c(0,1),
    values = c("gray", "blue"),
    labels = c("No Policy Change States", "California")
  ) +
  labs(
    title = "Average Test Scores by Subgroup: Treated vs Untreated States",
    x = "Year",
    y = "Average Score",
    color = "State Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )