library(ggplot2)
library(dplyr)


# Step 1: Aggregate by year and control_group
plot_data <- master_panel_final %>%
  group_by(year, control_group) %>%
  summarise(
    avg_score = mean(score_all_students, na.rm = TRUE),
    .groups = "drop"
  )

# Step 2: Create the plot
plot1<- ggplot(plot_data, aes(x = year, y = avg_score, color = control_group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "red") +
  annotate("text", x = 2014, y = max(plot_data$avg_score) + 1, 
           label = "CA-PFL Policy Cohort Reaches 4th Grade", 
           color = "red", angle = 0, vjust = 1, hjust = 0) +
  scale_color_manual(
    name = "Group Type",  # Legend title
    values = c(
      "California"   = "black",
      "Non-PFL States" = "purple",
      "Large States" = "brown",
      "Neighbors"    = "blue"

    ),
    labels = c(
      "California"   = "California (Treated)",
      "Large States" = "Large States (Control)",
      "Neighbors"    = "Neighboring States (Control)",
      "Non-PFL States" = "Non-PFL States (Control)"
    )
  ) +
  labs(
    title = "Parallel Trends",
    x = "Year",
    y = "Average Score for All Students"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    legend.position = "bottom"
  )
print(plot1)
# Step 3: Save as LaTeX figure
# Save plot as PDF
ggsave(
  filename = "parallel_trends.pdf",  # file name
  plot = plot1,                          # plot object
  width = ,                         # width in inches
  height = 4,                        # height in inches
  units = "in"
)
library(ggplot2)
library(dplyr)


# Step 1: Aggregate by year and control_group
plot_data <- master_panel_final %>%
  group_by(year, control_group) %>%
  summarise(
    avg_score = mean(score_all_students, na.rm = TRUE),
    .groups = "drop"
  )

# Step 2: Create the plot
plot1<- ggplot(plot_data, aes(x = year, y = avg_score, color = control_group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "red") +
  annotate("text", x = 2014, y = max(plot_data$avg_score) + 1, 
           label = "CA-PFL Policy Cohort Reaches 4th Grade", 
           color = "red", angle = 0, vjust = 1, hjust = 0) +
  scale_color_manual(
    name = "Group Type",  # Legend title
    values = c(
      "California"   = "black",
      "Non-PFL States" = "purple",
      "Large States" = "brown",
      "Neighbors"    = "blue"
      
    ),
    labels = c(
      "California"   = "California (Treated)",
      "Large States" = "Large States (Control)",
      "Neighbors"    = "Neighboring States (Control)",
      "Non-PFL States" = "Non-PFL States (Control)"
    )
  ) +
  labs(
    title = "Parallel Trends",
    x = "Year",
    y = "Average Score for All Students"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    legend.position = "bottom"
  )
print(plot1)
# Step 3: Save as LaTeX figure
# Save plot as PDF
ggsave(
  filename = "parallel_trends.pdf",  # file name
  plot = plot1,                          # plot object
  width = ,                         # width in inches
  height = 4,                        # height in inches
  units = "in"
)
library(ggplot2)
library(dplyr)


# Step 1: Aggregate by year and control_group
plot_data <- master_panel_final %>%
  group_by(year, control_group) %>%
  summarise(
    avg_score = mean(score_all_students, na.rm = TRUE),
    .groups = "drop"
  )

# Step 2: Create the plot
plot1<- ggplot(plot_data, aes(x = year, y = avg_score, color = control_group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "red") +
  annotate("text", x = 2014, y = max(plot_data$avg_score) + 1, 
           label = "CA-PFL Policy Cohort Reaches 4th Grade", 
           color = "red", angle = 0, vjust = 1, hjust = 0) +
  scale_color_manual(
    name = "Group Type",  # Legend title
    values = c(
      "California"   = "black",
      "Non-PFL States" = "purple",
      "Large States" = "brown",
      "Neighbors"    = "blue"
      
    ),
    labels = c(
      "California"   = "California (Treated)",
      "Large States" = "Large States (Control)",
      "Neighbors"    = "Neighboring States (Control)",
      "Non-PFL States" = "Non-PFL States (Control)"
    )
  ) +
  labs(
    title = "Parallel Trends",
    x = "Year",
    y = "Average Score for All Students"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    legend.position = "bottom"
  )
print(plot1)
# Step 3: Save as LaTeX figure
# Save plot as PDF
ggsave(
  filename = "parallel_trends.pdf",  # file name
  plot = plot1,                          # plot object
  width = ,                         # width in inches
  height = 4,                        # height in inches
  units = "in"
)

# subgroup facet plot 

plot_data_subgroups <- master_panel_final %>%
  select(year, control_group, score_econ_disadv, score_Hispanic, score_Black, score_ell) %>%
  pivot_longer(
    cols = starts_with("score_"),      # all your subgroup score columns
    names_to = "subgroup",
    values_to = "avg_score"
  ) %>%
  group_by(year, control_group, subgroup) %>%
  summarise(
    avg_score = mean(avg_score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    subgroup = recode(subgroup,
                      "score_econ_disadv" = "Economically Disadvantaged",
                      "score_Hispanic" = "Hispanic",
                      "score_Black" = "Black",
                      "score_ell" = "English Language Learners")
  )
plot_subgroups <- ggplot(plot_data_subgroups, aes(x = year, y = avg_score, color = control_group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "red") +
  scale_color_manual(
    name = "Group Type",
    values = c(
      "California"     = "black",
      "Non-PFL States" = "purple",
      "Large States"   = "brown",
      "Neighbors"      = "blue"
    ),
    labels = c(
      "California"     = "California (Treated)",
      "Large States"   = "Large States (Control)",
      "Neighbors"      = "Neighboring States (Control)",
      "Non-PFL States" = "Non-PFL States (Control)"
    )
  ) +
  facet_wrap(~ subgroup, scales = "free_y") +  # each subgroup gets its own panel
  labs(
    title = "Parallel Trends by Subgroup",
    x = "Year",
    y = "Average Score"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    legend.position = "bottom"
  )

print(plot_subgroups)

