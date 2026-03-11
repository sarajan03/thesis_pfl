# -------------------------------
# 1. SD columns in your master_panel_final
# -------------------------------
sd_columns <- c("sd_all_students", "sd_econ_disadv", "sd_Hispanic", "sd_Black", "sd_ell")

# Compute unweighted pooled SDs
pooled_sds <- sapply(sd_columns, function(col) {
  values <- master_panel_final_r[[col]]
  values <- values[!is.na(values)]
  sqrt(mean(values^2))   # unweighted pooled SD
})

# -------------------------------
# 2. Mapping from outcome names to SD columns
# -------------------------------
sd_mapping <- c(
  "lhs: score_all_students" = "sd_all_students",
  "lhs: score_econ_disadv"  = "sd_econ_disadv",
  "lhs: score_Hispanic"      = "sd_Hispanic",
  "lhs: score_Black"         = "sd_Black",
  "lhs: score_ell"           = "sd_ell"
)

# -------------------------------
# 3. Function to extract DiD coefficients from a list of models
# -------------------------------
extract_did_table <- function(group) {
  did_values <- sapply(group, function(model) coef(model)["DiD"])
  outcomes <- names(group)
  data.frame(Outcome = outcomes, DiD = did_values, row.names = NULL)
}

# -------------------------------
# 4. Function to compute Cohen's d from DiD table
# -------------------------------
compute_cohens_d <- function(did_table, pooled_sds, sd_mapping) {
  sapply(did_table$Outcome, function(outcome) {
    beta <- did_table$DiD[did_table$Outcome == outcome]
    sd_col <- sd_mapping[outcome]
    beta / pooled_sds[sd_col]
  })
}

# -------------------------------
# 5. Process each control group
# -------------------------------
groups <- list(
  Non_PFL = subgroup_models_r,
  Large_States = subgroup_large_r,
  Neighbor_States = subgroup_neighbors_r
)

# Initialize output table
cohens_d_all <- data.frame(Outcome = names(subgroup_models_r))

# Loop through groups
for (grp_name in names(groups)) {
  did_table <- extract_did_table(groups[[grp_name]])
  cohens_d_all[[grp_name]] <- compute_cohens_d(did_table, pooled_sds, sd_mapping)
}

# -------------------------------
# 6. Save in latex
library(kableExtra)

# 1. Clean and relabel outcomes
cohens_d_clean <- cohens_d_all
cohens_d_clean$Outcome <- c(
  "All Students Score",
  "Economically Disadvantaged Score",
  "Hispanic Score",
  "Black Score",
  "English Language Learner Score"
)

# 2. Round effect sizes for nicer display
cohens_d_clean[, 2:4] <- round(cohens_d_clean[, 2:4], 3)

# 3. Generate LaTeX table (clean, no shading)
latex_table <- kable(
  cohens_d_clean,
  format = "latex",
  booktabs = TRUE,
  caption = "Cohen's d Effect Sizes by Outcome and Control Group",
  label = "tab:cohens_d",
  align = c("l", "c", "c", "c")
) %>%
  kable_styling(latex_options = c("hold_position")) %>%  # keep position, no gray
  add_header_above(c(" " = 1, "Effect Size (Cohen's d)" = 3)) %>%
  row_spec(0, bold = TRUE)  # bold header row

# 4. Save to .tex file
sink("effect_size_r.tex")
cat(latex_table)
sink()


