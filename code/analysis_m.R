library(dplyr)
library(fixest)

master_panel_final <- master_panel_final %>%
  mutate(DiD = treated * post)

subgroup_models <- feols(
  c(score_all_students,
    score_econ_disadv,
    score_Hispanic,
    score_Black,
    score_ell) 
  ~ DiD | jurisdiction + year,
  data = master_panel_final,
  cluster = ~jurisdiction
)

large_states <- c("Florida", "Pennsylvania", "Texas")
subgroup_large <- feols(
  c(score_all_students,
    score_econ_disadv,
    score_Hispanic,
    score_Black,
    score_ell) 
  ~ DiD | jurisdiction + year,
  data = master_panel_final %>%
    filter(jurisdiction %in% c("California", large_states)),
  cluster = ~jurisdiction
)

neighbors <- c("Arizona", "Oregon", "Nevada")
subgroup_neighbors <- feols(
  c(score_all_students,
    score_econ_disadv,
    score_Hispanic,
    score_Black,
    score_ell) 
  ~ DiD | jurisdiction + year,
  data = master_panel_final %>%
    filter(jurisdiction %in% c("California", neighbors)),
  cluster = ~jurisdiction
)
etable(
  subgroup_models,
  headers = c("All Students", "Econ. Disadv.", "Hispanic", "Black", "ELL"),
  dict = c(DiD = "Post × California"),
  keep = "%DiD",
  se.below = TRUE,
  fitstat = ~ n + r2
)
etable(
  subgroup_large,
  headers = c("All Students", "Econ. Disadv.", "Hispanic", "Black", "ELL"),
  dict = c(DiD = "Post × California"),
  keep = "%DiD",
  se.below = TRUE,
  fitstat = ~ n + r2
)

etable(
  subgroup_neighbors,
  headers = c("All Students", "Econ. Disadv.", "Hispanic", "Black", "ELL"),
  dict = c(DiD = "Post × California"),
  keep = "%DiD",
  se.below = TRUE,
  fitstat = ~ n + r2
)


etable(
  subgroup_models,
  dict = c(
    score_all_students = "All Students Score",
    score_econ_disadv  = "Economically Disadvantaged Score",
    score_Hispanic     = "Hispanic Score",
    score_Black        = "Black Score",
    score_ell        = "English Language Learner Score",
    DiD                = "$\\\\beta_3$:"
  ),
  keep = "%DiD",
  se.below = TRUE,
  fitstat = ~ n + r2,
  headers = list(
    "Control Group: Non-PFL States" = as.character(1:5)  # convert to character
  ),
  tex = TRUE,                    # generate LaTeX
  file = "did_other_states.tex"  # save to file,
)

did_large_states  <- 
etable(
  subgroup_large,
  dict = c(
    score_all_students = "All Students Score",
    score_econ_disadv  = "Economically Disadvantaged Score",
    score_Hispanic     = "Hispanic Score",
    score_Black        = "Black Score",
    score_ell        = "English Language Learner Score",
    DiD                = "$\\\\beta_3$"
  ),
  keep = "%DiD",
  se.below = TRUE,
  fitstat = ~ n + r2,
  headers = list(
    "Control Group: Large States" = as.character(1:5)  # convert to character
  ),
  tex = TRUE,                    # generate LaTeX
  file = "did_large_states.tex"  # save to file
)

did_neighbor_states <- 
  etable(
  subgroup_neighbors,
  dict = c(
    score_all_students = "All Students Score",
    score_econ_disadv  = "Economically Disadvantaged Score",
    score_Hispanic     = "Hispanic Score",
    score_Black        = "Black Score",
    score_ell        = "English Language Learner Score",
    DiD                = "$\\\\beta_3$"
  ),
  keep = "%DiD",
  se.below = TRUE,
  fitstat = ~ n + r2,
  headers = list(
    "Control Group: Neighboring States" = as.character(1:5)  # convert to character
  ),
  tex = TRUE,                    # generate LaTeX
  file = "did_neighbor_states.tex"  # save to file
)
