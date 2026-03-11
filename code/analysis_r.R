library(dplyr)
library(fixest)

master_panel_final_r <- master_panel_final_r %>%
  mutate(DiD = treated * post)

subgroup_models_r <- feols(
  c(score_all_students,
    score_econ_disadv,
    score_Hispanic,
    score_Black,
    score_ell) 
  ~ DiD | jurisdiction + year,
  data = master_panel_final_r,
  cluster = ~jurisdiction
)

large_states_r <- c("Florida", "Pennsylvania", "Texas")
subgroup_large_r <- feols(
  c(score_all_students,
    score_econ_disadv,
    score_Hispanic,
    score_Black,
    score_ell) 
  ~ DiD | jurisdiction + year,
  data = master_panel_final_r %>%
    filter(jurisdiction %in% c("California", large_states)),
  cluster = ~jurisdiction
)

neighbors_r <- c("Arizona", "Oregon", "Nevada")
subgroup_neighbors_r <- feols(
  c(score_all_students,
    score_econ_disadv,
    score_Hispanic,
    score_Black,
    score_ell) 
  ~ DiD | jurisdiction + year,
  data = master_panel_final_r %>%
    filter(jurisdiction %in% c("California", neighbors)),
  cluster = ~jurisdiction
)
etable(
  subgroup_models_r,
  headers = c("All Students", "Econ. Disadv.", "Hispanic", "Black", "ELL"),
  dict = c(DiD = "Post × California"),
  keep = "%DiD",
  se.below = TRUE,
  fitstat = ~ n + r2
)
etable(
  subgroup_large_r,
  headers = c("All Students", "Econ. Disadv.", "Hispanic", "Black", "ELL"),
  dict = c(DiD = "Post × California"),
  keep = "%DiD",
  se.below = TRUE,
  fitstat = ~ n + r2
)

etable(
  subgroup_neighbors_r,
  headers = c("All Students", "Econ. Disadv.", "Hispanic", "Black", "ELL"),
  dict = c(DiD = "Post × California"),
  keep = "%DiD",
  se.below = TRUE,
  fitstat = ~ n + r2
)


etable(
  subgroup_models_r,
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
  file = "did_non-pfl_states_r.tex"  # save to file,
)

did_large_states_r  <- 
  etable(
    subgroup_large_r,
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
    file = "did_large_states_r.tex"  # save to file
  )

did_neighbor_states_r <- 
  etable(
    subgroup_neighbors_r,
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
    file = "did_neighbor_states_r.tex"  # save to file
  )
