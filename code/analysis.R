
library(fixest)

master_panel_clean <- master_panel_clean %>%
  mutate(DiD = treated * post)

# Baseline: only year FE
model1 <- feols(score_all_students ~ DiD | year, data = master_panel_clean)
summary(model1)
#negative effect, but ca always had lower scores

#Robust Model

feols(score_all_students ~ DiD + treated | year, data = master_panel_clean)
#no significance, pfl had no impact
# Define the subgroups you want to test
subgroup_models <- feols(c(score_all_students, 
                           score_econ_disadv, 
                           score_free_lunch, 
                           score_Hispanic, 
                           score_Black) ~ DiD + treated | year, 
                         data = master_panel_clean)

# Option A: The quick fix (just remove 'stars')
etable(subgroup_models, 
       headers = c("All", "Econ Disadv", "Free Lunch", "Hispanic", "Black"))

# Option B: The "Thesis-Ready" way (renames variables automatically)
# This replaces the messy variable names with clean ones in the table
etable(subgroup_models, 
       dict = c(score_all_students = "All Students", 
                score_econ_disadv = "Econ. Disadvantaged", 
                score_free_lunch = "Free Lunch Eligible", 
                score_Hispanic = "Hispanic", 
                score_Black = "Black"),
       order = "DiD") # Puts your main DiD coefficient at the top

#