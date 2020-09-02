# Summarize results for text.
#  Biomass (Difference and Proportion)
results_bio = 
  results %>% 
  filter(Variable == "Numbers" & Year == 15) %>% 
  mutate(Biomass = fun_l_w(pars_base["a_lw", 1],
                           fun_a_l(Age - 0.5,
                                   pars_base["linf_al", 1],
                                   pars_base["k_al", 1],
                                   pars_base["t0_al", 1]),
                           pars_base["b_lw", 1]) / 1000 * Result) %>%
  group_by(Scenario,
           Run) %>% 
  summarize(Biomass = sum(Biomass)) %>% 
  group_by(Scenario) %>% 
  summarize(Biomass = mean(Biomass)) %>% 
  pivot_wider(names_from = Scenario,
              values_from = Biomass) %>% 
  pivot_longer(cols = 1:3,
               values_to = "Result",
               names_to = "Intervention") %>% 
  mutate(Difference = Result - `Status Quo`,
         Proportion = Result / `Status Quo`,
         Variable = "Stock Biomass") %>% 
  select(-`Status Quo`)
  
#  Catches (Difference and Proportion)
results_cat = 
  results %>% 
  filter(Variable == "Catches" & Year == 15) %>% 
  mutate(Biomass = fun_l_w(pars_base["a_lw", 1],
                           fun_a_l(Age - 0.5,
                                   pars_base["linf_al", 1],
                                   pars_base["k_al", 1],
                                   pars_base["t0_al", 1]),
                           pars_base["b_lw", 1]) / 1000 * Result) %>%
  group_by(Scenario,
           Run) %>% 
  summarize(Biomass = sum(Biomass)) %>% 
  group_by(Scenario) %>% 
  summarize(Biomass = mean(Biomass)) %>% 
  pivot_wider(names_from = Scenario,
              values_from = Biomass) %>% 
  pivot_longer(cols = 1:3,
               values_to = "Result",
               names_to = "Intervention") %>% 
  mutate(Difference = Result - `Status Quo`,
         Proportion = Result / `Status Quo`,
         Variable = "Catch Biomass") %>% 
  select(-`Status Quo`)

#  Effort (Difference and Proportion)
results_eff = 
  results %>% 
  filter(Variable == "Effort" & Year == 15) %>% 
  group_by(Scenario) %>% 
  summarize(Result = mean(Result)) %>% 
  pivot_wider(names_from = Scenario,
              values_from = Result) %>% 
  pivot_longer(cols = 1:3,
               values_to = "Result",
               names_to = "Intervention") %>% 
  mutate(Difference = Result - `Status Quo`,
         Proportion = Result / `Status Quo`,
         Variable = "Effort") %>% 
  select(-`Status Quo`)

#  Price (Difference and Proportion)
results_pri = 
  results %>% 
  filter(Variable == "Price" & Year == 15) %>% 
  group_by(Scenario) %>% 
  summarize(Result = mean(Result)) %>% 
  pivot_wider(names_from = Scenario,
              values_from = Result) %>% 
  pivot_longer(cols = 1:3,
               values_to = "Result",
               names_to = "Intervention") %>% 
  mutate(Difference = Result - `Status Quo`,
         Proportion = Result / `Status Quo`,
         Variable = "Price") %>% 
  select(-`Status Quo`)

#  Revenue (Fishery) (Difference and Proportion)
results_rev_f = 
  results %>% 
  filter(Variable == "Poaching Revenue" & Year == 15) %>% 
  group_by(Scenario) %>% 
  summarize(Result = mean(Result)) %>% 
  pivot_wider(names_from = Scenario,
              values_from = Result) %>% 
  pivot_longer(cols = 1:3,
               values_to = "Result",
               names_to = "Intervention") %>% 
  mutate(Difference = Result - `Status Quo`,
         Proportion = Result / `Status Quo`,
         Variable = "Revenue (Fishery)") %>% 
  select(-`Status Quo`)

#  Revenue (Aquaculture) (Difference and Proportion)
results_rev_a = 
  results %>% 
  filter(Variable == "Aquaculture Revenue" & Year > 10) %>% # Band-Aid to get last five years for smoothing.
  group_by(Scenario) %>% 
  summarize(Result = mean(Result)) %>% 
  pivot_wider(names_from = Scenario,
              values_from = Result) %>% 
  pivot_longer(cols = 1:3,
               values_to = "Result",
               names_to = "Intervention") %>% 
  mutate(Difference = Result - `Status Quo`,
         Proportion = Result / `Status Quo`,
         Variable = "Revenue (Aquaculture)") %>% 
  select(-`Status Quo`)

# Tabulate!
results_sum = 
  bind_rows(results_bio,
            results_cat,
            results_eff,
            results_pri,
            results_rev_f,
            results_rev_a)

# Print!
print(results_sum)

# Export!
kable(results_sum, "html", digits = 2) %>% cat(file = "./out/results_sum.html")
