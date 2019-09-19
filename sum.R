# ---- sum ----

# Summarize results for text. This round of code iteration does not explicate multiple production scales.
results_bio = results %>% 
  filter(Variable == "Numbers" & Year == max(Year)) %>% 
  mutate(Biomass = fun_l_w(pars_base[4, 1], 
                           fun_a_l(Age - 0.5, 
                                   pars_base[1, 1], 
                                   pars_base[2, 1], 
                                   pars_base[3, 1]),  
                           pars_base[5, 1]) / 1000 * Result) %>% 
  group_by(Run, Scenario) %>% 
  summarize(SumBio = sum(Biomass)) %>% 
  ungroup() %>% 
  group_by(Scenario) %>% 
  summarize(MedBio = median(SumBio)) %>% 
  ungroup() %>% 
  spread(key = Scenario, value = MedBio) %>% 
  mutate(abs = Counterfactual - `Status Quo`,
         pro = Counterfactual / `Status Quo`)

# Differential and proportional impacts in biomass for the median of runs in the final year.
print(results_bio)

results_pi = results %>% 
  filter(Variable == "Poaching Profit") %>% 
  group_by(Run, Scenario) %>% 
  summarize(SumPi = sum(Result)) %>% 
  ungroup() %>% 
  group_by(Scenario) %>% 
  summarize(MedPi = median(SumPi)) %>% 
  ungroup() %>% 
  spread(key = Scenario, value = MedPi) %>% 
  mutate(abs = Counterfactual - `Status Quo`,
         pro = Counterfactual / `Status Quo`)

# Differential and proportional impacts in profit for the median of runs in the final year.
print(results_pi)

results_pr = results %>% 
  filter(Variable == "Price") %>% 
  group_by(Run, Scenario) %>% 
  summarize(MeaPr = mean(Result)) %>% 
  ungroup() %>% 
  group_by(Scenario) %>% 
  summarize(MedPr = median(MeaPr)) %>% 
  ungroup() %>% 
  spread(key = Scenario, value = MedPr) %>% 
  mutate(abs = Counterfactual - `Status Quo`,
         pro = Counterfactual / `Status Quo`)

# Differential and proportional impacts in prices at a = 13.5 for the median of runs in the final year.
print(results_pr)

# Easy code for cost results check.
View(filter(results, Variable == "Poaching Cost per Metric Ton"))
View(filter(results, Variable == "Aquaculture Cost per Metric Ton"))
