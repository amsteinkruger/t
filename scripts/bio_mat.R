# ---- bio_mat ----

# Summarize results for matrix panels.
#  Find median biomass for each cohort and year.
results_mat = results %>% 
  filter(Variable == "Numbers") %>% 
  mutate(Biomass = fun_l_w(pars_base[4, 1], 
                           fun_a_l(Age - 0.5, 
                                   pars_base[1, 1], 
                                   pars_base[2, 1], 
                                   pars_base[3, 1]),  
                           pars_base[5, 1]) / 1000 * Result) %>% 
  group_by(Year,
           Age, 
           Scenario) %>% 
  summarize(MedNum = median(Result), 
            MedBio = median(Biomass)) %>% 
  ungroup()

#  Get median biomasses of first year by age for proportional numbers.
results_bas = results_mat %>% 
  filter(Year == 1) %>% 
  select(Age, 
         BasNum = MedNum, 
         BasBio = MedBio)

#  Calculate proportions.
results_mat = results_mat %>% 
  left_join(results_bas, by = "Age") %>% 
  mutate(ProNum = MedNum / BasNum,
         ProBio = MedBio / BasBio)

# Plot matrices.
plot_bio_mat_medbio = 
  ggplot() +
  geom_tile(data = results_mat,
            aes(x = Year,
                y = Age, 
                fill = MedBio)) +
  scale_fill_viridis_c(option = "D") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  facet_wrap(~Scenario)

plot_bio_mat_pronum = 
  ggplot() +
  geom_tile(data = results_mat,
            aes(x = Year,
                y = Age, 
                fill = ProNum)) +
  scale_fill_viridis_c(option = "D") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  facet_wrap(~Scenario)
