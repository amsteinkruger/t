# ---- bio_mat ----

# Summarize results for matrix panels.
#  Find median biomass for each cohort and year.
results_mat = 
  results %>% 
  filter(Variable == "Numbers") %>% 
  mutate(Biomass = fun_l_w(pars_base["a_lw", 1], 
                           fun_a_l(Age - 0.5, 
                                   pars_base["linf_al", 1], 
                                   pars_base["k_al", 1], 
                                   pars_base["t0_al", 1]),  
                           pars_base["b_lw", 1]) / 1000 * Result) %>% 
  group_by(Year,
           Age, 
           Scenario) %>% 
  summarize(MedNum = median(Result), 
            MedBio = median(Biomass)) %>% 
  ungroup()

#  Get median biomasses of first year by age for proportional numbers.
results_bas = 
  results_mat %>% 
  filter(Year == 1 & Scenario == "Status Quo") %>% 
  select(Age,
         BasNum = MedNum, 
         BasBio = MedBio)

#  Calculate proportions and difference of proportions.
results_mat = 
  results_mat %>% 
  left_join(results_bas, by = "Age") %>% 
  mutate(ProNum = MedNum / BasNum,
         ProBio = MedBio / BasBio) %>% 
  filter(Scenario == "Status Quo" | Scenario == "Aquaculture Intervention") %>% 
  select(Year, Age, Scenario, ProBio) %>% 
  pivot_wider(names_from = Scenario,
              values_from = ProBio) %>% 
  mutate(Difference = `Aquaculture Intervention` - `Status Quo`) %>% 
  pivot_longer(cols = c("Aquaculture Intervention", "Status Quo", "Difference"),
               names_to = "Scenario",
               values_to = "Values") %>% 
  mutate(Scenario_f = factor(Scenario,
                             levels = c("Status Quo", 
                                        "Aquaculture Intervention", 
                                        "Difference"), 
                             labels = c("Status Quo", 
                                        "Aquaculture Intervention", 
                                        "Difference")))

# Plot matrices.
plot_bio_mat = 
  ggplot() +
  geom_tile(data = results_mat,
            aes(x = Year + 2016,
                y = Age, 
                fill = Values)) +
  scale_fill_viridis_c(option = "D") +
  scale_x_continuous(expand = c(0, 0),
                     breaks = c(2019, 2029)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "", 
       y = "Age",
       fill = "% of 2017 Biomass") +
  theme_pubr() +
  guides(fill = guide_colorbar(barwidth = 15,
                               barheight = 0.5,
                               ticks = FALSE)) +
  facet_wrap(~Scenario_f)

print(plot_bio_mat)

# Save.
ggsave("plot_mat.png", 
       plot_bio_mat,
       dpi = 300,
       bg = "transparent",
       width = 11, 
       height = 7)
