# ---- bio_mat ----

# Summarize results for matrix panels.
#  Find median biomass for each cohort and year.
results_mat = results %>% 
  filter(Variable == "Numbers" & Age > 4) %>% 
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
            aes(x = Year + 2016,
                y = Age, 
                fill = ProNum)) +
  scale_fill_viridis_c(option = "D") +
  scale_x_continuous(expand = c(0, 0),
                     breaks = c(2017, 2036)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "", y = "Age") +
  theme_classic() +
  theme(legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        strip.background = element_blank(),
        #strip.text = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.spacing.x = unit(5, "lines"),
        plot.background = element_rect(fill = "transparent", color = NA)) + 
  facet_wrap(~Scenario)

print(plot_bio_mat_pronum)

# Save.
ggsave("./out/plot_mat.png", 
       plot_bio_mat_pronum,
       dpi = 300,
       bg = "transparent",
       width = 9.00, 
       height = 4.13)
