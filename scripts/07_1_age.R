# Summarize results in matrix plot of differential outcomes by cohort biomass.
#  Visualize w/ age structure, w/o error.
vis_bio_age_pro = 
  results %>% 
  filter(Scenario == "Status Quo" | Scenario == "Aquaculture Intervention") %>% 
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
  summarize(Biomass = sum(Biomass)) %>% 
  ungroup %>% 
  pivot_wider(names_from = Scenario,
              values_from = Biomass) %>% 
  mutate(Difference = `Aquaculture Intervention` - `Status Quo`,
         Proportion = `Aquaculture Intervention` / `Status Quo`) %>% 
  pivot_longer(cols = c("Difference",
                        "Proportion"),
               names_to = "Which",
               values_to = "Values") %>% 
  filter(Which == "Proportion") %>% 
  ggplot +
  geom_raster(aes(x = Year + 2016,
                  y = Age, 
                  fill = Values)) +
  labs(x = "",
       fill = "Biomass Effect (Intervention:Status Quo)") +
  guides(fill = guide_colorbar(barwidth = 13, 
                               barheight = 0.5,
                               ticks = FALSE,
                               title.position = "bottom",
                               title.hjust = 0.50)) +
  scale_fill_viridis_c(option = "D") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr() +
  theme(title = element_text(size = 9))

vis_bio_age_dif = 
  results %>% 
  filter(Scenario == "Status Quo" | Scenario == "Aquaculture Intervention") %>% 
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
  summarize(Biomass = sum(Biomass)) %>% 
  ungroup %>% 
  pivot_wider(names_from = Scenario,
              values_from = Biomass) %>% 
  mutate(Difference = `Aquaculture Intervention` - `Status Quo`,
         Proportion = `Aquaculture Intervention` / `Status Quo`) %>% 
  pivot_longer(cols = c("Difference",
                        "Proportion"),
               names_to = "Which",
               values_to = "Values") %>% 
  filter(Which == "Difference") %>% 
  mutate(Values = Values * (1 / 1000000)) %>% # Transform numbers from individuals to millions of individuals.
  ggplot +
  geom_raster(aes(x = Year + 2016,
                  y = Age, 
                  fill = Values)) +
  labs(x = "",
       fill = "Biomass Effect (Intervention - Status Quo)") +
  guides(fill = guide_colorbar(barwidth = 13, 
                               barheight = 0.5,
                               ticks = FALSE,
                               title.position = "bottom",
                               title.hjust = 0.50)) +
  scale_fill_viridis_c(option = "D") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_pubr() +
  theme(title = element_text(size = 9))

print(vis_bio_age_pro)
print(vis_bio_age_dif)

ggsave("./out/vis_bio_age_pro.png",
       vis_bio_age_pro,
       dpi = 300,
       width = 3.25)

ggsave("./out/vis_bio_age_dif.png",
       vis_bio_age_dif,
       dpi = 300,
       width = 3.25)