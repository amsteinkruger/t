# Summarize results in barplots of differential outcomes wrt total reproductive biomass and biomass by cohort.
#  Reproductive biomass w/ aq. scale.
vis_bio_sum = 
  results %>% 
  filter(Variable == "Numbers" & Age > 3) %>% 
  mutate(Biomass = fun_l_w(pars_base["a_lw", 1], 
                           fun_a_l(Age - 0.5, 
                                   pars_base["linf_al", 1], 
                                   pars_base["k_al", 1], 
                                   pars_base["t0_al", 1]),  
                           pars_base["b_lw", 1]) / 1000 * Result) %>% 

  group_by(Year,
           Run, 
           Scenario) %>% 
  summarize(Biomass = sum(Biomass)) %>% 
  pivot_wider(names_from = Scenario,
              values_from = Biomass) %>% 
  mutate(`Aquaculture Intervention` = `Aquaculture Intervention` - `Status Quo`,
         `Enforcement Intervention` = `Enforcement Intervention` - `Status Quo`,
         `Both Interventions` = `Aquaculture and Enforcement Interventions` - `Status Quo`) %>% 
  select(-`Status Quo`,
         -`Aquaculture and Enforcement Interventions`) %>% 
  pivot_longer(cols = c("Aquaculture Intervention",
                        "Enforcement Intervention",
                        "Both Interventions"),
               names_to = "Scenario",
               values_to = "Values") %>% 
  group_by(Year,
           Scenario) %>% 
  summarize(Mid = mean(Values, na.rm = TRUE),
            High = mean(Values, na.rm = TRUE) + sd(Values, na.rm = TRUE),
            Low = mean(Values, na.rm = TRUE) - sd(Values, na.rm = TRUE)) %>% 
  ungroup %>% 
  mutate(Scenario = factor(Scenario, 
                           levels = c("Aquaculture Intervention",
                                      "Enforcement Intervention",
                                      "Both Interventions"),
                           labels = c("Aquaculture Intervention",
                                      "Enforcement Intervention",
                                      "Both Interventions"))) %>% 
  ggplot + 
  geom_crossbar(aes(x = Year + 2016,
                   y = Mid,
                   ymin = Low,
                   ymax = High,
                   group = Year + 2016,
                   fill = Mid),
                width = 1) +
  labs(x = "",
       y = "Effect of Intervention(s) on Adult Biomass (t)",
       fill = "Mean") +
  scale_x_continuous(breaks = c(2019, 2024, 2029)) +
  scale_fill_viridis_c() +
  guides(fill = guide_colorbar(barwidth = 15,
                               barheight = 0.5,
                               ticks = FALSE)) +
  theme_pubr() +
  theme(legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(margin = margin(l = 2.5, r = 2.5), hjust = 0),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA)) +
    facet_wrap(~ Scenario,
               nrow = 1)

#   Print.
print(vis_bio_sum)

#   Save.
ggsave("./out/vis_bio_sum.png",
       vis_bio_sum,
       dpi = 300,
       width = 6.5)