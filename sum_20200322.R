# ---- sum_20200322 ----

# Testing commit + push + pull on a fresh machine.

# Get outputs to think through variable relationships in a big ol' panel figure.
dat_sum_panel = 
  results %>% 
  filter(Scenario =="Status Quo" | 
           Scenario == "Aquaculture Intervention") %>% # Filter scenarios
  filter(Variable == "Numbers" | 
           Variable == "Catches" | 
           Variable == "Effort" |
           Variable == "Price" |
           Variable == "Poaching Profit" | 
           Variable == "Poaching Cost per Metric Ton" |
           Variable == "Aquaculture Profit" |
           Variable == "Aquaculture Cost per Metric Ton") %>% # Filter variables.
  mutate(Result = ifelse(Variable == "Numbers" | Variable == "Catches",
                         fun_l_w(pars_base["a_lw", 1],
                                 fun_a_l(Age - 0.5,
                                         pars_base["linf_al", 1],
                                         pars_base["k_al", 1],
                                         pars_base["t0_al", 1]),
                                 pars_base["b_lw", 1]) / 1000 * Result,
                         Result)) %>% # Convert numbers of fish to biomass of fish, both caught and in the water.
  mutate(Variable = ifelse(Variable == "Numbers",
                           "Biomass",
                           Variable), # Rename post-conversion.
         Variable_f = factor(Variable, 
                             levels = c("Biomass", 
                                        "Catches",
                                        "Effort",
                                        "Price",
                                        "Poaching Profit",
                                        "Poaching Cost per Metric Ton",
                                        "Aquaculture Profit",
                                        "Aquaculture Cost per Metric Ton"),
                             labels = c("Biomass (MT)", 
                                        "Catches (MT)",
                                        "Effort (Boat-Years)",
                                        "Price (US$2017/Dry Gram)",
                                        "Poaching Profit (US$2017)",
                                        "Poaching Unit Cost (US$2017/Dry MT)",
                                        "Aquaculture Profit (US$2017)",
                                        "Aquaculture U.C. (US$2017/Dry MT)")),
         Scenario_f = factor(Scenario,
                             levels = c("Status Quo",
                                        "Aquaculture Intervention"),
                             labels = c("Status Quo",
                                        "Aquaculture Intervention"))) %>% # Factor variables to order facets.
  group_by(Year,
           Run,
           Scenario,
           Scenario_f,
           Variable,
           Variable_f) %>% 
  summarize(Result = sum(Result, na.rm = TRUE)) %>% 
  ungroup %>% 
  group_by(Year,
           Scenario,
           Scenario_f,
           Variable,
           Variable_f) %>% 
  summarize(Mean = mean(Result, na.rm = TRUE),
            Pct9 = quantile(Result, 0.90, na.rm = TRUE),
            Pct1 = quantile(Result, 0.10, na.rm = TRUE)) %>% 
  ungroup


plot_sum_panel =
  dat_sum_panel %>% 
  filter(Variable == "Biomass" | 
           Variable == "Catches" | 
           Variable == "Effort" |
           Variable == "Price" |
           Variable == "Poaching Profit" | 
           Variable == "Aquaculture Profit") %>% 
  ggplot() +
  geom_line(aes(x = Year + 2016,
                y = Mean,
                color = Variable),
            size = 1.25,
            alpha = 0.75) +
  geom_ribbon(aes(x = Year + 2016,
                  ymin = Pct1,
                  ymax = Pct9,
                  color = Variable,
                  fill = Variable),
              alpha = 0.50) +
  geom_point(aes(x = Year + 2016,
                 y = Mean,
                 color = Variable),
             fill = NA,
             shape = 21,
             size = 1.25) +
  geom_hline(yintercept = 0,
             color = "firebrick",
             linetype = "dashed") +
  labs(x = "",
       y = "") +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  facet_grid(Variable_f ~ Scenario_f,
             scales = "free_y") +
  theme_pubr() +
  theme(legend.position = "none")

ggsave("plot_sum_panel.png",
       plot_sum_panel,
       dpi = 300,
       width = 8.5,
       height = 12)
