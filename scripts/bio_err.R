# ---- bio_err ----

# Summarize results for ribbon panels.
#  Find biomass for each cohort and year, then sum by cohort.
results_sum = results %>% 
  filter(Variable == "Numbers") %>% 
  mutate(Biomass = fun_l_w(pars_base[4, 1], 
                           fun_a_l(Age - 0.5, 
                                   pars_base[1, 1], 
                                   pars_base[2, 1], 
                                   pars_base[3, 1]),  
                           pars_base[5, 1]) / 1000 * Result) %>% 
  group_by(Year,
           Run, 
           Scenario,
           Cages) %>% 
  summarize(SumNum = sum(Result), 
            SumBio = sum(Biomass)) %>% 
  ungroup() 

# Keep counterfactual runs. Bin runs by scale of aquaculture, then find quantile outcomes.
results_cag = results_sum %>% 
  filter(Scenario == "Counterfactual") %>% 
  mutate(Cages = ifelse(Cages > 0, 
                        ifelse(Cages > 25, 
                               ifelse(Cages > 50, 
                                      ifelse(Cages > 75, 
                                             "75 - 100", 
                                             "50 - 75"), 
                                      "25 - 50"), 
                               "1 - 25"),
                        "0")) %>% 
  group_by(Year,
           Scenario,
           Cages) %>% 
  summarize(MedBio = median(SumBio),
            ForBio = quantile(SumBio, 0.40),
            SixBio = quantile(SumBio, 0.60)) %>% 
  ungroup()

#  Find maxima and minima of all runs and quantiles. Filter for status quo runs for inner quantiles in geom_ribbon below.
results_sce = results_sum %>% 
  group_by(Year,
           Scenario) %>% 
  summarize(MedBio = median(SumBio),
            MaxBio = max(SumBio),
            MinBio = min(SumBio),
            TenBio = quantile(SumBio, 0.10),
            NinBio = quantile(SumBio, 0.90),
            ForBio = quantile(SumBio, 0.40),
            SixBio = quantile(SumBio, 0.60)) %>% 
  ungroup()

#  Run out a vector of facet labels by scenario.
labs_bio_err = c(Counterfactual = "A", "Status Quo" = "B")

# Plot the summary numbers.
plot_bio = 
  ggplot() + 
  geom_ribbon(data = results_sce,  # Outer runs.
              aes(x = Year + 2016,
                  ymin = MinBio, 
                  ymax = MaxBio),
              fill = "grey95",
              color = "grey85") +
  geom_ribbon(data = results_sce,  # Outer runs.
              aes(x = Year + 2016,
                  ymin = TenBio, 
                  ymax = NinBio),
              fill = "grey90",
              color = "grey80") +
  geom_ribbon(data = results_cag,  # Quantiles for counterfactual runs by production scale.
              aes(x = Year + 2016,
                  ymin = ForBio, 
                  ymax = SixBio,
                  color = Cages,
                  group = Cages,
                  fill = Cages),
              alpha = 0.75) +
  geom_point(data = results_cag,
            aes(x = Year + 2016,
                y = MedBio,
                color = Cages),
            shape = 18) +
  geom_ribbon(data = filter(results_sce, # Quantiles for status quo runs.
                            Scenario == "Status Quo"),
              aes(x = Year + 2016,
                  ymin = ForBio, 
                  ymax = SixBio),
              fill = "grey85",
              color = "grey75") +
  geom_point(data = filter(results_sce, # Quantiles for status quo runs.
                          Scenario == "Status Quo"),
             aes(x = Year + 2016,
                 y = MedBio),
             color = "grey65",
             shape = 18) +
  geom_vline(data = results_sce,
             aes(xintercept = 3 + 2016),
             color = "firebrick4",
             linetype = "dashed") +
  scale_color_manual(values = pal_col) +
  scale_fill_manual(values = pal_fil) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 20000),
                     labels = scales::comma) + 
  scale_x_continuous(#breaks = c(2017, 2022, 2027),
                     expand = c(0, 0.75)) +  
  labs(x = "Year", y = "Biomass (Tonnes)") + 
  theme_classic() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA)) + 
  facet_wrap(~Scenario,
             labeller = labeller(Scenario = labs_bio_err))

# Print for .Rmd
print(plot_bio)

# Save.
ggsave("./out/plot_bio.png", 
       plot_bio,
       width = 9.32, 
       height = 3.25)
