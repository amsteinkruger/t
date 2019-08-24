# ---- bio ----

# Summarize biomass.
results_sum =  results %>% 
  #na.omit() %>% # This is a Band-Aid to filter out any surprise NAs. Swap out for a better solution in set.R.
  #filter(Age < 3) %>% # This is a quick trick to filter out juveniles and subadults and keep reproductive biomass.
  filter(Variable == "Numbers") %>% 
  mutate(Biomass = fun_l_w(pars_base$default[4], 
                           fun_a_l(Age - 0.5, 
                                   pars_base$default[1], 
                                   pars_base$default[2], 
                                   pars_base$default[3]),  
                           pars_base$default[5]) / 1000 * Result) %>% 
  group_by(Year, 
           Variable, 
           Run, 
           Scenario,
           Cages) %>% 
  summarize(SumNum = sum(Result), 
            SumBio = sum(Biomass)) %>% 
  ungroup() %>% 
  mutate(Run = as.factor(Run))

# Summarize biomass by cages and quantiles.
results_summer = results_sum %>% 
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
           Variable, 
           Scenario,
           Cages) %>% 
  summarize(AveBio = mean(SumBio),
            ForBio = quantile(SumBio, 0.40),
            SixBio = quantile(SumBio, 0.60)) %>% 
  ungroup()

# Ditto, but just more so?
results_summerer = results_sum %>% 
  group_by(Year, 
           Variable, 
           Scenario) %>% 
  summarize(MaxBio = max(SumBio),
            MinBio = min(SumBio),
            TweBio = quantile(SumBio, 0.20),
            SevBio = quantile(SumBio, 0.80)) %>% 
  ungroup()

# Plot the summary numbers.
plot_bio = 
  ggplot() + 
  geom_ribbon(data = filter(results_summerer, 
                            Variable == "Numbers"),
              aes(x = Year + 2016,
                  ymin = MinBio, 
                  ymax = MaxBio),
              alpha = 0.25) +
  geom_ribbon(data = filter(results_summerer, 
                            Variable == "Numbers"),
              aes(x = Year + 2016,
                  ymin = TweBio, 
                  ymax = SevBio),
              alpha = 0.50) +
  geom_ribbon(data = filter(results_summer, 
                            Variable == "Numbers",
                            Scenario == "Counterfactual"),
              aes(x = Year + 2016,
                  ymin = ForBio, 
                  ymax = SixBio,
                  color = Cages,
                  group = Cages,
                  fill = Cages),
              alpha = 0.50) +
  geom_ribbon(data = filter(results_summer, 
                            Variable == "Numbers",
                            Scenario == "Status Quo"),
              aes(x = Year + 2016,
                  ymin = ForBio, 
                  ymax = SixBio),
              alpha = 0.75) +
  scale_color_manual(values = c("steelblue1", 
                                "steelblue2", 
                                "steelblue3", 
                                "steelblue4")) +
  scale_fill_manual(values = c("steelblue1", 
                               "steelblue2", 
                               "steelblue3", 
                               "steelblue4")) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 15000),
                     labels = scales::comma) + 
  scale_x_continuous(breaks = c(2017, 2022, 2027),
                     expand = c(0, 0.75)) +  
  labs(x = "Year", y = "Biomass (Tonnes)") + 
  theme_classic() + #base_family = "avenir"
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA)) + 
  facet_wrap(~Scenario)

# Print for .Rmd
print(plot_bio)

# Save.
ggsave("./out/plot_bio.png", plot_bio, width = 8.5, height = 5.5)