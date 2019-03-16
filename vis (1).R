# Stock and catch of reproductive biomass in numbers and mass.
results_sum = filter(results, Age > 3) %>%
  mutate(Biomass = fun_l_w(pars$default[4], fun_a_l(Age - 0.5, pars$default[1], pars$default[2], pars$default[3]),  pars$default[5]) / 1000 * Result) %>% 
  group_by(Year, Variable, Run, Scenario, Estimate) %>% # Run, 
  summarize(SumNum = sum(Result), SumBio = sum(Biomass)) %>% 
  mutate(LogNum = log(SumNum + 1), LogBio = log(SumBio + 1)) %>% 
  ungroup() %>% 
  mutate(Run = as.factor(Run)) %>% 
  unite("Estimate | Scenario", Estimate, Scenario, sep = " | ", remove = FALSE)

# Plot the summary numbers.
plot_fig = 
  ggplot(filter(results_sum, Variable == "Numbers")) +
  geom_line(aes(x = Year, y = SumBio, group = Run, color = Scenario, linetype = Estimate), size = 1.25) + #group = Run,
  scale_color_brewer(palette = "Set1", direction = -1) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA), labels = scales::comma) +
  scale_x_continuous(expand = c(0, 0), labels = scales::comma) +  
  labs(x = "Year", y = "Tonnes of Reproductive Biomass") +
  theme_classic()

#ggsave("plot_fig.png", plot_fig, dpi = 300, width = 6.5, height = 6.5)

# Data wrangling for a cleaner dataframe.
results_sum_pi = results %>% 
  filter(Estimate == "Central" & Variable == "Poaching Revenue" |
           Estimate == "Central" & Variable == "Poaching Cost" |
           Estimate == "Central" & Variable == "Poaching Profit" | 
           Estimate == "Central" & Variable == "Aquaculture Revenue" | 
           Estimate == "Central" & Variable == "Aquaculture Cost" |
           Estimate == "Central" & Variable == "Aquaculture Profit") %>% 
  unite("Variable | Run", Variable, Run, sep = " | ", remove = FALSE)

# Profits for both sectors in both scenarios, sort of.
plot_pi = 
  ggplot(filter(results_sum_pi, Variable == "Poaching Profit"| Variable == "Aquaculture Profit" & Scenario == "w/ Aquaculture")) +
  geom_col(aes(x = Year, y = Result, fill = Variable), position = "dodge") +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1, 10), labels = scales::comma) +  
  labs(x = "Year", y = "Fat Stacks") +
  theme_classic() +
  facet_wrap(vars(Scenario))