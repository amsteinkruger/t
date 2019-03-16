# ---- vis ----

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
plot_nfig = 
  ggplot(filter(results_sum, Variable == "Numbers")) +
  geom_line(aes(x = Year, y = SumBio, group = Run, color = Scenario, linetype = Estimate), size = 1.25) + #group = Run,
  scale_color_brewer(palette = "Set1", direction = -1) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA), labels = scales::comma) +
  scale_x_continuous(expand = c(0, 0), labels = scales::comma) +  
  labs(x = "Year", y = "Tonnes of Reproductive Biomass") +
  theme_classic()

print(plot_nfig)

#ggsave("plot_fig.png", plot_fig, dpi = 300, width = 6.5, height = 6.5)

plot_cfig = 
  ggplot(filter(results_sum, Variable == "Catches")) +
  geom_line(aes(x = Year, y = SumBio, group = Run, color = Scenario, linetype = Estimate), size = 1.25) + #group = Run,
  scale_color_brewer(palette = "Set1", direction = -1) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA), labels = scales::comma) +
  scale_x_continuous(expand = c(0, 0), labels = scales::comma) +  
  labs(x = "Year", y = "Tonnes of Reproductive Biomass") +
  theme_classic()

print(plot_cfig)

#ggsave("plot_cfig.png", plot_fig, dpi = 300, width = 6.5, height = 6.5)

# Plot recruitment.
#plot_recfig = 
#  ggplot(filter(results, Variable == "Recruitment")) +
#  geom_line(aes(x = Year, y = Result, group = Run, color = Scenario, linetype = Estimate), size = 1.25) + #group = Run,
#  scale_color_brewer(palette = "Set1", direction = -1) +
#  scale_y_continuous(expand = c(0, 0), limits = c(0, NA), labels = scales::comma) +
#  scale_x_continuous(expand = c(0, 0), labels = scales::comma) +  
#  labs(x = "Year", y = "Juveniles") +
#  theme_classic()

#print(plot_recfig)

# Plot effort.
plot_efig = 
  ggplot(filter(results, Variable == "Effort")) +
  geom_line(aes(x = Year, y = Result, group = Run, color = Scenario, linetype = Estimate), size = 1.25) + #group = Run,
  scale_color_brewer(palette = "Set1", direction = -1) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA), labels = scales::comma) +
  scale_x_continuous(expand = c(0, 0), labels = scales::comma) +  
  labs(x = "Year", y = "Boat-Years") +
  theme_classic()

print(plot_efig)

#ggsave("plot_cfig.png", plot_fig, dpi = 300, width = 6.5, height = 6.5)

# Data wrangling for a cleaner dataframe.
results_sum_pi = results %>% 
  filter(Estimate == "Central" & Variable == "Poaching Revenue" |
           Estimate == "Central" & Variable == "Poaching Cost" |
           Estimate == "Central" & Variable == "Poaching Profit")

# Profits for in both scenarios, sort of.
plot_pi = 
  ggplot(filter(results_sum_pi, Variable == "Poaching Profit")) +
  geom_col(aes(x = Year, y = Result, fill = Variable), position = "dodge") +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1, 10), labels = scales::comma) +  
  labs(x = "Year", y = "Fat Stacks") +
  theme_classic()

print(plot_pi)

#ggsave("plot_pi.png", plot_pi, dpi = 300, width = 6.5, height = 6.5)

# Revenues and costs for the central scenario. Stacked bar!
plot_rc = 
  ggplot(filter(results_sum_pi, Variable == "Poaching Revenue" | Variable == "Poaching Cost")) +
  geom_col(aes(x = Year, y = Result, fill = Variable), position = "dodge") +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  scale_y_continuous(expand = c(0, 0), labels = scales::comma) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1, 10), labels = scales::comma) +  
  labs(x = "Year", y = "Fat Stacks") +
  theme_classic()

print(plot_rc)

#ggsave("plot_rc.png", plot_rc, dpi = 300, width = 6.5, height = 6.5)

# Wrangle effort.
results_e_sum = filter(results_e, Variable == "Effort") %>% 
  group_by(Run) %>% 
  summarize(mean = mean(Result)) %>% 
  ungroup() %>% 
  mutate("Annual Tonnes" = Run - 1) %>% 
  rename("Mean Effort" = mean)

