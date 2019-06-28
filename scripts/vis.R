# ---- vis ----

# Set-up.
library(ggpubr)
library(gridExtra)
library(showtext)

# Load Avenir from an open text file in the working directory.
#  package showtext works for certain graphic devices so that saved files display Avenir but previews in RStudio do not.
font_paths(".")
font_add("avenir", "avenir.otf")
showtext_auto()

# Stock and catch of reproductive biomass in numbers and mass.
results_sum =  results %>% #filter(results, Age > 3)
  mutate(Biomass = fun_l_w(pars$default[4], fun_a_l(Age - 0.5, pars$default[1], pars$default[2], pars$default[3]),  pars$default[5]) / 1000 * Result) %>% 
  group_by(Year, Variable, Run, Scenario) %>% # Run, #, Estimate, Scenario
  summarize(SumNum = sum(Result), SumBio = sum(Biomass)) %>% 
  ungroup() %>% 
  mutate(Run = as.factor(Run))

# Plot the summary numbers.
#  Check whether inputs are tonnes or numbers.
plot_n = 
  ggplot(filter(results_sum, Variable == "Numbers")) + # & Estimate == "Central"
  geom_hline(yintercept = 12136, size = 0.95, color = "#EF5645") +
  geom_line(aes(x = Year + 2016, y = SumBio * 0.33, group = Run, color = Scenario), size = 1.15, alpha = 0.10) + #, linetype = Estimate #, color = Scenario
  scale_color_manual(values = c("#04859B", "#003660")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 20000), labels = scales::comma) +
  scale_x_continuous(expand = c(0, 0.25)) +  
  labs(x = "Year", y = "Tonnes of Biomass") +
  theme_classic(base_family = "avenir") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none") +
  facet_wrap(~Scenario)

print(plot_n)

# Data wrangling for a cleaner dataframe.
results_sum_pi = results %>% 
  filter(Variable == "Aquaculture Profit" |
         Variable == "Poaching Profit")

# Profits for in both scenarios, sort of.
plot_pi = 
  ggplot(results_sum_pi) +
  geom_col(aes(x = Year, y = Result, fill = Variable), position = "dodge") +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1, 10), labels = scales::comma) +  
  labs(x = "Year", y = "Fat Stacks") +
  theme_classic()

print(plot_pi)

