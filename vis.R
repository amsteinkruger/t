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
  geom_line(aes(x = Year, y = SumBio, group = Run, color = Scenario, linetype = Estimate), size = 1.25) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA), labels = scales::comma) +
  scale_x_continuous(expand = c(0, 0), labels = scales::comma) +  
  labs(x = "Year", y = "Tonnes of Reproductive Biomass") +
  theme_classic()

print(plot_nfig)

ggsave("plot_nfig.png", plot_nfig, dpi = 300, width = 6.5, height = 6.5)

plot_cfig = 
  ggplot(filter(results_sum, Variable == "Catches")) +
  geom_line(aes(x = Year, y = SumBio, group = Run, color = Scenario, linetype = Estimate), size = 1.25) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA), labels = scales::comma) +
  scale_x_continuous(expand = c(0, 0), labels = scales::comma) +  
  labs(x = "Year", y = "Catch (Tonnes)") +
  theme_classic()

print(plot_cfig)

#ggsave("plot_cfig.png", plot_fig, dpi = 300, width = 6.5, height = 6.5)

# Plot recruitment.
#plot_recfig = 
#  ggplot(filter(results, Variable == "Recruitment")) +
#  geom_line(aes(x = Year, y = Result, group = Run, color = Scenario, linetype = Estimate), size = 1.25) +
#  scale_color_brewer(palette = "Set1", direction = -1) +
#  scale_y_continuous(expand = c(0, 0), limits = c(0, NA), labels = scales::comma) +
#  scale_x_continuous(expand = c(0, 0), labels = scales::comma) +  
#  labs(x = "Year", y = "Juveniles") +
#  theme_classic()

#print(plot_recfig)

plot_pfig = 
  ggplot(filter(results, Variable == "Price")) +
  geom_line(aes(x = Year, y = Result, group = Run, color = Scenario, linetype = Estimate), size = 1.25) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA), labels = scales::comma) +
  scale_x_continuous(expand = c(0, 0), labels = scales::comma) +  
  labs(x = "Year", y = "Price (USD2018 Per Gram)") +
  theme_classic()

print(plot_pfig)

# Plot effort.
plot_efig = 
  ggplot(filter(results, Variable == "Effort")) +
  geom_line(aes(x = Year, y = Result, group = Run, color = Scenario, linetype = Estimate), size = 1.25) +
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

# Revenues and costs for both sectors in both scenarios.
#plot_rc =
#  ggplot(filter(results_sum_pi))

#ggsave("plot_pi.png", plot_pi, dpi = 300, width = 6.5, height = 6.5)

# Revenues and costs for the central estimate.
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
  rename("Mean Effort" = mean) %>% 
  mutate("25% Reduction" = (`Mean Effort`[1] * 0.75)) %>% 
  mutate("50% Reduction" = (`Mean Effort`[1] * 0.50)) %>% 
  mutate("90% Reduction" = (`Mean Effort`[1] * 0.10))  
  



#write.csv(results_e_sum, file = "results_e_sum.csv")

# Plot efforts.
plot_earb = 
  ggplot(results_e_sum, aes(`Annual Tonnes`, `Mean Effort`)) +
    geom_line(aes(`Annual Tonnes`, `25% Reduction`), linetype = "dashed", color = "red") +
    geom_line(aes( `Annual Tonnes`, `50% Reduction`), linetype = "dashed", color = "red") +
    geom_line(aes( `Annual Tonnes`, `90% Reduction`), linetype = "dashed", color = "red") +
    geom_vline(xintercept = 1, linetype = "dashed", color = "blue")+
    geom_hline(yintercept = 169.3064, linetype = "dashed", color = "blue")+
    geom_line(color = "black", size = 2) +
    annotate("text", x = 13, y = 140, label = "25% Reduction in Effort", size = 3) + #, family = "Century Gothic"
    annotate("text", x = 10, y = 95, label = "50% Reduction in Effort", size = 3) + #, family = "Century Gothic"
    annotate("text", x = 10, y = 25, label = "90% Reduction in Effort", size = 3) + #, family = "Century Gothic"
    annotate("text", x = 7, y = 173, label = "3% Reduction in Effort with Current Production Capacity", size = 3) +
    ylim(0,200)+
    xlim(0,15)+
    #scale_x_continuous(expand = c(0, 0)) + 
    #scale_y_continuous(expand = c(0, 0))+
    labs( x = "Tonnes of Aquaculture", y = "Effort (Boat Years)")+
    ggtitle("Reduction in Poaching Effort per Increase in Aquaculture Output")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust=1, vjust=1, face = 'bold'))

ggsave("plot_earb.png", plot_earb, dpi = 300, width = 6.5, height = 6.5)

print(plot_earb)



# Plot prices.
results_p_sum = filter(results_e, Variable == "Price") %>% 
  group_by(Run) %>% 
  summarize(mean = mean(Result)) %>% 
  ungroup() %>% 
  mutate("Annual Tonnes" = Run - 1)  %>% 
  rename("Mean Price" = mean) %>% 
  mutate("25% Reduction" = (`Mean Price`[1] * 0.75)) %>% 
  mutate("50% Reduction" = (`Mean Price`[1] * 0.50)) %>% 
  mutate("90% Reduction" = (`Mean Price`[1] * 0.10)) 

as.data.frame(results_p_sum)

#View(results_p_sum)

#write.csv(results_p_sum, file = "results_p_sum.csv")
  

plot_parb = 
  ggplot(results_p_sum, aes(`Annual Tonnes`, `Mean Price`)) +
  geom_line(aes(`Annual Tonnes`, `25% Reduction`), linetype = "dashed", color = "red") +
  geom_line(aes( `Annual Tonnes`, `50% Reduction`), linetype = "dashed", color = "red") +
  geom_line(aes( `Annual Tonnes`, `90% Reduction`), linetype = "dashed", color = "red") +
  geom_line(color = "black", size = 2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "blue")+
  geom_hline(yintercept = 5.776472, linetype = "dashed", color = "blue")+
  annotate("text", x = 9, y = 5.9, label = "6% Reduction in Price with Current Production Capacity", size = 3)+
  annotate("text", x = 7, y = 4.75, label = "25% Reduction in Price", size = 3) + #, family = "Century Gothic"
  annotate("text", x = 12, y = 3.25, label = "50% Reduction in Price", size = 3) + #, family = "Century Gothic"
  annotate("text", x = 9, y = 0.75, label = "90% Reduction in Price", size = 3) + #, family = "Century Gothic"
  #scale_x_continuous(expand = c(0, 0)) + 
  #scale_y_continuous(expand = c(0, 0))+
  #scale_x_discrete(expand = c(0,0), limits = c(0,20)) + #limits = c(0, 20)) + 
  #scale_y_discrete(expand = c(0,0), limits = c(0,10)) +
  labs( x = "Tonnes of Aquaculture", y = "Price (USD2018 Per Gram)")+
  ggtitle("Reduction in Market Price per Increase in Aquaculture Output")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),plot.title = element_text(hjust=1, vjust=1, face = 'bold'))

print(plot_parb)

ggsave("plot_parb.png", plot_parb, dpi = 300, width = 6.5, height = 6.5)

#ggsave("plot_earb_1.png", plot_earb_.1, dpi = 300, width = 6.5, height = 6.5)