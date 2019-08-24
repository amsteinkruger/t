# ---- pi ----

# Summarize profit impact.
results_pi = 
  results %>% 
  select(-Age, -Group) %>% 
  filter(Variable == "Aquaculture Profit" |
           Variable == "Poaching Profit") %>% 
  mutate(Variable = str_remove(string = Variable, 
                               pattern = " Profit")) %>%  
  mutate(Result = Result * 0.000001) %>%
  mutate(Cages = ifelse(Cages > 0, 
                        ifelse(Cages > 25, 
                               ifelse(Cages > 50, 
                                      ifelse(Cages > 75, 
                                             "75 - 100", 
                                             "50 - 75"), 
                                      "25 - 50"), 
                               "1 - 25"),
                        "0")) %>% 
  na.omit() %>% # Track down origin of NAs.
  group_by(Year, 
           Run,
           Variable, 
           Scenario,
           Cages) %>% 
  mutate(Result = ifelse(Scenario == "Counterfactual", 
                         Result, 
                         -Result)) %>% 
  ungroup() %>% 
  group_by(Year,
           Run,
           Variable,
           Cages) %>% 
  summarize(Difference = sum(Result)) %>% 
  ungroup() %>% 
  group_by(Year, 
           Variable,
           Cages) %>% 
  summarize(Med = median(Difference),
            Mea = mean(Difference))


# Plot differences.
plot_pi = 
  ggplot(data = results_pi) + 
  geom_col(aes(x = Year + 2016,
               y = Mea,
               fill = Cages),
           position = "dodge") +
  geom_hline(aes(yintercept = 0),
             color = "firebrick",
             linetype = "dashed") +
  scale_fill_manual(values = c("steelblue1", 
                               "steelblue2", 
                               "steelblue3", 
                               "steelblue4")) +
  labs(x = "", y = "\u0394 \u03C0 (US$M 2018)") +
  scale_x_continuous(breaks = c(2017, 2022, 2027),
                     expand = c(0, 0.75)) + 
  theme_classic() +
  facet_wrap(~Variable)

# Print for .Rmd
print(plot_pi)

# Save.
ggsave("./out/plot_pi.png", plot_pi, width = 8.5, height = 5.5)