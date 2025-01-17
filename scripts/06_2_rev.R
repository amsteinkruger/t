# Summarize revenue impact.
results_rev = 
  results %>% 
  select(-Age) %>% 
  filter(Scenario == "Status Quo" | Scenario == "Aquaculture Intervention") %>% 
  filter(Variable == "Aquaculture Revenue" | Variable == "Poaching Revenue") %>%
  mutate(Variable = str_remove(string = Variable,
                               pattern = " Revenue")) %>%
  mutate(Result = Result * 0.000001) %>%
  mutate(Cages = ifelse(Cages > 0,
                        ifelse(Cages > 25,
                               ifelse(Cages > 50,
                                      ifelse(Cages > 75,
                                             "0.75-1.00",
                                             "0.50-0.75"),
                                      "0.25-0.50"),
                               "0.01-0.25"),
                        "0")) %>% # Bin scale.
  drop_na() %>% # Track down origin of NAs.
  group_by(Year, 
           Run,
           Variable, 
           Scenario,
           Cages) %>% 
  mutate(Result = ifelse(Scenario == "Aquaculture Intervention", 
                         Result, 
                         -Result)) %>% # Change sign on status quo runs for tidy difference calculation.
  ungroup() %>% 
  group_by(Year,
           Run,
           Variable,
           Cages) %>% 
  summarize(Difference = sum(Result)) %>% # Sum runs by run for tidy difference.
  ungroup() %>% 
  group_by(Year, 
           Variable,
           Cages) %>% 
  summarize(Mea = mean(Difference)) %>% # Summarize differences by scale bin.
  ungroup %>% 
  mutate(Years = ceiling(Year * 0.33)) %>% # Summarize mean differences by four-year average.
  group_by(Years,
           Variable,
           Cages) %>% 
  summarize(Mea = mean(Mea)) %>% 
  ungroup() %>% 
  mutate(Years = ifelse(Years > 1,
                        ifelse(Years > 2,
                               ifelse(Years > 3,
                                             ifelse(Years > 4,
                                                    "2029 - 2031",
                                             "2026 - 2028"),
                                      "2023 - 2025"),
                               "2020 - 2022"),
                        "2017 - 2019")) %>%
  mutate(Years = as.factor(Years))


# Plot differences.
#  First, tweak the manual fill palette.
pal_fil = viridis(4, 
                  begin = 0.00, 
                  end = 0.50, 
                  direction = -1, 
                  option = "D")

#  Then plot.
vis_rev = 
  ggplot(data = results_rev) + 
  geom_col(aes(x = Years,
               y = Mea,
               fill = Cages),
           position = "dodge",
           width = 0.80) +
  geom_hline(aes(yintercept = 0),
             color = "firebrick",
             linetype = "dashed") +
  scale_fill_manual(values = pal_fil) +
  labs(x = "", 
       y = "Effects on Revenue by Sector (US$M 2018)", 
       fill = expression(paste("Aquaculture Scale (", 10^6, m^3, ")"))) +
  theme_pubr() +
  theme(axis.text.x = element_text(size = 10,
                                   angle = 45,
                                   hjust = 0.50,
                                   vjust = 0.60),
        axis.text.y = element_text(size = 10),
        legend.position = "right",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9)) +
  facet_grid(rows = vars(Variable),
             scales = "free")

# Print.
print(vis_rev)

# Save.
ggsave("./out/vis_rev.png",
       vis_rev,
       dpi = 300,
       width = 6.5)