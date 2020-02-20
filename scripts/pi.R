# ---- pi ----

# Summarize profit impact.
results_pi = 
  results %>% 
  select(-Age) %>% 
  filter(Scenario == "Status Quo" | Scenario == "Aquaculture Intervention") %>% 
  filter(Variable == "Aquaculture Profit" | Variable == "Poaching Profit") %>%
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
  mutate(Years = ceiling(Year * 0.25)) %>% # Summarize mean differences by four-year average.
  group_by(Years,
           Variable,
           Cages) %>% 
  summarize(Mea = mean(Mea)) %>% 
  ungroup() %>% 
  mutate(Years = ifelse(Years > 1, 
                        ifelse(Years > 2,
                               ifelse(Years > 3,
                                      ifelse(Years > 4,
                                             ifelse(Years > 5,
                                                    "2037 - 2042",
                                                    "2033 - 2036"),
                                             "2029 - 2032"),
                                      "2025 - 2028"),
                               "2021 - 2024"),
                        "2017 - 2020")) %>% 
  mutate(Years = as.factor(Years))


# Plot differences.
#  First, tweak the manual fill palette.
pal_fil = viridis(4, 
                  begin = 0.00, 
                  end = 0.50, 
                  direction = -1, 
                  option = "D")

#  Then plot.
plot_pi = 
  ggplot(data = results_pi) + 
  geom_col(aes(x = Years,
               y = Mea,
               fill = Cages),
           position = "dodge") +
  geom_hline(aes(yintercept = 0),
             color = "firebrick",
             linetype = "dashed") +
  scale_fill_manual(values = pal_fil) +
  labs(x = "", 
       y = "\u0394 \u03C0 (US$M 2018)", 
       fill = "Net Pens") +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 0.50,
                                   vjust = 0.60),
        legend.background = element_rect(fill = "transparent"),
        legend.position = "top",
        legend.text = element_text(margin = margin(l = 2.5, r = 2.5), hjust = 0),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  facet_grid(rows = vars(Variable),
             scales = "free")

# Print for .Rmd
print(plot_pi)

# Save.
ggsave("./out/plot_pi.png", 
       plot_pi, 
       dpi = 300,
       width = 5, 
       height = 10,
       bg = "transparent")
