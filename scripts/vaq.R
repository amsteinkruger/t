# ---- vaq ----

# Describe outcomes for vaquita with different scenarios.
vaq = 
  data.frame("year" = seq(1, 50),
             "worse" = seq(1, 50) ^ -0.90,
             "bad" = seq(1, 50) ^ -0.50,
             "better" = seq(1, 50) ^ -0.10) %>% 
  pivot_longer(cols = c("worse", 
                        "bad",
                        "better"),
               names_to = "scenario",
               values_to = "population") %>% 
  ggplot() +
  geom_line(aes(x = year,
                y = population,
                color = scenario)) +
  theme_classic()

ggsave("plot_vaq.png", 
       dpi = 300, 
       height = 5.0, 
       width = 5.0)
