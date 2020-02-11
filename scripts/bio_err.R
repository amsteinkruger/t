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

# (Deprecated)
# Keep counterfactual runs. Bin runs by scale of aquaculture, then find quantile outcomes.
# results_cag = results_sum %>% 
#   filter(Scenario == "Foreign and Domestic Markets") %>% 
#   mutate(Cages = ifelse(Cages > 0, 
#                         ifelse(Cages > 25, 
#                                ifelse(Cages > 50, 
#                                       ifelse(Cages > 75, 
#                                              "75 - 100", 
#                                              "50 - 75"), 
#                                       "25 - 50"), 
#                                "1 - 25"),
#                         "0")) %>% 
#   group_by(Year,
#            Scenario,
#            Cages) %>% 
#   summarize(MedBio = median(SumBio),
#             ForBio = quantile(SumBio, 0.40),
#             SixBio = quantile(SumBio, 0.60)) %>% 
#   ungroup()

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
  ungroup() %>% 
  mutate(Label = ifelse(Scenario == "Status Quo",
                        "A",
                        ifelse(Scenario == "Aquaculture Intervention",
                               "B",
                               ifelse(Scenario == "Enforcement Intervention",
                                      "C",
                                      "D"))),
         Label = factor(Label, levels = c("A", "B", "C", "D")))

# Plot the summary numbers.
plot_bio = 
  ggplot(results_sce) + 
  geom_ribbon(aes(x = Year + 2016,
                  ymin = MinBio, 
                  ymax = MaxBio,
                  fill = Label,
                  color = Label)) +
  geom_ribbon(aes(x = Year + 2016,
                  ymin = TenBio, 
                  ymax = NinBio,
                  fill = Label,
                  color = Label)) +
  geom_point(aes(x = Year + 2016,
                 y = MedBio,
                 fill = Label,
                 color = Label),
             shape = 21,
             size = 1.50) +
  geom_vline(data = filter(results_sce, Scenario == "Aquaculture Intervention" | Scenario == "Aquaculture and Enforcement Interventions"),
             aes(xintercept = 3 + 2016),
             color = "firebrick4",
             linetype = "dashed") +
  geom_text(aes(x = 2018,
               y = 23000,
               label = Label)) +
  scale_color_manual(values = pal_col) +
  scale_fill_manual(values = pal_fil) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 25000),
                     labels = scales::comma) +
  scale_x_continuous(expand = c(0, 0.75),
                     breaks = c(2019, 2030, 2040)) +
  labs(x = "", y = "Biomass (Tonnes)") +
  theme_pubr() +
  theme(legend.background = element_rect(fill = "transparent"),
        legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  facet_wrap(~Label,
             nrow = 1)

# Plot only median outcomes with overlay instead of faceting.
plot_bio_over = 
  ggplot(results_sce) + 
  geom_line(aes(x = Year + 2016,
                 y = MedBio,
                 color = Label),
            alpha = 0.,
            size = 1.25) +
  geom_point(aes(x = Year + 2016,
                 y = MedBio,
                 fill = Label,
                 color = Label),
             shape = 21,
             size = 2.00) +
  geom_vline(aes(xintercept = 3 + 2016),
             color = "firebrick4",
             linetype = "dashed") +
  annotate("text",
           x = 2043,
           y = 14000,
           label = "A") +
  annotate("text",
           x = 2043,
           y = 18000,
           label = "B") +
  annotate("text",
           x = 2043,
           y = 18700,
           label = "C") +
  annotate("text",
           x = 2043,
           y = 19200,
           label = "D") +
  scale_color_manual(values = pal_col) +
  scale_fill_manual(values = pal_fil) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 25000),
                     labels = scales::comma) +
  scale_x_continuous(expand = c(0, 0.75),
                     limits = c(2016, 2044),
                     breaks = c(2019, 2030, 2040)) +
  labs(x = "", y = "Biomass (Tonnes)") +
  theme_pubr() +
  theme(legend.background = element_rect(fill = "transparent"),
        legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA))

plots_bio_over = 
  plot_bio_over +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Save.
ggsave("./out/plot_bio.png", 
       plot_bio,
       dpi = 300,
       bg = "transparent",
       width = 12, 
       height = 6)

ggsave("./out/plot_bio_over.png", 
       plot_bio_over,
       dpi = 300,
       bg = "transparent",
       width = 6, 
       height = 5)

# Compile plots.
plots_bio = 
  arrangeGrob(plot_bio,
              plots_bio_over,
              ncol = 2,
              widths = c(7, 2))

ggsave("./out/plots_bio.png", 
       plots_bio,
       dpi = 300,
       bg = "transparent",
       width = 18, 
       height = 5)
