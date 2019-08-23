# ---- vis ----

# Set-up.
# For rayshading heatmap experiment.
#remotes::install_github("tylermorganwall/rayshader")
#library(rayshader)
# For fonts.
#library(showtext)
# For joining and formatting plots.
#library(gridExtra)
#library(ggpubr)

# Load Avenir from an open text file in the working directory.
#  package showtext works for certain graphic devices so that saved files display Avenir but previews in RStudio do not.
#font_paths(".")
#font_add("avenir", "avenir.otf")
#showtext_auto()

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
plot_n_lin = 
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

# Data wrangling for profit. This generalizes well to price and production.
results_sum_pi = results %>%
  filter(Variable == "Aquaculture Profit" |
         Variable == "Poaching Profit") %>% 
  mutate(Result = Result * 0.000001) %>% # Convert to millions.
  mutate(Sign = ifelse(Result > 0, 1, 0)) %>% # Get a dummy for sign of results for scale tricks.
  select(-Age, -Group) %>% 
  na.omit()

# Profits.
plot_pi_pos = 
  ggplot() +
  geom_bin2d(data = results_sum_pi, 
             aes(x = Year + 2016,
                 y = Result,
                 alpha = factor(Sign)),
             binwidth = c(1, 25)) +
  scale_x_continuous(expand = c(0.05, 0.05), 
                     breaks = c(2017, 2027)) + 
  scale_y_continuous(limits = c(-100, 200),
                     breaks = c(-100, 0, 200)) +
  scale_fill_distiller(palette = "Blues",
                       direction = 1) +
  scale_alpha_manual(values = c(0, 1)) +
  labs(x = "", 
       y = "") +
  theme_classic() +
  facet_wrap(~Variable + Scenario)

plot_pi_neg = 
  ggplot() +
  geom_bin2d(data = results_sum_pi, 
             aes(x = Year + 2016,
                 y = Result,
                 alpha = factor(Sign)),
             binwidth = c(1, 25)) +
  scale_x_continuous(expand = c(0.05, 0.05), 
                     breaks = c(2017, 2027)) + 
  scale_y_continuous(limits = c(-100, 200),
                     breaks = c(-100, 0, 200)) +
  scale_fill_distiller(palette = "Oranges",
                       direction = 1) +
  scale_alpha_manual(values = c(1, 0)) +
  labs(x = "", 
       y = "") +
  theme_classic() +
  facet_wrap(~Variable + Scenario)

# Put in a tricky bit of grid work here for overlapping plots. Or not.

# Data wrangling for differences.
results_sum_pi_dif = 
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
plot_pi_dif = 
  ggplot(data = results_sum_pi_dif) + 
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

# Data wrangling for production.
results_sum_pr = results %>% 
  filter(Variable == "Aquaculture Production (Wet Tonnes)")

# Production for both scenarios.
plot_pr = 
  ggplot(results_sum_pr, 
         aes(x = factor(Year + 2016), 
             y = Result)) +
  geom_boxplot(color = "black") + 
  scale_fill_brewer(palette = "Set1", 
                    direction = -1) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(expand = c(0.05, 0.05), 
                   breaks = c(2017, 2025, 2032)) + 
  labs(x = "", 
       y = "") +
  theme_classic() +
  facet_wrap(~Scenario)

# Data wrangling for price (a = 13).
results_sum_p = results %>% 
  filter(Variable == "Price")

# Prices for both scenarios.
plot_p = 
  ggplot(results_sum_p, 
         aes(x = factor(Year + 2016), 
             y = Result,
             color = Cages),
         alpha = 0.50) +
  geom_jitter() +
  scale_color_viridis_c(option = "magma") +
  #geom_boxplot(color = "black") + 
  #scale_fill_brewer(palette = "Set1", 
  #                  direction = -1) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(expand = c(0.05, 0.05), 
                   breaks = c(2017, 2025, 2032)) + 
  labs(x = "", 
       y = "") +
  theme_classic() +
  facet_wrap(~Scenario)

# Principal Component Analysis of parameters on biomass in final year.
# Wrangle parameter sets for association with corresponding runs.
pars_pca = pars %>% 
  select(7:length(pars)) %>% 
  t() %>% 
  as_tibble() %>% 
  rownames_to_column("Run") 

# Set up results w/o age structure w/ parameters for PCA.
results_pca = results_sum %>% # This is reproductive biomass only (7+).
  filter(Variable == "Numbers", 
         Year == max(Year)) %>% # Get rid of non-stock results and reduce down to final year's results.
  left_join(pars_pca, by = "Run") %>%  # Join parameters by run. Check that fact-char conversion sometime.
  #select_if(function(.) n_distinct(.) != 1) %>% # Filter out cols, especially parameters, without variance.
  #select(-Run, -SumNum) %>% # Filter out unhelpful cols.
  mutate(Scenario = ifelse(Scenario == "w/ Aquaculture", # Mind the hard-coding for values.
                           1,
                           0)) %>% 
  select(Scenario, SumBio) %>% #, c_2017 #, cage_size_aq, by1, by2 #, e_2017, eta_limit, sale_size_aq
  na.omit()

# Run PCA
#pca = prcomp(results_pca, scale = TRUE)

# Plot PCs.
#plot_pc = 
#ggbiplot(pca, 
#         groups = factor(results_pca$Scenario), 
#         ellipse = TRUE,
#         alpha = 0.10) +
#  scale_color_brewer(palette = "Set1", 
#                     direction = -1) +
#  theme_classic() +
#  theme(panel.grid.major = element_blank(), 
#        panel.grid.minor = element_blank(),
#        panel.background = element_rect(fill = "transparent", color = NA),
#        plot.background = element_rect(fill = "transparent", color = NA),
#        legend.position = "none")

# Tabulate PCA.
# factoextra option.
#factoextra::facto_summarize(pca, "var")
# w/o factoextra: yoink importance.
#pca_imp <- data.frame(summary(pca)$importance)
# ditto rotations (what are these someone help)
#pca_rot <- data.frame(summary(pca)$rotation)

  # Save interesting plots.
# Biomass.
ggsave("plot_n_lin.png", 
       plot_n_lin, 
       dpi = 300, 
       width = 4.5, 
       height = 6.0)
#ggsave("plot_n_bin.png", 
#       plot_n_bin, 
#       dpi = 300, 
#       width = 4.5, 
#       height = 6.0)
# Profit.
ggsave("plot_pi.png", 
       plot_pi, 
       dpi = 300, 
       width = 6.5, 
       height = 6.5)
# Price.
ggsave("plot_p.png", 
       plot_p, 
       dpi = 300, 
       width = 6.5, 
       height = 6.5)
# PCA.
#ggsave("plot_pc.png", 
#       plot_pc, 
#       dpi = 300, 
#       width = 6.5, 
#       height = 6.5)

# Save tables, too.
# Importances of PCs.
#kable(pca_imp, "html") %>%
#  cat(., file = "pca_imp.html")
# Rotations of varaibles in PCs.
#kable(pca_rot, "html") %>%
#  cat(., file = "pca_rot.html")

print(plot_n_lin)
#print(plot_pc)
