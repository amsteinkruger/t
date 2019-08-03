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

# Stock and catch of reproductive biomass in numbers and mass.
results_sum =  results %>% 
  #na.omit() %>% # This is a Band-Aid to filter out any surprise NAs. Swap out for a better solution in set.R.
  #filter(Age < 3) %>% # This is a quick trick to filter out juveniles and subadults and keep reproductive biomass.
  mutate(Biomass = fun_l_w(pars$default[4], fun_a_l(Age - 0.5, pars$default[1], pars$default[2], pars$default[3]),  pars$default[5]) / 1000 * Result) %>% 
  group_by(Year, 
           Variable, 
           Run, 
           Scenario) %>% 
  summarize(SumNum = sum(Result), 
            SumBio = sum(Biomass)) %>% 
  ungroup() %>% 
  mutate(Run = as.factor(Run))

# Ditto, but by mean of runs.
results_summer = results_sum %>% 
  group_by(Year, 
           Variable, 
           Scenario) %>% 
  summarize(AveNum = mean(SumNum),
            AveBio = mean(SumBio)) %>% 
  ungroup()

# Plot the summary numbers.
#  Check whether inputs are tonnes or numbers.
plot_n_lin = 
  ggplot() + 
  # Threshold (e.g. K * 0.6)
  geom_hline(yintercept = 12136, 
             size = 0.95, 
             color = "#EF5645") +
  # Runs.
  geom_line(data = filter(results_sum, 
                          Variable == "Numbers"),
            aes(x = Year + 2016, 
                y = SumBio, 
                group = Run, 
                color = Scenario), 
            size = 1.15, 
            alpha = 0.05) + 
  # Mean of runs.
  geom_point(data = filter(results_summer, 
                          Variable == "Numbers"),
            aes(x = Year + 2016,
                y = AveBio,
                color = Scenario),
            size = 1.25) +
  scale_color_manual(values = c("#04859B", "#003660")) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 20000),
                     labels = scales::comma) + 
  scale_x_continuous(expand = c(0, 0.25)) +  
  labs(x = "Year", y = "Biomass (Tonnes)") + 
  theme_classic() + #base_family = "avenir"
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none") +
  facet_wrap(~Scenario)

#plot_n_bin = 
#  ggplot(filter(results_sum, 
#                Variable == "Numbers"), 
#         aes(x = Year + 2016, 
#             y = SumBio)) +
#  geom_bin2d(binwidth = c(1, 500)) +
  #geom_point(data = filter(results_summer, 
  #                         Variable == "Numbers"),
  #           aes(x = Year + 2016,
  #               y = AveBio,
  #               color = Scenario),
  #           size = 1.25,
  #           linetype = "dotted") +
#  geom_hline(yintercept = 12136, 
#             size = 0.95, 
#             color = "#EF5645") +
#  scale_fill_viridis_c(option = "E") +
#  scale_y_continuous(expand = c(0, 0), 
#                     limits = c(0, 20000),
#                     labels = scales::comma) + #, 
#  scale_x_continuous(expand = c(0, 0.25)) +  
#  labs(x = "Year", 
#       y = "Biomass (Tonnes)") + #Tonnes of Biomass
#  theme_classic() + #base_family = "avenir"
#  theme(panel.grid.major = element_blank(), 
#        panel.grid.minor = element_blank(),
#        panel.background = element_rect(fill = "grey75"),
#        #panel.background = element_rect(fill = "transparent", color = NA),
#        plot.background = element_rect(fill = "transparent", color = NA),
#        legend.position = "none") +  
#  facet_wrap(~Scenario)

# Data wrangling for profit. This generalizes well to price and production.
results_sum_pi = results %>% 
  filter(Variable == "Aquaculture Profit" |
         Variable == "Poaching Profit")

# Profits for both scenarios.
plot_pi = 
  ggplot(results_sum_pi, 
         aes(x = factor(Year + 2016), 
             y = Result / 1000000, 
             fill = Variable)) +
  geom_boxplot(color = "black") + 
  scale_fill_brewer(palette = "Set1", 
                    direction = -1) +
  scale_color_brewer(palette = "Set1", 
                     direction = -1) +
  scale_y_continuous(labels = scales::comma,
                     limits = c(-50,50)) +
  scale_x_discrete(expand = c(0.05, 0.05), 
                   breaks = c(2017, 2025, 2032)) + 
  labs(x = "", 
       y = "") +
  theme_classic() +
  facet_wrap(~Variable + Scenario)

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

# Production for both scenarios.
plot_p = 
  ggplot(results_sum_p, 
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

# Principal Component Analysis of parameters on biomass in final year.
#  Watch out for plyr-dplyr conflict in loading these libraries - fix wrt key.Rmd (1).
#library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
library(factoextra)

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
pca = prcomp(results_pca, scale = TRUE)

# Plot PCs.
plot_pc = 
ggbiplot(pca, 
         groups = factor(results_pca$Scenario), 
         ellipse = TRUE,
         alpha = 0.10) +
  scale_color_brewer(palette = "Set1", 
                     direction = -1) +
  theme_classic() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none")

# Tabulate PCA.
# factoextra option.
#factoextra::facto_summarize(pca, "var")
# w/o factoextra: yoink importance.
pca_imp <- data.frame(summary(pca)$importance)
# ditto rotations (what are these someone help)
pca_rot <- data.frame(summary(pca)$rotation)

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
# PCA.
ggsave("plot_pc.png", 
       plot_pc, 
       dpi = 300, 
       width = 6.5, 
       height = 6.5)

# Save tables, too.
#library(kableExtra)
# Importances of PCs.
#kable(pca_imp, "html") %>%
#  cat(., file = "pca_imp.html")
# Rotations of varaibles in PCs.
#kable(pca_rot, "html") %>%
#  cat(., file = "pca_rot.html")

print(plot_n_lin)
print(plot_pi)
print(plot_pr)
print(plot_p)
print(plot_pc)
