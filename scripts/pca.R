# ---- pca ----

# Principal Component Analysis of parameters on biomass in final year.
# Wrangle parameter sets for association with corresponding runs.
#pars_pca = pars %>% 
#  select(7:length(pars)) %>% 
#  t() %>% 
#  as_tibble() %>% 
#  rownames_to_column("Run") 

# Set up results w/o age structure w/ parameters for PCA.
#results_pca = results_sum %>% # This is reproductive biomass only (7+).
#  filter(Variable == "Numbers", 
#         Year == max(Year)) %>% # Get rid of non-stock results and reduce down to final year's results.
#  left_join(pars_pca, by = "Run") %>%  # Join parameters by run. Check that fact-char conversion sometime.
  #select_if(function(.) n_distinct(.) != 1) %>% # Filter out cols, especially parameters, without variance.
  #select(-Run, -SumNum) %>% # Filter out unhelpful cols.
#  mutate(Scenario = ifelse(Scenario == "w/ Aquaculture", # Mind the hard-coding for values.
#                           1,
#                           0)) %>% 
#  select(Scenario, SumBio) %>% #, c_2017 #, cage_size_aq, by1, by2 #, e_2017, eta_limit, sale_size_aq
#  na.omit()

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