# Principal Component Analysis of parameters on biomass in final year.
#  Get an R package to visualize PCA.
#   # Install from Github ("vqv/ggbiplot").
library(ggbiplot)

# Wrangle results. 
res_pca = 
  results %>% 
  filter(Variable == "Numbers" | Variable == "Catches" | Variable == "Poaching Profit" | Variable == "Aquaculture Profit" | Variable == "Price") %>% 
  filter(Scenario == "Status Quo" | Scenario == "Aquaculture Intervention") %>% 
  mutate(Result = ifelse(Variable == "Numbers" | Variable == "Catches",
                         fun_l_w(pars_base["a_lw", 1], 
                                 fun_a_l(Age - 0.5, 
                                         pars_base["linf_al", 1], 
                                         pars_base["k_al", 1], 
                                         pars_base["t0_al", 1]),  
                                 pars_base["b_lw", 1]) / 1000 * Result,
                         Result)) %>% 
  mutate(Run = ifelse(Scenario == "Aquaculture Intervention", # Get unique numbers for scenarios.
                      Run + n,
                      Run),
         Intervention = ifelse(Scenario == "Status Quo", # Get a numeric scenario column.
                                        0,
                                        1)) %>% 
  group_by(Year,
           Run,
           Scenario,
           Intervention,
           Variable) %>% 
  dplyr::summarize(Result = sum(Result, na.rm = TRUE)) %>% 
  ungroup %>% 
  pivot_wider(names_from = Variable,
              values_from = Result) %>% 
  dplyr::select(-Year,
                -Run) %>% 
  dplyr::rename(Stock = Numbers,
                Catch = Catches)

groups_pca =
  res_pca %>% 
  select(Scenario)

in_pca = 
  res_pca %>% 
  select(-Scenario) %>% 
  mutate_all(~ as.numeric(as.character(.x)))

# Run refined PCA.
pca_pan = prcomp(in_pca, scale = TRUE)

# Plot refined PCs.
vis_pan =
  ggbiplot(pca_pan,
           groups = as.character(groups_pca$Scenario),
           ellipse = FALSE,
           alpha = 0.05,
           varname.size = 3.25,
           varname.adjust = 1.05) +
  scale_color_manual(values = c(pal_col[2], pal_col[1])) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(-2, 3)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-3, 3)) +
  labs(x = "Standardized Principal Component (1) (1.48 SD, 0.37 Var)",
       y = "Standardized Principal Component (2) (1.26 SD, 0.26 Var)") +
  theme_pubr() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none")

vis_pan$layers <- c(vis_pan$layers[[2]], vis_pan$layers[[1]], vis_pan$layers[[3]]) # Change order of layers.

print(vis_pan)

# Save!
ggsave("./out/vis_pan.png", 
       vis_pan,
       dpi = 300,
       width = 6.5)

# Tabulate refined PCA.
#  yoink importance and print
pan_imp = data.frame(summary(pca_pan)$importance)
print(pan_imp)
#  yoink rotations and print
pan_rot = data.frame(summary(pca_pan)$rotation)
print(pan_rot)

# Save tables.
# Importances of PCs.
kable(pan_imp, "html") %>% cat(., file = "./out/pan_imp.html")
# Rotations of variables in PCs.
kable(pan_rot, "html") %>% cat(., file = "./out/pan_rot.html")