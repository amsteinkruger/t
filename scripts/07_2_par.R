# Principal Component Analysis of parameters on biomass in final year.
#  Get an R package to visualize PCA.
#   # Install from Github ("vqv/ggbiplot").
library(ggbiplot)

# Wrangle results. 
#  Wrangle parameters by run.
par_pca = 
  pars_0 %>% 
  rownames_to_column("name") %>% 
  bind_cols(pars_1) %>% 
  t() %>%
  as.data.frame() %>% 
  row_to_names(1) %>% 
  mutate(Run = seq(1, nrow(.)))

# Wrangle results into biomass in final year by run. You might run this w/ profits and effort, too. Just pivot and pray.
res_pca = results %>% 
  filter(Variable == "Numbers" & Year == max(Year)) %>% 
  filter(Scenario == "Status Quo" | Scenario == "Aquaculture Intervention") %>% 
  mutate(Biomass = fun_l_w(pars_base["a_lw", 1], 
                           fun_a_l(Age - 0.5, 
                                   pars_base["linf_al", 1], 
                                   pars_base["k_al", 1], 
                                   pars_base["t0_al", 1]),  
                           pars_base["b_lw", 1]) / 1000 * Result) %>% 
  dplyr::group_by(Run, 
                  Scenario) %>% 
  dplyr::summarize(Sum = sum(Biomass, na.rm = TRUE)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(Run = ifelse(Scenario == "Aquaculture Intervention",
                             Run + n,
                             Run))



# Try the whole thing again, but with a little more taste in choosing variables.
in_pca = 
  left_join(par_pca,
            res_pca,
            by = "Run") %>% 
  select(Scenario,
         'Initial Biomass' = nprop,
         'Initial Effort' = e_2017,
         'Effort Cost' = c_2017,
         'Effort Entry' = eta_limit,
         # 'Stocking Density' = dens,
         'Aquaculture Export' = switch_aq,
         # 'Aquaculture Mortality' = mmin,
         'Final Biomass' = Sum) # Keep columns w/ bootstrapping. Constants don't work for PCA.
groups_pca =
  in_pca %>% 
  select(Scenario)

in_pca = 
  in_pca %>% 
  select(-Scenario) %>%
  mutate_all(~ as.numeric(as.character(.x)))


# Run refined PCA.
pca_par = prcomp(in_pca, scale = TRUE)

# Plot refined PCs.
vis_par =
  ggbiplot(pca_par,
           groups = as.character(groups_pca$Scenario),
           ellipse = FALSE,
           alpha = 0.25,
           varname.size = 3.25,
           varname.adjust = 1.05) +
  scale_color_manual(values = c(pal_col[2], pal_col[1])) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(-2.5, 2)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-2, 2)) +
  labs(x = "Standardized Principal Component (1) (1.34 SD, 0.30 Var)",
       y = "Standardized Principal Component (2) (1.00 SD, 0.17 Var)") +
  theme_pubr() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none")

vis_par$layers <- c(vis_par$layers[[2]], vis_par$layers[[1]], vis_par$layers[[3]]) # Change order of layers.

print(vis_par)

# Save!
ggsave("./out/vis_par.png", 
       vis_par,
       dpi = 300,
       width = 6.5, 
       height = 7.0)

# Tabulate refined PCA.
#  yoink importance and print
par_imp = data.frame(summary(pca_par)$importance)
print(par_imp)
#  yoink rotations and print
par_rot = data.frame(summary(pca_par)$rotation)
print(par_rot)

# Save tables.
# Importances of PCs.
kable(par_imp, "html") %>% cat(., file = "./out/par_imp.html")
# Rotations of variables in PCs.
kable(par_rot, "html") %>% cat(., file = "./out/par_rot.html")