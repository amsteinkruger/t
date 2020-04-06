# ---- pca ----

# Out of date as of 2/11. Rehashing forward.

# Principal Component Analysis of parameters on biomass in final year.
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
  group_by(Run, 
           Scenario) %>% 
  summarize(Sum = sum(Biomass, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Run = ifelse(Scenario == "Aquaculture Intervention",
                      Run + 1000,
                      Run))

# Squish parameters and results together, then pull groups out for visualization.
in_pca_0 = 
  left_join(par_pca,
            res_pca,
            by = "Run") %>% 
  select(Scenario,
         e_2017,
         c_2017,
         g_r,
         eta_limit,
         c_cages,
         sale_size,
         mmin,
         dens,
         disc,
         switch_aq,
         biomass = Sum) %>% # Keep columns w/ bootstrapping. Constants don't work for PCA.
  mutate_if(is.factor, ~ as.numeric(as.character(.x)))

groups_pca_0 =
  in_pca_0 %>% 
  select(Scenario)

in_pca_0 = 
  in_pca_0 %>% 
  select(-Scenario)

# Run naive PCA
pca_0 = prcomp(in_pca_0, scale = TRUE)

# Plot naive PCs.
vis_pca_0 =
  ggbiplot(pca_0,
           groups = as.character(groups_pca_0$Scenario),
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

# Tabulate naive PCA.
# factoextra option.
factoextra::facto_summarize(pca_0, "var")
# w/o factoextra: yoink importance.
pca_imp_0 <- data.frame(summary(pca_0)$importance)
# ditto rotations (what are these someone help)
pca_rot_0 <- data.frame(summary(pca_0)$rotation)

# Try the whole thing again, but with a little more taste in choosing variables.
in_pca_1 = 
  left_join(par_pca,
            res_pca,
            by = "Run") %>% 
  select(Scenario,
         'Initial Biomass' = nprop,
         'Initial Effort' = e_2017,
         'Effort Cost' = c_2017,
         'Effort Entry' = eta_limit,
         'Stocking Density' = dens,
         'Aquaculture Export' = switch_aq,
         'Aquaculture Mortality' = mmin,
         'Final Biomass' = Sum) %>% # Keep columns w/ bootstrapping. Constants don't work for PCA.
  mutate_if(is.factor, ~ as.numeric(as.character(.x)))

groups_pca_1 =
  in_pca_1 %>% 
  select(Scenario)

in_pca_1 = 
  in_pca_1 %>% 
  select(-Scenario)

# Run refined PCA.
pca_1 = prcomp(in_pca_1, scale = TRUE)

# Plot refined PCs.
# vis_pca_1_r =
#   ggbiplot(pca_1,
#            groups = as.character(groups_pca_1$Scenario),
#            ellipse = TRUE,
#            alpha = 0.25,
#            var.axes = FALSE,
#            varname.size = 0) +
#   scale_color_manual(values = c(pal_col[2], pal_col[1])) +
#   scale_x_continuous(expand = c(0, 0),
#                      limits = c(-2, 2)) +
#   scale_y_continuous(expand = c(0, 0),
#                      limits = c(-2, 2)) +
#   theme_pubr() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_rect(fill = "transparent", color = NA),
#         plot.background = element_rect(fill = "transparent", color = NA),
#         legend.position = "none",
#         axis.title.y = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank())

vis_pca_1_l =
  ggbiplot(pca_1,
           groups = as.character(groups_pca_1$Scenario),
           ellipse = TRUE,
           alpha = 0.15,
           varname.size = 3.25,
           varname.adjust = 1.05) +
  scale_color_manual(values = c(pal_col[2], pal_col[1])) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(-2, 2)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-2, 2)) +
  theme_pubr() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none")

# vis_pca_1_scree =
#   ggscreeplot(pca_1,
#               type = "pev") +
#   scale_x_continuous(expand = c(0, 0.25),
#                      limits = c(1, 8),
#                      breaks = c(1, 2, 3, 4, 5, 6, 7, 8)) +
#   scale_y_continuous(expand = c(0, 0),
#                      limits = c(0, 0.25),
#                      breaks = c(0, 0.05, 0.10, 0.15, 0.20, 0.25)) +
#   theme_pubr()
# 
# # Tabulate refined PCA.
# # factoextra option.
# factoextra::facto_summarize(pca_1, "var")
# # w/o factoextra: yoink importance.
# pca_imp <- data.frame(summary(pca_1)$importance)
# # ditto rotations (what are these someone help)
# pca_rot <- data.frame(summary(pca_1)$rotation)
# 
# # Compile plots.
# vis_pca = 
#   arrangeGrob(vis_pca_1_l,
#               vis_pca_1_r,
#               vis_pca_1_scree,
#               ncol = 3,
#               widths = c(6.25, 6, 3))

# Save plots.
ggsave("./out/vis_pca.png", 
       vis_pca_1_l,
       dpi = 300,
       width = 7.25, 
       height = 7.25)

# Save tables, too.
# Importances of PCs.
#kable(pca_imp, "html") %>%
#  cat(., file = "pca_imp.html")
# Rotations of varaibles in PCs.
#kable(pca_rot, "html") %>%
#  cat(., file = "pca_rot.html")