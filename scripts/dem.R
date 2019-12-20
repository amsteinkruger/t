# # ---- dem ----
# 
# # Test sensitivity of biomass outcomes to demand.
# 
# # in: optimize demand to outweigh production for each production in a range of scenarios
# # wrapper function to feed demand value into range of scenarios
# 
# # Get median status quo outcome first.
# med = results %>%
#   filter(Scenario == "Status Quo") %>%
#   filter(Year == max(Year)) %>%
#   filter(Variable == "Numbers") %>%
#   mutate(Biomass = fun_l_w(pars_base[4, 1],
#                            fun_a_l(Age - 0.5,
#                                    pars_base[1, 1],
#                                    pars_base[2, 1],
#                                    pars_base[3, 1]),
#                            pars_base[5, 1]) / 1000 * Result) %>%
#   group_by(Year,
#            Run) %>%
#   summarize(SumNum = sum(Result),
#             SumBio = sum(Biomass)) %>%
#   ungroup() %>%
#   filter(Year == max(Year)) %>%
#   summarize(Bio = median(SumBio)) %>%
#   as.numeric()
# 
# # Define function to optimize over.
# fun_dem = function(dem, opt){
# 
#   # Argument "opt" is
#   #  (1) parameters and initial values for the counterfactual forecast,
#   #  (2) capitalization in cages,
#   #  (3) the median biomass outcome of the final year in the status quo forecast.
#   #  (Position matters. That could be fixed by naming list items for optimx, maybe?)
#   med = opt[[3]]
#   par = opt[[1]]
#   par["c_ma",] = par["c_ma",] * dem
#   par["c_cages",] = opt[[2]]
#   n = ncol(par)
# 
#   results = vector("list", n)
# 
#   for(i in 1:n){output = fun(par[i])
#                 output$Run = i
#                 results[[i]] = output}
# 
#   results = bind_rows(results)
# 
#   out = results %>%
#     filter(Variable == "Numbers") %>%
#     mutate(Biomass = fun_l_w(par[4, 1],
#                              fun_a_l(Age - 0.5,
#                                      par[1, 1],
#                                      par[2, 1],
#                                      par[3, 1]),
#                              par[5, 1]) / 1000 * Result) %>%
#     group_by(Year,
#              Run) %>%
#     summarize(SumNum = sum(Result),
#               SumBio = sum(Biomass)) %>%
#     ungroup() %>%
#     filter(Year == max(Year)) %>%
#     summarize(Bio = median(SumBio)) %>%
#     as.numeric() %>%
#     `-` (med) #%>%
#     #abs()
# 
#   return(out)
# 
# }
# 
# # test
# fun_dem(2.75, opt = list(pars_1, 100, med))
# # neat! it works (2019/09/12)
# 
# 
# 
# # Optimize proportional, instantaneous change in demand to match increasing capitalization.
# #  Functionally identical to optimizing capitalization to match increasing demand, but easier to compute.
# watch_go = proc.time()
# 
# # this sort of works, but not all that well
# opt = optimx(par = 1,
#              fn = fun_dem,
#              opt = list(pars_1, 10, med),
#              lower = 1,
#              upper = 10,
#              control = list(all.methods = TRUE,
#                             save.failures = TRUE,
#                             trace = 0,
#                             maxit = 100))
# 
# watch_stop = proc.time() - watch_go
# print(watch_stop)
# 
# # this sort of works, but less well
# nlm = nlminb(start = 1,
#              objective = fun_dem,
#              opt = list(pars_1, 10, med),
#              lower = 1,
#              control = list(step.min = 0.01,
#                             step.max = 0.50))
# 
# # set range of production scenarios
# # iterate
# 
# # results of pseudo-optimization by hand, down to ~25t error in biomass w/ 100 runs.
# # slapdash = data.frame(Cages = c(5, 10, 15, 25, 50, 75, 100),
# #                       Demand = c(1.125, 1.25, 1.415, 1.65, 2.05, 2.375, 2.75))
# 
# # Plot the production-demand frontier.
# plot_dem =
#   ggplot() +
#   # geom_ribbon(data = ,
#   #             aes(x = ,
#   #                 ymin = 0,
#   #                 ymax = ),
#   #             fill = ) +
#   # geom_ribbon(data = ,
#   #             aes(x = ,
#   #                 ymin = ,
#   #                 ymax = Inf),
#   #             fill = ) +
#   # geom_path(data = slapdash,
#   #           aes(x = Demand,
#   #               y = Cages),
#   #           #color = ,
#   #           size = 1.05) +
#   geom_hline(yintercept = 10,
#              linetype = "dashed") +
#   labs(x = "Quantities Demanded (Proportion of 2017 Estimate)",
#        y = "Cages") +
#   theme_classic()
# 
# # Print for .Rmd.
# print(plot_dem)
# 
# # Save.
# ggsave("./out/plot_dem.png", plot_dem, dpi = 300, width = 6.5, height = 4.5)