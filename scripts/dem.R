# ---- dem ----

# Test sensitivity of biomass outcomes to demand.

# in: optimize demand to outweigh production for each production in a range of scenarios
# wrapper function to feed demand value into range of scenarios

# Get median status quo outcome first.

fun_dem = function(dem, par, cag, med){
  
  par = par
  par["c_ma"] = par["c_ma"] * dem
  par["c_cages"] = cag
  n = ncol(par)
  
  results = vector("list", n)
  
  for(i in 1:n){par = select(par, i)
  output = fun(par)
  results[[i]] = output}
  
  results = bind_rows(results)
  
  out = results %>% 
    filter(Variable == "Numbers") %>% 
    mutate(Biomass = fun_l_w(par[4, 1], 
                             fun_a_l(Age - 0.5, 
                                     par[1, 1], 
                                     par[2, 1], 
                                     par[3, 1]),  
                             par[5, 1]) / 1000 * Result) %>% 
    group_by(Year,
             Run) %>% 
    summarize(SumNum = sum(Result), 
              SumBio = sum(Biomass)) %>% 
    ungroup() %>% 
    filter(Year == max(Year)) %>% 
    summarize(Bio = median(SumBio)) %>% 
    as.numeric() %>% 
    `-` (med)
  
  return(out)
    
}

# run optimization function for range of scenarios
# set range of production scenarios
# iterate

# minimize distance of medians

# bind optimization results



# Plot the production-demand frontier.
plot_dem = 
  ggplot() +
  geom_ribbon(data = ,
              aes(x = ,
                  ymin = 0,
                  ymax = ),
              fill = ) +
  geom_ribbon(data = ,
              aes(x = ,
                  ymin = ,
                  ymax = Inf),
              fill = ) +
  geom_path(data = ,
            aes(x = ,
                y = ),
            color = ,
            size = ) +
  geom_hline(yintercept = ) +
  theme_classic()

# Print for .Rmd.
print(plot_dem)

# Save.
ggsave("./out/plot_dem.png", plot_dem, dpi = 300, width = 6.5, height = 4.5)