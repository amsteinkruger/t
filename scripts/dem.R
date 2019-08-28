# ---- dem ----

# Test sensitivity of biomass outcomes to demand.

# in: optimize demand to outweigh production for each production in a range of scenarios
# wrapper function to feed demand value into range of scenarios
fun_dem = 

# arguments are parameter matrix and choke multiplier and n
  
# first set production to single input
  
# run model with single input and choke multiplier
  
# parse outcomes to compare medians
  
# return distance of medians (with production scenario and choke multiplier?) 
  
  
  
# run optimization function for range of scenarios
# set range of production scenarios
# iterate

# minimize distance of medians

# bind optimization results



# Plot the production-demand frontier.
plot_dem = 
  ggplot() +
  geom_path(data = ,
            aes(x = ,
                y = ),
            color = ,
            size = ) +
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
  geom_hline(yintercept = ) +
  theme_classic()

# Print for .Rmd.
print(plot_dem)

# Save.
ggsave("./out/plot_dem.png", plot_dem, dpi = 300, width = 6.5, height = 4.5)