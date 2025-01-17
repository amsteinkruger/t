# Test sensitivity of biomass outcomes to demand and substitution.
# define wrapper function that takes parameters and returns scalar difference of counterfactual and status quo median biomass.
fun_opt = function(scale, 
                   pars){
  
  pars["c_cages", "b"] = 10
  pars["cage_size", "b"] = scale / 10 # Band-Aid to get a continuous-ish input. This works out to m^3 of production.
  
  out_0 =
    pars %>%
    select(a) %>%
    fun %>%
    filter(Variable == "Numbers") %>%
    mutate(Biomass = fun_l_w(pars_base["a_lw", 1], 
                             fun_a_l(Age - 0.5, 
                                     pars_base["linf_al", 1], 
                                     pars_base["k_al", 1], 
                                     pars_base["t0_al", 1]),  
                             pars_base["b_lw", 1]) / 1000 * Result) %>% 
    group_by(Year) %>%
    summarize(Sum = sum(Biomass)) %>%
    ungroup() %>%
    filter(Year == max(Year)) %>%
    pull(Sum)

  out_1 =
    pars %>%
    select(b) %>%
    fun %>%
    filter(Variable == "Numbers") %>%
    mutate(Biomass = fun_l_w(pars_base["a_lw", 1], 
                             fun_a_l(Age - 0.5, 
                                     pars_base["linf_al", 1], 
                                     pars_base["k_al", 1], 
                                     pars_base["t0_al", 1]),  
                             pars_base["b_lw", 1]) / 1000 * Result) %>% 
    group_by(Year) %>%
    summarize(Sum = sum(Biomass)) %>%
    ungroup() %>%
    filter(Year == max(Year)) %>%
    pull(Sum)

  dif = abs(out_1 - out_0)

  return(dif)
  
}

fun_opter = function(dem,
                     sub,
                     pars){
  
  pars = 
    pars %>% 
    mutate(b = ifelse(names == "dem",
                      dem,
                      ifelse(names == "sub",
                             sub,
                             b))) %>%  # Change out null values for demand and substitution changes for matrix values.
    column_to_rownames("names")
  
  opt = optim(par = 0, # Give a starting value for scale.
              fn = fun_opt,
              method = "Brent",
              lower = 0,
              upper = 100000,
              pars = pars)
  
  return(list(opt$par, 
              opt$value))
}

# Get parameters together. You might get a cleaner outcome by finding medians of bootstrapped parameters.
par_0 = 
  pars_base %>% # Snag parameters for the status quo.
  select(1) %>% # Keep the central estimates (and remember that central estimates != inputs for median outcome).
  rename(a = mid) %>% # Get a unique name to avoid overwriting at join.
  rownames_to_column("names")

par_1 =
  pars_base %>% # ""
  select(1) %>% # ""
  rename(b = mid) %>% # ""
  rownames_to_column("names") %>% 
  mutate(b = ifelse(names == "switch_aq",
                    1,
                    b))

pars = 
  inner_join(par_0, 
             par_1)
  
# Get a matrix of parameters for demand and substitution.  
mat = expand_grid(dem = seq(1.00, 2.00, by = 0.10), 
                  sub = seq(0.00, 1.00, by = 0.10))

# Optimize on full set.
opt = 
  mat %>% 
  mutate(opt = map2(.x = dem,
                    .y = sub,
                    .f = fun_opter,
                    pars = pars))

# Manipulate results.
use =
  opt %>%
  unnest(opt) %>%
  mutate(opt = as.numeric(opt),
         set = rep(1:(nrow(.) / 2), each = 2),
         val = rep(1:2, nrow(.) / 2)) %>% 
  pivot_wider(names_from = val,
              values_from = opt) %>% 
    rename(scale = `1`,
           diff = `2`)

# Plot results.
vis_dem = 
  use %>% 
  mutate(scale = ifelse(diff > 500,
                        NA,
                        scale)) %>% 
  ggplot() +
  geom_raster(aes(x = sub,
                  y = dem,
                  fill = scale / 100000)) +
  geom_text(aes(x = sub,
                y = dem,
                label = ifelse(round(scale / 100000, 2) == 1, "1.00+", round(scale / 100000, 2))),
            size = 3.25) +
  scale_x_reverse(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis(direction = -1,
                     na.value = "grey90") +
  guides(fill = guide_colorbar(barwidth = 15,
                               barheight = 0.5,
                               ticks = FALSE)) +
  labs(x = "Substitution (Ratio of Prices for Aquaculture and Fishery Products)",
       y = "Demand (Scaling of Estimated Choke Price, 2014-2018)",
       fill = expression(paste("Aquaculture Scale (", 10^6, m^3, ")"))) +
  theme_pubr()

# Print.
print(vis_dem)

# Save.
ggsave("./out/vis_dem.png",
       vis_dem,
       dpi = 300,
       width = 6.5)