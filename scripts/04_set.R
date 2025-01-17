# Set up palettes for visualization.
pal_fil = viridis(4, 
                  begin = 0.00, 
                  end = 0.50, 
                  direction = -1, 
                  option = "D",
                  alpha = 0.50)

pal_col = viridis(4, 
                  begin = 0.00, 
                  end = 0.50, 
                  direction = -1, 
                  option = "D")

# Set up price model.
#  Estimate inverse demand.
#   Nonlinear model doesn't really pay off, but here's the code anyhow.
# nlm = nls(p ~ q * a + (g ^ b) + c, 
#           data = dat_p, 
#           start = c(a = -5.00, b = 2.00, c = 20))

#   Linear model works fine.
lm_p = lm(p ~ q + g,
          data = dat_p)

#  Clean results.
lm_tidy = tidy(lm_p)

# Set up aquaculture model.
#  Estimate incremental mortalities.
#  Regression:
am_reg = nls(m_months ~ b1 * exp(b2 * a_months), dat_aqm, start = list(b1 = 8.00, b2 = - 1.00))
#  Clean results.
am_reg_tidy = tidy(am_reg)

# Pull initial and intermediate parameters together.
pars_full = dat_par %>%
  # Add market outputs to parameter table.
  add_row(name_long = "Quantity Elasticity", 
          name_short = "a_ma", 
          "function" = "Demand", 
          mid = lm_tidy$estimate[2], 
          low = lm_tidy$estimate[2] - lm_tidy$std.error[2], 
          high = lm_tidy$estimate[2] + lm_tidy$std.error[2], 
          units = NA, 
          module = "Fishery", 
          source = "Intermediate") %>% 
  add_row(name_long = "Size Premium", 
          name_short = "b_ma", 
          "function" = "Demand", 
          mid = lm_tidy$estimate[3], 
          low = lm_tidy$estimate[3] - lm_tidy$std.error[3], 
          high = lm_tidy$estimate[3] + lm_tidy$std.error[3], 
          units = NA, 
          module = "Fishery", 
          source = "Intermediate") %>%
  add_row(name_long = "Choke Price", 
          name_short = "c_ma", 
          "function" = "Demand", 
          mid = lm_tidy$estimate[1], 
          low = lm_tidy$estimate[1] - lm_tidy$std.error[1], 
          high = lm_tidy$estimate[1] + lm_tidy$std.error[1], 
          units = NA, 
          module = "Fishery", 
          source = "Intermediate") %>%
  # Add aquaculture outputs to parameter table.
  add_row(name_long = "Aq. Mortality Coefficient", 
          name_short = "b1_mort", 
          "function" = "Aquaculture Mortality", 
          mid = am_reg_tidy$estimate[1], 
          low = am_reg_tidy$estimate[1] + am_reg_tidy$std.error[1],
          high = am_reg_tidy$estimate[1] - am_reg_tidy$std.error[1], 
          units = NA, 
          module = "Aquaculture", 
          source = "Intermediate") %>%
  add_row(name_long = "Aq. Mortality Coefficient", 
          name_short = "b2_mort", 
          "function" = "Aquaculture Mortality", 
          mid = am_reg_tidy$estimate[2], 
          low = am_reg_tidy$estimate[2] + am_reg_tidy$std.error[2], 
          high = am_reg_tidy$estimate[2] - am_reg_tidy$std.error[2], 
          units = NA, 
          module = "Aquaculture", 
          source = "Intermediate")
    
# Turn parameters into a matrix for multiple model runs.
pars_base = pars_full %>% 
  select(2, 4:6) %>%
  column_to_rownames(var = "name_short")

# Define n runs.
n = 5000

# Build n runs w/o aquaculture.
pars_0 = pars_base[1]
pars_0[2:n] = pars_0[1]

# Draws. 
#  Fishery.
pars_0["nprop", ] = rnorm(n, 
                          mean = 1, 
                          sd = 0.085)

pars_0["e_2017", ] = runif(n,
                           min = pars_base["e_2017", 2],
                           max = pars_base["e_2017", 3])

pars_0["c_2017", ] = runif(n,
                           min = pars_base["c_2017", 2],
                           max = pars_base["c_2017", 3])

pars_0["eta_limit", ] = runif(n,
                              min = pars_base["eta_limit", 2],
                              max = pars_base["eta_limit", 3])

pars_0["multi_en", ] = runif(n,
                           min = pars_base["multi_en", 2],
                           max = pars_base["multi_en", 3])

pars_0["c_enf", ] = runif(n,
                           min = pars_base["c_enf", 2],
                           max = pars_base["c_enf", 3])

pars_0["g_r", ] = runif(n,
                          min = pars_base["g_r", 2],
                          max = pars_base["g_r", 3])
 
#  Aquaculture.
pars_0["sale_size", 1:n] = runif(n,
                                    min = pars_base["sale_size", 2],
                                    max = pars_base["sale_size", 3])

pars_0["dens", 1:n] = runif(n,
                               min = pars_base["dens", 2],
                               max = pars_base["dens", 3])

pars_0["mmin", 1:n] = runif(n,
                               min = pars_base["mmin", 2],
                               max = pars_base["mmin", 3])

pars_0["disc", 1:n] = runif(n,
                               min = pars_base["disc", 2],
                               max = pars_base["disc", 3])

pars_0["c_cages", 1:n] = ceiling(runif(n,
                                       min = pars_base["c_cages", 2],
                                       max = pars_base["c_cages", 3]))

# Build n runs w/ aquaculture.
pars_1 = pars_0
pars_1["switch_aq", ] = 1
# Build n runs w/ increased enforcement.
pars_2 = pars_0
pars_2["switch_en", ] = 1
# Build n runs w/ aquaculture and increased enforcement.
pars_3 = pars_0
pars_3["switch_aq", ] = 1
pars_3["switch_en", ] = 1