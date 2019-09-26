# ---- set ----

# Visualization
#  Set up palettes.
pal_fil = viridis(4, 
                  begin = 0.00, 
                  end = 0.50, 
                  direction = -1, 
                  option = "D")
pal_col = viridis(4, 
                  begin = 0.00, 
                  end = 0.50, 
                  direction = -1, 
                  option = "D")

# Market
#  Estimate inverse demand.
nlm = nls(p ~ q * a + (g ^ b) + c, 
          data = dat_p, 
          start = c(a = -5.00, b = 2.00, c = 20))
#  Clean results.
nlm_tidy = tidy(nlm)

# Aquaculture
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
          mid = nlm_tidy$estimate[1], 
          low = nlm_tidy$estimate[1] - nlm_tidy$std.error[1], 
          high = nlm_tidy$estimate[1] + nlm_tidy$std.error[1], 
          units = "-", 
          module = "Fishery", 
          source_def = "Intermediate", 
          source_pess = NA, 
          source_opt = NA) %>% 
  add_row(name_long = "Size Premium", 
          name_short = "b_ma", 
          "function" = "Demand", 
          mid = nlm_tidy$estimate[2], 
          low = nlm_tidy$estimate[2] - nlm_tidy$std.error[2], 
          high = nlm_tidy$estimate[2] + nlm_tidy$std.error[2], 
          units = "-", 
          module = "Fishery", 
          source_def = "Intermediate", 
          source_pess = NA, 
          source_opt = NA) %>%
  add_row(name_long = "Choke Price", 
          name_short = "c_ma", 
          "function" = "Demand", 
          mid = nlm_tidy$estimate[3], 
          low = nlm_tidy$estimate[3] - nlm_tidy$std.error[3], 
          high = nlm_tidy$estimate[3] + nlm_tidy$std.error[3], 
          units = "-", module = "Fishery", 
          source_def = "Intermediate", 
          source_pess = NA, 
          source_opt = NA) %>%
  # Add aquaculture outputs to parameter table.
  add_row(name_long = "Aq. Mortality Coefficient", 
          name_short = "b1_mort_aq", 
          "function" = "Aquaculture Mortality", 
          mid = am_reg_tidy$estimate[1], 
          low = am_reg_tidy$estimate[1] + am_reg_tidy$std.error[1],
          high = am_reg_tidy$estimate[1] - am_reg_tidy$std.error[1], 
          units = "-", 
          module = "Aquaculture", 
          source_def = "Intermediate", 
          source_pess = NA, 
          source_opt = NA) %>%
  add_row(name_long = "Aq. Mortality Coefficient", 
          name_short = "b2_mort_aq", 
          "function" = "Aquaculture Mortality", 
          mid = am_reg_tidy$estimate[2], 
          low = am_reg_tidy$estimate[2] + am_reg_tidy$std.error[2], 
          high = am_reg_tidy$estimate[2] - am_reg_tidy$std.error[2], 
          units = "-", 
          module = "Aquaculture", 
          source_def = "Intermediate", 
          source_pess = NA, 
          source_opt = NA)
    
# Turn parameters into a matrix for multiple model runs.
pars_base = pars_full %>% 
  select(2, 4:6) %>%
  column_to_rownames(var = "name_short")

# Define n runs.
n = 2500

# Build n runs w/o aquaculture.
pars_0 = pars_base[1]
pars_0[2:n] = pars_0[1]

# Draws. 
#  Switch.
pars_0["switch_aq", ] = 0
#  Fishery.
pars_0["nprop", ] = rnorm(n, mean = 1, sd = 0.085)

#pars_0["f_2017", ] = runif(n, 
#                           min = pars_base["f_2017", 2], 
#                           max = pars_base["f_2017", 3])

pars_0["e_2017", ] = runif(n, 
                           min = pars_base["e_2017", 2], 
                           max = pars_base["e_2017", 3])

pars_0["c_2017", ] = runif(n, 
                           min = pars_base["c_2017", 2], 
                           max = pars_base["c_2017", 3])

pars_0["eta_limit", ] = runif(n, 
                              min = pars_base["eta_limit", 2], 
                              max = pars_base["eta_limit", 3])

#  Aquaculture.
pars_0["sale_size_aq", 1:n] = runif(n, 
                                    min = pars_base["sale_size_aq", 2], 
                                    max = pars_base["sale_size_aq", 3])

#pars_0["cage_size_aq", 1:n] = runif(n, 
#                                        min = pars_base["cage_size_aq", 2], 
#                                        max = pars_base["cage_size_aq", 3])

pars_0["dens_aq", 1:n] = runif(n, 
                               min = pars_base["dens_aq", 2], 
                               max = pars_base["dens_aq", 3])

pars_0["mmin_aq", 1:n] = runif(n, 
                               min = pars_base["mmin_aq", 2], 
                               max = pars_base["mmin_aq", 3])

pars_0["disc_aq", 1:n] = runif(n, 
                               min = pars_base["disc_aq", 2], 
                               max = pars_base["disc_aq", 3])

pars_0["by1", 1:n] = runif(n, 
                           min = pars_base["by1", 2], 
                           max = pars_base["by1", 3])

pars_0["by2", 1:n] = runif(n, 
                           min = pars_base["by2", 2], 
                           max = pars_base["by2", 3])

pars_0["c_cages", 1:n] = ceiling(runif(n, 
                                       min = pars_base["c_cages", 2], 
                                       max = pars_base["c_cages", 3]))

# Build n runs w/ aquaculture.
pars_1 = pars_0
pars_1["switch_aq", ] = 1
