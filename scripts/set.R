# ---- set ----

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
pars_full = pars %>%
  # Add market outputs to parameter table.
  add_row(name_long = "Quantity Elasticity", 
          name_short = "a_ma", 
          "function" = "Demand", 
          default = nlm_tidy$estimate[1], 
          pessimistic = nlm_tidy$estimate[1] - nlm_tidy$std.error[1], 
          optimistic = nlm_tidy$estimate[1] + nlm_tidy$std.error[1], 
          units = "-", 
          module = "Fishery", 
          source_def = "Intermediate", 
          source_pess = NA, 
          source_opt = NA, 
          same_pess = NA, 
          same_opt = NA) %>% 
  add_row(name_long = "Size Premium", 
          name_short = "b_ma", "function" = "Demand", 
          default = nlm_tidy$estimate[2], 
          pessimistic = nlm_tidy$estimate[2] - nlm_tidy$std.error[2], 
          optimistic = nlm_tidy$estimate[2] + nlm_tidy$std.error[2], 
          units = "-", 
          module = "Fishery", 
          source_def = "Intermediate", 
          source_pess = NA, 
          source_opt = NA, 
          same_pess = NA, 
          same_opt = NA) %>%
  add_row(name_long = "Choke Price", 
          name_short = "c_ma", 
          "function" = "Demand", 
          default = nlm_tidy$estimate[3], 
          pessimistic = nlm_tidy$estimate[3] - nlm_tidy$std.error[3], 
          optimistic = nlm_tidy$estimate[3] + nlm_tidy$std.error[3], 
          units = "-", module = "Fishery", 
          source_def = "Intermediate", 
          source_pess = NA, 
          source_opt = NA, 
          same_pess = NA, 
          same_opt = NA) %>%
  # Add aquaculture outputs to parameter table.
  add_row(name_long = "Aq. Mortality Coefficient", 
          name_short = "b1_mort_aq", 
          "function" = "Aquaculture Mortality", 
          default = am_reg_tidy$estimate[1], 
          pessimistic = am_reg_tidy$estimate[1] + am_reg_tidy$std.error[1],
          optimistic = am_reg_tidy$estimate[1] - am_reg_tidy$std.error[1], 
          units = "-", 
          module = "Aquaculture", 
          source_def = "Intermediate", 
          source_pess = NA, 
          source_opt = NA, 
          same_pess = NA, 
          same_opt = NA) %>%
  add_row(name_long = "Aq. Mortality Coefficient", 
          name_short = "b2_mort_aq", 
          "function" = "Aquaculture Mortality", 
          default = am_reg_tidy$estimate[2], 
          pessimistic = am_reg_tidy$estimate[2] + am_reg_tidy$std.error[2], 
          optimistic = am_reg_tidy$estimate[2] - am_reg_tidy$std.error[2], 
          units = "-", 
          module = "Aquaculture", 
          source_def = "Intermediate", 
          source_pess = NA, 
          source_opt = NA, 
          same_pess = NA, 
          same_opt = NA)
    
# Turn parameters into a matrix for multiple model runs.
pars = pars_full %>% 
  select(2, 4:6) %>%
  column_to_rownames(var = "name_short")

# Build out a matrix of parameters for sensitivity analysis.
pars[4:6] = pars[1:3]

# Define the aquaculture switch for two scenarios.
pars["switch_aq", 1:3] = 0
pars["switch_aq", 4:6] = 1

# Extend the dataframe for n runs.
#  Define your n real quick. Put this into par.csv and be better about data management.
n = 1000 # n = 10000 ~> 105.3833m runtime (2019/7/3).
pars[7:(7 + n / 2)] = pars[1]
pars[(7 + n / 2 + 1):(6 + n)] = pars[4]

# Fill the spaghetti runs with draws from appropriate distributions by variable. If you squint hard enough, this part is great.
#  Fishery.
pars["e_2017", 7:(n + 6)] = runif(n, 
                                  min = pars["e_2017", 2], 
                                  max = pars["e_2017", 3])

pars["c_2017", 7:(n + 6)] = runif(n, 
                                  min = pars["c_2017", 2], 
                                  max = pars["c_2017", 3])

pars["eta_limit", 7:(n + 6)] = runif(n, 
                                     min = pars["eta_limit", 2], 
                                     max = pars["eta_limit", 3])

pars["y_arb", (7 + n / 2):(6 + n)] = runif(n / 2, 
                                           min = 2500, 
                                           max = 25000)

#  Aquaculture.
pars["sale_size_aq", 7:(n + 6)] = runif(n, 
                                        min = pars["sale_size_aq", 2], 
                                        max = pars["sale_size_aq", 3])

pars["cage_size_aq", 7:(n + 6)] = runif(n, 
                                        min = pars["cage_size_aq", 2], 
                                        max = pars["cage_size_aq", 3])

pars["by1", 7:(n + 6)] = runif(n, 
                                        min = pars["by1", 2], 
                                        max = pars["by1", 3])

pars["by2", 7:(n + 6)] = runif(n, 
                               min = pars["by2", 2], 
                               max = pars["by2", 3])

#pars["mmin_aq", 7:(n + 6)] = runif(n, 
#                                   min = pars["mmin_aq", 3], 
#                                   max = pars["mmin_aq", 2])

#pars["disc_aq", 7:(n + 6)] = runif(n, 
#                                   min = pars["disc_aq", 3], 
#                                   max = pars["disc_aq", 2])

# Band-Aid for NAs.
#pars = pars[ , colSums(is.na(pars)) == 0]
