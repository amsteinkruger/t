# ---- set ----

# Market
#  Estimate inverse demand.
nlm = nls(p ~ q * a + (g ^ b) + c, data = ma_dat, start = c(a = -5.00, b = 2.00, c = 20))
#  Clean results.
nlm_tidy = tidy(nlm)

# Aquaculture
#  Estimate incremental mortalities.
#  Regression:
am_reg = nls(m_months ~ b1 * exp(b2 * a_months), aq_mort_dat, start = list(b1 = 8.00, b2 = - 1.00))
#  Clean results.
am_reg_tidy = tidy(am_reg)


# Pull initial and intermediate parameters together.
pars_full = pars %>%
  # Add market outputs to parameter table.
  add_row(name_long = "Quantity Elasticity", name_short = "a_ma", "function" = "Demand", default = nlm_tidy$estimate[1], pessimistic = nlm_tidy$estimate[1] - nlm_tidy$std.error[1], optimistic = nlm_tidy$estimate[1] + nlm_tidy$std.error[1], units = "-", module = "Fishery", source_def = "Intermediate", source_pess = NA, source_opt = NA, same_pess = NA, same_opt = NA) %>% 
  add_row(name_long = "Size Premium", name_short = "b_ma", "function" = "Demand", default = nlm_tidy$estimate[2], pessimistic = nlm_tidy$estimate[2] - nlm_tidy$std.error[2], optimistic = nlm_tidy$estimate[2] + nlm_tidy$std.error[2], units = "-", module = "Fishery", source_def = "Intermediate", source_pess = NA, source_opt = NA, same_pess = NA, same_opt = NA) %>%
  add_row(name_long = "Choke Price", name_short = "c_ma", "function" = "Demand", default = nlm_tidy$estimate[3], pessimistic = nlm_tidy$estimate[3] - nlm_tidy$std.error[3], optimistic = nlm_tidy$estimate[3] + nlm_tidy$std.error[3], units = "-", module = "Fishery", source_def = "Intermediate", source_pess = NA, source_opt = NA, same_pess = NA, same_opt = NA) %>%
  # Add aquaculture outputs to parameter table.
  add_row(name_long = "Aq. Mortality Coefficient", name_short = "b1_mort_aq", "function" = "Aquaculture Mortality", default = am_reg_tidy$estimate[1], pessimistic = am_reg_tidy$estimate[1] + am_reg_tidy$std.error[1], optimistic = am_reg_tidy$estimate[1] - am_reg_tidy$std.error[1], units = "-", module = "Aquaculture", source_def = "Intermediate", source_pess = NA, source_opt = NA, same_pess = NA, same_opt = NA) %>%
  add_row(name_long = "Aq. Mortality Coefficient", name_short = "b2_mort_aq", "function" = "Aquaculture Mortality", default = am_reg_tidy$estimate[2], pessimistic = am_reg_tidy$estimate[2] + am_reg_tidy$std.error[2], optimistic = am_reg_tidy$estimate[2] - am_reg_tidy$std.error[2], units = "-", module = "Aquaculture", source_def = "Intermediate", source_pess = NA, source_opt = NA, same_pess = NA, same_opt = NA)

    
# Turn parameters into a matrix for multiple model runs.
pars = pars_full %>% 
  select(2, 4:6) %>%
  column_to_rownames(var = "name_short")

# Build out a matrix of parameters for sensitivity analysis.
pars[4:6] = pars[1:3]
pars[]

# Change parameters in new columns for sensitivity analysis.
pars["switch_aq",1:3] = 1
pars["switch_aq",4:6] = 0

# Turn parameters into a different matrix for analysis of outcomes from different arbitrary annual aquaculture outputs.
pars_arb = pars_full %>% 
  select(2, 4) %>% 
  column_to_rownames(var = "name_short")

pars_arb["switch_aq", 1] = 0 #turn aqua on/off
pars_arb["eta_limit", 1] = 0.1 #stiffness parameter, 1= free entry/exit, 0= no entry/exit
pars_arb[2:21] = pars_arb[1] #make 20 columns, 20 runs
pars_arb["y_arb", 2:21] = seq(1, 20)
