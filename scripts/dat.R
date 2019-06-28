# ---- dat ----

# Parameters.
pars = read_csv("./data/dat_pars.csv") %>% 
  clean_names()

# Market
# Prices and characteristics of buche in end markets. Several sources.
dat_p = read_csv("./data/dat_p.csv")

# Aquaculture
#  Marginal mortalities per month in aquaculture from COF (2017).
dat_aqm = read_csv("./data/dat_aqm.csv") 


# Fishery
#  Biomass at age for 2017 from INAPESCA (2018).
dat_b = read_csv("./data/dat_b.csv")