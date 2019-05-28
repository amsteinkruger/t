# ---- data ----

# Parameters.
pars = read_csv("pars.csv") %>% 
  clean_names()

# Market
# Prices and characteristics of buche in end markets. Several sources.
ma_dat = read_csv("ma_dat.csv")

# Aquaculture
#  Marginal mortalities per month in aquaculture from COF (2017).
aq_mort_dat = read_csv("aq_mort_dat.csv") 


# Fishery
#  Biomass at age for 2017 from INAPESCA (2018).
fi_biom_dat = read_csv("fi_biom_dat.csv")

