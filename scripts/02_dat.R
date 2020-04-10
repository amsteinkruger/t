# Parameters from several sources noted in the file.
dat_par = read_csv("./data/dat_pars.csv") %>% 
  clean_names()

# Prices and characteristics of buche in end markets. Several sources.
dat_p = read_csv("./data/dat_p.csv")

#  Marginal mortalities per month in aquaculture from Cygnus Ocean Farms (2017).
dat_aqm = read_csv("./data/dat_aqm.csv") 

#  Biomass at age for 2017 from INAPESCA (2018).
dat_bio = read_csv("./data/dat_bio.csv")