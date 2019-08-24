# ---- map ----

# Generate a map of the Gulf of California displaying local topographies, bathymetries, and features of interest in publication quality.

# Libraries
#library(readr)
#library(dplyr)
#library(ggplot2)
#library(grid)
#library(sf)
#library(rgdal)
#library(rnaturalearth)

# Data
#  Read data from local sources.
#   Seizures.
#dat_int = read_csv("tma_int.csv")
#  IUCN Range.
#dat_range = st_read("tma_range.gpkg")

# Read data from cloud sources.
#  Land.
#ocean = ne_download(scale = 110, type = "ocean", category = "physical", returnclass = "sf")
#  Countries.
#count = ne_download(scale = 10, type = "countries", category = "cultural", returnclass = "sf")
#  Coastlines.
#coast = ne_download(scale = 10, type = "coastline", category = "physical", returnclass = "sf")
# Rivers.
#river = ne_download(scale = 10, type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")
# Bathymetry.
#bathy0 = ne_download(scale = 10, type = "bathymetry_L_0", category = "physical", returnclass = "sf")
#bathy200 = ne_download(scale = 10, type = "bathymetry_K_200", category = "physical", returnclass = "sf")
#bathy1000 = ne_download(scale = 10, type = "bathymetry_J_1000", category = "physical", returnclass = "sf")
#bathy2000 = ne_download(scale = 10, type = "bathymetry_I_2000", category = "physical", returnclass = "sf")
#bathy3000 = ne_download(scale = 10, type = "bathymetry_H_3000", category = "physical", returnclass = "sf")
#bathy4000 = ne_download(scale = 10, type = "bathymetry_G_4000", category = "physical", returnclass = "sf")
# Smoosh bathymetric data.
#bathy = rbind(bathy0, 
#              bathy200, 
#              bathy1000, 
#              bathy2000, 
#              bathy3000, 
#              bathy4000)

# Urban Areas.
#urban = ne_download(scale = 10, type = "urban_areas", category = "cultural", returnclass = "sf")