# ---- map ----

# Generate a map of the Gulf of California displaying local topographies, bathymetries, and features of interest in publication quality.

# Coordinate Reference System
#crs = "+init=epsg:4485"
crs = "+proj=robin"

# Data
#  Interdictions.
dat_int = read_csv("./data/dat_int.csv")

dat_int = 
  st_as_sf(dat_int, 
           coords = c("long", "lat"), 
           crs = "+proj=longlat") %>% 
  st_transform(crs)

# Read data from rnaturalearth.
#  Dry.
land = 
  ne_download(scale = 10, 
              type = "land", 
              category = "physical", 
              returnclass = "sf") %>% 
  st_transform(crs)

#  Wet.
ocean = 
  ne_download(scale = 10, 
                    type = "ocean", 
              category = "physical",
              returnclass = "sf") %>% 
  st_transform(crs)

#  Wet among dry.
river = 
  ne_download(scale = 10, 
              type = "rivers_lake_centerlines", 
              category = "physical", 
              returnclass = "sf") %>% 
  st_transform(crs)

# Concrete.
urban = 
  ne_download(scale = 10, 
              type = "urban_areas", 
              category = "cultural", 
              returnclass = "sf") %>% 
  st_transform(crs)

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


#  Define area.
lonbox = c(-115, -109, -109, -115, -115)
latbox = c(33, 33, 22.75, 22.75, 33)
#  Transform area to CRS.
box = cbind(lonbox, latbox) %>% 
  list() %>% 
  st_polygon() %>% 
  st_sfc(crs = "+proj=longlat") %>% 
  st_transform(crs = crs) %>% 
  st_bbox

# Plot.
plot_map = 
  ggplot() + 
  geom_sf(data = ocean, 
          color = NA, 
          fill = "slategray1") +
  geom_sf(data = land, 
          color = NA, 
          fill = "slategray3") +
  geom_sf(data = urban, 
          color = NA, 
          fill = "slategray4") +
  geom_sf(data = river, 
          color = "slategray1") +
  geom_sf(data = dat_int,
          shape = 1) +
  #scale_fill_manual(values = c("#EF5645", "#7A8D39")) +
  #geom_sf(data = points, aes(fill = type), pch = 21, size = 2.25) +
  coord_sf(xlim = c(box["xmin"], 
                    box["xmax"]), 
           ylim = c(box["ymin"], 
                    box["ymax"]), 
           datum = NA) +
  #labs(caption = "Here's your caption.", title = "Here's your title.") +
  theme_classic() #+
  #theme(panel.background = element_rect(fill = "#003660", colour = NA)) +
  #theme(legend.position = "none")

# Print for .Rmd
print(plot_map)

# Save.
ggsave("./out/plot_map.png", plot_map)
