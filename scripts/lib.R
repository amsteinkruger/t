# ---- lib ----

# Check out packages.
#  Watch out for plyr-dplyr conflict in loading.
# library(devtools)
# install_github("vqv/ggbiplot") # For updates to ggbiplot development.
library(plyr) # Fixing plyr loading after dplyr in vis.R.
library(knitr)
library(tidyverse)
library(janitor)
library(broom)
library(reshape2)
library(ggpubr)
library(ggbiplot)
library(optimx)
library(factoextra)
library(viridisLite)
library(rgdal)
library(rnaturalearth)