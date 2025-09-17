# Clean environment
rm(list=ls())
# Load needed libraries
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(sf,terra,tidyverse,dplyr)

# Assign paths
main_path = "C:/Users/sebas/Dropbox/Documents/Proyecto_UsosDeTierra"
setwd(main_path)

rasters_path  <- "RawData/MapBiomas/"
muni_path     <- "RawData/MUNICIPIOS_GEODANE"

# Parameters
CRS = "epsg:32618"

# 1. Load Municipality SHP & List rasters ------------------------------------
munis_sf  <- read_sf(paste0(muni_path))%>%st_transform(crs=CRS)
#munis_sf<-munis_sf[!duplicated(munis_sf$MPIO_CDPMP),] No duplicated
rast_list <- list.files(rasters_path)

#### Prueba ####
ras<-terra::rast(paste0(rasters_path,rast_list[1]))
ras<-terra::project(ras,CRS,method="near")
pixels<-sum(ras[],na.rm = TRUE)
ras_muni <- terra::mask(ras,terra::vect(munis_sf$geometry[1]))
gc()
pixels<-sum(ras_muni[],na.rm = TRUE)
print(pixels)
################



#library(parallel)
# Calculate the number of cores
#no_cores <- detectCores() - 1
# Initiate cluster
#cl <- makeCluster(no_cores)

