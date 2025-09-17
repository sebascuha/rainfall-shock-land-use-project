# Intro -------------------------------------------------------------------
# 
# This code works with Hydologic zones un colombia (IGAC-IDEAM)
# OUTPUTS:
# - "CreatedData/Temporary/munisXsubhidrozones.csv"
#


# Clean evinonment
rm(list=ls())

# Libraries
library(pacman)
p_load(tidyverse,dplyr,stringr,haven,readxl,zoo,sf,ggplot2,units)

# Paths
if(Sys.info()["user"]=="sebas"){
  main_path <- paste0("C:/Users/",Sys.info()["user"],"/Dropbox/Documents/Proyecto_UsosDeTierra/")
}else{
  main_path <- paste0("C:/Users/",Sys.info()["user"],
                      ) # Add your directory path
}

zona_hidro_path <- "RawData/Zonificacion_Hidrografica_2013/SHAPE/"
zona_hidro_name <- "Zonificacion_hidrografica_2013.shp"
munis_path <- "RawData/MUNICIPIOS_GEODANE/"
munis_name <- "MGN_MPIO_POLITICO.shp"
sensors_path <- "CreatedData/Temporary/IDEAM_sensors_coor/"

# Dealing with geometries -------------------------------------------------

# Load municipalities with DIVIPOLA code shapefile
munis_sf <- read_sf(paste0(main_path,
                           munis_path,
                           munis_name))%>%
  st_transform(crs=9377)
munis_sf$codmpio <- as.numeric(munis_sf$MPIO_CDPMP)
munis_sf<-munis_sf%>%select(codmpio,DPTO_CNMBR,MPIO_CNMBR,geometry)
colnames(munis_sf)<-c("codmpio","DEPTO","MUNI","geometry")
munis_sf$muni_area<-st_area(munis_sf)
# ggplot(data = munis_sf) + geom_sf()

# Load hidrologic subzones shapefile
hidro_zone_subZH <- read_sf(paste0(main_path,zona_hidro_path,zona_hidro_name)) %>%
  st_transform(crs=9377)
hidro_zone_subZH <- hidro_zone_subZH %>% arrange(COD_SZH)
duplicates<-hidro_zone_subZH[duplicated(hidro_zone_subZH$COD_SZH),]
hidro_zone_subZH <- hidro_zone_subZH[!duplicated(hidro_zone_subZH$geometry),]
duplicates<-hidro_zone_subZH[duplicated(hidro_zone_subZH$COD_SZH),]
# Cheking duplicated geoms
duplicates<-hidro_zone_subZH[duplicated(hidro_zone_subZH$COD_SZH),]
dup_hidro_zone_subZH<-hidro_zone_subZH%>%
  filter(COD_SZH%in%duplicates$COD_SZH)
# Searching on IDEAM map
# https://www.arcgis.com/home/webmap/viewer.html?webmap=103b63dcc9f448acbd63f22b728b1a02#!
# The 5309 has wrong shape, needed to merge his two geometryes
hidro_zone_subZH[301,"geometry"]<-st_union(hidro_zone_subZH[301,"geometry"],
                                           hidro_zone_subZH[302,"geometry"]) 
hidro_zone_subZH[302,"geometry"]<-hidro_zone_subZH[301,"geometry"] 
hidro_zone_subZH <- hidro_zone_subZH[!duplicated(hidro_zone_subZH$geometry),]
# Same geometries as subhidrologic zones?
length(unique(hidro_zone_subZH$COD_SZH))==length(unique(hidro_zone_subZH$geometry))
#ggplot(data = hidro_zone_subZH) + geom_sf()
#plot(hidro_zone_subZH$geometry)

# ------------------------------------------------------------------
# #If want the hidrologic zones
# 
#hidro_zone_ZH <- hidro_zone %>%
#  group_by(NOM_ZH) %>%
#  summarise(geometry_ZH = st_union(geometry))
#
# Loop that merge hydrographic subzones into hydrographic zones.
# lista <- c() # empty vector.
# for(i in 1:nrow(hidro_zone_ZH)){ # iteration in each hydrographic zone.
#   for (j in 1:nrow(hidro_zone)){ # iteration every observation on hydro_zone data.
#     if(hidro_zone$NOM_ZH[j]==hidro_zone_ZH$NOM_ZH[i]){ # conditional must be TRUE.
#       # cat(hidro_zone$NOM_ZH[i],"=",hidro_zone_ZH$NOM_ZH[i],"\n")
#       lista[j] <- st_geometry(hidro_zone_ZH[i,2]) # adding to list depend on the order.
#     }else{next}
#   }
# }
# hidro_zone_polyZH <- st_drop_geometry(hidro_zone)
# hidro_zone_polyZH$geometry <- lista
# #class(hidro_zone_polyZH)
# hidro_zone_polyZH <- st_as_sf(hidro_zone_polyZH) %>%
#   st_set_crs(st_crs(hidro_zone))
# #class(hidro_zone_polyZH)
# st_crs(hidro_zone_polyZH)
# print("Se creo la base de datos de zonas hidrologicas")
# rm(lista)
# #Graph shapefiles
#
#ggplot(data = hidro_zone_polyZH)+
#  geom_sf(aes(fill = factor(NOM_AH)))+
#  scale_color_viridis_d(option = "C")+
#  geom_sf(data=munis_sf)+
#  geom_sf_text(
#    data = munis_sf,
#    aes(label = codmpio),
#    check_overlap = TRUE,
#    size = 2,
#    color = "black",
#    nudge_x = -0.1,
#    nudge_y = 0.1
#  )


# continue ----------------------------------------------------------------

### Intersect geometries to find municipalities and hidrozones match
munisXhidro_zone_c <- st_intersection(hidro_zone_subZH,
                                      munis_sf)
colnames(munisXhidro_zone_c)
munisXhidro_zone_c <- munisXhidro_zone_c %>%
  select(codmpio,DEPTO,MUNI,muni_area,COD_AH,COD_ZH,COD_SZH,NOM_AH,NOM_ZH,NOM_SZH,geometry)

# Load IDEAM sensors location
sensors<-read_sf(paste0(main_path,sensors_path))
length(unique(sensors$CODIGO))==nrow(sensors)

# Adding sensors codes
munisXhidro_zone <- st_join(munisXhidro_zone_c,sensors)
munisXhidro_zone$shz_area <- st_area(munisXhidro_zone$geometry)

interes_vars <- c("codmpio","CODIGO","DEPTO","MUNI","muni_area","shz_area",
                  "COD_AH","COD_ZH","COD_SZH","NOM_AH","NOM_ZH","NOM_SZH",
                  "geometry")
munisXhidro_zone<-munisXhidro_zone%>%
  select(all_of(interes_vars))%>%
  mutate(muni_area_km2=set_units(muni_area,"km^2"),
         shz_area_km2=set_units(shz_area,"km^2"),
         weight_shz_in_muni_km = shz_area_km2/muni_area_km2,
         #weight_shz_in_muni_m = shz_area/muni_area,
         )
summary(munisXhidro_zone[,c("weight_shz_in_muni_km")])
temp <- munisXhidro_zone %>% filter(is.na(CODIGO)==T)

#### -- Saving data set -- ####
colnames(munisXhidro_zone)
write_csv(st_drop_geometry(munisXhidro_zone),
          paste0(main_path,"CreatedData/Temporary/munisXsubhidrozones.csv"))

sum(is.na(munisXhidro_zone$szh_area))
print("Hydrologic subzone were created with municipality intersection")
gc() # Clean RAM 
############################
print("Finished.")
############################

mapview::mapview(hidro_zone_subZH)
master_SBH<-read.csv(paste0(main_path,"CreatedData/Temporary/munisXsubhidrozones.csv"))

# ANALIZING ---------------------------------------------------------------
# Understanding treatment on municipalities -------------------------------
## Muni example
sub_muni_sf<-munis_sf[munis_sf$DEPTO=="CHOCÓ",]
sub_muni_sf<-munis_sf[munis_sf$MUNI=="CARMEN DEL DARIÉN",]
#sub_muni_sf<-sub_muni_sf[1:10,]
hidro_zone_subZH_intersects<-st_join(munisXhidro_zone,sub_muni_sf)

# Showing hidrographic zones in municipalities
pal = mapview::mapviewPalette("mapviewTopoColors")

mapview::mapview(sub_muni_sf,alpha.region=0)+
  mapview::mapview(hidro_zone_subZH_intersects,col_regions = pal(100),
                   zcol="NOM_SZH",alpha.regions = 0.3)
ggplot(sub_muni_sf)+geom_sf()+ 
  geom_sf(fill=hidro_zone_subZH_intersects$COD_SZH,
          data=hidro_zone_subZH_intersects)


