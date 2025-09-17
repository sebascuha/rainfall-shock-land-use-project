# Intro -------------------------------------------------------------------
# Clean evinonment
rm(list=ls())
# Libraries
library(pacman)
p_load(tidyverse,sf)

# Setting relevant paths and working directory
if(Sys.info()["user"]=="sebas"){
  main_path <- paste0("C:/Users/",Sys.info()["user"],
                      "/Dropbox/Documents/Proyecto_UsosDeTierra/")
}else{
  main_path <- paste0("C:/Users/",Sys.info()["user"],
                      "") # Pon el resto de la direccion donde se encuentra la informacion en tu computador
}

setwd(main_path)
ninia_10_11_path <- "RawData/Áreas inundadas 2011/SHAPE/"
munis_path <- "RawData/MUNICIPIOS_GEODANE/"
resguardos <- "RawData/RESGUARDO_INDIGENA_LEGALIZADO_-1375114163527104882.geojson"

# 1. Load data ---------------------------------------------------------------
# Municipalities data
munis_sf<-read_sf(munis_path,quiet=T)%>%st_transform(9377)
munis_sf$area_muni<-st_area(munis_sf)
colnames(munis_sf)
munis_sf<-munis_sf%>%select(codmpio=MPIO_CDPMP,namempio=MPIO_CNMBR,area_muni,geometry)

# Inundaciones Data
ninia_2010<-read_sf(ninia_10_11_path,quiet=T)%>%st_transform(9377)
colnames(ninia_2010)
ninia_2010<-ninia_2010%>%select(RULEID,EMERGENCIA,geometry)
# Resguardos indigenas
resguardos_sf<-geojsonsf::geojson_sf(resguardos)%>%st_transform(9377)
colnames(resguardos_sf)
resguardos_sf<-resguardos_sf%>%select(PUEBLO,MUNICIPIO,DEPARTAMENTO,OBJECTID,geometry)

#ggplot2::ggplot()+
#  geom_sf(data=munis_sf,color="darkgray")+
#  geom_sf(data=ninia_2010,color="darkblue")+
#  geom_sf(data=resguardos_sf,colorr="darkred")+
#  theme_bw()

# 2. Extract geometries intersection between munis and niña 2010-11 to get the area 
# of intervencion in the municipality by flood
ninaXmunis<-st_intersection(ninia_2010,munis_sf)
ninaXmunis$area<-st_area(ninaXmunis) # getting area
ninaXmunis<-ninaXmunis%>% # calculing weight
  mutate(areakm2=units::set_units(area,"km^2"),
         area_munikm2=units::set_units(area_muni,"km^2"),
         por_inund_area=areakm2/area_munikm2)
colnames(ninaXmunis)
ninaXmunis<-ninaXmunis%>% # filter interesting data
  select(codmpio,namempio,por_inund_area,geometry)%>%
  st_drop_geometry()%>%
  group_by(codmpio,namempio)%>% # grouping data
  summarise(por_inund_area_muni=sum(por_inund_area))
ninaXmunis$codmpio<-as.numeric(ninaXmunis$codmpio)
# Saving df result
saveRDS(ninaXmunis,"CreatedData/Temporary/inundaciones.R")

# 3. Indentify munis with indigenous lands
resguardosXmunis<-st_join(resguardos_sf,munis_sf)
resguardosXmunis<-resguardosXmunis[!duplicated(resguardosXmunis[,c("PUEBLO","MUNICIPIO")]),]
resguardosXmunis$MUNICIPIO<-ifelse(is.na(resguardosXmunis$MUNICIPIO),
                                   resguardosXmunis$codmpio,resguardosXmunis$MUNICIPIO)
resguardosXmunis<-resguardosXmunis[,c(1,2)]
# Saving results 
saveRDS(resguardosXmunis,"CreatedData/Temporary/resguardos.R")
