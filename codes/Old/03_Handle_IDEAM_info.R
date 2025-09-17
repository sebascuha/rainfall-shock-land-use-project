# 1. Intro -------------------------------------------------------------------
#
# This code works with Hydologic zones un colombia (IGAC-IDEAM)
# OUTPUTS:
# - "CreatedData/Temporary/IDEAM_sensor_TS.csv"
# - "CreatedData/Temporary/Treatment_covariates.csv"
#

# Clean evinonment
rm(list=ls())

# Libraries
library(pacman)
p_load(tidyverse,dplyr,stringr,haven,readxl,zoo,sf,ggplot2)

# Paths
if(Sys.info()["user"]=="sebas"){
  main_path <- paste0("C:/Users/",
                      Sys.info()["user"],
                      "/Dropbox/Documents/Proyecto_UsosDeTierra/")
}else{
  main_path <- paste0("C:/Users/",
                      Sys.info()["user"],
  ) # Pon el resto de la direccion donde se encuentra la informacion en tu computador
}

setwd(main_path)
precip_path <- "RawData/Respuesta Solicitud_Ideam/"
sensores_path <- "PrecipitacionNacionalDiaria/"
master_shzone <- "CreatedData/Temporary/munisXsubhidrozones.csv"
munis_path <- "RawData/MUNICIPIOS_GEODANE/"
munis_name <- "MGN_MPIO_POLITICO.shp"
output_path <- "CreatedData/Temporary/"

# 2.Loading every sensor data ----------------------------------------------------

# Enlist sensors files
files<-list.files(paste0(main_path,precip_path,sensores_path))
# length(str_extract(files,"^[A-Z_]{1,}@"))
# unique(str_extract(files,"^[A-Z_]{1,}@"))
length(files)
# Creating empty list where gonna save the sensors metrics in annual aggregate
lista_bases <- list()
# Loop to read and aggregate months to annual
for(i in 1:length(files)){
  # Extracting sensor name
  sensor <- str_extract(string = files[i],pattern ="[0-9]{1,}")
  # cat("\n Opening file",i)
  # Creating annual precipitation
  DF <- read.table(paste0(main_path,
                          precip_path,
                          sensores_path,
                          files[i]),
                   header=T,
                   sep="|")
  
  # cat("\n File",i," read.","Sensor:",sensor)
  DF$Fecha<-as.Date(DF$Fecha)
  DF$ano <- format(as.Date(DF$Fecha),"%Y")
  DF_sum <- DF %>% 
    group_by(ano) %>%
    summarise(anual_precip_mm = sum(Valor))
  
  #DF_sum$mean <- mean(DF_sum$anual_precip_mm[2:length(DF_sum$anual_precip_mm)-1])
  #DF_sum$sd <- sqrt(var(DF_sum$anual_precip_mm[2:length(DF_sum$anual_precip_mm)-1]))
  #if(length(DF_sum$anual_precip_mm)>10) {
  #  DF_sum$ma10 <- rollmean(DF_sum$anual_precip_mm,k = 10,fill = NA,align = "right")
  #  DF_sum$sd10 <- rollapply(DF_sum$anual_precip_mm,width = 10,FUN = sd,fill = NA,align ="right")
  #}else{
  #  DF_sum$ma10 <- NA
  #  DF_sum$sd10 <- NA
  #}
  DF_sum$sensor_code <- sensor
  
  # Adding the data to a list of dataframes
  lista_bases[[i]] <- DF_sum
  names(lista_bases)[i] <- sensor
  # Optimizing loop
  rm(DF,DF_sum)
}
cat(i,"files were read\n")

# Loading CNE IDEAM & Merging with the sensors data ----------------------------
CNE_IDEAM <- read_excel(paste0(main_path,precip_path,"CNE_IDEAM.xls"))
#colnames(CNE_IDEAM)
interes_vars <- c("CODIGO","FECHA_INSTALACION","latitud","longitud",
                  "DEPARTAMENTO","MUNICIPIO","AREA_HIDROGRAFICA",
                  "ZONA_HIDROGRAFICA","FECHA_SUSPENSION","SUBZONA_HIDROGRAFICA")
CNE_IDEAM <- CNE_IDEAM %>% select(all_of(interes_vars))
CNE_IDEAM_shp <- st_as_sf(CNE_IDEAM,coords = c("longitud","latitud"))%>%
  st_set_crs(4326)%>%st_transform(9377)
class(CNE_IDEAM_shp)
CNE_IDEAM_shp <- write_sf(CNE_IDEAM_shp,
                          paste0(main_path,output_path,"IDEAM_sensors_coor/IDEAM_sensors.shp"))

# Creating a empty dataframe to be fill with all information
master <- data.frame()
length(unique(names(lista_bases))) # numer of sensors
for (i in 1:length(names(lista_bases))){
  # Merge with iteraterated sensor.
  mg <- merge(CNE_IDEAM,lista_bases[[names(lista_bases)[i]]],
              by.x = "CODIGO",by.y = "sensor_code")
  master<-rbind(master,mg) # adding to master
  rm(mg)
}
cat(i, "files are merged with the master.\n")
# master<-read_csv(paste0(main_path,output_path,"IDEAM_sensor_TS.csv"))
# Filtering sensors .shp data
CNE_IDEAM_fil<-CNE_IDEAM_shp%>%
  mutate(f_ini=year(FECHA_INSTALACION),f_fin=year(FECHA_SUSPENSION))%>%
  select(CODIGO,DEPARTAMENTO,MUNICIPIO,geometry)%>%
  filter(CODIGO%in%master$CODIGO)%>%
  mutate(in_period=ifelse(CODIGO%in%master[master$ano>=2015,]$CODIGO,1,0))
length(unique(CNE_IDEAM_fil$CODIGO))
sum(CNE_IDEAM_fil$in_period)
gc() # Clean RAM
rm(CNE_IDEAM_shp)
# 3.Merge with munis shp ----------------------------------------------------
munis_sf <- read_sf(paste0(main_path,
                           munis_path,
                           munis_name))%>%
  st_transform(crs=9377)
munis_sf$codmpio <- as.numeric(munis_sf$MPIO_CDPMP)
munis_sf<-munis_sf%>%select(codmpio,DPTO_CNMBR,MPIO_CNMBR,geometry)
colnames(munis_sf)<-c("codmpio","DEPTO","MUNI","geometry")
master<-st_as_sf(master,coords = c("longitud","latitud"))%>%
  st_set_crs(4326)%>%st_transform(9377)
master<-st_join(master,munis_sf)%>%st_drop_geometry()

colnames(master)
master<-master[,c("codmpio","DEPTO","MUNI","CODIGO","AREA_HIDROGRAFICA",
                  "ZONA_HIDROGRAFICA","FECHA_SUSPENSION","SUBZONA_HIDROGRAFICA",
                  "subred","ano","anual_precip_mm")]

# 4. Measure mean and standard deviation -------------------------------------
master<-master%>%filter(ano>=1970)
# Mean between 1970-2000
master_sum_treatment_pre2000 <- master%>%
  filter(ano%in%c(1970:1999))%>%
  group_by(codmpio,ano)%>%
  summarise(#precipitacion_ano = sum(anual_precip_mm,na.rm = T),
            #n_sensors = n(),
            #mean_muni_before2000 = precipitacion_ano/n_sensors,
            mean_muni_before2000 = mean(anual_precip_mm,na.rm = T),
            #sd_muni_before2000 = sd(anual_precip_mm,na.rm = T),
            )%>%group_by(codmpio)%>%
  summarise(mean_before2000 = mean(mean_muni_before2000),
            sd_before2000 = sd(mean_muni_before2000))
# Mean between all disponible years
master_sum_treatment_all <- master%>%
  group_by(codmpio,ano)%>%
  summarise(mean_muni_all = mean(anual_precip_mm,na.rm = T),
            )%>%group_by(codmpio)%>%
  summarise(mean_all = mean(mean_muni_all),
            sd_all = sd(mean_muni_all))
# Mean on period analise years
master_sum_treatment <- master%>%
  filter(ano%in%c(2015:2022))%>%
  group_by(codmpio,ano)%>%
  summarise(mean_muni_t_period = mean(anual_precip_mm,na.rm = T),
  )%>%group_by(codmpio)%>%
  summarise(mean_t_period = mean(mean_muni_t_period),
            sd_t_period = sd(mean_muni_t_period))
# Mean muni presipitation by year
master_sum <- master %>%
  group_by(codmpio,ano) %>%
  summarise(muni_mean_precipitation=mean(mean(anual_precip_mm,na.rm = T)))

# Load munisXhidrozone
HIDRO<-read.csv(master_shzone)
HIDRO<-HIDRO[,-1] # Erraising index
# IDEAM master merge with hidrozones
master_hidro<-merge(master,HIDRO,by=c("codmpio","CODIGO","DEPTO","MUNI"),all = T)
colnames(master_hidro)

# Mean between 1970-2000
master_sum_treatment_hidrobefore200 <- master_hidro%>%
  select(codmpio,CODIGO,ano,anual_precip_mm,
         COD_AH,COD_ZH,COD_SZH,NOM_AH,NOM_ZH,NOM_SZH,
         muni_area_km2,shz_area_km2,weight_shz_in_muni_km)%>%
  filter(ano%in%c(1970:1999))%>%
  group_by(codmpio,ano)%>%
  summarise(weighted_mean_muni_before2000 = weighted.mean(anual_precip_mm,weight_shz_in_muni_km,
                                                 na.rm = T)
            #sd_muni_before2000 = sd(anual_precip_mm,na.rm = T),
            )%>%
  group_by(codmpio)%>%
  summarise(weighted_mean_before2000 = mean(weighted_mean_muni_before2000),
            weighted_sd_before2000 = sd(weighted_mean_muni_before2000))

# Mean between all aviable years
master_sum_treatment_hidroall <- master_hidro%>%
  select(codmpio,CODIGO,ano,anual_precip_mm,
         COD_AH,COD_ZH,COD_SZH,NOM_AH,NOM_ZH,NOM_SZH,
         muni_area_km2,shz_area_km2,weight_shz_in_muni_km)%>%
  filter(ano>=1970)%>%
  group_by(codmpio,ano)%>%
  summarise(weighted_mean_muni_all = weighted.mean(anual_precip_mm,weight_shz_in_muni_km,
                                                   na.rm = T)
            #sd_muni_before2000 = sd(anual_precip_mm,na.rm = T),
            )%>%
  group_by(codmpio)%>%
  summarise(weighted_mean_all = mean(weighted_mean_muni_all),
            weighted_sd_all = sd(weighted_mean_muni_all))
# Mean on period analyse years
master_sum_treatment_hidro <- master_hidro%>%
  select(codmpio,CODIGO,ano,anual_precip_mm,
         COD_AH,COD_ZH,COD_SZH,NOM_AH,NOM_ZH,NOM_SZH,
         muni_area_km2,shz_area_km2,weight_shz_in_muni_km)%>%
  filter(ano%in%c(2015:2022))%>%
  group_by(codmpio,ano)%>%
  summarise(weighted_mean_muni_t_period = weighted.mean(anual_precip_mm,weight_shz_in_muni_km,
                                                          na.rm = T)
            #sd_muni_before2000 = sd(anual_precip_mm,na.rm = T),
  )%>%group_by(codmpio)%>%
  summarise(weighted_mean_t_period = mean(weighted_mean_muni_t_period),
            weighted_sd_t_period = sd(weighted_mean_muni_t_period))

rm(master_hidro,munis_sf,lista_bases,CNE_IDEAM,CNE_IDEAM_shp,HIDRO)

# 5.Merging means measurements ----------------------------------------------
# Simple mean
simple_mean<-merge(master_sum_treatment_all,master_sum_treatment_pre2000,all.x=T)
simple_mean<-merge(simple_mean,master_sum_treatment,all.x=T)

# weighted mean
weighted_mean<-merge(master_sum_treatment_hidroall,master_sum_treatment_hidrobefore200,all.x=T)
weighted_mean<-merge(weighted_mean,master_sum_treatment_hidro,all.x = T)

# merge both measurements
mean_measure<-merge(simple_mean,weighted_mean)

#  6. Final precipitation data set and save it. ---------------------------
master_fn<-merge(master_sum,mean_measure,by=c("codmpio"),all.x = T)
master_fn<-master_fn%>%arrange(codmpio,ano)

# saving data
write.csv(master,paste0(main_path,output_path,"IDEAM_sensor_TS.csv"))
write.csv(master_fn,paste0(main_path,output_path,"Treatment_covariates.csv"))

#Sensor map
# Plotting data
ggplot()+
  geom_sf(data=munis_sf)+
  geom_sf(data=CNE_IDEAM_fil[CNE_IDEAM_fil$in_period==0,],
          aes(color="Unactive"))+
  geom_sf(data=CNE_IDEAM_fil[CNE_IDEAM_fil$in_period==1,],
          aes(color="Active"))+
  scale_color_manual(name="Sensors",values=c("Unactive"=scales::alpha('darkred',0.5),
                                             "Active"=scales::alpha('darkgreen',0.3)))+
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor=element_blank(),
        panel.background = element_blank())
ggsave("Slides/Figures/IDEAM_sensors.pdf",width = 6,height = 4)
ggsave("Paper/Figures/IDEAM_sensors.pdf",width = 6,height = 4)
gc()  

nrow(CNE_IDEAM_shp[CNE_IDEAM_shp$in_period==0,])
nrow(CNE_IDEAM_shp[CNE_IDEAM_shp$in_period==1,])
