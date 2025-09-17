# Intro -------------------------------------------------------------------
# Clean evinonment
rm(list=ls())

# Libraries
library(pacman)
p_load(tidyverse,dplyr,stringr,haven,readxl,zoo,sf,ggplot2,Hmisc)

# Setting relevant paths and working directory
if(Sys.info()["user"]=="sebas"){
  main_path <- paste0("C:/Users/",Sys.info()["user"],
                      "/Dropbox/Documents/Proyecto_UsosDeTierra/")
}else{
  main_path <- paste0("C:/Users/",Sys.info()["user"],
                      "") # Pon el resto de la direccion donde se encuentra la informacion en tu computador
}

setwd(main_path)
#master_cede <- "CreatedData/master_CEDE.csv"
master_dw <- "CreatedData/Temporary/DW_panel_mun_level.csv"
master_ideam <- "CreatedData/Temporary/IDEAM_sensors_ts.csv"
master_shzone <- "CreatedData/Temporary/munisXsubhidrozones.csv"
munis_path <- "RawData/MUNICIPIOS_GEODANE/"
sensors_path <- "CreatedData/Temporary/IDEAM_sensors_coor"
covariates_name <-"CreatedData/Temporary/Covariates_usosuelo.csv"

# Handle covariates -------------------------------------------------------
# Loading covariates dataset
covariates_df <- read.csv(covariates_name)
covariates_df <- covariates_df[,-1]

# Handle data
muni_treat <- covariates_df %>%
  select(codmpio,Departamento,Municipio,year, # Observation identification
         muni_area_DW = area_mun_km2, area_analyzed_DW = area_analyzed,
         trees_mun,grass_mun,flooded_vegetation_mun,crops,# DW info
         CODIGO, anual_precip_mm=anual_precip_mm.x, # Sensors info
         COD_AH,COD_ZH,COD_SZH,NOM_AH,NOM_ZH,NOM_SZH,
         muni_area_km2_hidro =muni_area_km2, shz_area_km2_hidro=muni_area_km2,
         weight_shz_in_muni_km # Hidrozones info
  )%>%
  arrange(codmpio) %>% 
  group_by(codmpio) %>% 
  mutate(
    # Calcules with mean 
    mean_precip = mean(anual_precip_mm),
    sd_precip = sd(anual_precip_mm),
    #ma10_precip = rollmean(anual_precip_mm, k = 10, fill = NA, align = "right"),
    #sd10_precip = rollapply(anual_precip_mm,width = 10, FUN = sd, fill = NA, align ="right"), 
    weigthed_precip = weighted.mean(anual_precip_mm, weight_shz_in_muni_km, na.rm = T),
    weigthed_sd_precip = sqrt(wtd.var(anual_precip_mm,weight_shz_in_muni_km,na.rm = T)),
    #weigthed_ma10_precip = weighted.mean(
    #  rollmean(anual_precip_mm, k = 10,fill = NA, align = "right"),
    #  weight_shz_in_muni_km, na.rm = T),
    #weigthed_sd10_precip = weighted.mean(
    #  rollapply(anual_precip_mm,width = 10, FUN = sd, fill = NA, align ="right"),
    #  weight_shz_in_muni_km, na.rm = T),
  )

muni_data <- muni_treat %>% #drop_na()%>% 
  group_by(codmpio,Departamento,Municipio,year) %>%
  summarise(p_trees = mean(trees_mun)/mean(muni_area_DW),
            p_grass = mean(grass_mun)/mean(muni_area_DW),
            p_flooded_vegetation = mean(flooded_vegetation_mun)/mean(muni_area_DW),
            p_crops = mean(crops)/mean(muni_area_DW),
            muni_area_DW = mean(muni_area_DW,na.rm =T),
            mean_precip = mean(mean_precip),
            sd_precip = mean(sd_precip),
            #ma10_precip = mean(ma10_precip),
            #sd10_precip = mean(sd10_precip), 
            weigthed_precip = mean(weigthed_precip),
            weigthed_sd_precip = mean(weigthed_sd_precip),
            #weigthed_ma10_precip = mean(weigthed_ma10_precip),
            #weigthed_sd10_precip = mean(weigthed_sd10_precip),
            deviation = (mean(anual_precip_mm)-mean_precip)/sd_precip,
            deviation_weigthed = (mean(anual_precip_mm)-weigthed_precip)/weigthed_sd_precip
            )
summary(muni_data[,c(5:8,10:15)])

dev_mean <- muni_data$deviation
dev_mean_w <- muni_data$deviation_weigthed
extreme_climante <- c()
extreme_climante_w <- c()
for (i in 1:nrow(muni_data)){
  if(is.na(muni_data$deviation_weigthed[i])==FALSE){
    if(muni_data$deviation_weigthed[i]>=1){
      extreme_climante_w[i]<-"Rainy"
    }else if(muni_data$deviation_weigthed[i]<=-1){
      extreme_climante_w[i]<-"Dry"
    }else{
      extreme_climante_w[i]<-"Usual"
    }
  }else{next}
}

for (i in 1:nrow(muni_data)){
  if(is.na(muni_data$deviation[i])==FALSE){
    if(muni_data$deviation[i]>=1){
      extreme_climante[i]<-"Rainy"
    }else if(muni_data$deviation[i]<=-1){
      extreme_climante[i]<-"Dry"
    }else{
      extreme_climante[i]<-"Usual"
    }
  }else{next}
}
muni_data$extreme_climate<-as.factor(extreme_climante)
levels(muni_data$extreme_climate)
muni_data$extreme_climate_w<-as.factor(extreme_climante_w)
levels(muni_data$extreme_climate_w)

data<-data%>%group_by(codmpio)%>%
  mutate(t_group=ifelse(extreme_climate%in%c("Rainy"),year,NA),
         treatment=ifelse(sum(t_group,na.rm = T)!=0,1,0),
         t_group=ifelse(treatment==1,min(t_group,na.rm = T),0),
         t_dummy=ifelse(treatment==1 & year>=t_group,1,0),
  )
write.csv(muni_data,"CreatedData/municipality_dataset.csv")
