# Intro -------------------------------------------------------------------
# Clean evinonment
rm(list=ls())
# Libraries
library(pacman)
p_load(tidyverse,sf,ggplot)

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
master_EVA <-"CreatedData/Temporary/TS_ciclo_siembra_cultivo_ha.R" # from 01 - Municipal
master_dw <- "CreatedData/Temporary/municipal_DW_cat.csv" # from 04 - Municipal
treatment_path <- "CreatedData/Temporary/Treatment_covariates.csv" # from 03 - Municipal
munis_path <- "RawData/MUNICIPIOS_GEODANE/" # Raw data
sensors_path <- "CreatedData/Temporary/IDEAM_sensors_coor" # from 03 - Municipal
#covariates_name <-"CreatedData/Temporary/Covariates_usosuelo.csv" 
#educ_path<- "RawData/GEIH_MARCO 2018(II. semestre)/GEIH - Diciembre - Marco - 2018/CSV/Características generales, seguridad social en salud y educación.csv"
#inundaciones_path<-"CreatedData/Temporary/inundaciones.R"
#resguardos_path<-"CreatedData/Temporary/resguardos.R"

# ---------------------------------------------------------------------------- #
# 1. Load datasets -----------------------------------------------------------

# Load master EVA
EVA<-readRDS(paste0(main_path,master_EVA))
# Load master Dynamic World
DW_mun_lvl<-read.csv(master_dw)

# Load IDEAM
treatment<-read.csv(treatment_path)[,-1]

#plot(density(x=treatment$mean_all))
#boxplot(treatment$mean_all,horizontal=TRUE)

# ---------------------------------------------------------------------------- #

# 2. Handle data ----------------------------------------------------------
# ---------------------------------------------------------------------------- #
#                         Measure SD from precipitation                        # 
# ---------------------------------------------------------------------------- #
# Before 2000
treatment_before2000<-treatment%>%
  mutate(deviation_mean_pre2000=(muni_mean_precipitation-mean_before2000)/sd_before2000,
         w_deviation_mean_pre2000=(muni_mean_precipitation-weighted_mean_before2000)/weighted_sd_before2000,
         
         )
# before all years
treatment_allyears<-treatment%>%
  mutate(deviation_mean_all=(muni_mean_precipitation-mean_all)/sd_all,
         w_deviation_mean_all=(muni_mean_precipitation-weighted_mean_all)/weighted_sd_all,
         )
# period analysed
treatment_period<-treatment%>%
  mutate(deviation_mean_t_period=(muni_mean_precipitation-mean_t_period)/sd_t_period,
         w_deviation_mean_t_period=(muni_mean_precipitation-weighted_mean_t_period)/weighted_sd_t_period,
         )
# merge different measures to treatment approximation
treatment<-merge(treatment,treatment_allyears)
treatment<-merge(treatment,treatment_before2000)
treatment<-merge(treatment,treatment_period)

# Exploring munnicipalities
(N_munis_dw<-length(unique(DW_mun_lvl$codmpio)))# 1120 total municipalities
write(N_munis_dw,"Slides/Numbers/N_munis_dw.txt") # Saving number
(N_munis_pp<-sum(unique(DW_mun_lvl$codmpio)%in%
                   unique(treatment$codmpio))) # 863 with active sensors
write(N_munis_pp,"Slides/Numbers/N_munis_pp.txt") # Saving number

# Cleaning environment
rm(treatment_allyears,treatment_before2000,treatment_period)

# Load GEIH education
# education <- read.csv(educ_path)
# colnames(education) #P_3042
# education <- education%>%select(P3042,P3042S1,P3042S2,FEX_C18)
# rm(education)
# ---------------------------------------------------------------------------- #
# 3. Merging covatiates ---------------------------------------------------
# ---------------------------------------------------------------------------- #
# Searching merge keys
colnames(DW_mun_lvl)
colnames(treatment)
# Merge DW info with DW
colnames(treatment)[2]<-"year"
mg <- merge(DW_mun_lvl, treatment, by = c("codmpio","year"),all.x = T)

# Searching merge keys with icfes results
colnames(mg)

# Merge master with icfes scores 
#mg <- merge(mg, muni_icfes19, by = c("codmpio"))
#summary(mg[,"mean_icfes_score"])[5]

mg<-mg%>%
  mutate(log_max_trees=log(trees_max),
         log_max_grass=log(grass_max),
         log_max_crops=log(crops_max),
         log_max_flooded_veg=log(flooded_vegetation_max),
         p_max_trees=trees_max/area_km2,
         p_max_grass=grass_max/area_km2,
         p_max_crops=crops_max/area_km2,
         p_max_flooded_veg=flooded_vegetation_max/area_km2,
         pp_max_trees=trees_max/total_analyzed_max,
         pp_max_grass=grass_max/total_analyzed_max,
         pp_max_crops=crops_max/total_analyzed_max,
         pp_max_flooded_veg=flooded_vegetation_max/total_analyzed_max,
         max_viewed=total_analyzed_max/area_km2,
         deviation_mean_pre2000=(muni_mean_precipitation-mean_before2000)/sd_before2000,
         w_deviation_mean_pre2000=(muni_mean_precipitation-weighted_mean_before2000)/weighted_sd_before2000,
         deviation_mean_all=(muni_mean_precipitation-mean_all)/sd_all,
         w_deviation_mean_all=(muni_mean_precipitation-weighted_mean_all)/weighted_sd_all,
         deviation_mean_t_period=(muni_mean_precipitation-mean_t_period)/sd_t_period,
         w_deviation_mean_t_period=(muni_mean_precipitation-weighted_mean_t_period)/weighted_sd_t_period,
         )

# Merging EVA data
# colnames(mg)
# colnames(EVA)
# mg<-merge(mg,EVA,by=c("codmpio","year"))

# ---------------------------------------------------------------------------- #
#                         Treatment variables                                  #
# ---------------------------------------------------------------------------- #
# * All: w_deviation_mean_all - deviation_mean_all                             #
# * Pre 2000: w_deviation_mean_pre2000 - deviation_mean_pre2000                #
# * Analyzed Period: w_deviation_mean_t_period - deviation_mean_t_period       #
# ---------------------------------------------------------------------------- #
# Checking NAs in prescipitation
sum(is.na(mg$deviation_mean_all))
sum(is.na(mg$w_deviation_mean_all))

sum(is.na(mg$deviation_mean_pre2000))
sum(is.na(mg$w_deviation_mean_pre2000))

sum(is.na(mg$deviation_mean_t_period))
sum(is.na(mg$w_deviation_mean_t_period))
# ---------------------------------------------------------------------------- #
# Function to identify tipe of climate 
treatment <- function(data,variable,SD){
  extreme_climante <- c()
  for (i in 1:nrow(data)){
    if(is.na(data[i,variable])==FALSE){
      if(data[i,variable]>=SD){
        extreme_climante[i]<-paste0("Rainy")
      }else if(data[i,variable]<=-SD){
        extreme_climante[i]<-paste0("Dry")
      }else{
        extreme_climante[i]<-paste0("Usual")
      }
    }else{next}
  }
  return(extreme_climante)
}
# ---------------------------------------------------------------------------- #
# ----                    Municipal precipitation                         ---- #
# ---------------------------------------------------------------------------- #
# All time period
mg$extreme_climate_all_1SD<-treatment(mg,"deviation_mean_all",1)
mg$extreme_climate_all_2SD<-treatment(mg,"deviation_mean_all",2)
mg$extreme_climate_all_3SD<-treatment(mg,"deviation_mean_all",3)
mg$extreme_climate_all_4SD<-treatment(mg,"deviation_mean_all",4)

# Pre 2000
mg$extreme_climate_pre2000_1SD<-treatment(mg,"deviation_mean_pre2000",1)
mg$extreme_climate_pre2000_2SD<-treatment(mg,"deviation_mean_pre2000",2)
mg$extreme_climate_pre2000_3SD<-treatment(mg,"deviation_mean_pre2000",3)
mg$extreme_climate_pre2000_4SD<-treatment(mg,"deviation_mean_pre2000",4)

# Analyzed period
mg$extreme_climate_t_period_1SD<-treatment(mg,"deviation_mean_t_period",1)
mg$extreme_climate_t_period_2SD<-treatment(mg,"deviation_mean_t_period",2)
mg$extreme_climate_t_period_3SD<-treatment(mg,"deviation_mean_t_period",3)
mg$extreme_climate_t_period_4SD<-treatment(mg,"deviation_mean_t_period",4)
# ---------------------------------------------------------------------------- #
# ----                     Weighted precipitation                         ---- #
# ---------------------------------------------------------------------------- #
#All time period
#mg$w_extreme_climate_all_1SD<-treatment(mg,"w_deviation_mean_all",1)
#mg$w_extreme_climate_all_2SD<-treatment(mg,"w_deviation_mean_all",2)
#mg$w_extreme_climate_all_3SD<-treatment(mg,"w_deviation_mean_all",3)
#mg$w_extreme_climate_all_4SD<-treatment(mg,"w_deviation_mean_all",4)

# Pre 2000
#mg$w_extreme_climate_pre2000_1SD<-treatment(mg,"w_deviation_mean_pre2000",1)
#mg$w_extreme_climate_pre2000_2SD<-treatment(mg,"w_deviation_mean_pre2000",2)
#mg$w_extreme_climate_pre2000_3SD<-treatment(mg,"w_deviation_mean_pre2000",3)
#mg$w_extreme_climate_pre2000_4SD<-treatment(mg,"w_deviation_mean_pre2000",4)

# Analyzed period
#mg$w_extreme_climate_t_period_1SD<-treatment(mg,"w_deviation_mean_t_period",1)
#mg$w_extreme_climate_t_period_2SD<-treatment(mg,"w_deviation_mean_t_period",2)
#mg$w_extreme_climate_t_period_3SD<-treatment(mg,"w_deviation_mean_t_period",3)
#mg$w_extreme_climate_t_period_4SD<-treatment(mg,"w_deviation_mean_t_period",4)
# ---------------------------------------------------------------------------- #
#                       Getting treatment dummies                              #
# ---------------------------------------------------------------------------- #
#                                   1 SD                                       #
# ---------------------------------------------------------------------------- #
# Treatment specification with all time mean
mg<-mg%>%group_by(codmpio)%>%
  mutate(t_group_all_1SD = ifelse(extreme_climate_all_1SD%in%c("Rainy"),year,NA),
         treatment_all_1SD = ifelse(sum(t_group_all_1SD,na.rm = T)!= 0,1,0),
         t_group_all_1SD = ifelse(treatment_all_1SD == 1,min(t_group_all_1SD,na.rm = T),0),
         t_dummy_all_1SD = ifelse(treatment_all_1SD == 1 & year >= t_group_all_1SD,1,0),
  )
# Treatment specification with pre2000 mean
mg<-mg%>%group_by(codmpio)%>%
  mutate(t_group_pre2000_1SD = ifelse(extreme_climate_pre2000_1SD%in%c("Rainy"),year,NA),
         treatment_pre2000_1SD = ifelse(sum(t_group_pre2000_1SD,na.rm = T)!= 0,1,0),
         t_group_pre2000_1SD = ifelse(treatment_pre2000_1SD == 1,min(t_group_pre2000_1SD,na.rm = T),0),
         t_dummy_pre2000_1SD = ifelse(treatment_pre2000_1SD == 1 & year >= t_group_pre2000_1SD,1,0),
  )
# Treatment specification with period of analysis mean
mg<-mg%>%group_by(codmpio)%>%
  mutate(t_group_t_period_1SD = ifelse(extreme_climate_t_period_1SD%in%c("Rainy"),year,NA),
         treatment_t_period_1SD = ifelse(sum(t_group_t_period_1SD,na.rm = T)!= 0,1,0),
         t_group_t_period_1SD = ifelse(treatment_t_period_1SD == 1,min(t_group_t_period_1SD,na.rm = T),0),
         t_dummy_t_period_1SD = ifelse(treatment_t_period_1SD == 1 & year >= t_group_t_period_1SD,1,0),
  )
# ---------------------------------------------------------------------------- #
#                                   2 SD                                       #
# ---------------------------------------------------------------------------- #
# Treatment specification with all time mean
mg<-mg%>%group_by(codmpio)%>%
  mutate(t_group_all_2SD = ifelse(extreme_climate_all_2SD%in%c("Rainy"),year,NA),
         treatment_all_2SD = ifelse(sum(t_group_all_2SD,na.rm = T)!= 0,1,0),
         t_group_all_2SD = ifelse(treatment_all_2SD == 1,min(t_group_all_2SD,na.rm = T),0),
         t_dummy_all_2SD = ifelse(treatment_all_2SD == 1 & year >= t_group_all_2SD,1,0),
  )
# Treatment specification with pre2000 mean
mg<-mg%>%group_by(codmpio)%>%
  mutate(t_group_pre2000_2SD = ifelse(extreme_climate_pre2000_2SD%in%c("Rainy"),year,NA),
         treatment_pre2000_2SD = ifelse(sum(t_group_pre2000_2SD,na.rm = T)!= 0,1,0),
         t_group_pre2000_2SD = ifelse(treatment_pre2000_2SD == 1,min(t_group_pre2000_2SD,na.rm = T),0),
         t_dummy_pre2000_2SD = ifelse(treatment_pre2000_2SD == 1 & year >= t_group_pre2000_2SD,1,0),
  )
# Treatment specification with period of analysis mean
mg<-mg%>%group_by(codmpio)%>%
  mutate(t_group_t_period_2SD = ifelse(extreme_climate_t_period_2SD%in%c("Rainy"),year,NA),
         treatment_t_period_2SD = ifelse(sum(t_group_t_period_2SD,na.rm = T)!= 0,1,0),
         t_group_t_period_2SD = ifelse(treatment_t_period_2SD == 1,min(t_group_t_period_2SD,na.rm = T),0),
         t_dummy_t_period_2SD = ifelse(treatment_t_period_2SD == 1 & year >= t_group_t_period_2SD,1,0),
  )
# ---------------------------------------------------------------------------- #
#                                   3 SD                                       #
# ---------------------------------------------------------------------------- #
# Treatment specification with all time mean
mg<-mg%>%group_by(codmpio)%>%
  mutate(t_group_all_3SD = ifelse(extreme_climate_all_3SD%in%c("Rainy"),year,NA),
         treatment_all_3SD = ifelse(sum(t_group_all_3SD,na.rm = T)!= 0,1,0),
         t_group_all_3SD = ifelse(treatment_all_3SD == 1,min(t_group_all_3SD,na.rm = T),0),
         t_dummy_all_3SD = ifelse(treatment_all_3SD == 1 & year >= t_group_all_3SD,1,0),
  )
# Treatment specification with pre2000 mean
mg<-mg%>%group_by(codmpio)%>%
  mutate(t_group_pre2000_3SD = ifelse(extreme_climate_pre2000_3SD%in%c("Rainy"),year,NA),
         treatment_pre2000_3SD = ifelse(sum(t_group_pre2000_3SD,na.rm = T)!= 0,1,0),
         t_group_pre2000_3SD = ifelse(treatment_pre2000_3SD == 1,min(t_group_pre2000_3SD,na.rm = T),0),
         t_dummy_pre2000_3SD = ifelse(treatment_pre2000_3SD == 1 & year >= t_group_pre2000_3SD,1,0),
  )
# Treatment specification with period of analysis mean
mg<-mg%>%group_by(codmpio)%>%
  mutate(t_group_t_period_3SD = ifelse(extreme_climate_t_period_3SD%in%c("Rainy"),year,NA),
         treatment_t_period_3SD = ifelse(sum(t_group_t_period_3SD,na.rm = T)!= 0,1,0),
         t_group_t_period_3SD = ifelse(treatment_t_period_3SD == 1,min(t_group_t_period_3SD,na.rm = T),0),
         t_dummy_t_period_3SD = ifelse(treatment_t_period_3SD == 1 & year >= t_group_t_period_3SD,1,0),
  )
# ---------------------------------------------------------------------------- #
#                                   4 SD                                       #
# ---------------------------------------------------------------------------- #
# Treatment specification with all time mean
mg<-mg%>%group_by(codmpio)%>%
  mutate(t_group_all_4SD = ifelse(extreme_climate_all_4SD%in%c("Rainy"),year,NA),
         treatment_all_4SD = ifelse(sum(t_group_all_4SD,na.rm = T)!= 0,1,0),
         t_group_all_4SD = ifelse(treatment_all_4SD == 1,min(t_group_all_4SD,na.rm = T),0),
         t_dummy_all_4SD = ifelse(treatment_all_4SD == 1 & year >= t_group_all_4SD,1,0),
  )
# Treatment specification with pre2000 mean
mg<-mg%>%group_by(codmpio)%>%
  mutate(t_group_pre2000_4SD = ifelse(extreme_climate_pre2000_4SD%in%c("Rainy"),year,NA),
         treatment_pre2000_4SD = ifelse(sum(t_group_pre2000_4SD,na.rm = T)!= 0,1,0),
         t_group_pre2000_4SD = ifelse(treatment_pre2000_4SD == 1,min(t_group_pre2000_4SD,na.rm = T),0),
         t_dummy_pre2000_4SD = ifelse(treatment_pre2000_4SD == 1 & year >= t_group_pre2000_4SD,1,0),
  )
# Treatment specification with period of analysis mean
mg<-mg%>%group_by(codmpio)%>%
  mutate(t_group_t_period_4SD = ifelse(extreme_climate_t_period_4SD%in%c("Rainy"),year,NA),
         treatment_t_period_4SD = ifelse(sum(t_group_t_period_4SD,na.rm = T)!= 0,1,0),
         t_group_t_period_4SD = ifelse(treatment_t_period_4SD == 1,min(t_group_t_period_4SD,na.rm = T),0),
         t_dummy_t_period_4SD = ifelse(treatment_t_period_4SD == 1 & year >= t_group_t_period_4SD,1,0),
  )
# ---------------------------------------------------------------------------- #
# Merging with indundaciones data
#mg<-merge(mg,inundaciones[,c("codmpio","por_inund_area_muni")],all.x=T)
#mg$por_inund_area_muni<-ifelse(is.na(mg$por_inund_area_muni),0,mg$por_inund_area_muni)

# Merging with resguardos data
#mg<-merge(mg,resguardos[,c("codmpio","PUEBLO")],all.x=T)
#class(mg)

# Save covariates
#apply(mg,MARGIN = 2,class)
colnames(mg)
interes_vars<-c("codmpio","year","month","area_km2",
                # DW data
                "total_analyzed_max","trees_max","grass_max","crops_max","flooded_vegetation_max",
                # Precipitation - IDEAM rawdata
                "muni_mean_precipitation",
                "mean_all","sd_all","mean_before2000","sd_before2000","mean_t_period","sd_t_period",
                "weighted_mean_all","weighted_sd_all","weighted_mean_before2000","weighted_sd_before2000","weighted_mean_t_period","weighted_sd_t_period",
                # SDs
                "deviation_mean_all","w_deviation_mean_all",
                "deviation_mean_pre2000","w_deviation_mean_pre2000",
                "deviation_mean_t_period","w_deviation_mean_t_period",
                # Percentage from visualized data analyzed
                "log_max_trees","log_max_grass","log_max_crops","log_max_flooded_veg",
                "p_max_trees","p_max_grass","p_max_crops","p_max_flooded_veg",
                "pp_max_trees","pp_max_grass","pp_max_crops","pp_max_flooded_veg",
                # Extreme climate
                "extreme_climate_all_1SD","extreme_climate_all_2SD","extreme_climate_all_3SD","extreme_climate_all_4SD",
                "extreme_climate_pre2000_1SD","extreme_climate_pre2000_2SD","extreme_climate_pre2000_3SD","extreme_climate_pre2000_4SD",
                "extreme_climate_t_period_1SD","extreme_climate_t_period_2SD","extreme_climate_t_period_3SD","extreme_climate_t_period_4SD",
                # Treatment dummies
                ## 1 SD
                "t_group_all_1SD","treatment_all_1SD","t_dummy_all_1SD",
                "t_group_pre2000_1SD","treatment_pre2000_1SD","t_dummy_pre2000_1SD",         
                "t_group_t_period_1SD","treatment_t_period_1SD","t_dummy_t_period_1SD",
                ## 2 SD
                "t_group_all_2SD","treatment_all_2SD","t_dummy_all_2SD",
                "t_group_pre2000_2SD","treatment_pre2000_2SD","t_dummy_pre2000_2SD",
                "t_group_t_period_2SD","treatment_t_period_2SD","t_dummy_t_period_2SD",
                ## 3 SD
                "t_group_all_3SD","treatment_all_3SD","t_dummy_all_3SD",
                "t_group_pre2000_3SD","treatment_pre2000_3SD","t_dummy_pre2000_3SD",
                "t_group_t_period_3SD","treatment_t_period_3SD","t_dummy_t_period_3SD",
                ## 4 SD
                "t_group_all_4SD","treatment_all_4SD","t_dummy_all_4SD",
                "t_group_pre2000_4SD","treatment_pre2000_4SD","t_dummy_pre2000_4SD",
                "t_group_t_period_4SD","treatment_t_period_4SD","t_dummy_t_period_4SD"  
                ) 
mg<-mg[,interes_vars]
interes_vars<-c("codmpio","year","month","area_km2",
                # DW data
                "total_analyzed_max","trees_max","grass_max","crops_max","flooded_vegetation_max",
                # Precipitation - IDEAM rawdata
                "muni_mean_precipitation",
                "mean_all","sd_all","mean_before2000","sd_before2000","mean_t_period","sd_t_period",
                "weighted_mean_all","weighted_sd_all","weighted_mean_before2000","weighted_sd_before2000","weighted_mean_t_period","weighted_sd_t_period",
                # SDs
                "deviation_mean_all","w_deviation_mean_all",
                "deviation_mean_pre2000","w_deviation_mean_pre2000",
                "deviation_mean_t_period","w_deviation_mean_t_period",
                # Percentage from visualized data analyzed
                "p_max_trees","p_max_grass","p_max_crops","p_max_flooded_veg",
                "pp_max_trees","pp_max_grass","pp_max_crops","pp_max_flooded_veg",
                # Treatment dummies
                ## 1 SD
                "t_group_all_1SD","treatment_all_1SD","t_dummy_all_1SD",
                "t_group_pre2000_1SD","treatment_pre2000_1SD","t_dummy_pre2000_1SD",         
                "t_group_t_period_1SD","treatment_t_period_1SD","t_dummy_t_period_1SD",
                ## 2 SD
                "t_group_all_2SD","treatment_all_2SD","t_dummy_all_2SD",
                "t_group_pre2000_2SD","treatment_pre2000_2SD","t_dummy_pre2000_2SD",
                "t_group_t_period_2SD","treatment_t_period_2SD","t_dummy_t_period_2SD",
                ## 3 SD
                "t_group_all_3SD","treatment_all_3SD","t_dummy_all_3SD",
                "t_group_pre2000_3SD","treatment_pre2000_3SD","t_dummy_pre2000_3SD",
                "t_group_t_period_3SD","treatment_t_period_3SD","t_dummy_t_period_3SD",
                ## 4 SD
                "t_group_all_4SD","treatment_all_4SD","t_dummy_all_4SD",
                "t_group_pre2000_4SD","treatment_pre2000_4SD","t_dummy_pre2000_4SD",
                "t_group_t_period_4SD","treatment_t_period_4SD","t_dummy_t_period_4SD"
                )
mg[,interes_vars]<-lapply(mg[,interes_vars],FUN = as.numeric)

# Adding some dummies
# temp<-mg%>%group_by(codmpio)%>%
#   summarise(por_inund_area_muni=mean(por_inund_area_muni))
# temp<-temp[!is.na(temp$por_inund_area_muni),]
# summary(temp)
# plot(density(temp$por_inund_area_muni))
# temp2<-temp[temp$por_inund_area_muni>=0.10,]

mg<-mg[!duplicated(mg[,c("codmpio","year")]),]
table(mg$year)
# Saving results
haven::write_dta(mg,"CreatedData/dataset_landuse.dta")
gc()
