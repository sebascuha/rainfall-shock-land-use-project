# Intro -------------------------------------------------------------------
#
# This code works with Encuestas Agopecuarias Municipales(EVA) 2007-2022
#  OUTPUTS:
#  - CreatedData/Temporary/TS_ciclo_siembra_cultivo_ha.R
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
  setwd(main_path)
}else{
  main_path <- paste0("C:/Users/",
                      Sys.info()["user"],
  ) # Pon el resto de la direccion donde se encuentra la informacion en tu computador
}

crops_path <- "RawData/EVA/"
PDET_path <- "RawData/MunicipiosPDET.xlsx"
munis_path <- "RawData/MUNICIPIOS_GEODANE/"
munis_name <- "MGN_MPIO_POLITICO.shp"


# 1. Loading data ------------------------------------------------------------
# EVA 2006B-2018
EVA07_18 <- readxlsb::read_xlsb(paste0(crops_path,"Copia de Base Agrícola EVA 2007-2018 (P).xlsb"),sheet ="BASE",skip = 1)
EVA07_18 <- EVA07_18%>%select(coddepto=CÓD...DEP.,deptoname=DEPARTAMENTO,codmpio=CÓD..MUN.,mpioname=MUNICIPIO,
                              gp_cultivo=GRUPO..DE.CULTIVO,subgp_cultivo=SUBGRUPO..DE.CULTIVO,cultivo=CULTIVO,
                              desagregacion_cultivo=DESAGREGACIÓN.REGIONAL.Y.O.SISTEMA.PRODUCTIVO,year=AÑO,
                              period=PERIODO,area_sem_ha=Área.Sembrada..ha.,area_cos_ha=Área.Cosechada..ha.,
                              produc_ton=Producción..t.,yield_ton_ha=Rendimiento..t.ha.,ciclo_cultivo=CICLO.DE.CULTIVO)
# EVA 2019-2022
EVA19_22 <- read.csv(paste0(crops_path,"Evaluaciones_Agropecuarias_Municipales___EVA._2019_-_2022._Base_Agr_cola_20240329.csv"))
EVA19_22 <- EVA19_22%>%select(coddepto=Código.Dane.departamento,deptoname=Departamento,codmpio=Código.Dane.municipio,
                              mpioname=Municipio,gp_cultivo=Grupo.cultivo,subgp_cultivo=Subgrupo,cultivo=Cultivo,
                              desagregacion_cultivo=Desagregación.cultivo,year=Año,period=Periodo,area_sem_ha=Área.sembrada,
                              area_cos_ha=Área.cosechada,produc_ton=Producción,yield_ton_ha=Rendimiento,ciclo_cultivo=Ciclo.del.cultivo)
# Merging both EVAs
EVA_tot <- rbind(EVA07_18,EVA19_22)
# Saving merge
saveRDS(EVA_tot,"RawData/EVA/EVA_tot.R")

# 2. Exploring data ----------------------------------------------------------
#nas<-EVA_tot[is.na(EVA_tot$year),]
#rm(nas)
EVA_tot <- readRDS("RawData/EVA/EVA_tot.R")
EVA_tot <- EVA_tot[!is.na(EVA_tot$year),]
EVA_tot$cultivo <- toupper(EVA_tot$cultivo)
EVA_tot$ciclo_cultivo <- toupper(EVA_tot$ciclo_cultivo)
summary(EVA_tot)
# Filter data since
EVA_pos2007 <- EVA_tot%>%filter(year>=2007)
EVA_pos2007 <- EVA_pos2007%>%group_by(codmpio,mpioname,cultivo,year,period,ciclo_cultivo)%>%
  summarise(sum_area_sem_ha=sum(area_sem_ha),
            sum_area_cos_ha=sum(area_cos_ha),
            sum_produc_ton =sum(produc_ton))

EVA_pos2007_max <- EVA_pos2007%>%
  group_by(codmpio,mpioname,cultivo,year,ciclo_cultivo)%>%
  summarise(#sum_area_sem_ha=sum_area_sem_ha,
            max_area_sem=max(sum_area_sem_ha),
            #sum_area_cos_ha=sum_area_cos_ha,
            max_area_cos=max(sum_area_cos_ha))

# Looking for which crops are "anual"
h<-EVA_pos2007_max[EVA_pos2007_max$ciclo_cultivo=="ANUAL",]
unique(h$cultivo)
#[1] "YUCA"         "ARRACACHA"    "ÑAME"         "MALANGA"      "ACHIOTE"      "GUANDUL"      "TABACO NEGRO" "ACHIRA"      
#[9] "CHONQUE"      "YACON"        "AJI"          "RUDA"         "CEBOLLIN"     "JENGIBRE"     "BORE"         "ORELLANA" 
rm(h)

# Handle data
EVA_pos2007_max_sum <- EVA_pos2007_max%>%
  group_by(codmpio,year,ciclo_cultivo)%>% # grouping data
  summarise(sum_ciclo_area_sem_ha=sum(max_area_sem), 
            sum_ciclo_area_cos_ha=sum(max_area_cos))%>%
  group_by(codmpio,year)%>%  # grouping data
  mutate(sum_muni_area_sem_ha=sum(sum_ciclo_area_sem_ha),
         sum_muni_area_cos_ha=sum(sum_ciclo_area_cos_ha))
EVA_pos2007_max_sum <- EVA_pos2007_max_sum%>%
  mutate(por_area_sem_ciclo=sum_ciclo_area_sem_ha/sum_muni_area_sem_ha,
         por_area_cos_ciclo=sum_ciclo_area_cos_ha/sum_muni_area_sem_ha)

EVA_pos2007_max_sum$por_area_sem_ciclo<-ifelse(is.na(EVA_pos2007_max_sum$por_area_sem_ciclo),0,EVA_pos2007_max_sum$por_area_sem_ciclo)
EVA_pos2007_max_sum$por_area_cos_ciclo<-ifelse(is.na(EVA_pos2007_max_sum$por_area_cos_ciclo),0,EVA_pos2007_max_sum$por_area_cos_ciclo)
summary(EVA_pos2007_max_sum)

EVA_pos2007_fn<-EVA_pos2007_max_sum%>%
  pivot_wider(names_from = ciclo_cultivo,
              values_from = c(sum_ciclo_area_sem_ha,sum_ciclo_area_cos_ha,
                              sum_muni_area_sem_ha,sum_muni_area_cos_ha,
                              por_area_sem_ciclo,por_area_cos_ciclo))
saveRDS(EVA_pos2007_fn,"CreatedData/Temporary/TS_ciclo_siembra_cultivo_ha.R")


