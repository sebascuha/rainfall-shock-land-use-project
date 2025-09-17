# Intro -------------------------------------------------------------------
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

precip_path <- "RawData/Respuesta Solicitud_Ideam/"
crops_path <- "RawData/CEDE/"
PDET_path <- "RawData/MunicipiosPDET.xlsx"
munis_path <- "RawData/MUNICIPIOS_GEODANE/"
munis_name <- "MGN_MPIO_POLITICO.shp"
output_path <- "CreatedData/"

# Loading CEDE data -------------------------------------------------------
crops <- read_dta(paste0(main_path,
                         crops_path,
                         "PANEL_AGRICULTURA_Y_TIERRA(2021).dta"))

# Planted area of the crop
planted_area_vars <- str_extract(colnames(crops),pattern = "^as_[a-z_]{1,}")
planted_area_vars<- planted_area_vars[is.na(planted_area_vars) == FALSE]

# Harvested area of the crop
harvested_area_vars <- str_extract(colnames(crops),pattern = "^ac_[a-z_]{1,}")
harvested_area_vars <- harvested_area_vars[is.na(harvested_area_vars) == FALSE]

# Production of the crop in tons
production <- str_extract(colnames(crops),pattern = "^p_[a-z_]{1,}")
production <- production[is.na(production) == FALSE]

# Crop yield
crop_yield <- str_extract(colnames(crops),pattern = "^r_[a-z_]{1,}")
crop_yield <- crop_yield[is.na(crop_yield) == FALSE]

# filter crops data -------------------------------------------------------
# Search anos of interest.
#summ_crop <- crops %>%
#  group_by(ano) %>%
#  summarise(acelga = sum(ac_acelga,na.rm = TRUE))
#summ_crop_1 <- crops %>%
#  group_by(ano) %>%
#  summarise(acelga = sum(as_acelga,na.rm = TRUE))
# rm(summ_crop,summ_crop_1)

## Only has data between 2007 - 2020 in crops

crops_final <- crops %>% 
  select(all_of(c("codmpio","ano",
                  #harvested_area_vars,production,crop_yield,
                  planted_area_vars
  )))%>%
  filter(ano >= 2007 & ano <= 2020)
class(crops_final$codmpio)

crops_final <- pivot_longer(crops_final,
                            cols = all_of(planted_area_vars),
                            names_to = "cultivo",
                            values_to = "planted_ha")
crops_final$cultivo <- sub("as_","",crops_final$cultivo)

# Adding PDET label municipalities.
PDET <- read_excel(paste0(main_path,PDET_path))
crops_final$PDET <- ifelse(
  crops_final$codmpio %in% PDET$codmpio
  & crops_final$ano%in%2017:2022,
  1,0)

length(unique(crops_final$cultivo))

write_csv(crops_final,paste0(main_path,output_path,"master_CEDE.csv"))