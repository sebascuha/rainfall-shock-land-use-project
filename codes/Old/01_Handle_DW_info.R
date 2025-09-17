# Intro -------------------------------------------------------------------
# Clean evinonment
rm(list=ls())

# Libraries
library(pacman)
p_load(tidyverse,dplyr,stringr,haven,readxl,zoo,sf,ggplot2)

# Paths
if(Sys.info()["user"]=="sebas"){
  main_path <- paste0("C:/Users/",Sys.info()["user"],"/Dropbox/Documents/Proyecto_UsosDeTierra/")
}else{
  main_path <- paste0("C:/Users/",Sys.info()["user"],) # Pon el resto de la direccion donde se encuentra la informacion en tu computador
}

veredas_path <- "RawData/ShapeFile_veredas2020"
DW_panel_path <- "RawData/Banrep/CreatedData/Panel_DW_COL_code_full.csv"


# Load datasets -----------------------------------------------------------

veredas<-read_sf(paste0(main_path,veredas_path))
nrow(veredas)*7
colnames(veredas)
colnames(veredas)<-c("OBJECTID","DPTOMPIO","CODIGO_VER","NOM_DEP","NOMB_MPIO",
                     "NOMBRE_VER","VIGENCIA","FUENTE","DESCRIPCIO","SEUDONIMOS",
                     "AREA_HA","COD_DPTO","OBSERVACIO","CONSEJE","ORIG_FID",
                     "SHAPE_Leng","SHAPE_Area","geometry")
veredas<-veredas[,c("DPTOMPIO","CODIGO_VER","NOM_DEP","NOMB_MPIO","NOMBRE_VER",
                    "COD_DPTO","geometry")]
DW_panel <- read.csv(paste0(main_path,DW_panel_path))
# Adding centros poblados + codmipio names in DW_panel
DW_panel$CODIGO_VER<-as.numeric(DW_panel$CODIGO_VER)
veredas$CODIGO_VER<-as.numeric(veredas$CODIGO_VER)

# Merging DW panel with veredas codes
DW_panel_fn <- left_join(DW_panel, veredas, by = "CODIGO_VER", multiple="all")
colnames(DW_panel_fn)
interes_vars <- c("CODIGO_VER","area_km2","year","month","water","trees","grass",
                  "flooded_vegetation","crops","shrub_and_scrub","built",
                  "bare","snow_and_ice","total_analyzed","DPTOMPIO","NOM_DEP",
                  "NOMB_MPIO","NOMBRE_VER","COD_DPTO","geometry" )
DW_panel_fn <- DW_panel_fn%>%select(all_of(interes_vars))%>%
  select(codmpio=DPTOMPIO, 
         codver=CODIGO_VER,
         area_ver_km2=area_km2,
         ano=year,
         month, 
         area_analyzed=total_analyzed, 
         trees,grass,flooded_vegetation,crops,
         DEPTO=NOM_DEP,
         MUNI=NOMB_MPIO,
         VEREDA=NOMBRE_VER,
         geometry)
sum(duplicated(DW_panel_fn[,colnames(DW_panel_fn),]))
DW_panel_fn$DPTOMPIO<-as.numeric(DW_panel_fn$codmpio)
rm(interes_vars,veredas,DW_panel)
gc()

# Getting max area viewed on the year
DW_panel_fn2<-DW_panel_fn%>%
  group_by(ano,codver)%>%
  mutate(max_analyzed=ifelse(area_analyzed==max(area_analyzed),max(area_analyzed),NA),
         max_analyzed=mean(max_analyzed,na.rm=T),
         max_trees=ifelse(area_analyzed==max_analyzed,trees,NA),
         max_trees=mean(max_trees,na.rm=T),
         max_grass=ifelse(area_analyzed==max_analyzed,grass,NA), 
         max_grass=mean(max_grass,na.rm=T),
         max_crops=ifelse(area_analyzed==max_analyzed,crops,NA),
         max_crops=mean(max_crops,na.rm=T),
         max_flooded_vegetation=ifelse(area_analyzed==max_analyzed,flooded_vegetation,NA),
         max_flooded_vegetation=mean(max_flooded_vegetation,na.rm=T),
  )
gc()
colnames(DW_panel_fn2)
max_DW_panel_munis<-DW_panel_fn2%>%group_by(codmpio,ano)%>%
  summarise(area_mun_km2=sum(area_ver_km2),
            max_area_analyzed=sum(max_analyzed),
            max_trees_mun=sum(max_trees),
            max_grass_mun=sum(max_grass),
            max_flooded_vegetation_mun=sum(max_flooded_vegetation),
            max_crops=sum(max_crops),
  )
rm(DW_panel_fn2)
gc()
# Getting max area viewed in dicember the year
DW_panel_fn3<-DW_panel_fn%>%filter(ano<2022)%>%
  group_by(ano,codver)%>%
  mutate(dec_analyzed=ifelse(month==12,area_analyzed,NA),
         dec_analyzed=mean(dec_analyzed,na.rm=T),
         dec_trees=ifelse(month==12,trees,NA),
         dec_trees=mean(dec_trees,na.rm=T),
         dec_grass=ifelse(month==12,grass,NA), 
         dec_grass=mean(dec_grass,na.rm=T),
         dec_crops=ifelse(month==12,crops,NA),
         dec_crops=mean(dec_crops,na.rm=T),
         dec_flooded_vegetation=ifelse(month==12,flooded_vegetation,NA),
         dec_flooded_vegetation=mean(dec_flooded_vegetation,na.rm=T),
  )
dec_DW_panel_munis<-DW_panel_fn3%>%group_by(codmpio,ano)%>%
  summarise(area_mun_km2=sum(area_ver_km2),
            dec_area_analyzed=sum(dec_analyzed),
            dec_trees_mun=sum(dec_trees),
            dec_grass_mun=sum(dec_grass),
            dec_flooded_vegetation_mun=sum(dec_flooded_vegetation),
            dec_crops=sum(dec_crops),
  )
rm(DW_panel_fn3)
gc()
DW_panel_fn3c<-DW_panel_fn%>%filter(ano==2022)%>%
  group_by(ano,codver)%>%
    mutate(dec_analyzed=ifelse(month==9,area_analyzed,NA),
           dec_analyzed=mean(dec_analyzed,na.rm=T),
           dec_trees=ifelse(month==9,trees,NA),
           dec_trees=mean(dec_trees,na.rm=T),
           dec_grass=ifelse(month==9,grass,NA), 
           dec_grass=mean(dec_grass,na.rm=T),
           dec_crops=ifelse(month==9,crops,NA),
           dec_crops=mean(dec_crops,na.rm=T),
           dec_flooded_vegetation=ifelse(month==9,flooded_vegetation,NA),
           dec_flooded_vegetation=mean(dec_flooded_vegetation,na.rm=T),
    )
dec_DW_panel_munisc<-DW_panel_fn3c%>%group_by(codmpio,ano)%>%
  summarise(area_mun_km2=sum(area_ver_km2),
            dec_area_analyzed=sum(dec_analyzed),
            dec_trees_mun=sum(dec_trees),
            dec_grass_mun=sum(dec_grass),
            dec_flooded_vegetation_mun=sum(dec_flooded_vegetation),
            dec_crops=sum(dec_crops),
  )
rm(DW_panel_fn3c)
gc()
dec_DW_panel_munisf<-rbind(dec_DW_panel_munis,dec_DW_panel_munisc)
rm(dec_DW_panel_munis,dec_DW_panel_munisc)

# Merge max_area with dec_area
DW_panel_fn_muni<-merge(dec_DW_panel_munisf,max_DW_panel_munis,
                        by=c("codmpio","ano","area_mun_km2"),all = T)
rm(dec_DW_panel_munisf,max_DW_panel_munis,DW_panel_fn)
DW_panel_fn_muni$codmpio<-as.numeric(DW_panel_fn_muni$codmpio)
DW_panel_fn_muni<-DW_panel_fn_muni%>%arrange(codmpio)
DW_panel_fn_muni<-DW_panel_fn_muni[!duplicated(DW_panel_fn_muni[,c("codmpio","ano","area_mun_km2")]),]
colnames(DW_panel_fn_muni)
write.csv(DW_panel_fn_muni,paste0(main_path,"CreatedData/Temporary/DW_panel_mun_level.csv"))

