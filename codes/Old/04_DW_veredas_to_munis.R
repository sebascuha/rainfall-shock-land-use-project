# Clean environment
rm(list=ls())
# Load needed libraries
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(sf,tidyverse,dplyr,stringr,fst)
# Assign paths

#### Paths definition ####
if (Sys.info()[['user']] == 'sebas'){
  main_path <- "C:/Users/sebas/Dropbox/Documents/Proyecto_UsosDeTierra/"
} else {}
setwd(main_path)
getwd()
# Paths
DW_file <- "RawData/BanRep/CreatedData/Panel_DW_COL_code_full.csv"
munis_path <-"RawData/MUNICIPIOS_GEODANE"

# 1. Load Data ------------------------------------------------------------
# DW veredas
dw<-readr::read_csv(DW_file) # Load data
dw<-dw%>%arrange(CODIGO_VER) # sort data by vereda code
dw$CODIGO_VER<-as.character(dw$CODIGO_VER) # Change variable class/type
dw$codmpio<-str_sub(string=dw$CODIGO_VER, end=-4) # Extracting municipality identificator
dw$codmpio<-ifelse(dw$codmpio=="566","50606",dw$codmpio) # Correct atipic id which does not exist
dw$codmpio<-as.numeric(dw$codmpio) # Change variable class/type
dw<-dw%>%arrange(codmpio) #sort data by codmpio
length(unique(dw$codmpio)) # Checking nubmer of municipalities
colnames(dw) # Cheking variables names

# 2. Handle data ----------------------------------------------------------
# Collapse data from vereda to municipality observations
dw<-dw%>%
  group_by(codmpio,year,month)%>%
  summarize(area_km2=sum(area_km2),
            total_analyzed=sum(total_analyzed),
            trees=sum(trees),grass=sum(grass),crops=sum(crops), # Interest variables
            water=sum(water),flooded_vegetation=sum(flooded_vegetation),
            snow_and_ice=sum(snow_and_ice),shrub_and_scrub=sum(shrub_and_scrub),
            built=sum(built),bare=sum(bare),
            )
# Get maximum area analized by DW
dw<-dw%>%group_by(codmpio,year)%>%
  mutate(codmpio=as.numeric(codmpio),max_view=max(total_analyzed)) 
#dw<-dw[dw$total_analyzed!=0,]
## Filering data
# Dicember data
dw_dec<-dw%>%filter(month==12)%>%select(-max_view ) # filter data)
colnames(dw_dec)
for(i in 5:ncol(dw_dec)){ # Loop changing names
  colnames(dw_dec)[i]<-paste0(colnames(dw_dec)[i],"_dec")
}
# Nothings analyzed by DW in dicember
dw_dec_empty<-dw_dec%>%filter(total_analyzed_dec==0)
empty.dec=195*100/8960
write(empty.dec,"Slides/Numbers/empty_data_dec.txt") #Saving number

# Maximum datadw_dic# Maximum data analyzed
dw_max<-dw%>%filter(total_analyzed==max_view)%>%select(-max_view) # filter data
colnames(dw_max)
dw_max<-dw_max[!duplicated(dw_max[c("codmpio","year")]),] # Drop duplicates by municipality and year
for(i in 5:ncol(dw_max)){ # Loop changing names
  colnames(dw_max)[i]<-paste0(colnames(dw_max)[i],"_max")
}
dw_max_empty<-dw_max%>%filter(total_analyzed_max==0)
dw_max<-dw_max%>%
  mutate(trees_max=ifelse(total_analyzed_max==0,NA,trees_max),
         grass_max=ifelse(total_analyzed_max==0,NA,grass_max),
         crops_max=ifelse(total_analyzed_max==0,NA,crops_max),
         water_max=ifelse(total_analyzed_max==0,NA,water_max),
         flooded_vegetation_max=ifelse(total_analyzed_max==0,NA,flooded_vegetation_max),
         snow_and_ice_max=ifelse(total_analyzed_max==0,NA,snow_and_ice_max),
         shrub_and_scrub_max=ifelse(total_analyzed_max==0,NA,shrub_and_scrub_max),
         total_analyzed_max=ifelse(total_analyzed_max==0,NA,total_analyzed_max))

empty.max=195*100/8960
#empty.max<-table(empty.max)
write(empty.max,"Slides/Numbers/empty_data_max.txt") #Saving number

# 3. Saving data -------------------------------------------------------------
write_csv(dw_max,"CreatedData/Temporary/municipal_DW_cat.csv")



