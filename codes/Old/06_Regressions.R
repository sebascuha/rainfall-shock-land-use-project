# Intro -------------------------------------------------------------------
# Clean evinonment
rm(list=ls())

# Libraries
library(pacman)
p_load(tidyverse,sf,ggplot2,did)

# Setting relevant paths and working directory
if(Sys.info()["user"]=="sebas"){
  main_path <- paste0("C:/Users/",Sys.info()["user"],
                      "/Dropbox/Documents/Proyecto_UsosDeTierra/")
}else{
  main_path <- paste0("C:/Users/",Sys.info()["user"],
                      "") # Pon el resto de la direccion donde se encuentra la informacion en tu computador
}

setwd(main_path)
#muni_data_name <- "CreatedData/municipality_dataset.csv"
muni_data_name <- "CreatedData/dataset_landuse.dta"

# load data ---------------------------------------------------------------
data<-haven::read_dta(muni_data_name)
colnames(data)
interest_vars<-c("codmpio","year","area_km2","p_max_trees","p_max_grass","p_max_crops",
                 "mean_before2000","sd_before2000",
                 "extreme_climate_pre2000_1SD","extreme_climate_pre2000_2SD",
                 "extreme_climate_pre2000_3SD","extreme_climate_pre2000_4SD",
                 "t_group_pre2000_1SD","treatment_pre2000_1SD","t_dummy_pre2000_1SD",
                 "t_group_pre2000_2SD","treatment_pre2000_2SD","t_dummy_pre2000_2SD",
                 "t_group_pre2000_3SD","treatment_pre2000_3SD","t_dummy_pre2000_3SD",
                 "t_group_pre2000_4SD","treatment_pre2000_4SD","t_dummy_pre2000_4SD")      
data_fil<-data%>%select(all_of(interest_vars))%>%mutate(coddepto=(codmpio-codmpio%%1000)/1000) 
summary(data_fil,digits = 4)

#DS<-as.table(summary(data[,c(5:7,9,11)],digits = 4))[1:6,]
#DS<-rbind(DS,c("NA's:0","NA's:0","NA's:0","NA's:560","NA's:560"))
#DS<-rbind(DS,sapply(data[,c(5:7,9,11)], sd, na.rm=TRUE))
#DS<-rbind(DS,rep(nrow(data),5))
#xtable(DS, type = "latex")
#data$extreme_climate<-as.factor(data$extreme_climate)
data_fil<-data_fil[!is.na(data_fil$mean_before2000),]

attgt_reg1<-att_gt(yname = "p_max_trees", tname = "year", idname = "codmpio",
                  gname = "t_group_pre2000_1SD", data = data_fil,
                  clustervars = "coddepto",#allow_unbalanced_panel = TRUE,
                  control_group = 'notyettreated',
                  pl = TRUE, cores = parallel::detectCores()-6,
                  #bstrap =F, #cband = F 
                  )
attgt_reg2<-att_gt(yname = "p_max_trees", tname = "year", idname = "codmpio",
                   gname = "t_group_pre2000_1SD", data = data_fil,
                   clustervars = "coddepto",#allow_unbalanced_panel = TRUE,
                   control_group = 'notyettreated',
                   pl = TRUE, cores = parallel::detectCores()-6,
                   #bstrap =F, #cband = F 
)
attgt_reg3<-att_gt(yname = "p_max_trees", tname = "year", idname = "codmpio",
                   gname = "t_group_pre2000_1SD", data = data_fil,
                   clustervars = "coddepto",#allow_unbalanced_panel = TRUE,
                   control_group = 'notyettreated',
                   pl = TRUE, cores = parallel::detectCores()-6,
                   #bstrap =F, #cband = F 
)
attgt_reg4<-att_gt(yname = "p_max_trees", tname = "year", idname = "codmpio",
                   gname = "t_group_pre2000_1SD", data = data_fil,
                   clustervars = "coddepto",#allow_unbalanced_panel = TRUE,
                   control_group = 'notyettreated',
                   pl = TRUE, cores = parallel::detectCores()-6,
                   #bstrap =F, #cband = F 
)
agg.simple1 <- aggte(attgt_reg1, type = "simple")
agg.simple2 <- aggte(attgt_reg2, type = "simple")
agg.simple3 <- aggte(attgt_reg3, type = "simple")
agg.simple4 <- aggte(attgt_reg4, type = "simple")

summary(agg.simple1)
summary(agg.simple2)
summary(agg.simple3)
summary(agg.simple4)

agg.gp1 <- aggte(attgt_reg1, type = "group")
agg.gp2 <- aggte(attgt_reg2, type = "group")
agg.gp3 <- aggte(attgt_reg3, type = "group")
agg.gp4 <- aggte(attgt_reg4, type = "group")

summary(agg.gp1)
summary(agg.gp2)
summary(agg.gp3)
summary(agg.gp4)


# Dinamic agregation for event study
agg.crop <- aggte(attgt_crops, type = "dynamic")
summary(agg.crop)
# Dinamic agregation for event study
agg.trees <- aggte(attgt_trees, type = "dynamic")
summary(agg.trees)
# Dinamic agregation for event study
agg.grass <- aggte(attgt_grass, type = "dynamic")
summary(agg.grass)

(img_crop <- ggdid(agg.crop))
ggsave("Slides/Figures/es_crops.pdf")
(img_grass <-ggdid(agg.grass))
ggsave("Slides/Figures/es_grass.pdf")
(img_trees<-ggdid(agg.trees))
ggsave("Slides/Figures/es_trees.pdf")


