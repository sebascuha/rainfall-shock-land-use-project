# Clean environment
rm(list=ls())
# Load needed libraries
library(pacman)
p_load(ggplot2,tidyverse,dplyr,mapview,sf)
# Assign paths
main_path = "C:/Users/sebas/Dropbox/Documents/Proyecto_UsosDeTierra" 
setwd(main_path)

# Data exploration --------------------------------------------------------
muni_data<-haven::read_dta("CreatedData/dataset_landuse_V2.dta")
colnames(muni_data)

## Pre-analysis:
# Plotting deviation dsitribution by year
par(mfrow=c(2,4))
for (i in 2015:2022){
  temp <- muni_data[muni_data$year==i,]
  plot(density(temp$w_deviation_mean_all,na.rm = T,from=-3,to=3),col="darkblue",
       main = year)
  lines(density(temp$deviation_mean_all,na.rm = T,from=-3,to=3),col="black")
  #legend("topleft",lty = 0.01,
  #       c("Avergare","Weighted average"),
  #       fill=c("darkblue","black"))
  rm(temp)
}
dev.copy(pdf,file="Slides/Figures/density_by_year.pdf",
         width=9, heigh=6)
dev.off()

# Histograms --------------------------------------------------------------
# Get frequencies by extreme climate using the mean
freqs_ec <- muni_data %>% 
  group_by(year,extreme_climate_all) %>%
  summarise(count=n())
freqs_ec$extreme_climate_all<-ifelse(freqs_ec$extreme_climate_all=="",
                                     NA,freqs_ec$extreme_climate_all)
freqs_ec<-drop_na(freqs_ec)
# Plot histogram of factor frequencies by year
ggplot(data = freqs_ec, aes(x=year, y=count,fill=extreme_climate_all))+
  geom_bar(stat="identity", 
           position = "dodge")+
  geom_text(aes(label=count), position=position_dodge(width=0.9), vjust=-0.25)+
  labs(x="Year",
       y="Frequency",
       #title = "Frequency of municipalities' faced climate in each year",
       fill="Type climate")+
  scale_x_discrete(limits=2015:2022)+
  theme_bw()
ggsave("Slides/Figures/freq_munis_all.pdf",width = 6,height = 4)
ggsave("Slides/Figures/freq_munis_all.png",width = 6,height = 4)


# Get frequencies by extreme climate using the mean
freqs_ec <- muni_data %>% 
  group_by(year,extreme_climate_pre2000) %>%
  summarise(count=n())
freqs_ec$extreme_climate_pre2000<-ifelse(freqs_ec$extreme_climate_pre2000=="",
                                         NA,freqs_ec$extreme_climate_pre2000)
freqs_ec<-drop_na(freqs_ec)
# Plot histogram of factor frequencies by year
ggplot(data = freqs_ec, aes(x=year, y=count,fill=extreme_climate_pre2000))+
  geom_bar(stat="identity", 
           position = "dodge")+
  geom_text(aes(label=count), position=position_dodge(width=0.9), vjust=-0.25)+
  labs(x="Year",
       y="Frequency",
       #title = "Frequency of municipalities' faced climate in each year",
       fill="Type climate")+
  scale_x_discrete(limits=2015:2022)+
  theme_bw()
ggsave("Slides/Figures/freq_munis_pre2000.pdf",width = 6,height = 4)
ggsave("Slides/Figures/freq_munis_pre2000.png",width = 6,height = 4)

# Get frequencies by extreme climate using the mean
freqs_ec <- muni_data %>% 
  group_by(year,extreme_climate_t_period) %>%
  summarise(count=n())
freqs_ec$extreme_climate_t_period<-ifelse(freqs_ec$extreme_climate_t_period=="",
                                          NA,freqs_ec$extreme_climate_t_period)
freqs_ec<-drop_na(freqs_ec)
# Plot histogram of factor frequencies by year
ggplot(data = freqs_ec, aes(x=year, y=count,fill=extreme_climate_t_period))+
  geom_bar(stat="identity", 
           position = "dodge")+
  geom_text(aes(label=count), position=position_dodge(width=0.9), vjust=-0.25)+
  labs(x="Year",
       y="Frequency",
       #title = "Frequency of municipalities' faced climate in each year",
       fill="Type climate")+
  scale_x_discrete(limits=2015:2022)+
  theme_bw()
ggsave("Slides/Figures/freq_munis_period.pdf",width = 6,height = 4)
ggsave("Slides/Figures/freq_munis_period.png",width = 6,height = 4)

dev.off()

# maps --------------------------------------------------------------------

IDEAM_coor<-read_sf("CreatedData/Temporary/IDEAM_sensors_coor")
IDEAM_ts<-read.csv("CreatedData/Temporary/IDEAM_sensor_TS.csv")
municipality<-read_sf("RawData/MUNICIPIOS_GEODANE")
IDEAM_ts_before2000<-IDEAM_ts%>%filter(ano<2000)
IDEAM_ts_2015_2022<-IDEAM_ts%>%filter(ano%in%c(2015:2022))

# Adding geometries
IDEAM_ts_before2000_sf<-IDEAM_coor%>%filter(CODIGO%in%unique(IDEAM_ts_before2000$CODIGO))
IDEAM_ts_2015_2022_sf<-IDEAM_coor%>%filter(CODIGO%in%unique(IDEAM_ts_2015_2022$CODIGO))

ggplot()+
  geom_sf(data=municipality,)+
  geom_sf(data=IDEAM_ts_before2000_sf,color="black")+
  geom_sf(data=IDEAM_ts_2015_2022_sf,color="darkgreen")+
  theme_bw()
ggsave("Slides/Figures/IDEAM_sensors.pdf",width = 6,height = 4)
ggsave("Slides/Figures/IDEAM_sensors.png")
gc()
