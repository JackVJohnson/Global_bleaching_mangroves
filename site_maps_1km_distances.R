# create a worldmap for the methods to show sampling points

rm(list=ls())

library(rgdal)
library(ggplot2)
library(RColorBrewer)
library(plotrix)
library(GISTools)
library(dplyr)
library(viridis)
library(Cairo)
library(sf)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)

world <- ne_countries(scale = "small", returnclass = "sf")
class(world)

ggplot() +
  geom_sf(data=world, fill="white")

#working directory 

worldmapwd<-"C:/Users/40274182/OneDrive - Queen's University Belfast/PhD/world_shape_files"
setwd(worldmapwd)

#bleaching directory 

Bleaching_data_directory="C:/Users/40274182/OneDrive - Queen's University Belfast/PhD/Chapters/Bleaching/Mangroves/Data files"
setwd(Bleaching_data_directory)

output_directory="C:/Users/40274182/OneDrive - Queen's University Belfast/PhD/Chapters/Bleaching/Mangroves/Working_file"


Bleaching_Data <- read.csv("RC_ERG_CoRTAD_GMW_distances.csv")


Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$SSTA_DHW),]
Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$Site),]
Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$Ecoregion),]
Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$Year),]
#Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$area4km),]
#Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$area2km),]
#Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$area1km),]
Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$Count),]
Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$kd490_value),]


Bleaching_Data$Ecoregion <- as.factor(Bleaching_Data$Ecoregion)


# make the map

sf_use_s2(FALSE)


# none mangrove sites
mapdistance <- Bleaching_Data

dfmapdist <- aggregate(mapdistance$distance, by=list(Site=mapdistance$Site, Lat=mapdistance$Latitude.Degrees, Lon=mapdistance$Longitude.Degrees), FUN=mean)

library(fishualize)

p1 <- ggplot(data = world) +
  geom_sf() +
  geom_point(data = dfmapdist, aes(x=Lon, y=Lat, color = log1p(x)), size = .5) +
  scale_color_fish(option = "Gramma_loreto") + #,
  #limits = c(1, 40),
  #breaks = c(6, 12, 18, 24, 30, 36),
  #labels = c(6, 12, 18, 24, 30, 36)) +
  #coord_sf(ylim =c(40, -40), xlim=c(-180, 180), expand = FALSE) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm") ,
                         height = unit(1, "cm"), width = unit(1, "cm") ,
                         style = north_arrow_fancy_orienteering) +
  scale_x_continuous(breaks =c(-120, -60, 0, 60, 120)) +
  scale_y_continuous(breaks =seq(-40, 40, by =20)) +
  labs(y= "", x="", color = "Distance (log)") +
  ggtitle("Distance of each site from nearest mangrove") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p1


library(patchwork)
png(file=file.path(output_directory,'Surveysmap_sf.png'),height=4000,width=7000,res=900)
p1
dev.off()


