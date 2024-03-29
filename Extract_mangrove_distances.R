
library(sp)
library(geosphere)
library(maptools)
library(rgdal)
library(raster)
library(rgeos)
library(tictoc)

# directory 

mangrovewd = "D:/PhD/Global_mangrove_watch/01_Data"
setwd(mangrovewd)

gmw_2007_spdf <- readOGR("GMW_2007_v2.shp")
gmw_2008_spdf <- readOGR("GMW_2008_v2.shp")
gmw_2009_spdf <- readOGR("GMW_2009_v2.shp")
gmw_2010_spdf <- readOGR("GMW_2010_v2.shp")
gmw_2015_spdf <- readOGR("GMW_2015_v2.shp")
gmw_2016_spdf <- readOGR("GMW_2016_v2.shp")



# bleaching data

Bleaching_data_directory="C:/Users/40274182/OneDrive - Queen's University Belfast/PhD/Chapters/Bleaching/Mangroves/Data files"
setwd(Bleaching_data_directory)

Bleaching_Data <- read.csv("RC_ERG_CoRTAD_Mangrove_kd490_MPA_GMW.csv")

head(Bleaching_Data)

Bleaching_Data <- Bleaching_Data[-c(1)]

# only sites up to, and including, 2007 

Bleaching_Data_2007 <- subset(Bleaching_Data, Year <=2007)
Bleaching_Data_2008 <- subset(Bleaching_Data, Year ==2008)
Bleaching_Data_2009 <- subset(Bleaching_Data, Year ==2009)
Bleaching_Data_2010 <- subset(Bleaching_Data, Year ==2010)
Bleaching_Data_2015 <- subset(Bleaching_Data, Year >2010 & Year<= 2015)
Bleaching_Data_2016 <- subset(Bleaching_Data, Year >=2016)


# run in parallel
pts_2007 <- subset(Bleaching_Data_2007, select=c(Longitude.Degrees, Latitude.Degrees))

cents_2007 <- gCentroid(gmw_2007_spdf, byid = TRUE)

pts_2007_1 <- pts_2007[1,]


library(parallel)
parallel::detectCores()

setCores<-round(detectCores()*0.8) 
cl <- makeCluster(getOption("cl.cores",setCores))
cl.pkg <- clusterEvalQ(cl,library(geosphere)) 
clusterExport(cl,"pts_2007")
clusterExport(cl,"cents_2007")
clusterExport(cl,"pts_2007_1")

library(tictoc)

tic()
test <- parLapply(cl=cl,1,function(i){
  dist2Line(pts_2007_1, cents_2007)
}
)
toc()

# 8.94 seconds

tic()
dist_2007 <- parLapply(cl=cl,1,function(i){
  dist2Line(pts_2007, cents_2007)
}
)
toc()

# 2008 
pts_2008 <- subset(Bleaching_Data_2008, select=c(Longitude.Degrees, Latitude.Degrees))

cents_2008 <- gCentroid(gmw_2008_spdf, byid = TRUE)

clusterExport(cl,"pts_2008")
clusterExport(cl,"cents_2008")

tic()
dist_2008 <- parLapply(cl=cl,1,function(i){
  dist2Line(pts_2008, cents_2008)
}
)
toc()

# 2009
pts_2009 <- subset(Bleaching_Data_2009, select=c(Longitude.Degrees, Latitude.Degrees))

cents_2009 <- gCentroid(gmw_2009_spdf, byid = TRUE)

clusterExport(cl,"pts_2009")
clusterExport(cl,"cents_2009")

tic()
dist_2009 <- parLapply(cl=cl,1,function(i){
  dist2Line(pts_2009, cents_2009)
}
)
toc()

# 2010

pts_2010 <- subset(Bleaching_Data_2010, select=c(Longitude.Degrees, Latitude.Degrees))

cents_2010 <- gCentroid(gmw_2010_spdf, byid = TRUE)

clusterExport(cl,"pts_2010")
clusterExport(cl,"cents_2010")

tic()
dist_2010 <- parLapply(cl=cl,1,function(i){
  dist2Line(pts_2010, cents_2010)
}
)
toc()

# 2015

pts_2015 <- subset(Bleaching_Data_2015, select=c(Longitude.Degrees, Latitude.Degrees))

cents_2015 <- gCentroid(gmw_2015_spdf, byid = TRUE)

clusterExport(cl,"pts_2015")
clusterExport(cl,"cents_2015")

tic()
dist_2015 <- parLapply(cl=cl,1,function(i){
  dist2Line(pts_2015, cents_2015)
}
)
toc()

# 2016

pts_2016 <- subset(Bleaching_Data_2016, select=c(Longitude.Degrees, Latitude.Degrees))

cents_2016 <- gCentroid(gmw_2016_spdf, byid = TRUE)

clusterExport(cl,"pts_2016")
clusterExport(cl,"cents_2016")

tic()
dist_2016 <- parLapply(cl=cl,1,function(i){
  dist2Line(pts_2016, cents_2016)
}
)
toc()

# combine files

d7 <- as.data.frame(dist_2007[[1]])
d8 <- as.data.frame(dist_2008[[1]])
d9 <- as.data.frame(dist_2009[[1]])
d10 <- as.data.frame(dist_2010[[1]])
d15 <- as.data.frame(dist_2015[[1]])
d16 <- as.data.frame(dist_2016[[1]])

Bleaching_Data_2007$distance <- d7$distance
Bleaching_Data_2008$distance <- d8$distance
Bleaching_Data_2009$distance <- d9$distance
Bleaching_Data_2010$distance <- d10$distance
Bleaching_Data_2015$distance <- d15$distance
Bleaching_Data_2016$distance <- d16$distance

bleaching_data_with_mangrove_distances <- rbind(Bleaching_Data_2007, Bleaching_Data_2008, Bleaching_Data_2009, Bleaching_Data_2010, Bleaching_Data_2015, Bleaching_Data_2016)

write.csv(bleaching_data_with_mangrove_distances, file = "RC_ERG_CoRTAD_GMW_distances.csv")


            
