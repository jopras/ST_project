
# "Clustering of delivery coordinates & Feature Creation"

#calling libraries
library(dbscan)
library(ggplot2)
library(dplyr)
library(maps)
library(dbscan)

#loading the dataset
All_in_one=read.csv("Final_All_in_one.csv", stringsAsFactors = F)
coords_del <- as.data.frame(cbind(lon=All_in_one$lng,lat=All_in_one$lat))


library(sp)
#change the crimes data into a SpatialPointsDataFrame
coords_1 <- cbind(Longitude = as.numeric(as.character(All_in_one$lng)), Latitude = as.numeric(as.character(All_in_one$lat)))
deliveries.pts <- SpatialPointsDataFrame(coords_del, All_in_one[,-(36:35)], proj4string = CRS("+init=epsg:4326"))

#trying to find the best EPS
UK_del_mat=as.matrix(All_in_one[,35:36])
dbscan::kNNdistplot(UK_del_mat, k =5)
abline(h = 0.4, lty = 2)

#EPS was set arbitrarily
EPS <- 0.035
clusters <- dbscan(select(All_in_one,lat,lng), eps = EPS,minPts = 10)
All_in_one$cluster <- clusters$cluster


#Finally we can split the original data into two according to whether dbscan has assigned or cluster or noise.

groups  <- All_in_one %>% filter(cluster != 0)
noise  <- All_in_one %>% filter(cluster == 0)

#ploting cluster of delivery orders
library(ggplot2)
library(dplyr)
library(maps)
#ploting the clusters
ggplot(All_in_one, aes(x = lng, y = lat, alpha = 0.5)) + 
  geom_point(aes(fill = "black"), noise) +
  geom_point(aes(colour = as.factor(cluster)), groups,
             size = 2) +
  coord_map() +
  theme(legend.position = "none")

#saving the updated data set
write.csv(All_in_one,file="Final_All_in_one.csv")


