
#"Spatial Visualizations & Feature Creation"

All_in_one=read.csv("Final_All_in_one.csv", stringsAsFactors = F)

# Creating some visualisations regarding the deliveries area
library(ggmap)
library(ggplot2)
library(dplyr)
library(maps)
library(mapdata)
coords_del <- as.data.frame(cbind(lon=All_in_one$lng,lat=All_in_one$lat))

library(sp)
#change the deliveries coordinates into a SpatialPointsDataFrame
coords_1 <- cbind(Longitude = as.numeric(as.character(All_in_one$lng)), Latitude = as.numeric(as.character(All_in_one$lat)))
deliveries.pts <- SpatialPointsDataFrame(coords_del, All_in_one[,-(35:34)], proj4string = CRS("+init=epsg:4326"))


#plot just the delivery points
plot(deliveries.pts, pch = ".", col = "darkred")

library(MASS)
library(survival)
library(fitdistrplus)
library(qmap)
library(ggplot2)
library(ggmap)
#plot the  hybrid Google Maps basemap
map <- ggmap(get_googlemap('London', zoom = 8, maptype = 'hybrid',scale = 2),
             size = c(600, 600),extent='normal', darken = 0)

#plot the delivery points on top
map + geom_point(data = All_in_one, aes(x = All_in_one$lng, y = All_in_one$lat), color="red", size=0.2, alpha=0.5)


#finding data(long, lat,population) for worldwide cities
data("world.cities")
#flter only for UK
UK <- world.cities %>% filter(country.etc == "UK")


#tranforming our variables as to match them with UK cities
coords_del$new_lng=substr(coords_del$lon,1,nchar(coords_del$lon)-5 )
coords_del$new_lat=substr(coords_del$lat,1,nchar(coords_del$lat)-5 )

coords_del=coords_del[,-1]
coords_del=coords_del[,-1]

library(sqldf)
#match UK cities based on lat and long
UK_del=sqldf("select distinct * from coords_del a inner join UK b 
             on a.new_lat=b.lat and a.new_lng=b.long")


#removing the duplicated cities 
UK_del=UK_del[-21,]
UK_del=UK_del[-3,]

UK_cities=read.csv("UK_cities.csv")

library(acs)
library(geosphere)

# Calculates the geodesic distance between two points specified by radian latitude/longitude using the
# Haversine formula (hf)
gcd.hf <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d) # Distance in km
}

#Calculating the delivery distance from each city centre 
for(i in 1 :nrow(All_in_one)){
  All_in_one$Dist_Crowborough[i]=gcd.hf(All_in_one$lng[i],All_in_one$lat[i],UK_del$long[1],UK_del$lat[1])
  All_in_one$Dist_Royal_Tunbridge_Wells[i]=gcd.hf(All_in_one$lng[i],All_in_one$lat[i],UK_del$long[2],UK_del$lat[2])  
  All_in_one$Dist_Bexhill[i]=gcd.hf(All_in_one$lng[i],All_in_one$lat[i],UK_del$long[3],UK_del$lat[3])  
  All_in_one$Dist_Hastings[i]=gcd.hf(All_in_one$lng[i],All_in_one$lat[i],UK_del$long[4],UK_del$lat[4])  
  All_in_one$Dist_Ashford[i]=gcd.hf(All_in_one$lng[i],All_in_one$lat[i],UK_del$long[5],UK_del$lat[5])  
  All_in_one$Dist_Uckfield[i]=gcd.hf(All_in_one$lng[i],All_in_one$lat[i],UK_del$long[6],UK_del$lat[6])  
  All_in_one$Dist_Halstead[i]=gcd.hf(All_in_one$lng[i],All_in_one$lat[i],UK_del$long[7],UK_del$lat[7])  
  All_in_one$Dist_Sevenoaks[i]=gcd.hf(All_in_one$lng[i],All_in_one$lat[i],UK_del$long[8],UK_del$lat[8])  
  All_in_one$Dist_Tonbridge[i]=gcd.hf(All_in_one$lng[i],All_in_one$lat[i],UK_del$long[9],UK_del$lat[9])  
  All_in_one$Dist_East_Grinstead[i]=gcd.hf(All_in_one$lng[i],All_in_one$lat[i],UK_del$long[10],UK_del$lat[10])  
  All_in_one$Dist_Gillingham[i]=gcd.hf(All_in_one$lng[i],All_in_one$lat[i],UK_del$long[11],UK_del$lat[11])  
  All_in_one$Dist_Snodland[i]=gcd.hf(All_in_one$lng[i],All_in_one$lat[i],UK_del$long[12],UK_del$lat[12])  
  All_in_one$Dist_Aylesford_East_Malling[i]=gcd.hf(All_in_one$lng[i],All_in_one$lat[i],UK_del$long[13],UK_del$lat[13])  
  All_in_one$Dist_Strood[i]=gcd.hf(All_in_one$lng[i],All_in_one$lat[i],UK_del$long[14],UK_del$lat[14])  
  All_in_one$Dist_Maidstone[i]=gcd.hf(All_in_one$lng[i],All_in_one$lat[i],UK_del$long[15],UK_del$lat[15])  
  All_in_one$Dist_Sheerness[i]=gcd.hf(All_in_one$lng[i],All_in_one$lat[i],UK_del$long[16],UK_del$lat[16])  
  All_in_one$Dist_Sittingbourne[i]=gcd.hf(All_in_one$lng[i],All_in_one$lat[i],UK_del$long[17],UK_del$lat[17])  
  All_in_one$Dist_Rochester[i]=gcd.hf(All_in_one$lng[i],All_in_one$lat[i],UK_del$long[18],UK_del$lat[18])  
  All_in_one$Dist_Dartford[i]=gcd.hf(All_in_one$lng[i],All_in_one$lat[i],UK_del$long[19],UK_del$lat[19])  
  All_in_one$Dist_Broadstairs[i]=gcd.hf(All_in_one$lng[i],All_in_one$lat[i],UK_del$long[20],UK_del$lat[20])  
  All_in_one$Dist_Margate[i]=gcd.hf(All_in_one$lng[i],All_in_one$lat[i],UK_del$long[21],UK_del$lat[21])  
  All_in_one$Dist_Whitstable[i]=gcd.hf(All_in_one$lng[i],All_in_one$lat[i],UK_del$long[22],UK_del$lat[22])  
  All_in_one$Dist_Canterbury[i]=gcd.hf(All_in_one$lng[i],All_in_one$lat[i],UK_del$long[23],UK_del$lat[23])  
  All_in_one$Dist_Folkestone[i]=gcd.hf(All_in_one$lng[i],All_in_one$lat[i],UK_del$long[24],UK_del$lat[24])  
  All_in_one$Dist_Ramsgate[i]=gcd.hf(All_in_one$lng[i],All_in_one$lat[i],UK_del$long[25],UK_del$lat[25])  
}


#creating a variable "minimum distance", which returns the minimun distance from the closest city centre

for(i in 1 :nrow(All_in_one)){
  All_in_one$Min_Dist[i]=with(All_in_one, pmin(Dist_Crowborough[i],Dist_Royal_Tunbridge_Wells[i],Dist_Bexhill[i],
                                               Dist_Hastings[i],Dist_Ashford[i],Dist_Uckfield[i],Dist_Halstead[i],
                                               Dist_Sevenoaks[i],Dist_Tonbridge[i],Dist_East_Grinstead[i],Dist_Gillingham[i],
                                               Dist_Snodland[i],Dist_Aylesford_East_Malling[i],Dist_Strood[i],Dist_Maidstone[i],
                                               Dist_Sheerness[i],Dist_Sittingbourne[i],Dist_Rochester[i],Dist_Dartford[i],Dist_Broadstairs[i],
                                               Dist_Margate[i],Dist_Whitstable[i],Dist_Canterbury[i],Dist_Folkestone[i],Dist_Ramsgate[i]))
  
}

#Plot the distances from the city center 
library(graphics)
library(plotly)
library(mime)
library(crosstalk)

closeness <- density(All_in_one$Min_Dist)

p <- plot_ly(x = ~density$x, y = ~density$y, type = 'scatter', mode = 'lines', fill = 'tozeroy') %>%
  layout(xaxis = list(title = 'Distance from City Centre'),
         yaxis = list(title = 'Closeness'))
p

closeness_to_city=(All_in_one$Min_Dist)
quantile(closeness_to_city)

for(i in 1 :nrow(All_in_one)){
  if(All_in_one$Min_Dist[i]<=50){
    All_in_one$Density_profile[i] ="High Closeness"
  }else if(All_in_one$Min_Dist[i]<=150){
    All_in_one$Density_profile[i]="Medium Closeness"
  }else if(All_in_one$Min_Dist[i]<=200){
    All_in_one$Density_profile[i]="Low Closeness"
  }else 
    All_in_one$Density_profile[i]="Outskirts"
}


library(plotly)

high <- All_in_one[which(All_in_one$Density_profile == "High Closeness"),]
density1 <- density(high$Min_Dist)

medium <- All_in_one[which(All_in_one$Density_profile =="Medium Closeness"),]
density2 <- density(medium$Min_Dist)

low <- All_in_one[which(All_in_one$Density_profile == "Low Closeness"),]
density3 <- density(low$Min_Dist)

out <- All_in_one[which(All_in_one$Density_profile == "Outskirts"),]
density4 <- density(out$Min_Dist)

p <- plot_ly(x = ~density1$x, y = ~density1$y, type = 'scatter', mode = 'lines', name = 'High Closeness', fill = 'tozeroy') %>%
  add_trace(x = ~density2$x, y = ~density2$y, name = 'Medium Closeness', fill = 'tozeroy') %>%
  add_trace(x = ~density3$x, y = ~density3$y, name = 'Low Closeness', fill = 'tozeroy') %>%
  add_trace(x = ~density4$x, y = ~density4$y, name = 'Outskirts', fill = 'tozeroy') %>%
  layout(xaxis = list(title = 'Distance from City Centre'),
         yaxis = list(title = 'Closeness'))


p#print plot 

write.csv(All_in_one,"Final_All_in_one.csv")
