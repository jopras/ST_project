# "Removing the outliers (Approach 2)"



#loading the data
features=read.csv("Final_Features.csv", stringsAsFactors = F)
featuresFilt=features

#imputing the zero values
#item_weight_sum
for(i in 1:nrow(featuresFilt)){
  if(featuresFilt$item_weight_sum[i]==0){
    featuresFilt$item_weight_sum[i]=mean(featuresFilt$item_weight_sum)
  }
}


for(i in 1:nrow(featuresFilt)){
  if(featuresFilt$item_weight_max[i]==0){
    featuresFilt$item_weight_max[i]=mean(featuresFilt$item_weight_max)
  }
}


#individual_pieces_sum
for(i in 1:nrow(featuresFilt)){
  if(featuresFilt$individual_pieces_sum[i]==0){
    featuresFilt$individual_pieces_sum[i]=mean(featuresFilt$individual_pieces_sum)
  }
}

#item_volume_sum

for(i in 1:nrow(featuresFilt)){
  if(featuresFilt$item_volume_sum[i]==0){
    featuresFilt$item_volume_sum[i]=mean(featuresFilt$item_volume_sum)
  }
}

#DELIVERY_DIST
for(i in 1:nrow(featuresFilt)){
  if(featuresFilt$DELIVERY_DIST[i]==0){
    featuresFilt$DELIVERY_DIST[i]=mean(featuresFilt$DELIVERY_DIST)
  }
}

#order_value

for(i in 1:nrow(featuresFilt)){
  if(featuresFilt$order_value[i]==0){
    featuresFilt$order_value[i]=mean(featuresFilt$order_value)
  }
}


#Removing manually stopping time outliers
Final_Dataset=sqldf("select * from featuresFilt where STOPPING_TIME>=300 and STOPPING_TIME<=1800 ")

#Removing manually individual_pieces outliers
Final_Dataset2=sqldf("select * from Final_Dataset where individual_pieces_sum<=10")

#converting categorical variables into characters
Final_Dataset4$drop_sequence=as.character(Final_Dataset4$drop_sequence)
Final_Dataset4$cluster=as.character(Final_Dataset4$cluster)

library(dummies)
#transform the factors into dummies
completed_dummies=dummy.data.frame(Final_Dataset4,sep = ".")
names(completed_dummies)

write.csv(completed_dummies,"Second_Approach.csv")

