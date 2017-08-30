
#Merging the Data

#uploading the datasets Schedule Plan & Deliveries History
Scedule_Plan=read.csv("....Schedule_plan.csv", stringsAsFactors = F)
Scedule_Plan$date= as.Date(Scedule_Plan$date)
del_hist=read.csv("Delivery_hist.csv", stringsAsFactors = F)
del_hist$delivery_date= as.Date(del_hist$delivery_date)

library(sqldf)#calling sql library

Schedule_plan_distinct=sqldf("select distinct * from Scedule_Plan ")#removing all duplicates from Scedule_Plan
Schedule_plan_6months=ap_distinct[which(Schedule_plan_distinct$date>=as.Date("2016-10-01")),]#keep only observations from the last 6-month period

#keep only the unique values from apollo dataset (same order id different details)
Schedule_plan_unique=sqldf("select distinct orderID,postcode,vehicle,date,lat,lng,timeAtDoor from Schedule_plan_6months")

del_hist_6months=del_hist[which(del_hist$delivery_date>=as.Date("2016-10-01")),]#keep only observations from from the last 6-month period


#join schedule plan & deliveries history datasets
first_join=sqldf("select * from Schedule_plan_unique a 
                 left outer join del_hist_6months b  on a.orderID=b.order_number
                 and a.date=b.delivery_date and a.vehicle=b.van_no 
                 and a.postcode=b.delivery_address_postcode
                 ")

first_join=na.omit(first_join)#dropping the NA's

#upload the MasterStops Dataset
master=read.csv("MasterStops.csv",stringsAsFactors = F)
master$DELIVERY_DATE=as.Date(master$DELIVERY_DATE,"%d/%m/%y")
#select only distinct values where stopping time is more than 3 minutes
master_distinct=sqldf("select distinct * from master where STOPPING_TIME>=180")
master_6months=master_distinct[which(master_distinct$DELIVERY_DATE>=as.Date("2016-10-01")),]#keep only observations from the last 6-month period

#creating variable sortpostcode with less postcode digits
for(i in 1 :nrow(first_join)){
  postcode=first_join$postcode[i]
  first_join$sortpostcode[i]=substr(first_join,1,regexpr(" ",postcode)[1]+1 )
}


#creating variable sortaddress without street numbers
for(i in 1:nrow(first_join)){
  delivery_address_line=first_join$delivery_address_line[i]
  first_join$sortdeladress[i]=gsub("[[:digit:]]", "",delivery_address_line )
}

#join first_join with Masterstops dataset
second_join=sqldf("select distinct * from first_join a
                  left outer join master_only_6months b on a.sortpostcode=b.TOWN3
                  and a.date=b.DELIVERY_DATE and a.vehicle=b.VANID
                  and a.sortdeladress=b.STOP_STREET
                  ")

#instal packages maps and sp

library(geosphere)
library(fields)
second_join$Distance_Coor=distHaversine(ap_del_mas[,6:5],ap_del_mas[,40:39])#calculating the coordinate distance between stopping location and delivery address location

colnames(second_join)[39]<-"LATITUDE"#Renaming the lat column
colnames(second_join)[40]<-"LONGTITUDE"#Renaming the long column
second_join=second_join[,-33]# removing common columns 

#count the unique values in second_join
second_join_unique=sqldf("select orderID,count(1) 
                         from second_join group by orderID 
                         having count(1)=1")

#creating a subset with the unique values in second_join
unique_orders=sqldf("select a.* from second_join a inner 
                    join second_join_unique b on a.orderID=b.orderID
                    and a.DRIVER_NAME is not null
                    ")
#creating a subset with the null values in second_join
unique_nulls=sqldf("select a.* from second_join a inner 
                   join second_join_unique b on a.orderID=b.orderID 
                   and a.DRIVER_NAME is null 
                   ")
#count the duplicated values in second_join
second_join_dupl=sqldf("select orderID,count(1) cnt
                       from second_join group by orderID 
                       having count(1)>1")

#creating a subset with the duplicated values in second_join
dupl_orders=sqldf("select * from second_join a inner 
                  join second_join_dupl b on a.orderID=b.orderID
                  and a.DRIVER_NAME is not null
                  ")

#creating a subset with the null duplicated values in second_join
dupl_nulls=sqldf("select a.* from second_join a inner 
                 join second_join_dupl b on a.orderID=b.orderID 
                 and a.DRIVER_NAME is null
                 ")

dupl_orders_sort=dupl_orders[order(dupl_orders$orderID,decreasing=T),]#sorting the duplicates descenting
duplicates=dupl_orders_sort[which(dupl_orders_sort$cnt<=5),]# selecting only orders with less than 5 times repetition 

allnewrows=c()# creating an empty bin to input the duplicates which are executed at similar time intervals
orders=unique(duplicates$orderID)
for(i in 1:length(orders)){
  dupl_mini=duplicates[which(duplicates$orderID==orders[i]),]
  max_time=max(dupl_mini$STOP_TIME)
  min_time=min(dupl_mini$START_TIME)
  h1=as.POSIXct(strptime(max_time,"%H:%M:%S"))
  h2=as.POSIXct(strptime(min_time,"%H:%M:%S"))
  STOPPING_TIME_1=sum(dupl_mini$STOPPING_TIME)
  max_stop=max(dupl_mini$STOPPING_TIME)
  min_stop=min(dupl_mini$STOPPING_TIME)
  if (as.difftime(h1-h2)<="Time difference of 1.5 hours" & as.numeric(max_stop-min_stop)>0){
    dupl_row=dupl_mini[1,]
    dupl_row$STOP_TIME<-max_time
    dupl_row$START_TIME<-min_time
    dupl_row$STOPPING_TIME<-STOPPING_TIME_1
    allnewrows=rbind(allnewrows,dupl_row)
  }else if (as.difftime(h1-h2)<="Time difference of 1.5 hours" & as.numeric(max_stop-min_stop)<=0){
    dupl_row=dupl_mini[1,]
    dupl_row$STOP_TIME<-max_time
    dupl_row$START_TIME<-min_time
    allnewrows=rbind(allnewrows,dupl_row) 
  }else
    dupl_row=dupl_mini[1,]
  dupl_row$STOP_TIME<-max_time
  dupl_row$START_TIME<-min_time
}

#changing the column names
colnames(allnewrows)[41] <- "orderID2"
colnames(dupl_orders_sort)[41]<- "orderID2"

#finding the rest unmatched rows
all_rest_dupl=sqldf("select a.* from dupl_orders_sort a
                    left outer join allnewrows b on a.orderID=b.orderID 
                    where b.orderID is null
                    ")

all_rest_dupl_sort=all_rest_dupl[order(all_rest_dupl$orderID,decreasing=T),]

allnewrows2=c()#creating a second bin to merge duplicates which are in a coordinate distance of less than 350 meters
orders_2=unique(all_rest_dupl_sort$orderID)
for(j in 1:length(orders_2)){
  dist_mini=all_rest_dupl_sort[which(all_rest_dupl_sort$orderID==orders_2[j]),]
  Coor_dist=dist_mini$Distance_Coor
  STOPPING_TIME_2=sum(dupl_mini$STOPPING_TIME)
  if ((as.numeric(Coor_dist))<=350) {
    dist_row=dist_mini[1,]
    dist_row$Distance_Coor<-Coor_dist[1]
    dist_row$STOPPING_TIME<-STOPPING_TIME_2
    allnewrows2=rbind(allnewrows2,dist_row)
  }
}

#finding the missing values(nulls) from null datasets by merging it with MasterStops(less key points were used to succeed matching)

unique_nulls_rest=sqldf("select * from unique_nulls  a
                        inner join master_6months b on
                        a.date =b.DELIVERY_DATE and a.vehicle=b.VANID
                        ")
#renaming and removing below columns 
unique_nulls_rest=unique_nulls_rest[,-25:-39]
colnames(unique_nulls_rest)[40]<-"LATITD"
colnames(unique_nulls_rest)[41]<-"LONGTD"
unique_nulls_rest=unique_nulls_rest[,-34]
unique_nulls_rest=unique_nulls_rest[,-25]

#calculating the coordinate distance between stopping location and delivery location 
unique_nulls_rest$Distance_Coord=distHaversine(unique_nulls_rest[,6:5],unique_nulls_rest[,39:38])

#keeping only values where coordinate distance is less than 350 meters
unique_nulls_rest2=unique_nulls_rest[which(unique_nulls_rest$Distance_Coord<=350),]

#counting the unique values in the unique_nulls_rest data set
unique_orders_fr_nulls=sqldf("select orderID,count(1) 
                             from unique_nulls_rest2 group by orderID 
                             having count(1)=1")
#creating a subset with the uniques 
unique_orders_fr_nulls2=sqldf("select a.* from unique_nulls_rest2 a
                              inner join unique_orders_fr_nulls b
                              on a.orderID=b.orderID and a.DRIVER_NAME
                              is not null")
#counting the duplicates
dupl_orders_fr_nulls=sqldf("select orderID,count(1) cnt
                           from unique_nulls_rest2  group by orderID 
                           having count(1)>1")

#creating a subset with the duplicates
dupl_orders_fr_nulls2=sqldf("select * from unique_nulls_rest2 a inner 
                            join dupl_orders_fr_nulls b on 
                            a.orderID=b.orderID and a.DRIVER_NAME
                            is not null")
#sorting the duplicates
dupl_orders_fr_nulls2_sort=dupl_orders_fr_nulls2[order(dupl_orders_fr_nulls2$orderID,decreasing=T),]
# selecting only orders with less than 3 times repetition 
dupl_orders_fr_nulls2_sort=dupl_orders_fr_nulls2_sort[,-41]
dupl_orders_fr_nulls2_dupl_2_3=dupl_orders_fr_nulls2_sort[which(dupl_orders_fr_nulls2_sort$cnt<=3),]

allnewrows3=c()# creating an empty bin to input the duplicates which are executed at similar time intervals
orders_3=unique(dupl_orders_fr_nulls2_dupl_2_3$orderID)
for(k in 1:length(orders_3)){
  dupl_mini_2=dupl_orders_fr_nulls2_dupl_2_3[which(dupl_orders_fr_nulls2_dupl_2_3$orderID==orders_3[k]),]
  max_time_2=max(dupl_mini_2$STOP_TIME)
  min_time_2=min(dupl_mini_2$START_TIME)
  STOPPING_TIME_2=sum(dupl_mini_2$STOPPING_TIME)
  max_stop_2=max(dupl_mini_2$STOPPING_TIME)
  min_stop_2=min(dupl_mini_2$STOPPING_TIME)
  h1_2=as.POSIXct(strptime(max_time_2,"%H:%M:%S"))
  h2_2=as.POSIXct(strptime(min_time_2,"%H:%M:%S"))
  #h1_2=(as.numeric(substr(max_time_2,1,2))*60+as.numeric(substr(max_time_2,4,5)))/60
  #h2_2=(as.numeric(substr(min_time_2,1,2))*60+as.numeric(substr(min_time_2,4,5)))/60             
  if (as.difftime(h1_2-h2_2)<="Time difference of 1.5 hours" & as.numeric(max_stop_2-min_stop_2)>0){   
    dupl_row_2=dupl_mini_2[1,]
    dupl_row_2$STOP_TIME<-max_time_2
    dupl_row_2$START_TIME<-min_time_2
    dupl_row_2$STOPPING_TIME<-STOPPING_TIME_2
    allnewrows3=rbind(allnewrows3,dupl_row_2)
  }else if (as.difftime(h1_2-h2_2)<="Time difference of 1.5 hours" & as.numeric(max_stop_2-min_stop_2)<=0){
    dupl_row_2=dupl_mini_2[1,]
    dupl_row_2$STOP_TIME<-max_time_2
    dupl_row$START_TIME<-min_time_2
    allnewrows3=rbind(allnewrows3,dupl_row_2) 
  }else
    dupl_row=dupl_mini[1,]
  dupl_row$STOP_TIME<-max_time_2
  dupl_row$START_TIME<-min_time_2
}

#finding the rest unmatched rows
all_rest_3=sqldf("select a.* from dupl_orders_fr_nulls2_sort a
                 left outer join allnewrows3 b on a.orderID=b.orderID 
                 where b.orderID is null
                 ")
all_rest_3_sort=all_rest_3[order(all_rest_3$orderID,decreasing=T),]

allnewrows3b=c()#creating a second bin to merge duplicates which are in a coordinate distance of less than 350 meters
orders_3b=unique(all_rest_3_sort$orderID)
for(m in 1:length(orders_3b)){
  dist_mini_2=all_rest_3_sort[which(all_rest_3_sort$orderID==orders_3b[m]),]
  Coor_dist=dist_mini_2$Distance_Coor
  STOPPING_TIME_3=sum(dupl_mini_2$STOPPING_TIME)
  if ((as.numeric(Coor_dist))<=350) {
    dist_row_2=dist_mini_2[1,]
    dist_row_2$Distance_Coor<-Coor_dist[1]
    dist_row_2$STOPPING_TIME<-STOPPING_TIME_2
    allnewrows3b=rbind(allnewrows3b,dist_row_2)
  }
}


#finding missing values from dupl_nulls
dupl_nulls_rest=sqldf("select * from dupl_nulls  a
                      inner join master_only2017 b on
                      a.date =b.DELIVERY_DATE and a.vehicle=b.VANID
                      ")
#removing and renaming below columns
dupl_nulls_rest=dupl_nulls_rest[,-25:-39]
colnames(dupl_nulls_rest)[40]<-"LATIT"
colnames(dupl_nulls_rest)[41]<-"LONGTIT"
dupl_nulls_rest=dupl_nulls_rest[,-34]
dupl_nulls_rest=dupl_nulls_rest[,-25]

#calculating the coordinate distance between stopping location and delivery location 
dupl_nulls_rest$Distance_Coord=distHaversine(dupl_nulls_rest[,6:5],dupl_nulls_rest[,39:38])

#keeping only values where coordinate distance is less than 350 meters
dupl_nulls_rest_2=dupl_nulls_rest[which(dupl_nulls_rest$Distance_Coord<=350),]

#counting the unique values in the dupl_nulls_rest_2 data set
unique_orders_fr_d_nulls=sqldf("select orderID,count(1) cnt
                               from dupl_nulls_rest_2 group by orderID 
                               having count(1)=1")
#creating a subset with the uniques 
unique_orders_fr_d_nulls2=sqldf("select a.* from dupl_nulls_rest_2 a
                                inner join unique_orders_fr_d_nulls b
                                on a.orderID=b.orderID and a.DRIVER_NAME
                                is not null")
#counting the duplicates
dupl_orders_fr_d_nulls=sqldf("select orderID,count(1) cnt
                             from dupl_nulls_rest_2  group by orderID 
                             having count(1)>1")

#creating a subset with the duplicates
dupl_orders_fr_d_nulls2=sqldf("select * from  dupl_nulls_rest_2  a inner 
                              join dupl_orders_fr_d_nulls b on 
                              a.orderID=b.orderID and a.DRIVER_NAME
                              is not null")
#sorting the duplicates
dupl_orders_fr_d_nulls_sort=dupl_orders_fr_d_nulls2[order(dupl_orders_fr_d_nulls2$orderID,decreasing=T),]
dupl_orders_fr_d_nulls_sort=dupl_orders_fr_d_nulls_sort[,-41]
# selecting only orders with less than 3 times repetition 
dupl_orders_fr_d_nulls2_2_3=dupl_orders_fr_d_nulls_sort[which(dupl_orders_fr_d_nulls_sort$cnt<=3),]

allnewrows4=c()# creating an empty bin to input the duplicates which are executed at similar time intervals
orders_4=unique(dupl_orders_fr_d_nulls2_2_3$orderID)
for(z in 1:length(orders_4)){
  dupl_mini_3=dupl_orders_fr_d_nulls2_2_3[which(dupl_orders_fr_d_nulls2_2_3$orderID==orders_4[z]),]
  max_time_3=max(dupl_mini_3$STOP_TIME)
  min_time_3=min(dupl_mini_3$START_TIME)
  STOPPING_TIME_3=sum(dupl_mini_3$STOPPING_TIME)
  max_stop_3=max(dupl_mini_3$STOPPING_TIME)
  min_stop_3=min(dupl_mini_3$STOPPING_TIME)
  h1_3=as.POSIXct(strptime(max_time_3,"%H:%M:%S"))
  h2_3=as.POSIXct(strptime(min_time_3,"%H:%M:%S"))
  if (as.difftime(h1_3-h2_3)<="Time difference of 1.5 hours" & as.numeric(max_stop_3-min_stop_3)>0){   
    dupl_row_3=dupl_mini_3[1,]
    dupl_row_3$STOP_TIME<-max_time_3
    dupl_row_3$START_TIME<-min_time_3
    dupl_row_3$STOPPING_TIME<-STOPPING_TIME_3
    allnewrows4=rbind(allnewrows4,dupl_row_3)
  } else if (as.difftime(h1_3-h2_3)<="Time difference of 1.5 hours" & as.numeric(max_stop_3-min_stop_3)<=0){
    dupl_row_3=dupl_mini_3[1,]
    dupl_row_3$STOP_TIME<-max_time_3
    dupl_row_3$START_TIME<-min_time_3
    allnewrows4=rbind(allnewrows4,dupl_row_3)        
  }else
    dupl_row_3=dupl_mini_3[1,]
  dupl_row_3$STOP_TIME<-max_time_3
  dupl_row_3$START_TIME<-min_time_3
}

#finding the rest unmatched rows
all_rest_4=sqldf("select a.* from dupl_orders_fr_d_nulls_sort a
                 left outer join allnewrows4 b on a.orderID=b.orderID 
                 where b.orderID is null
                 ")
#sort the all_rest_4 descenting
all_rest_4_sort=all_rest_4[order(all_rest_4$orderID,decreasing=T),]

allnewrows4b=c()#creating a second bin to merge duplicates which are in a coordinate distance of less than 350 meters
orders_4b=unique(all_rest_4_sort$orderID)
for(n in 1:length(orders_4b)){
  dist_mini_3=all_rest_4_sort[which(all_rest_4_sort$orderID==orders_4b[n]),]
  Coor_dist=dist_mini_3$Distance_Coor
  STOPPING_TIME_3=sum(dupl_mini_3$STOPPING_TIME)
  if ((as.numeric(Coor_dist))<=350) {
    dist_row_3=dist_mini_2[1,]
    dist_row_3$Distance_Coor<-Coor_dist[1]
    dist_row_3$STOPPING_TIME<-STOPPING_TIME_3
    allnewrows4b=rbind(allnewrows4b,dist_row_3)
  }
}

#removing undesired columns
allnewrows=allnewrows[,-42]
allnewrows=allnewrows[,-41]
allnewrows2=allnewrows2[,-42]
allnewrows2=allnewrows2[,-41]
allnewrows3=allnewrows3[,-41]
allnewrows3b=allnewrows3b[,-42]
allnewrows3b=allnewrows3b[,-41]
allnewrows4=allnewrows4[,-41]
allnewrows4b=allnewrows4b[,-42]
allnewrows4b=allnewrows4b[,-41]

#convertung all the column names as to have the same name in all the subsets
colnames(unique_orders_fr_d_nulls2)[38]<-"LATIT"
colnames(unique_orders_fr_d_nulls2)[39]<-"LONG"
colnames(unique_orders_fr_d_nulls2)[40]<-"Distance_Coor"


colnames(unique_orders_fr_nulls2)[38]<-"LATIT"
colnames(unique_orders_fr_nulls2)[39]<-"LONG"
colnames(unique_orders_fr_nulls2)[40]<-"Distance_Coor"

colnames(allnewrows4)[38]<-"LATIT"
colnames(allnewrows4)[39]<-"LONG"
colnames(allnewrows4)[40]<-"Distance_Coor"

colnames(allnewrows3)[38]<-"LATIT"
colnames(allnewrows3)[39]<-"LONG"
colnames(allnewrows3)[40]<-"Distance_Coor"

colnames(allnewrows3b)[38]<-"LATIT"
colnames(allnewrows3b)[39]<-"LONG"
colnames(allnewrows3b)[40]<-"Distance_Coor"

colnames(allnewrows4b)[38]<-"LATIT"
colnames(allnewrows4b)[39]<-"LONG"
colnames(allnewrows4b)[40]<-"Distance_Coor"

#finally merge them all together and end up with one data set consitutes of 8003 matches observations
total <-rbind(unique_orders_fr_d_nulls2,unique_orders_fr_nulls2,unique_orders,
              allnewrows,allnewrows2,allnewrows3,allnewrows3b,allnewrows4,allnewrows4b)

#saving the final data set
write.csv(total,"Final_total.csv")

#checking the structure and the summary of the final data set 
str(total)
summary(total)