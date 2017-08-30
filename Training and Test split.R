#Training and Test split

#load complete data from both approaches 

completedData1=read.csv("Approach_1.csv", stringsAsFactors = F)
completedData2=read.csv("Second_Approach.csv", stringsAsFactors = F)

#split our data into test , training and validation(use 70/30)
set.seed(9)
validation_index <- createDataPartition(completedData1$STOPPING_TIME, p=0.70, list=FALSE)
test1 <- completedData1[-validation_index,]
training1 <- completedData1[validation_index,]

#split our data into test , training and validation(use 70/30)
set.seed(9)
validation_index <- createDataPartition(completedData2$STOPPING_TIME, p=0.70, list=FALSE)
test2 <- completedData2[-validation_index,]
training2 <- completedData2[validation_index,]

write.csv(test1,file="test_1st_approach.csv")
write.csv(training1 ,file="training_1st_approach.csv")
write.csv(test2,file="test_2nd_approach.csv")
write.csv(training2 ,file="training_2nd_approach.csv")

