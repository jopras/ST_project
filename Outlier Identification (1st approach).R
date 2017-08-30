#"Removing the outliers, Approach 1"

#loading the data

features=read.csv("Final_Features.csv", stringsAsFactors = F)

#removing timeAtdoor from our  features
TAD=features$timeAtDoor
model_features=features

#finding the outliers by using findOutlier function
library(data.table)
library(dtables)
findOutlier <- function(data, cutoff = 3) {
  ## Calculate the sd
  sds <- apply(data, 2, sd, na.rm = TRUE)
  ## Identify the cells with value greater than cutoff * sd (column wise)
  result <- mapply(function(d, s) {
    which(d > cutoff * s)
  }, data, sds)
  result
}
#deploying the function to our features
outliers <- findOutlier(model_features)
#getting the outliers
print(outliers)


#removing the outliers by applying function removeOutlier
removeOutlier <- function(data, outliers) {
  result <- mapply(function(d, o) {
    res <- d
    res[o] <-"NA"
    return(res)
  }, data, outliers)
  return(as.data.frame(result))
}

library(car)
library(dplyr)
library(outliers)
#removing the identifies outliers
featuresFilt<- removeOutlier(model_features, outliers)
featuresFilt$timeAtDoor=TAD
#drop all outliers from our target variable STOPPING TIME

featuresFilt <- subset(featuresFilt, STOPPING_TIME != "NA")

#checking the structure of the data
str(featuresFilt)
summary(featuresFilt)
```
```{r} 
#convert all the numeric data into num
convert<-c(3:6,11:16)
featuresFilt[,convert]<-data.frame(apply(featuresFilt[convert], 2, as.numeric))


summary(featuresFilt)
```
```{r}

#Ploting the missing data before the imputation
library(mice)
md.pattern(featuresFilt)


library(datasets)
library(VIM)
aggr_plot <- aggr(featuresFilt, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(featuresFilt), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
```

```{r}
#Another plot,showing two variables at the same time
marginplot(featuresFilt[c(1,2)])

```
```{r}
#creating new dataset with the random imputed data
data=featuresFilt
#Imputing the missing data
tempData <- mice(data,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)
```

```{r}
#Imputing the missing data
#The mice() function takes care of the imputing process


#Multiply imputed data set
#Call:
mice(data = data, m = 5, method = "pmm", maxit = 50, seed = 500)
tempData$meth#returns back the method that was used for the imputation,here we used the mean.

#Now we can get back the completed dataset using the complete() function
completedData <- mice::complete(tempData,1)


#Inspecting the distribution of original and imputed data
#Let’s compare the distributions of original and imputed data using a some useful plots.
xyplot(tempData,order_value ~ Min_Dist+order_value,pch=18,cex=1)

#What we would like to see is that the shape of the magenta points (imputed) matches the shape of the blue ones (observed). The matching shape tells us that the imputed values are indeed “plausible values”.
```
```{r}
#The density of the imputed data for each imputed dataset is showed in magenta while the density of the observed data is showed in blue. Again, under our previous assumptions we expect the distributions to be similar.
densityplot(tempData)
```

```{r}
#Another useful visual take on the distributions can be obtained using the stripplot() function that shows the distributions of the variables as individual points
stripplot(tempData, pch = 20, cex = 1.2)

```

```{r}
#check the pattern of the data now that they are imputed
library(VIM)
aggr_plot <- aggr(completedData, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(completedData), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
```

```{r}

#converting to the suitable for each variable type
completedData$drop_sequence=as.character(completedData$drop_sequence)
completedData$cluster=as.character(completedData$cluster)
completedData$individual_pieces_sum=as.numeric(completedData$individual_pieces_sum)
completedData$IDLING_TIME=as.numeric(completedData$IDLING_TIME)
completedData$timeAtDoor=as.numeric(completedData$timeAtDoor)
completedData$STOPPING_TIME=as.numeric(completedData$STOPPING_TIME)


library(dummies)

#transform the categorical variables into dummies
completed_dummies=dummy.data.frame(completedData,sep = ".")
names(completed_dummies)

#saving the dataset for the 1st approach outlier identification
write.csv(completed_dummies,"Approach_1.csv")
