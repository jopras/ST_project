#Modelling

#loading the data

data=read.csv("training_1stapproach.csv", stringsAsFactors = F)

#Keeping only 8 important predictors, occured from the feature selection
final_data=data[,-1]
final_data=final_data[,-2:-10]
final_data=final_data[,-3:-13]
final_data=final_data[,-4:-19]
final_data=final_data[,-5]
final_data=final_data[,-7:-12]
final_data=final_data[,-8:-31]
final_data=final_data[,-9:-12]
final_data=final_data[,-10]


library(caret)#loading caret package

stop_time<-final_data$STOPPING_TIME#creating a variable with the actual values
final_data=final_data[,-9]#removing target variable from the data

set.seed(825)
#Cross-validation, 10-folds and 5-repeats
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated five times
  repeats =5)

#training all the different models
#SVR
svmfit<- train(final_data,stop_time,trControl=fitControl, method ="svmRadial",verbose = FALSE)

#Random Forest
rffit<- train(final_data,stop_time,trControl=fitControl, method = "rf",ntree=100, verbose = FALSE)

#Gradient Boosting
gbmfit<- train(final_data,stop_time,trControl=fitControl,method = "gbm", verbose = FALSE)

#Ridge Regression
ridgefit<- train(final_data,stop_time,trControl =fitControl, method = "ridge", verbose = FALSE)

#Lasso Regression
lassofit<- train(final_data,stop_time,trControl =fitControl,method = "glmnet")

#Multivariate Linear Regression
lmfit<- train(final_data,stop_time,trControl = fitControl, method = "lm",verbose = FALSE)

#Combining all the results into one list
allResamples <- resamples(list(svmRadial=svmfit,rf=rffit,gbm=gbmfit,lm=lmfit,glmnet=lassofit,ridge=ridgefit))


#calculating the predictions
final_data$gbm_prediction <- predict(gbmfit ,newdata=final_data)
final_data$rf_prediction <- predict(rffit ,newdata=final_data)
final_data$svm_prediction <- predict(svmfit ,newdata=final_data)
final_data$lasso_prediction <- predict(lassofit ,newdata=final_data)
final_data$ridge_prediction <- predict(ridgefit ,newdata=final_data)
final_data$linear_prediction <- predict(lmfit ,newdata=final_data)

#Calculating the prediction  per model
y=stop_time#actual TAD
x1=final_data$gbm_prediction# gbm prediction
x2=final_data$rf_prediction#rf prediction
x3=final_data$svm_prediction#svm prediction
x4=final_data$lasso_prediction#lasso prediction
x5=final_data$ridge_prediction#ridge prediction
x6=final_data$linear_prediction#linear prediction

#Calculating the error per model
final_data$gbm_error=y-x1
final_data$rf_error=y-x2
final_data$svm_error=y-x3
final_data$lasso_error=y-x4
final_data$ridge_error=y-x5
final_data$linear_error=y-x6

#Calculating the absolute error per model

final_data$abs_gbm_error=abs(final_data$gbm_error)
final_data$abs_rf_error=abs(final_data$rf_error)
final_data$abs_svm_error=abs(final_data$svm_error)
final_data$abs_lasso_error=abs(final_data$lasso_error)
final_data$abs_ridge_error=abs(final_data$ridge_error)
final_data$abs_linear_error=abs(final_data$linear_error)

#checking the % of errors in 3min interval
library(sqldf)
Hit_gbm=sqldf("select * from final_data where abs_gbm_error>180")
score_gbm=dim(Hit_gbm)/dim(final_data)

Hit_rf=sqldf("select * from final_data where abs_rf_error<=180")
score_rf=dim(Hit_rf)/dim(final_data)

Hit_svm=sqldf("select * from final_data where abs_svm_error>180")
score_svm=dim(Hit_svm)/dim(final_data)

Hit_lasso=sqldf("select * from final_data where abs_lasso_error>180")
score_lasso=dim(Hit_lasso)/dim(final_data)

Hit_ridge=sqldf("select * from final_data where abs_ridge_error>180")
score_ridge=dim(Hit_ridge)/dim(final_data)

Hit_linear=sqldf("select * from final_data where abs_linear_error>180")
score_linear=dim(Hit_linear)/dim(final_data)

#Ploting the RMSE & R squared accross the models 
parallelplot(allResamples)
parallelplot(allResamples , metric = "Rsquared")


#printing each model's results
print(svmfit)
print(gbmfit)
print(rffit)
print(lmfit)
print(lassofit)
print(ridgefit)
summary(allResamples)

#Creating several plots

plot(lassofit, main="Lasso")
plot(ridgefit,main="Ridge")
print(rffit)
ggplot(svmfit)

trellis.par.set(caretTheme())
print(gbmfit)

trellis.par.set(caretTheme())
plot(gbmfit, metric = "RMSE")


trellis.par.set(caretTheme())
ggplot(rffit, metric = "RMSE", plotType = "level",
       scales = list(x = list(rot = 90)))

#saving the results
write.csv(final_data,"model_crosscomparison.csv")

# Testing the second approach on the Random Forest Model.
#loading the data to test the second approach

data_2=read.csv("training_2ndapproach.csv", stringsAsFactors = F)
test_2=read.csv("test_2ndapproach.csv", stringsAsFactors = F)

#keeping only the important features
final_data2=data_2[,-1:-2]
final_data2=final_data2[,-2:-10]
final_data2=final_data2[,-3:-13]
final_data2=final_data2[,-4:-19]
final_data2=final_data2[,-5]
final_data2=final_data2[,-7:-19]
final_data2=final_data2[,-8:-43]
final_data2=final_data2[,-9:-12]
final_data2=final_data2[,-10]


library(caret)#loading caret package

stop_time2<-final_data2$STOPPING_TIME#creating a variable with the actual values
final_data2=final_data2[,-9]#removing target variable from the data

#Cross-validation, 10-folds and 5-repeats
set.seed(825)
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated five times
  repeats =5)

#training the random forest model with the 2nd approach data
rffit_2<- train(final_data2,stop_time2,trControl=fitControl, method = "rf",ntree=100, verbose = FALSE)

#Getting the predictions from the model
final_data2$rf_prediction <- predict(rffit_2 ,newdata=final_data2)
y2=stop_time2#actual TAD
x2_2=final_data2$rf_prediction#rf prediction

#calculating the error between the actual values and the prediction
final_data2$rf_error=y2-x2_2
#calculating the absolute error between the actual values and the prediction
final_data2$abs_rf_error=abs(final_data2$rf_error)

#getting random forest's results
print(rffit_2)
summary(rffit_2)

#Calculating the hit score for the 2nd approach for the 3-minute interval
Hit_rf_2=sqldf("select * from final_data2 where abs_rf_error<=180")
score_rf_2=dim(Hit_rf_2)/dim(final_data2)

#Saving the data
write.csv(final_data2,"predictions_approach2.csv")


#Final Model's Tuning and Calibration
#Grid Search

#using grid search to tune the model's performance
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")#10-fold crossvalidation & 3repeats
set.seed(344)
tunegrid <- expand.grid(.mtry=c(1:8))

rf_gridsearch <- train(final_data2,stop_time2, method="rf", metric="RMSE", tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)

# Trying a second approach to tune the model, called Algorithm Tune (tuneRF)
library(randomForest)
set.seed(324)
bestmtry <- tuneRF(final_data2,stop_time2, stepFactor=1.5, improve=1e-5, ntree=500)
print(bestmtry)
summary(bestmtry)
#both tuning models showed that mtry equal to 2 is the best model


#Based on the best parameters occured from Grid Search, we apply them to the best model
best_rf<-randomForest(final_data2,stop_time2,ntree=100,mtry=2,trControl=control,metric="RMSE")
#Then, print its performance
print(best_rf)


#Testing the model in the Test set (30% of the data)
test_data2=test_2[,-1:-2]
test_data2=test_data2[,-2:-10]
test_data2=test_data2[,-3:-13]
test_data2=test_data2[,-4:-19]
test_data2=test_data2[,-5]
test_data2=test_data2[,-7:-19]
test_data2=test_data2[,-8:-43]
test_data2=test_data2[,-9:-11]
TAD_test=test_data2$timeAtDoor
stop_test=test_data2$STOPPING_TIME
test_data2=test_data2[,-9:-10]


library(caret)

test_data2$predictions=predict(best_rf, newdata =test_data2 )
test_data2$stopping_time=stop_test#creating a new variable for stopping time
test_data2$TAD=TAD_test#creating a new variable for timeAtDoor

# Finding the error of predictions
test_data2$error=stop_test-test_data2$predictions

#Finding the Absolute Error of predictions

test_data2$abs_error=abs(test_data2$error)

#Finding the Error of TAD(current model company uses)
test_data2$error_TAD=stop_test-test_data2$TAD

#Absolute Error of TAD
test_data2$abs_error_TAD=abs(test_data2$error_TAD)

#Calculating the score for our model and the current one
Hit_Ratio_RF_tuned=sqldf("select * from test_data2 where abs_error<=180")
score_RF_test=dim(Hit_Ratio_RF_tuned)/dim(test_data2)
score_RF_test

Hit_Ratio_TDA=sqldf("select * from test_data2 where abs_error_TAD<=180")
score_TAD_test=dim(Hit_Ratio_TDA)/dim(test_data2)
score_TAD_test


#saving the results
write.csv(test_data2,"rf_testing.csv")

