##Required

library(readr)


##Load Data
existing_products <- read.csv("C:/Users/PLZ1669/Desktop/Data Analytics Certificate Program/Course Three/Task 3/existingproductattributes2017.csv", header= TRUE)
new_products <- read.csv("C:/Users/PLZ1669/Desktop/Data Analytics Certificate Program/Course Three/Task 3/newproductattributes2017.csv", header= TRUE)



#Required Libraries
library(caret)
library(lattice)
library(ggplot2)
library(lazyeval)
library(party)
library(plyr)
library(ggplot2)
library(labeling)
library(stats)
library(RColorBrewer)
library(e1071)
library(corrplot)
library(kernlab)

# dummify the data

newDataFrame <- dummyVars(" ~ .", data = existing_products)
readyData <- data.frame(predict(newDataFrame, newdata = existing_products))

#Correlation
##checking datatypes
str(readyData)

##Checking for Missing Data (NA)
summary(readyData)
### NA's   :15     

##Deleting attributes that have missing info (NA's)
readyData$BestSellersRank <- NULL

##Double check all NA's were deleted before doing correlation
summary(readyData)

#Correlation
##Correlation Matrix
CorrData <- cor(readyData)
CorrData

#Visualize correlation matrix - heatmap - need to install corrplot
corrplot(CorrData)

set.seed(123)

#Want to train 80% of dataset
Train <-createDataPartition(readyData$Volume, p=0.80, list=FALSE)
train <-readyData[Train, ]
test <-readyData[-Train, ]


#Cross Validation method 
#Cross validation will get 3 folds and will use repeated method for these folds to test the model and get the accuracy
fitControl <- trainControl(method = "repeatedcv", number = 3, repeats = 1)

##Modeling with Linear Model
set.seed(123)
system.time({lmFit1 <- train(Volume~., data=train, method="lm", trControl = fitControl)})
lmFit1
#Linear Regression 
#65 samples
#27 predictors

#No pre-processing
#Resampling: Cross-Validated (3 fold, repeated 1 times) 
#Summary of sample sizes: 44, 44, 42 
#Resampling results:
  
#  RMSE          Rsquared  MAE         
#  4.169575e-12  1         1.204374e-12

#Tuning parameter 'intercept' was held constant at a value of TRUE


#Modeling with Random Forest- 
fitControl <- trainControl(method = "repeatedcv", number = 4, repeats = 1)  
set.seed(123)
system.time({rfFit1 <- train(Volume~., data=train, method="rf", trControl = fitControl)})
rfFit1
#Random Forest 

#65 samples
#27 predictors

#No pre-processing
#Resampling: Cross-Validated (4 fold, repeated 1 times) 
#Summary of sample sizes: 49, 48, 49, 49 
#Resampling results across tuning parameters:
  
#mtry  RMSE      Rsquared   MAE     
#2    1084.328  0.7623676  442.6711
#14    1025.830  0.7567121  329.3748
#27    1036.679  0.7263670  329.5608

#RMSE was used to select the optimal model using the smallest value.
#he final value used for the model was mtry = 14.



#Modeling with Support Vector Machine- 
set.seed(123)
system.time({svmFit1 <- train(Volume~., data=train, method="svmLinear", trControl = fitControl)})
svmFit1
#Support Vector Machines with Linear Kernel 

#65 samples
#27 predictors

#No pre-processing
#Resampling: Cross-Validated (4 fold, repeated 1 times) 
#Summary of sample sizes: 49, 48, 49, 49 
#Resampling results:
  
#  RMSE      Rsquared   MAE     
# 541.0467  0.913079  235.0608

#Tuning parameter 'C' was held constant at a value of 1


#Modeling with Gradient Boosting- 
set.seed(123)
system.time({gbmFit1 <- train(Volume~., data=train, method="gbm", trControl = fitControl)})
gbmFit1
#Stochastic Gradient Boosting 

#65 samples
#27 predictors

#No pre-processing
#Resampling: Cross-Validated (4 fold, repeated 1 times) 
#Summary of sample sizes: 49, 48, 49, 49 
#Resampling results across tuning parameters:
  
##  interaction.depth  n.trees  RMSE       Rsquared   MAE     
#1                   50      1292.640  0.5053875  605.8129
#1                  100      1330.033  0.4445611  672.4575
#1                  150      1420.380  0.3929756  775.6567
#2                   50      1247.651  0.5058756  605.3469
#2                  100      1339.589  0.4474425  687.8429
#2                  150      1384.178  0.4009958  736.4200
#3                   50      1239.188  0.5523032  562.2173
#3                  100      1357.209  0.4854769  710.8337
#3                  150      1405.661  0.4496641  773.9827

#Tuning parameter 'shrinkage' was held constant at a value of 0.1
#Tuning parameter 'n.minobsinnode' was held constant at a value of 10
#RMSE was used to select the optimal model using the smallest value.
#The final values used for the model were n.trees = 50, interaction.depth = 3, shrinkage = 0.1 and n.minobsinnode = 10.



##Remove attributes that are not relevant or has a negative relationship/correlation with volume
readyData$ProductNum <- NULL
readyData$Price <- NULL
readyData$x5StarReviews <- NULL
readyData$x3StarReviews <- NULL
readyData$x1StarReviews <- NULL
readyData$NegativeServiceReview <- NULL
readyData$Recommendproduct <- NULL
readyData$ShippingWeight <- NULL
readyData$ProductDepth <- NULL
readyData$ProductWidth <- NULL
readyData$ProductHeight <- NULL
readyData$ProfitMargin <- NULL


##Double check all attributes were deleted
summary(readyData)

#Correlation
##Correlation Matrix
CorrData <- cor(readyData)
CorrData

#Visualize correlation matrix - heatmap - need to install corrplot
corrplot(CorrData)


set.seed(123)

#Want to train 80% of dataset
Train <-createDataPartition(readyData$Volume, p=0.80, list=FALSE)
train <-readyData[Train, ]
test <-readyData[-Train, ]


#Cross Validation method 
#Cross validation will get 3 folds and will use repeated method for these folds to test the model and get the accuracy
fitControl <- trainControl(method = "repeatedcv", number = 3, repeats = 1)



##Modeling with Linear Model
set.seed(123)
system.time({lmFit1 <- train(Volume~., data=train, method="lm", trControl = fitControl)})
lmFit1
#Linear Regression 
#65 samples
#15 predictors

#No pre-processing
#Resampling: Cross-Validated (4 fold, repeated 1 times) 
#Summary of sample sizes: 49, 48, 49, 49 
#Resampling results:
  
#  RMSE      Rsquared   MAE     
#623.8203  0.7421427  327.5358

#Tuning parameter 'intercept' was held constant at a value of TRUE



#Modeling with Random Forest- 
set.seed(123)
system.time({rfFit1 <- train(Volume~., data=train, method="rf", trControl = fitControl)})
rfFit1
#Random Forest 
#65 samples
#15 predictors

#No pre-processing
#Resampling: Cross-Validated (4 fold, repeated 1 times) 
#Summary of sample sizes: 49, 48, 49, 49 
#Resampling results across tuning parameters:

#  mtry  RMSE      Rsquared   MAE     
#2    840.3484  0.6873022  382.8702
#8    781.9916  0.7653431  250.6267
#15    770.5791  0.7805507  248.3905

#RMSE was used to select the optimal model using the smallest value.
#The final value used for the model was mtry = 15.



#Modeling with Support Vector Machine- 
set.seed(123)
system.time({svmFit1 <- train(Volume~., data=train, method="svmLinear", trControl = fitControl)})
svmFit1
#Support Vector Machines with Linear Kernel 
#65 samples
#15 predictors

#No pre-processing
#Resampling: Cross-Validated (4 fold, repeated 1 times) 
#Summary of sample sizes: 49, 48, 49, 49
#Resampling results:
  
#  RMSE      Rsquared   MAE     
#729.3469  0.7001609  292.9698

#Tuning parameter 'C' was held constant at a value of 1



#Modeling with Gradient Boosting- 
set.seed(123)
system.time({gbmFit1 <- train(Volume~., data=train, method="gbm", trControl = fitControl)})
gbmFit1
#Stochastic Gradient Boosting 
#65 samples
#15 predictors

#No pre-processing
#Resampling: Cross-Validated (4 fold, repeated 1 times) 
#Summary of sample sizes: 49, 48, 49, 49 
#Resampling results across tuning parameters:
  
#  interaction.depth  n.trees  RMSE      Rsquared   MAE     
#1                   50       945.0677  0.6892294  421.5539
#1                  100       917.4631  0.6972277  407.5320
#1                  150      1000.2939  0.6978205  472.9382
#2                   50       878.8640  0.7032211  385.8516
#2                  100       872.0849  0.7061734  370.5734
#2                  150       915.3781  0.6885208  408.4228
#3                   50       969.7459  0.6667485  426.8003
#3                  100       967.0362  0.6676212  442.2828
#3                  150       933.5746  0.6501936  418.3811

#Tuning parameter 'shrinkage' was held constant at a value of 0.1
#Tuning parameter 'n.minobsinnode' was held constant at a value of 10
#RMSE was used to select the optimal model using the smallest value.
#The final values used for the model were n.trees = 100, interaction.depth = 2, shrinkage = 0.1 and n.minobsinnode = 10.


#Compare Models
resample_results <-resamples(list(LinearRegression = lmFit1, RandomForest = rfFit1, SupportVectorMachineswithLinearKernel = svmFit1, StochasticGradientBoosting = gbmFit1))
resample_results
resample_results$Volume
summary(resample_results)
bwplot(resample_results)
diff_results<- diff(resample_results)
summary(diff_results, metric = "Rsquared")
compare_models(lmFit1, rfFit1, svmFit1, gbmFit1)


#PREDICTING with Linear Regression
testPredrf1 <- predict(lmFit1, test)
postResample(testPredrf1, test$Volume)
###RMSE    Rsquared         MAE 
###1108.019371    0.596162  582.389352 


#PREDICTING with Random Forest
testPredrf2 <- predict(rfFit1, test)
postResample(testPredrf2, test$Volume)
###RMSE     Rsquared          MAE 
###1389.7272795    0.5086586  456.6134044 


#PREDICTING with Support Vector Machine with Linear Kernal
testPredrf3 <- predict(svmFit1, test)
postResample(testPredrf3, test$Volume)
###RMSE    Rsquared         MAE 
###978.7712072   0.7218509 483.5072420


#PREDICTING with GBM 
testPredrf4 <- predict(gbmFit1, test)
postResample(testPredrf4, test$Volume)
###RMSE    Rsquared         MAE 
###1450.9286345    0.3031727  734.8636601 





# dummify the "new product" data

newDataFrame2 <- dummyVars(" ~ .", data = new_products)
readyData2 <- data.frame(predict(newDataFrame2, newdata = new_products))

#Correlation
##checking datatypes
str(readyData2)

##Checking for Missing Data (NA)
summary(readyData2)
### 0 NA's    


##Remove attributes that are not relevant or has a negative relationship/correlation with volume
readyData2$BestSellersRank <- NULL
readyData2$ProductNum <- NULL
readyData2$Price <- NULL
readyData2$x5StarReviews <- NULL
readyData2$x3StarReviews <- NULL
readyData2$x1StarReviews <- NULL
readyData2$NegativeServiceReview <- NULL
readyData2$Recommendproduct <- NULL
readyData2$ShippingWeight <- NULL
readyData2$ProductDepth <- NULL
readyData2$ProductWidth <- NULL
readyData2$ProductHeight <- NULL
readyData2$ProfitMargin <- NULL


##Double check data
summary(readyData2)

#Correlation
##Correlation Matrix
CorrData <- cor(readyData2)
CorrData

#Visualize correlation matrix - heatmap - need to install corrplot
corrplot(CorrData)






#FINAL PREDICTION with new products data
finalPred <- predict(svmFit1, readyData2)

#Need to get predicted totals
summary(finalPred)
###   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
###-145.965    8.769  109.824  552.591  299.696 4680.147 


#Add predictions to the new products data set
output <- readyData2
output$predictions <- finalPred



write.csv(output, file="C2.T3output.csv", row.names = TRUE)


