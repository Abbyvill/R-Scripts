#Loading and Preprocessing the data

library(readr)

SRC <- read.csv("C:/Users/PLZ1669/Desktop/Data Analytics Certificate Program/Course Three/Task 2/CompleteResponses.csv") 

SRC$elevel<-as.factor(SRC$elevel)

SRC$car<-as.factor(SRC$car)

SRC$brand<-as.factor(SRC$brand)

SRC$zipcode<-as.factor(SRC$zipcode)

str(SRC)


#Descriptive Analysis
##Required Libraries
library(plyr)
library(ggplot2)
library(readr)

SRC$brand=ifelse(SRC$brand=="1","sony","acer")
SRC$brand<-as.factor(SRC$brand)
ggplot(data = SRC ,mapping = aes(x = brand,fill=brand))+geom_bar()+
  geom_text(stat="count",aes(label=..count..,y=..count..), vjust=10)

#Need to view summary of the dataset
SRC$elevel<-as.factor(SRC$elevel)
SRC$car<-as.factor(SRC$car)
SRC$brand<-as.factor(SRC$brand)
SRC$zipcode<-as.factor(SRC$zipcode)
summary(SRC)

#Looking at histogram & bar charts of salary, age, elevel, car, zipcode, & credit
##SALARY
options(scipen = 999)
ggplot(data = SRC ,mapping = aes(x = salary,fill=brand))+geom_histogram(color="white")+
  ggtitle("Histogram of Salary")

##AGE
options(scipen = 999)
ggplot(data = SRC ,mapping = aes(x = age,fill=brand))+geom_histogram(color="white")+
  ggtitle("Histogram of Age")

##ELEVEL (Education level)
options(scipen = 999)
SRC$elevel<-as.numeric(SRC$elevel)
ggplot(data = SRC ,mapping = aes(x = elevel,fill=brand))+geom_bar()+ggtitle("Histogram of Elevel")

##CAR
options(scipen = 999)
SRC$car<-as.numeric(SRC$car)
ggplot(data = SRC ,mapping = aes(x = car,fill=brand))+geom_histogram(color="white")+ggtitle("Histogram of Car")

##ZIPCODE
options(scipen = 999)
SRC$zipcode<-as.numeric(SRC$zipcode)
ggplot(data = SRC ,mapping = aes(x = zipcode,fill=brand))+geom_bar()+ggtitle("Histogram of Zipcode")

##CREDIT
options(scipen = 999)
ggplot(data = SRC ,mapping = aes(x = credit,fill=brand))+geom_histogram(color="white")+
  ggtitle("Histogram of Credit")

#Decisions of Variables & Models
##Need to create a data partition to get 2 datasets (1 for training & 1 for testing)
###(Note to self): Data partition was explained in the caret turtorial 
##Need to apply model to training set. After we train the data and apply to model, will need to apply the same model to test set. 
###MPer notes, after we apply training and test data to model, if we get good results from it, we can then apply the model with results to the incomplete data

#1. Create training and data sets

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

set.seed(123)

#Want to train 75% of dataset
Train <-createDataPartition(SRC$brand, p=0.75, list=FALSE)
train <-SRC[Train, ]
test <-SRC[-Train, ]


#Cross Validation method (Explained with Mike and POA)
#Cross validation will get 10 folds and will use repeated method for these folds to test the model and get the accuracy
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

##Modeling with C5.0
set.seed(123)
c50Fit1 <- train(brand~., data=train, method="C5.0", trControl = fitControl, tuneLength = 5)
c50Fit1 

##C5.0 
#7424 samples# 6 predictor #2 classes: 'acer', 'sony' 
##The final values used for the model were trials = 20, model = rules and winnow = TRUE.

##model  winnow  trials  Accuracy   Kappa  
## rules   TRUE   20      0.9172944  0.8239616


#Additional model info
C50Fit1$finalModel$tuneValue
C50Fit1$finalModeltree
summary(c50Fit1)

## (a)   (b)    <-classified as
# ----  ----
#2581   227    (a): class acer #236  4380    (b): class sony
#Attribute usage:
  
#100.00%	salary
#100.00%	age
#84.05%	credit
#63.08%	car


plot(c50Fit1)

#Variable Importance C5.0
varImp(c50Fit1)

#C5.0 variable importance

#Overall
#age      100.00
#salary   100.00
#credit    84.05
#car       63.08
#zipcode    0.00
#elevel     0.0


#Modeling with Random Forest- 
set.seed(123)
system.time({rfFit1 <- train(brand~., data=train, method="rf", trControl = fitControl, tuneLength = 5)})
#user  system elapsed 
#1097.20   43.97 1142.04 

rfFit1
#Random Forest 

#7424 samples
#6 predictor
#2 classes: 'acer', 'sony' 

#mtry  Accuracy   Kappa    
#2     0.9182922  0.8267162
#3     0.9182918  0.8265451
#4     0.9173895  0.8245261
#5     0.9159621  0.8214285
#6     0.9140760  0.8173740

##The final value used for the model was mtry = 2.
#mtry  Accuracy   Kappa    
#2     0.9182922  0.8267162


varImp(rfFit1)
#rf variable importance

#Overall
#salary  100.000
#age      49.745
#credit   12.946
#car       4.514
#zipcode   1.954
#elevel    0.000

set.seed(123)
system.time({rfFit2 <-train(brand~.,data=trian, method="rf", trControl = fitControl)})

rfFit2
#Random Forest 

#7424 samples
#6 predictor
#2 classes: 'acer', 'sony' 

#The final value used for the model was mtry = 2.
#mtry  Accuracy   Kappa    
#2     0.9189797  0.8281455


varImp(rfFit2)
#rf variable importance

#Overall
#salary  100.000
#age      46.603
#credit   12.955
#car       4.681
#zipcode   1.775
#elevel    0.000




#Compare Models
resample_results <-resamples(list(RandomForest = rfFit1, C5.0 = c50Fit1))
resample_results
resample_results$values
summary(resample_results)
bwplot(resample_results)
diff_results<- diff(resample_results)
summary(diff_results, metric = "accuracy")
compare_models(c50Fit1, rfFit1)



##Models: RandomForest, C5.0 
#Number of resamples: 100 

#Accuracy 
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
#RandomForest 0.8962264 0.9137466 0.9179004 0.9182922 0.9232840 0.9421265    0
#C5.0         0.8936743 0.9110512 0.9176788 0.9172944 0.9236205 0.9367429    0

#Kappa                  Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
#####RandomForest 0.7777769 0.8174928 0.8262228 0.8267162 0.8380172 0.8772075    0
#####C5.0         0.7679474 0.8096091 0.8233803 0.8239616 0.8385332 0.8665278    0

#Accuracy 
#RandomForest C5.0     
#RandomForest              0.0009978
#C5.0         0.1877                

#Kappa 
#RandomForest C5.0    
#RandomForest              0.002755
#C5.0         0.09245 

#One Sample t-test

#data:  x
#t = -1.3267, df = 99, p-value = 0.1877
#alternative hypothesis: true mean is not equal to 0
#95 percent confidence interval:
#  -0.0024902222  0.0004945581
#sample estimates:
#  mean of x 
#-0.000997832 

#PREDICTING with Random Forrest
testPredrf1 <- predict(rfFit1, test)
postResample(testPredrf1, test$brand)
### Accuracy     Kappa 
###0.9244139 0.8403484
confusionMatrix(testPredrf1, test$brand)
###           Reference
###Prediction acer sony
###acer       858  109
###sony       78 1429

#               Accuracy : 0.9244              
#95% CI : (0.9133, 0.9345)    
#No Information Rate : 0.6217              
#P-Value [Acc > NIR] : < 0.0000000000000002

#Kappa : 0.8403              

#Mcnemar's Test P-Value : 0.02825             
                                              
#            Sensitivity : 0.9167              
#           Specificity : 0.9291              
#        Pos Pred Value : 0.8873              
#         Neg Pred Value : 0.9482              
#             Prevalence : 0.3783              
#         Detection Rate : 0.3468              
#   Detection Prevalence : 0.3909              
#      Balanced Accuracy : 0.9229              
                                              
#       'Positive' Class : acer 

#FINAL PREDICTION with Survey Incomplete data
finalPredrf1 <- predict(rfFit1, SurveyIncomplete)

#Need to get predicted totals
summary(finalPredrf1)
###acer sony 
###1906 3094

#NOW we need to get known preference totals:
summary(SRC$brand)
###acer sony 
###3744 6154

#GROUND TRUTH
postResample(finalPredrf1, SurveyIncomplete$brand)

summary(SurveyIncomplete$brand)

##Check totals results from incompleted and completed equal to 15,000
###Predictions from incompleted survey data:
1906+3094

###Known totals from Completed survey results:
3744+6154

###Total 5000 + 9898 to get total survey results
5000+9898

#Total is 14,898 which is really close to the 15,000 Mike asked us to demonstrate