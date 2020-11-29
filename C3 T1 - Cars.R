##Required

library(readr)


##Load Data
cars <- read.csv("C:\Users\PLZ1669\Desktop\Data Analytics Certificate Program\Course Three\Task 1\R Tutorial Data Sets\cars.csv") 

##Explore Data
###Listing atrributes within dataset
attributes(cars)

###Printing min,max,mean,median,& quartiles of each attribute:
summary(cars)

###Distplays the structure of dataset:
str(cars)

###Names of attributes
names(cars)

###Pritning out the instances within the particular column in dataset
cars$`name of car`

##Columns must be in numeric form to perform plots:
###Histogram Plot:
hist(cars$`speed of car`)

###Scatter (Box) Plot
plot(cars$`speed of car`,cars$`distance of car`)

###Normal Quantile Plot Way to see if data is normally distributed)
qqnorm(cars$`speed of car`)
qqnorm(cars$`distance of car`)

##Convert data types.
cars$`speed of car` <-as.numeric(cars$`speed of car`)

##Renaming dataset/attributes
names(cars)<-c("name","speed","distance") 

##Checking for missing values (Will count the number of NAs)
summary(cars)

##Will show NAs through logical data (TRUE if missing, FALSE if not)
is.na(cars)

###Remove any observations containing missing data.
####(Note: If the missing data is less than 10% of the total data 
####and only after comparing the min/max of all the features both with and without the missing data.)

#Drops any rows with missing values and omits them forever
na.omit(cars)

#Drops any rows with missing values, but keeps tracks of where they are:
na.exclude(cars)

#Replace missing data now with the mean. 
###Common technique but use with care as it can skew the data
cars$distance[is.na(cars$distance)] <-mean(cars$distance,na.rm = TRUE)

#CREATING TESTING & TRAINING SETS
##Below creates sizes of each set but do not create the sets
set.seed(123)
trainSize <- round(nrow(cars)*0.7)
testSize <- nrow(cars)-trainSize
##Number of instances will be in each set
testSize
trainSize

##Create the training and test sets
set.seed(123)
training_indices <-sample(seq_len(nrow(cars)),size =trainSize)
trainSet <-cars[training_indices,]
testSet <-cars[-training_indices,] 

#MODELING & PREDICTIONS
##Linear Regression Modeling
###Wanting to predict "Distance" (Dependent Variable/Response/Y variable)
lmFit1<-lm(distance~ speed, trainSet)

##To see Key Metrics
summary(lmFit1)

#PREDICTIONS
##Predicting the cars distances through the speed of the cars:
testPredlm1 <- predict(lmFit1,testSet)
testPredlm1






