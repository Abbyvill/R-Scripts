install.packages(readr)
library(readr)
IrisDataset <- read.csv("C:/Users/PLZ1669/Desktop/Data Analytics Certificate Program/Course Three/Task 1/R Tutorial Data Sets/iris.csv")
attributes(IrisDataset)
summary(IrisDataset) 
str(IrisDataset)
names(IrisDataset)
hist(IrisDataset$Sepal.Length)
plot(IrisDataset$Sepal.Length)
qqnorm(IrisDataset$Sepal.Length)
IrisDataset$Species<- as.numeric(IrisDataset$Species) 
set.seed(123)
training_indices <-sample(seq_len(nrow(IrisDataset)),size =trainSize)
trainSize <- round(nrow(IrisDataset) * 0.7)
testSize <- nrow(IrisDataset)-trainSize
trainSize
testSize
trainSet <- IrisDataset[training_indices,]
testSet <- IrisDataset[-training_indices,]
LinearModel<- lm(Petal.Width ~ Petal.Length, trainSet)
summary(LinearModel)
prediction<-predict(LinearModel,testSet)
prediction
