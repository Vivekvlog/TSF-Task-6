##GRIP @ The Sparks Foundation
#Task 6
#Create the Decision Tree classi???er and visualize it graphically.  
#The purpose is if we feed any new data to this  classi???er, 
#it would be able to predict the right class accordingly. 


##Important packages
install.packages("dplyr")
install.packages("rpart")
install.packages("rpart.plot")

##Important librarys
library(dplyr)
library(rpart)
library(rpart.plot)
library(tidyverse)
library(caret)

##Import Data
library(readr)
Iris_1_ <- read_csv("C:/Users/VIVEK CHAUHAN/Downloads/Iris (1).csv")
View(Iris_1_)


##Looking for NA values
sum(is.na(Iris_1_))


##Checking for Outliers
boxplot(Iris_1_$SepalLengthCm,Iris_1_$SepalWidthCm,Iris_1_$PetalLengthCm,
        Iris_1_$PetalWidthCm)
#We found Outliers in SepalWidthCm

##Removing Outliers
summary(Iris_1_$SepalWidthCm)

#Inner Quartile Range of SepalLengthCm
IQR_SepalWidthCm=3.300-2.800

#Upper Limit for Outliers SepalWidthCm
Upfen_SepalWidthCm=3.300+1.5*IQR_SepalWidthCm
Upfen_SepalWidthCm

#Removing Outliers above Upper Limit and Checking by Ploting data
summary(Iris_1_)
Iris = subset(Iris_1_,SepalWidthCm<=4.05 , select = c("SepalLengthCm","SepalWidthCm",
                                                      "PetalLengthCm","PetalWidthCm",
                                                      "Species"))
boxplot(Iris$SepalLengthCm,Iris$SepalWidthCm,Iris$PetalLengthCm,Iris$PetalWidthCm)

#Lower Limit for Outliers from SepalWidthCm
Lofen_SepalWidthCm=2.800-1.5*IQR_SepalWidthCm
Lofen_SepalWidthCm

#Removing Outliers below Lower Limit and checking by Ploting data
Iris = subset(Iris,SepalWidthCm>=2.05 , select = c("SepalLengthCm","SepalWidthCm",
                                                   "PetalLengthCm","PetalWidthCm",
                                                   "Species"))
boxplot(Iris$SepalLengthCm,Iris$SepalWidthCm,Iris$PetalLengthCm,Iris$PetalWidthCm)

##Data Partition
set.seed(10000)
training.sample=Iris$Species%>%
  createDataPartition(p=0.7,list = FALSE)  

#train data
train.data=Iris[training.sample,]
view(train.data)

#test data
test.data=Iris[-training.sample,]
view(test.data)

## Creating Decision Tree model by name Tree
Tree=rpart(Species~.,train.data,method="class");Tree

#Checking for Model
rpart.plot(Tree)

##Predicting Model on test.data
Prediction=predict(Tree,test.data[-5],type = "class")
Prediction

##Evaluating Model Accuracy
tab=table(Prediction,test.data$Species)
tab
Accuracy=(sum(diag(tab))/sum(tab))*100
Accuracy
