#!/usr/bin/env Rscript
#get data
library(RMySQL)
mydb <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov')
iris<-dbReadTable(conn=mydb,name='Iris')
iris$Species<-as.factor(iris$Species) #target must be a factor
sapply(iris, class)

#adaboost m1
library(adabag)
iris.adaboost <- boosting(Species~., data=iris, boos=TRUE,
                          mfinal=3)
importanceplot(iris.adaboost)

sub <- c(sample(1:50, 35), sample(51:100, 35), sample(101:150, 35))
#get 105 numbers spaced out between 1 and 150

iris.bagging <- bagging(Species~., data=iris[sub,], mfinal=3)
#(iris[sub,]) returns the records from iris for the correspondings rows (that are nums in sub)
#mfinal is the num trees

#Predicting with labeled data
iris.predbagging<-predict.bagging(iris.bagging, newdata=iris[-sub,])#use the rows that arent in sub
iris.predbagging

#Predicting with unlabeled data
iris.predbagging<- predict.bagging(iris.bagging, newdata=iris[-sub,-5])#use the rows that arent in sub, and take off the label
iris.predbagging


####################################################
#autoprune / rpart
## rpart library should be loaded
library(rpart)
data(iris)
iris.prune<-autoprune(Species~., data=iris)
iris.prune

## Comparing the test error of rpart and autoprune
library(mlbench)
data(BreastCancer)
l <- length(BreastCancer[,1])
sub <- sample(1:l,2*l/3)
BC.rpart <- rpart(Class~.,data=BreastCancer[sub,-1],cp=-1, maxdepth=5)
BC.rpart.pred <- predict(BC.rpart,newdata=BreastCancer[-sub,-1],type="class")


