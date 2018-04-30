#!/usr/bin/env Rscript
#1. apply regularised regression rather than straight classification
#get data
library(RMySQL)
mydb <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_processed')
spx <- dbReadTable(conn=mydb,name='spx')


training.split=2/3
#multiple linear regression
#dependent variable is continuous, independent variables are continuous/discrete, regression line is linear.
#http://r-statistics.co/Linear-Regression.html
library(e1071)
linearRegression <- function(dataset, target) {
  drops <- c("Ticker",
             "AZS.class",
             "AZS",
             "Tobins.Q",
             "Tobins.Q.class")
  drops=drops[drops != target]#dont want to remove whatever is passed as the target
  data.reduced<-dataset[ , !(names(dataset) %in% drops)] #remove unwanted columns
  data.reduced<-data.reduced[complete.cases(data.reduced[ , target]),]# we only want records with a class indicator
  data.reduced[[target]]=as.numeric(as.factor(data.reduced[[target]]))
  colnames(data.reduced)[colnames(data.reduced) == target] <- 'target'
  data.reduced<-data.frame(data.reduced)
  
  #to check the balance of the target variable
  plot(density(data.reduced$target), main="Density Plot", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(data.reduced$target), 2))) 
  polygon(density(data.reduced$target), col="red")
  
  #build the model
  len <- length(data.reduced[,1])
  sub <- sample(1:len,len*training.split)
  linearModel <- lm(data.reduced$target ~ ., data=data.reduced[sub, ])
  #print(linearModel)
  
} 
linearRegression(spx,"Tobins.Q")



#2. apply classification on regression
#3. results analysis and discussion
