#!/usr/bin/env Rscript
#1. apply regularised regression rather than straight classification
#get data
library(RMySQL)
mydb <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_processed')
spx <- dbReadTable(conn=mydb,name='spx')

training.split=2/3

#perform some variable selection
variableSelection <- function(dataset, target){
  
  
}


#multiple linear regression
#dependent variable is continuous, independent variables are continuous/discrete, regression line is linear.
#http://r-statistics.co/Linear-Regression.html
library(Matrix)
multipleLinearRegressionWithoutImputation <- function(dataset, target){
  #big problem here is that missing values are causing lots of test records to not be predictable
  #need imputation
  drops <- c("Ticker",
             "AZS.class", #one of these (the target) will be added back in
             "AZS", #one of these (the target) will be added back in
             "Tobins.Q", #one of these (the target) will be added back in
             "Tobins.Q.class", #one of these (the target) will be added back in
             "X..Indep.Dir.on.Comp.Cmte.1", #basically no variance
             "X..Indep.Dir.on.Aud.Cmte.1", #basically no variance
             "ROC", #too many missing values
             "Bd.Age.Limit" #too many missing values
  )
  drops <- drops[drops != target]#dont want to remove whatever is passed as the target
  data.reduced <- dataset[ , !(names(dataset) %in% drops)] #remove unwanted columns
  data.reduced <- data.reduced[complete.cases(data.reduced[ , target]),]# we only want records with a class indicator
  data.reduced[[target]] <- as.numeric(as.factor(data.reduced[[target]]))
  colnames(data.reduced)[colnames(data.reduced) == target] <- 'target'
  data.reduced <- data.frame(data.reduced)
  
  data.reduced$Feml.CEO.or.Equiv <- as.numeric(as.factor(data.reduced$Feml.CEO.or.Equiv))
  data.reduced$Prsdg.Dir <- as.numeric(as.factor(data.reduced$Prsdg.Dir))
  data.reduced$Clssfd.Bd.Sys <- as.numeric(as.factor(data.reduced$Clssfd.Bd.Sys))
  data.reduced$Indep.Lead.Dir <- as.numeric(as.factor(data.reduced$Indep.Lead.Dir))
  data.reduced$CEO.Duality <- as.numeric(as.factor(data.reduced$CEO.Duality))
  data.reduced$Indep.Chrprsn <- as.numeric(as.factor(data.reduced$Indep.Chrprsn))
  data.reduced$Frmr.CEO.or.its.Equiv.on.Bd <- as.numeric(as.factor(data.reduced$Frmr.CEO.or.its.Equiv.on.Bd))
  data.reduced <- data.reduced[!(is.na(data.reduced$target) | data.reduced$target==""), ]
  View(data.reduced)
  len <- length(data.reduced[,1])
  sub <- sample(1:len,len*training.split)
  data.reduced.train <-  data.frame(data.reduced[sub,])
  data.reduced.test <- data.frame(data.reduced[-sub,])
  
  model <- lm(target ~ ., data = data.reduced.train)
  summary(model)
  pred <- predict(model, data.reduced.test)
  View(data.frame(pred))
  View(data.frame(data.reduced.test$target))
  pred.actuals <- data.frame(cbind(actuals=data.reduced.test$target, predicteds=pred))
  print(cor(pred.actuals))
  head(pred.actuals)

  min.max.accuracy <- mean(apply(pred.actuals, 1, min) / apply(pred.actuals, 1, max)) 
  print(min.max.accuracy)
  
}
multipleLinearRegressionWithoutImputation(spx,"Tobins.Q")

library(e1071)
library(mice)
linearRegressionWithImputationMice <- function(dataset, target) {
  imputation.maxit=15
  imputation.m=5
  
  #some data manipulations
  drops <- c("Ticker",
             "AZS.class", #one of these (the target) will be added back in
             "AZS", #one of these (the target) will be added back in
             "Tobins.Q", #one of these (the target) will be added back in
             "Tobins.Q.class", #one of these (the target) will be added back in
             "X..Indep.Dir.on.Comp.Cmte.1", #basically no variance
             "X..Indep.Dir.on.Aud.Cmte.1", #basically no variance
             "ROC", #too many missing values
             "Bd.Age.Limit", #too many missing values
             "Exec.Dir.Bd.Dur", #causing errors in pooling(?)
             "Unit.or.2.Tier.Bd.Sys", #causing errors in pooling(?)
             "Age.Old.Dir" #causing errors in pooling(?)
             )
  drops <- drops[drops != target]#dont want to remove whatever is passed as the target
  data.reduced <- dataset[ , !(names(dataset) %in% drops)] #remove unwanted columns
  data.reduced <- data.reduced[complete.cases(data.reduced[ , target]),]# we only want records with a class indicator
  data.reduced[[target]] <- as.numeric(as.factor(data.reduced[[target]]))
  colnames(data.reduced)[colnames(data.reduced) == target] <- 'target'
  data.reduced <- data.frame(data.reduced)
  
  data.reduced$Feml.CEO.or.Equiv <- as.numeric(as.factor(data.reduced$Feml.CEO.or.Equiv))
  data.reduced$Prsdg.Dir <- as.numeric(as.factor(data.reduced$Prsdg.Dir))
  data.reduced$Clssfd.Bd.Sys <- as.numeric(as.factor(data.reduced$Clssfd.Bd.Sys))
  data.reduced$Indep.Lead.Dir <- as.numeric(as.factor(data.reduced$Indep.Lead.Dir))
  data.reduced$CEO.Duality <- as.numeric(as.factor(data.reduced$CEO.Duality))
  data.reduced$Indep.Chrprsn <- as.numeric(as.factor(data.reduced$Indep.Chrprsn))
  data.reduced$Frmr.CEO.or.its.Equiv.on.Bd <- as.numeric(as.factor(data.reduced$Frmr.CEO.or.its.Equiv.on.Bd))
  
  #to check the balance of the target variable
  #plot(density(data.reduced$target), main="Density Plot", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(data.reduced$target), 2))) 
  #polygon(density(data.reduced$target), col="red")
  
  #split dataset into training and test
  #md.pattern(data.reduced)
  #library(VIM) #for visualizations of the magnitude of missing values
  len <- length(data.reduced[,1])
  sub <- sample(1:len,len*training.split)
  data.reduced.train <-  data.frame(data.reduced[sub,])
  data.reduced.test <- data.frame(data.reduced[-sub,])
  
  #impute the training set independantly
  #https://stats.stackexchange.com/questions/95083/imputation-before-or-after-splitting-into-train-and-test?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
  data.reduced.train.imputed <- mice(data = data.reduced.train, m = imputation.m, method = "pmm", maxit = imputation.maxit, seed = 500)
  data.reduced.train.imputed.stacked <- complete(data.reduced.train.imputed, 1)
  for(i in 2:data.reduced.train.imputed$m){
    data.reduced.train.imputed.stacked <- rbind(data.reduced.train.imputed.stacked, complete(data.reduced.train.imputed, i))
  }
  data.reduced.train.imputed.stacked.complete=data.reduced.train.imputed.stacked[complete.cases(data.reduced.train.imputed.stacked), ]
  #print(nrow(data.reduced.train.imputed.stacked)) #theres a mismatch here. mice is returning missing values, which indicates collinearity. might need variable selection 
  #print(nrow(data.reduced.train.imputed.stacked.complete))
  
  #build the model
  model <- lm(target ~ ., data = data.reduced.train.imputed.stacked.complete)
  #print(summary(model))
  #print(coefficients(model)) # model coefficients
  #print(confint(model, level=0.95)) # CIs for model parameters 
  #print(fitted(model)) # predicted values
  #print(residuals(model)) # residuals
  #print(anova(model)) # anova table 
  #print(vcov(model)) # covariance matrix for model parameters 
  #print(influence(model)) # regression diagnostics
  
  #now impute the test set
  data.reduced.test.imputed <- mice(data = data.reduced.test, m = imputation.m, method = "pmm", maxit = imputation.maxit, seed = 500)
  data.reduced.test.imputed.stacked <- complete(data.reduced.test.imputed, 1)
  for(i in 2:data.reduced.test.imputed$m){
    data.reduced.test.imputed.stacked <- rbind(data.reduced.test.imputed.stacked, complete(data.reduced.test.imputed, i))
  }
  data.reduced.test.imputed.stacked.complete=data.reduced.test.imputed.stacked[complete.cases(data.reduced.test.imputed.stacked), ]
  
  #predict using test set and model
  pred <- predict(model, data.reduced.test.imputed.stacked.complete)
  
  #analysis
  pred.actuals <- data.frame(cbind(actuals=data.reduced.test.imputed.stacked.complete$target, predicteds=pred))
  #print(cor(pred.actuals))
  #head(pred.actuals)
  #https://stats.stackexchange.com/questions/287143/meaning-of-min-max-accuracy-of-a-regression-model?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
  min.max.accuracy <- mean(apply(pred.actuals, 1, min) / apply(pred.actuals, 1, max)) 
  print(min.max.accuracy)

}  
linearRegressionWithImputationMice(spx,"AZS")#eg 0.6423513
linearRegressionWithImputationMice(spx,"Tobins.Q")#0.6761327




#regularised linear regression

#2. apply classification on regression
#3. results analysis and discussion
