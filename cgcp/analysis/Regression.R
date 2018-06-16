#!/usr/bin/env Rscript
#1. apply regularised regression rather than straight classification
#get data
library(RMySQL)
library(glmnet)
mydb <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_processed')
mydb.imputed <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_imputed')
spx <- dbReadTable(conn=mydb.imputed,name='spx')
spx.mscore <- dbReadTable(conn=mydb.imputed,name='spx_mscore')

training.split=2/3
#multiple linear regression
#dependent variable is continuous, independent variables are continuous/discrete, regression line is linear.
#http://r-statistics.co/Linear-Regression.html
library(Matrix)

#this is likely a bit fucked due to missing values
linearRegression <- function(dataset, target){
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
  #data.reduced[[target]] <- as.numeric(as.factor(data.reduced[[target]]))
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
linearRegression(spx,"Tobins.Q")

#looks ok, but not regularised and prob not the best regression method for the data
linearRegressionWithImputationMice <- function(dataset, target) {
  library(e1071)
  library(mice)
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
  #data.reduced[[target]] <- as.numeric(as.factor(data.reduced[[target]]))
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
linearRegressionWithImputationMice(spx,"AZS")#eg 0.3097797
linearRegressionWithImputationMice(spx,"Tobins.Q")#eg 0.6644546

#regularised linear regression
#no imputation as of yet, so only operating on complete rows (very few)
regLinearRegression <- function(dataset, target){
  library(glmnet)
  #https://educationalresearchtechniques.com/2017/04/05/8601/
  #https://www.kaggle.com/jimthompson/regularized-linear-models-in-r
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
  #data.reduced[[target]] <- as.numeric(as.factor(data.reduced[[target]]))
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
  data.reduced.train <- data.frame(data.reduced[sub,])
  data.reduced.test <- data.frame(data.reduced[-sub,])
  data.reduced.train.complete=data.reduced.train[complete.cases(data.reduced.train), ]
  data.reduced.test.complete=data.reduced.test[complete.cases(data.reduced.test), ]
  
  #fit on the training set
  predictor.variables <- as.matrix(data.reduced.train.complete[ , !(names(data.reduced.train.complete) %in% "target")]) #remove unwanted columns
  target.measure <- as.matrix(data.reduced.train.complete$target)
  lasso <- cv.glmnet(
    predictor.variables,
    target.measure,
    family="gaussian",
    alpha=0 #this is elastic net
  )
  print(lasso)
  plot(lasso,xvar="lambda",label=T)
  plot(lasso,xvar='dev',label=T)
  
  #test
  data.reduced.test.complete.without.target <- as.matrix(data.reduced.test.complete[ , !(names(data.reduced.test.complete) %in% "target")]) #remove unwanted columns
  lasso.response <- predict(
    lasso,
    newx = data.reduced.test.complete.without.target,
    type = 'response',
    s=lasso$lambda.min
  )
  plot(lasso.response,data.reduced.test.complete$target)
  print(mean((lasso.response-data.reduced.test.complete$target)^2))
  
}
regLinearRegression(spx,"Tobins.Q")#eg 0.655467

#regularised linear regression
#no imputation as of yet, so only operating on complete rows (very few)
#shows method of varying alpha (ie moving from ridge to elnet to lasso)
regLinearRegressionMultiAlphaLamdba <- function(dataset, target){
  #from https://www4.stat.ncsu.edu/~post/josh/LASSO_Ridge_Elastic_Net_-_Examples.html
  set.seed(1)
  #----
  #depr drops 
  drops_depr <- c("Ticker",
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
             "Age.Old.Dir", #causing errors in pooling(?)
             "Indep.Dir.Bd.Mtg.Att..", #cant impute correctly
             "Board.Duration",
             "Sz.Aud.Cmte",
             "X..Empl.Reps.on.Bd",
             "Interest"
  )
  #----
  drops <- c("Ticker",
             "AZS.class", #one of these (the target) will be added back in
             "AZS", #one of these (the target) will be added back in
             "Tobins.Q", #one of these (the target) will be added back in
             "Tobins.Q.class", #one of these (the target) will be added back in
             #-----zero import
             "Bd.Age.Limit",
             "Board.Duration",
             "Exec.Dir.Bd.Dur",
             "Feml.CEO.or.Equiv",
             "Unit.or.2.Tier.Bd.Sys",
             "X..Empl.Reps.on.Bd",
             "X..Indep.Dir.on.Aud.Cmte",
             "X..Indep.Dir.on.Aud.Cmte.1",
             "X..Indep.Dir.on.Comp.Cmte.1",
             "X..Indep.Dir.on.Nom.Cmte",
             "X..Wmn.on.Bd",
             #------prevents imputing
             "Indep.Dir.Bd.Mtg.Att.."
  )
  drops <- drops[drops != target]#dont want to remove whatever is passed as the target
  data.reduced <- dataset[ , !(names(dataset) %in% drops)] #remove unwanted columns
  data.reduced <- data.reduced[complete.cases(data.reduced[ , target]),]# we only want records with a class indicator
  colnames(data.reduced)[colnames(data.reduced) == target] <- 'target'
  data.reduced <- data.frame(data.reduced)
  
  #data.reduced$Feml.CEO.or.Equiv <- as.numeric(as.factor(data.reduced$Feml.CEO.or.Equiv))
  data.reduced$Prsdg.Dir <- as.numeric(as.factor(data.reduced$Prsdg.Dir))
  data.reduced$Clssfd.Bd.Sys <- as.numeric(as.factor(data.reduced$Clssfd.Bd.Sys))
  data.reduced$Indep.Lead.Dir <- as.numeric(as.factor(data.reduced$Indep.Lead.Dir))
  data.reduced$CEO.Duality <- as.numeric(as.factor(data.reduced$CEO.Duality))
  data.reduced$Indep.Chrprsn <- as.numeric(as.factor(data.reduced$Indep.Chrprsn))
  data.reduced$Frmr.CEO.or.its.Equiv.on.Bd <- as.numeric(as.factor(data.reduced$Frmr.CEO.or.its.Equiv.on.Bd))
  print(summary(data.reduced))
  #split dataset into training and test
  #md.pattern(data.reduced)
  #library(VIM) #for visualizations of the magnitude of missing values
  len <- length(data.reduced[,1])
  sub <- sample(1:len,len*training.split)
  data.reduced.train <- data.frame(data.reduced[sub,])
  data.reduced.test <- data.frame(data.reduced[-sub,])
  
  data.reduced.train.complete=data.reduced.train[complete.cases(data.reduced.train), ]
  data.reduced.test.complete=data.reduced.test[complete.cases(data.reduced.test), ]
  print(dim(data.reduced.train.complete))
  print(dim(data.reduced.test.complete))
  predictor.variables.train <- as.matrix(data.reduced.train.complete[ , !(names(data.reduced.train.complete) %in% "target")]) #remove unwanted columns
  target.measure.train <- as.matrix(data.reduced.train.complete$target)
  predictor.variables.test <- as.matrix(data.reduced.test.complete[ , !(names(data.reduced.test.complete) %in% "target")]) #remove unwanted columns
  target.measure.test <- as.matrix(data.reduced.test.complete$target)
  
  #fit models
  fit.lasso <- glmnet(predictor.variables.train, target.measure.train, family="gaussian", alpha=1)
  fit.ridge <- glmnet(predictor.variables.train, target.measure.train, family="gaussian", alpha=0)
  fit.elnet <- glmnet(predictor.variables.train, target.measure.train, family="gaussian", alpha=.5)
  
  # 10-fold Cross validation for each alpha = 0, 0.1, ... , 0.9, 1.0
  for (i in 0:10) {
    assign(
      paste("fit", i, sep=""), 
      cv.glmnet(
        predictor.variables.train, 
        target.measure.train, 
        type.measure="mse", 
        alpha=i/10,
        family="gaussian"
      )
    )
  }
  
  # Plot solution paths:
  par(mfrow=c(3,2))
  plot(fit.ridge, xvar="lambda", main="fit.ridge")
  plot(fit0, main="Ridge")
  plot(fit.elnet, xvar="lambda",main="fit.elnet")
  plot(fit5, main="Elastic Net")
  plot(fit.lasso, xvar="lambda", main="fit.lasso")
  plot(fit10, main="LASSO")
  
  #musssssst be a better way of writing this code
  pred0.1se <- predict(fit0, s=fit0$lambda.1se, newx=predictor.variables.test)
  pred1.1se <- predict(fit1, s=fit1$lambda.1se, newx=predictor.variables.test)
  pred2.1se <- predict(fit2, s=fit2$lambda.1se, newx=predictor.variables.test)
  pred3.1se <- predict(fit3, s=fit3$lambda.1se, newx=predictor.variables.test)
  pred4.1se <- predict(fit4, s=fit4$lambda.1se, newx=predictor.variables.test)
  pred5.1se <- predict(fit5, s=fit5$lambda.1se, newx=predictor.variables.test)
  pred6.1se <- predict(fit6, s=fit6$lambda.1se, newx=predictor.variables.test)
  pred7.1se <- predict(fit7, s=fit7$lambda.1se, newx=predictor.variables.test)
  pred8.1se <- predict(fit8, s=fit8$lambda.1se, newx=predictor.variables.test)
  pred9.1se <- predict(fit9, s=fit9$lambda.1se, newx=predictor.variables.test)
  pred10.1se <- predict(fit10, s=fit10$lambda.1se, newx=predictor.variables.test)
  
  pred0.min <- predict(fit0, s=fit0$lambda.min, newx=predictor.variables.test)
  pred1.min <- predict(fit1, s=fit1$lambda.min, newx=predictor.variables.test)
  pred2.min <- predict(fit2, s=fit2$lambda.min, newx=predictor.variables.test)
  pred3.min <- predict(fit3, s=fit3$lambda.min, newx=predictor.variables.test)
  pred4.min <- predict(fit4, s=fit4$lambda.min, newx=predictor.variables.test)
  pred5.min <- predict(fit5, s=fit5$lambda.min, newx=predictor.variables.test)
  pred6.min <- predict(fit6, s=fit6$lambda.min, newx=predictor.variables.test)
  pred7.min <- predict(fit7, s=fit7$lambda.min, newx=predictor.variables.test)
  pred8.min <- predict(fit8, s=fit8$lambda.min, newx=predictor.variables.test)
  pred9.min <- predict(fit9, s=fit9$lambda.min, newx=predictor.variables.test)
  pred10.min <- predict(fit10, s=fit10$lambda.min, newx=predictor.variables.test)
  
  print("-----min")
  print (mean((target.measure.test - pred0.min)^2))
  print (mean((target.measure.test - pred1.min)^2))
  print (mean((target.measure.test - pred2.min)^2))
  print (mean((target.measure.test - pred3.min)^2))
  print (mean((target.measure.test - pred4.min)^2))
  print (mean((target.measure.test - pred5.min)^2))
  print (mean((target.measure.test - pred6.min)^2))
  print (mean((target.measure.test - pred7.min)^2))
  print (mean((target.measure.test - pred8.min)^2))
  print (mean((target.measure.test - pred9.min)^2))
  print (mean((target.measure.test - pred10.min)^2))
  print("-----1se")
  print (mean((target.measure.test - pred0.1se)^2))
  print (mean((target.measure.test - pred1.1se)^2))
  print (mean((target.measure.test - pred2.1se)^2))
  print (mean((target.measure.test - pred3.1se)^2))
  print (mean((target.measure.test - pred4.1se)^2))
  print (mean((target.measure.test - pred5.1se)^2))
  print (mean((target.measure.test - pred6.1se)^2))
  print (mean((target.measure.test - pred7.1se)^2))
  print (mean((target.measure.test - pred8.1se)^2))
  print (mean((target.measure.test - pred9.1se)^2))
  print (mean((target.measure.test - pred10.1se)^2))
  
  result=list(
    "fit.lasso"=fit.lasso,
    "fit.ridge"=fit.ridge,
    "fit.elnet"=fit.elnet,
    "fit0"=fit0,
    "fit1"=fit1,
    "fit2"=fit2,
    "fit3"=fit3,
    "fit4"=fit4,
    "fit5"=fit5,
    "fit6"=fit6,
    "fit7"=fit7,
    "fit8"=fit8,
    "fit9"=fit9,
    "fit10"=fit10,
    "data.reduced.train.complete"=data.reduced.train.complete,
    "data.reduced.test.complete"=data.reduced.test.complete
  )
  return(result)
  
}
tobin.q.results=regLinearRegressionMultiAlphaLamdba(spx,"Tobins.Q")
coef(tobin.q.results$fit3)
fit <- tobin.q.results$fit3
fit.r2 <- fit$glmnet.fit$dev.ratio[which(fit$glmnet.fit$lambda == fit$lambda.min)]
fit.r2 #0.896 [ish]
#training dim - 146  45
#test dim - 79 45


#regularised linear regression
#same as regLinearRegressionMultiAlphaLamdba but with imputation
regLinearRegressionMultiAlphaLamdbaImputedMice <- function(dataset, target){
  #from https://www4.stat.ncsu.edu/~post/josh/LASSO_Ridge_Elastic_Net_-_Examples.html
  set.seed(1)
  training.split=2/3
  #----
  #depr drops for now
  drops_depr <- c("Ticker",
             "AZS.class", #one of these (the target) will be added back in
             "AZS", #one of these (the target) will be added back in
             "Tobins.Q", #one of these (the target) will be added back in
             "Tobins.Q.class", #one of these (the target) will be added back in
             #--
             "X..Indep.Dir.on.Comp.Cmte.1", #basically no variance
             "X..Indep.Dir.on.Aud.Cmte.1", #basically no variance
             "ROC", #too many missing values
             "Bd.Age.Limit", #too many missing values
             "Exec.Dir.Bd.Dur", #causing errors in pooling(?)
             "Unit.or.2.Tier.Bd.Sys", #causing errors in pooling(?)
             "Age.Old.Dir", #causing errors in pooling(?)
             "Indep.Dir.Bd.Mtg.Att..", #cant impute correctly
             "Board.Duration",
             "Sz.Aud.Cmte",
             "X..Empl.Reps.on.Bd"
  )
  #----
  #new drops
  drops <- c("Ticker",
             "AZS.class", #one of these (the target) will be added back in
             "AZS", #one of these (the target) will be added back in
             "Tobins.Q", #one of these (the target) will be added back in
             "Tobins.Q.class", #one of these (the target) will be added back in
             "FiveVarEq", #one of these (the target) will be added back in
             "EightVarEq", #one of these (the target) will be added back in
             #-----zero import
             "Bd.Age.Limit",
             "Board.Duration",
             "Exec.Dir.Bd.Dur",
             "Feml.CEO.or.Equiv",
             "Unit.or.2.Tier.Bd.Sys",
             "X..Empl.Reps.on.Bd",
             "X..Indep.Dir.on.Aud.Cmte",
             "X..Indep.Dir.on.Aud.Cmte.1",
             "X..Indep.Dir.on.Comp.Cmte.1",
             "X..Indep.Dir.on.Nom.Cmte",
             "X..Wmn.on.Bd",
             #------prevents imputing
             "Indep.Dir.Bd.Mtg.Att..",
             #------do to with mscore
             "DSRI",
             "GMI",
             "AQI",
             "SGI",
             "DEPI",
             "SGAI",
             "TATA",
             "LVGI"
  )
  drops <- drops[drops != target]#dont want to remove whatever is passed as the target
  data.reduced <- dataset[ , !(names(dataset) %in% drops)] #remove unwanted columns
  data.reduced <- data.reduced[complete.cases(data.reduced[ , target]),]# we only want records with a class indicator
  colnames(data.reduced)[colnames(data.reduced) == target] <- 'target'
  data.reduced <- data.frame(data.reduced)
  
  #data.reduced$Feml.CEO.or.Equiv <- as.numeric(as.factor(data.reduced$Feml.CEO.or.Equiv))
  data.reduced$Prsdg.Dir <- as.numeric(as.factor(data.reduced$Prsdg.Dir))
  data.reduced$Clssfd.Bd.Sys <- as.numeric(as.factor(data.reduced$Clssfd.Bd.Sys))
  data.reduced$Indep.Lead.Dir <- as.numeric(as.factor(data.reduced$Indep.Lead.Dir))
  data.reduced$CEO.Duality <- as.numeric(as.factor(data.reduced$CEO.Duality))
  data.reduced$Indep.Chrprsn <- as.numeric(as.factor(data.reduced$Indep.Chrprsn))
  data.reduced$Frmr.CEO.or.its.Equiv.on.Bd <- as.numeric(as.factor(data.reduced$Frmr.CEO.or.its.Equiv.on.Bd))
  
  #split dataset into training and test
  len <- length(data.reduced[,1])
  sub <- sample(1:len,len*training.split)
  data.reduced.train <- data.frame(data.reduced[sub,])
  data.reduced.test <- data.frame(data.reduced[-sub,])
  
  data.reduced.train.predictors <- as.matrix(data.reduced.train[ , !(names(data.reduced.train) %in% "target")]) #remove unwanted columns
  data.reduced.train.target <- as.matrix(data.reduced.train$target)
  data.reduced.test.predictors <- as.matrix(data.reduced.test[ , !(names(data.reduced.test) %in% "target")]) #remove unwanted columns
  data.reduced.test.target <- as.matrix(data.reduced.test$target)

  
  library(glmnet)
  #fit models
  fit.lasso <- glmnet(
    data.reduced.train.predictors, 
    data.reduced.train.target, 
    family="gaussian", 
    alpha=1
  )
  fit.ridge <- glmnet(
    data.reduced.train.predictors, 
    data.reduced.train.target, 
    family="gaussian", 
    alpha=0
  )
  fit.elnet <- glmnet(
    data.reduced.train.predictors, 
    data.reduced.train.target, 
    family="gaussian", 
    alpha=0.5
  )

  
  # 10-fold Cross validation for each alpha = 0, 0.1, ... , 0.9, 1.0
  for (i in 0:10) {
    assign(
      paste("fit", i, sep=""), 
      cv.glmnet(
        data.reduced.train.predictors, 
        data.reduced.train.target, 
        type.measure="mse", 
        alpha=i/10,
        family="gaussian"
      )
    )
  }
  
  # Plot solution paths:
  par(mfrow=c(3,2))
  plot(fit.ridge, xvar="lambda", main="fit.ridge")
  plot(fit0, main="Ridge")
  plot(fit.elnet, xvar="lambda",main="fit.elnet")
  plot(fit5, main="Elastic Net")
  plot(fit.lasso, xvar="lambda", main="fit.lasso")
  plot(fit10, main="LASSO")
  
  #musssssst be a better way of writing this code
  pred0.1se <- predict(fit0, s=fit0$lambda.1se, newx=data.reduced.test.predictors)
  pred1.1se <- predict(fit1, s=fit1$lambda.1se, newx=data.reduced.test.predictors)
  pred2.1se <- predict(fit2, s=fit2$lambda.1se, newx=data.reduced.test.predictors)
  pred3.1se <- predict(fit3, s=fit3$lambda.1se, newx=data.reduced.test.predictors)
  pred4.1se <- predict(fit4, s=fit4$lambda.1se, newx=data.reduced.test.predictors)
  pred5.1se <- predict(fit5, s=fit5$lambda.1se, newx=data.reduced.test.predictors)
  pred6.1se <- predict(fit6, s=fit6$lambda.1se, newx=data.reduced.test.predictors)
  pred7.1se <- predict(fit7, s=fit7$lambda.1se, newx=data.reduced.test.predictors)
  pred8.1se <- predict(fit8, s=fit8$lambda.1se, newx=data.reduced.test.predictors)
  pred9.1se <- predict(fit9, s=fit9$lambda.1se, newx=data.reduced.test.predictors)
  pred10.1se <- predict(fit10, s=fit10$lambda.1se, newx=data.reduced.test.predictors)
  
  pred0.min <- predict(fit0, s=fit0$lambda.min, newx=data.reduced.test.predictors)
  pred1.min <- predict(fit1, s=fit1$lambda.min, newx=data.reduced.test.predictors)
  pred2.min <- predict(fit2, s=fit2$lambda.min, newx=data.reduced.test.predictors)
  pred3.min <- predict(fit3, s=fit3$lambda.min, newx=data.reduced.test.predictors)
  pred4.min <- predict(fit4, s=fit4$lambda.min, newx=data.reduced.test.predictors)
  pred5.min <- predict(fit5, s=fit5$lambda.min, newx=data.reduced.test.predictors)
  pred6.min <- predict(fit6, s=fit6$lambda.min, newx=data.reduced.test.predictors)
  pred7.min <- predict(fit7, s=fit7$lambda.min, newx=data.reduced.test.predictors)
  pred8.min <- predict(fit8, s=fit8$lambda.min, newx=data.reduced.test.predictors)
  pred9.min <- predict(fit9, s=fit9$lambda.min, newx=data.reduced.test.predictors)
  pred10.min <- predict(fit10, s=fit10$lambda.min, newx=data.reduced.test.predictors)
  
  print("-----min")
  print (mean((data.reduced.test.target - pred0.min)^2))
  print (mean((data.reduced.test.target - pred1.min)^2))
  print (mean((data.reduced.test.target - pred2.min)^2))
  print (mean((data.reduced.test.target - pred3.min)^2))
  print (mean((data.reduced.test.target - pred4.min)^2))
  print (mean((data.reduced.test.target - pred5.min)^2))
  print (mean((data.reduced.test.target - pred6.min)^2))
  print (mean((data.reduced.test.target - pred7.min)^2))
  print (mean((data.reduced.test.target - pred8.min)^2))
  print (mean((data.reduced.test.target - pred9.min)^2))
  print (mean((data.reduced.test.target - pred10.min)^2))
  print("-----1se")
  print (mean((data.reduced.test.target - pred0.1se)^2))
  print (mean((data.reduced.test.target - pred1.1se)^2))
  print (mean((data.reduced.test.target - pred2.1se)^2))
  print (mean((data.reduced.test.target - pred3.1se)^2))
  print (mean((data.reduced.test.target - pred4.1se)^2))
  print (mean((data.reduced.test.target - pred5.1se)^2))
  print (mean((data.reduced.test.target - pred6.1se)^2))
  print (mean((data.reduced.test.target - pred7.1se)^2))
  print (mean((data.reduced.test.target - pred8.1se)^2))
  print (mean((data.reduced.test.target - pred9.1se)^2))
  print (mean((data.reduced.test.target - pred10.1se)^2))
  
  result=list(
    "fit.lasso"=fit.lasso,
    "fit.ridge"=fit.ridge,
    "fit.elnet"=fit.elnet,
    "fit0"=fit0,
    "fit1"=fit1,
    "fit2"=fit2,
    "fit3"=fit3,
    "fit4"=fit4,
    "fit5"=fit5,
    "fit6"=fit6,
    "fit7"=fit7,
    "fit8"=fit8,
    "fit9"=fit9,
    "fit10"=fit10,
    "data.reduced.train"=data.reduced.train,
    "data.reduced.test"=data.reduced.test
  )
  return(result) 
  
}
tobin.q.results=regLinearRegressionMultiAlphaLamdbaImputedMice(spx, "Tobins.Q")
coef(tobin.q.results$fit9)
fit <- tobin.q.results$fit9
fit.r2 <- fit$glmnet.fit$dev.ratio[which(fit$glmnet.fit$lambda == fit$lambda.min)]
fit.r2 #0.764 [ish]
#training dim - 1670  45
#test dim - 830 45

mscore.results=regLinearRegressionMultiAlphaLamdbaImputedMice(spx.mscore, "EightVarEq")
coef(mscore.results$fit1)
fit <- mscore.results$fit9
fit.r2 <- fit$glmnet.fit$dev.ratio[which(fit$glmnet.fit$lambda == fit$lambda.min)]
fit.r2 #0.2603585...terrible
