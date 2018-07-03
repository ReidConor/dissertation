#!/usr/bin/env Rscript
#1. apply regularised regression rather than straight classification
#get data
library(RMySQL)
library(glmnet)
mydb.processed <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_processed')
mydb.imputed <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_imputed')
mydb.results <- dbConnect(MySQL(), user='root', password='', dbname='regression_results')

spx <- dbReadTable(conn=mydb.imputed,name='spx')
sxxp <- dbReadTable(conn=mydb.imputed,name='sxxp')
eebp <- dbReadTable(conn=mydb.imputed,name='eebp')
spx.ceo.comp <- dbReadTable(conn=mydb.imputed,name='spx_ceo_comp')
spx.mscore <- dbReadTable(conn=mydb.processed,name='spx_mscore')

training.split=2/3
#multiple linear regression
#dependent variable is continuous, independent variables are continuous/discrete, regression line is linear.
#http://r-statistics.co/Linear-Regression.html
library(Matrix)

#regularised linear regression
#same as regLinearRegressionMultiAlphaLamdba but developments
regLinearRegressionMultiAlphaLamdba_v2 <- function(dataset, target){
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
  View(data.reduced)
  data.reduced <- data.reduced[complete.cases(data.reduced[ , target]),]# we only want records with a class indicator
  colnames(data.reduced)[colnames(data.reduced) == target] <- 'target'
  data.reduced <- data.frame(data.reduced)
  
  #data.reduced$Feml.CEO.or.Equiv <- as.numeric(as.factor(data.reduced$Feml.CEO.or.Equiv))
  #data.reduced$Prsdg.Dir <- as.numeric(as.factor(data.reduced$Prsdg.Dir))
  data.reduced$Prsdg.Dir <- NULL
  #data.reduced$Clssfd.Bd.Sys <- as.numeric(as.factor(data.reduced$Clssfd.Bd.Sys))
  data.reduced$Clssfd.Bd.Sys <- NULL
  data.reduced$Indep.Lead.Dir <- as.numeric(as.factor(data.reduced$Indep.Lead.Dir))
  #data.reduced$CEO.Duality <- as.numeric(as.factor(data.reduced$CEO.Duality))
  data.reduced$CEO.Duality <- NULL
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
  
  errors <- c(
    "min0" = mean((data.reduced.test.target - pred0.min)^2),
    "min1" = mean((data.reduced.test.target - pred1.min)^2),
    "min2" = mean((data.reduced.test.target - pred2.min)^2),
    "min3" = mean((data.reduced.test.target - pred3.min)^2),
    "min4" = mean((data.reduced.test.target - pred4.min)^2),
    "min5" = mean((data.reduced.test.target - pred5.min)^2),
    "min6" = mean((data.reduced.test.target - pred6.min)^2),
    "min7" = mean((data.reduced.test.target - pred7.min)^2),
    "min8" = mean((data.reduced.test.target - pred8.min)^2),
    "min9" = mean((data.reduced.test.target - pred9.min)^2),
    "min10" = mean((data.reduced.test.target - pred10.min)^2),
    "1se0" = mean((data.reduced.test.target - pred0.1se)^2),
    "1se1" = mean((data.reduced.test.target - pred1.1se)^2),
    "1se2" = mean((data.reduced.test.target - pred2.1se)^2),
    "1se3" = mean((data.reduced.test.target - pred3.1se)^2),
    "1se4" = mean((data.reduced.test.target - pred4.1se)^2),
    "1se5" = mean((data.reduced.test.target - pred5.1se)^2),
    "1se6" = mean((data.reduced.test.target - pred6.1se)^2),
    "1se7" = mean((data.reduced.test.target - pred7.1se)^2),
    "1se8" = mean((data.reduced.test.target - pred8.1se)^2),
    "1se9" = mean((data.reduced.test.target - pred9.1se)^2),
    "1se10" = mean((data.reduced.test.target - pred10.1se)^2)
  )
  
  fits <- c(
    fit0, 
    fit1,
    fit2,
    fit3,
    fit4,
    fit5,
    fit6,
    fit7,
    fit8,
    fit9,
    fit10
  )
  
  r2 <- c(
    fit0$glmnet.fit$dev.ratio[which(fit0$glmnet.fit$lambda == fit0$lambda.min)],
    fit1$glmnet.fit$dev.ratio[which(fit1$glmnet.fit$lambda == fit1$lambda.min)],
    fit2$glmnet.fit$dev.ratio[which(fit2$glmnet.fit$lambda == fit2$lambda.min)],
    fit3$glmnet.fit$dev.ratio[which(fit3$glmnet.fit$lambda == fit3$lambda.min)],
    fit4$glmnet.fit$dev.ratio[which(fit4$glmnet.fit$lambda == fit4$lambda.min)],
    fit5$glmnet.fit$dev.ratio[which(fit5$glmnet.fit$lambda == fit5$lambda.min)],
    fit6$glmnet.fit$dev.ratio[which(fit6$glmnet.fit$lambda == fit6$lambda.min)],
    fit7$glmnet.fit$dev.ratio[which(fit7$glmnet.fit$lambda == fit7$lambda.min)],
    fit8$glmnet.fit$dev.ratio[which(fit8$glmnet.fit$lambda == fit8$lambda.min)],
    fit9$glmnet.fit$dev.ratio[which(fit9$glmnet.fit$lambda == fit9$lambda.min)],
    fit10$glmnet.fit$dev.ratio[which(fit10$glmnet.fit$lambda == fit10$lambda.min)]
  )
  
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
    "data.reduced.test"=data.reduced.test,
    "fits"=fits,
    "errors"=errors,
    "r2"=r2
  )
  return(result) 
  
}
#regular spx with tobin as target
spx.tobin.q.results = regLinearRegressionMultiAlphaLamdba_v2(spx, "Tobins.Q")
spx.altman.z.results = regLinearRegressionMultiAlphaLamdba_v2(spx, "AZS")
sxxp.tobin.q.results = regLinearRegressionMultiAlphaLamdba_v2(sxxp, "Tobins.Q")
sxxp.altman.z.results = regLinearRegressionMultiAlphaLamdba_v2(sxxp, "AZS")
eebp.tobin.q.results = regLinearRegressionMultiAlphaLamdba_v2(eebp, "Tobins.Q")
eebp.altman.z.results = regLinearRegressionMultiAlphaLamdba_v2(eebp, "AZS")


regression.results <- data.frame()
spx.tobin <- data.frame("spx","tobins.q",as.numeric(spx.tobin.q.results$r2[[which.max(spx.tobin.q.results$r2)]]))
colnames(spx.tobin) <- c("dataset", "target", "r2")
spx.azs <- data.frame("spx","asz",as.numeric(spx.altman.z.results$r2[[which.max(spx.altman.z.results$r2)]]))
colnames(spx.azs) <- c("dataset", "target", "r2")
sxxp.tobin <- data.frame("sxxp","tobins.q",sxxp.tobin.q.results$r2[[which.max(sxxp.tobin.q.results$r2)]])
colnames(sxxp.tobin) <- c("dataset", "target", "r2")
sxxp.azs <- data.frame("sxxp","asz",sxxp.altman.z.results$r2[[which.max(sxxp.altman.z.results$r2)]])
colnames(sxxp.azs) <- c("dataset", "target", "r2")
eebp.tobin <- data.frame("eebp","tobins.q",eebp.tobin.q.results$r2[[which.max(eebp.tobin.q.results$r2)]])
colnames(eebp.tobin) <- c("dataset", "target", "r2")
eebp.azs <- data.frame("eebp","asz",eebp.altman.z.results$r2[[which.max(eebp.altman.z.results$r2)]])
colnames(eebp.azs) <- c("dataset", "target", "r2")
regression.results <- rbind(
  regression.results,
  spx.tobin,
  spx.azs,
  sxxp.tobin,
  sxxp.azs,
  eebp.tobin,
  eebp.azs
)
dbWriteTable(mydb.results, value = regression.results, name = "glmnet_results", overwrite = TRUE, row.names=FALSE)




#tobin as target with ceo comp
tobin.q.ceo.comp.results=regLinearRegressionMultiAlphaLamdba_v2(spx.ceo.comp, "Tobins.Q")
tobin.q.ceo.comp.results$r2[[which.max(tobin.q.ceo.comp.results$r2)]]



#mscore as target regression
#mscore data hasnt been matched onto the full spx dataset so need to do that before feeding into algo
mscore.results = regLinearRegressionMultiAlphaLamdba_v2(spx.mscore, "EightVarEq")

coef(mscore.results$fit1)
fit <- mscore.results$fit9
fit.r2 <- fit$glmnet.fit$dev.ratio[which(fit$glmnet.fit$lambda == fit$lambda.min)]
fit.r2 #0.2603585...terrible
