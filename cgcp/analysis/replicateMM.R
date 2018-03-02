#!/usr/bin/env Rscript
#get data
library(RMySQL)
library(adabag)
library(pROC)
mydb <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_processed')
spx <- dbReadTable(conn=mydb,name='spx')
sxxp <- dbReadTable(conn=mydb,name='sxxp')
eebp <- dbReadTable(conn=mydb,name='eebp')

#**********************
#ADABOOST
#**********************
adaboost <- function(dataset) {
  drops <- c("Ticker",
             "AZS.class",
             "Tobins.Q",
             "AZS")
  data.reduced<-dataset[ , !(names(dataset) %in% drops)] #remove unwanted columns
  data.reduced<-data.reduced[complete.cases(data.reduced[ , "Tobins.Q.class"]),]# we only want records with a class indicator
  
  data.reduced$Tobins.Q.class=as.factor(data.reduced$Tobins.Q.class)
  data.reduced$Feml.CEO.or.Equiv=as.numeric(as.factor(data.reduced$Feml.CEO.or.Equiv))
  data.reduced$Prsdg.Dir=as.numeric(as.factor(data.reduced$Prsdg.Dir))
  data.reduced$Clssfd.Bd.Sys=as.numeric(as.factor(data.reduced$Clssfd.Bd.Sys))
  data.reduced$Indep.Lead.Dir=as.numeric(as.factor(data.reduced$Indep.Lead.Dir))
  data.reduced$CEO.Duality=as.numeric(as.factor(data.reduced$CEO.Duality))
  data.reduced$Indep.Chrprsn=as.numeric(as.factor(data.reduced$Indep.Chrprsn))
  data.reduced$Frmr.CEO.or.its.Equiv.on.Bd=as.numeric(as.factor(data.reduced$Frmr.CEO.or.its.Equiv.on.Bd))
  
  len <- length(data.reduced[,1])
  sub <- sample(1:len,2*len/3)#get 2/3rds of the records
  mfinal <- 100 #default is 100
  maxdepth <- 30 #30 is max
  data.adaboost <- boosting(Tobins.Q.class~., 
                           data=data.reduced[sub, ], 
                           boos=TRUE, 
                           mfinal=mfinal,
                           coeflearn="Zhu"
                           ,control=rpart.control(maxdepth=maxdepth) #not sure i need this 
  )
  data.adaboost.pred <- predict.boosting(data.adaboost,newdata=data.reduced[-sub, ])
  errorevol(data.adaboost,newdata=data.reduced[sub, ])->evol.train
  errorevol(data.adaboost,newdata=data.reduced[-sub, ])->evol.test
  
  trueNegative<-data.adaboost.pred$confusion[1]   
  falsePositive<-data.adaboost.pred$confusion[2]
  falseNegative<-data.adaboost.pred$confusion[3] 
  truePositive<-data.adaboost.pred$confusion[4] 
  
  # > precision
  data_precision_class0=trueNegative/(trueNegative + falseNegative)
  data_precision_class1=truePositive/(truePositive + falsePositive)
  
  # > recall
  data_recall=truePositive/(truePositive + falseNegative)
  
  # > ROC and AUC
  roc_obj <- roc(data.reduced[-sub,]$Tobins.Q.class, as.numeric(data.adaboost.pred$class))
  
  result=list(
     "confusion"=data.adaboost.pred$confusion,
     "error"=data.adaboost.pred$error,
     "evol.train"=evol.train,
     "evol.test"=evol.test,
     "trueNegative"=trueNegative,
     "falsePositive"=falsePositive,
     "falseNegative"=falseNegative,
     "truePositive"=truePositive,
     "precision_class0"=data_precision_class0,
     "precision_class1"=data_precision_class1,
     "recall"=data_recall,
     "roc_obj"=roc_obj
  )
  return(result)  
}
#call the function on each
spx.adaboost.results=adaboost(spx)
sxxp.adaboost.results=adaboost(sxxp)
eebp.adaboost.results=adaboost(eebp)
#analyze the results
c(spx.adaboost.results$precision_class0, spx.adaboost.results$precision_class1,auc(spx.adaboost.results$roc_obj))
c(sxxp.adaboost.results$precision_class0, sxxp.adaboost.results$precision_class1,auc(sxxp.adaboost.results$roc_obj))
c(eebp.adaboost.results$precision_class0, eebp.adaboost.results$precision_class1,auc(eebp.adaboost.results$roc_obj))
# > errors in train and test
plot.errorevol(spx.adaboost.results$evol.test,spx.adaboost.results$evol.train)
plot.errorevol(sxxp.adaboost.results$evol.test,sxxp.adaboost.results$evol.train)
plot.errorevol(eebp.adaboost.results$evol.test,eebp.adaboost.results$evol.train)



