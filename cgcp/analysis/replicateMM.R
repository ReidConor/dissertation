#!/usr/bin/env Rscript
#get data
library(RMySQL)
mydb <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_processed')
spx <- dbReadTable(conn=mydb,name='spx')
sxxp <- dbReadTable(conn=mydb,name='sxxp')
eebp <- dbReadTable(conn=mydb,name='eebp')

#lets start with spx
library(adabag)
drops <- c("Ticker",
           "AZS.class",
           "Tobins.Q",
           "AZS")
spx.reduced<-spx[ , !(names(spx) %in% drops)] #remove unwanted columns
spx.reduced<-spx.reduced[complete.cases(spx.reduced[ , "Tobins.Q.class"]),]# we only want records with a class indicator
spx.reduced$Tobins.Q.class=as.factor(spx.reduced$Tobins.Q.class)
spx.reduced$Feml.CEO.or.Equiv=as.numeric(as.factor(spx.reduced$Feml.CEO.or.Equiv))
len <- length(spx.reduced[,1])
sub <- sample(1:l,2*l/3)#get 2/3rds of the records
mfinal <- 100 #default is 100
maxdepth <- 30 #30 is max
spx.adaboost <- boosting(Tobins.Q.class~., 
                         data=spx.reduced[sub, ], 
                         boos=TRUE, 
                         mfinal=mfinal,
                         coeflearn="Zhu"
                         ,control=rpart.control(maxdepth=maxdepth) #not sure i need this 
                         )
spx.adaboost.pred <- predict.boosting(spx.adaboost,newdata=spx.reduced[-sub, ])
spx.adaboost.pred$confusion
spx.adaboost.pred$error
errorevol(spx.adaboost,newdata=spx.reduced[sub, ])->evol.train
errorevol(spx.adaboost,newdata=spx.reduced[-sub, ])->evol.test
plot.errorevol(evol.test,evol.train)


#***********
#evaluate
#***********
# > confusion matrix
trueNegative<-spx.adaboost.pred$confusion[1]   
falsePositive<-spx.adaboost.pred$confusion[2]
falseNegative<-spx.adaboost.pred$confusion[3] 
truePositive<-spx.adaboost.pred$confusion[4] 

# > precision
spx_precision_class0=trueNegative/(trueNegative + falseNegative)
spx_precision_class1=truePositive/(truePositive + falsePositive)
spx_precision_class0
spx_precision_class1

# > recall
spx_recall=truePositive/(truePositive + falseNegative)
spx_recall

# > ROC and AUC
library(pROC)
roc_obj <- roc(spx.reduced[-sub,]$Tobins.Q.class, as.numeric(spx.adaboost.pred$class))
auc(roc_obj)

