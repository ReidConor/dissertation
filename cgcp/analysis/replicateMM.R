#!/usr/bin/env Rscript
#get data
library(RMySQL)
library(adabag)
library(pROC)
library(C50)
mydb <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_processed')
spx <- dbReadTable(conn=mydb,name='spx')
sxxp <- dbReadTable(conn=mydb,name='sxxp')
eebp <- dbReadTable(conn=mydb,name='eebp')

training.split=2/3 #proportion of data that will be dedicated to training data subsets 

#**********************
#ADABOOST
#**********************
# Runs the adaboost algorithm on the given dataset
#
# Args:
#   dataset: A dataframe that contains the data to run the algorithm on 
#   
# Returns:
#   A number of elements of the results of adaboost
adaboost <- function(dataset, target) {
  drops <- c("Ticker",
             "AZS.class",
             "AZS",
             "Tobins.Q",
             "Tobins.Q.class")
  drops=drops[drops != target]#dont want to remove whatever is passed as the target
  data.reduced<-dataset[ , !(names(dataset) %in% drops)] #remove unwanted columns
  data.reduced<-data.reduced[complete.cases(data.reduced[ , target]),]# we only want records with a class indicator
  
  data.reduced$Feml.CEO.or.Equiv=as.numeric(as.factor(data.reduced$Feml.CEO.or.Equiv))
  data.reduced$Prsdg.Dir=as.numeric(as.factor(data.reduced$Prsdg.Dir))
  data.reduced$Clssfd.Bd.Sys=as.numeric(as.factor(data.reduced$Clssfd.Bd.Sys))
  data.reduced$Indep.Lead.Dir=as.numeric(as.factor(data.reduced$Indep.Lead.Dir))
  data.reduced$CEO.Duality=as.numeric(as.factor(data.reduced$CEO.Duality))
  data.reduced$Indep.Chrprsn=as.numeric(as.factor(data.reduced$Indep.Chrprsn))
  data.reduced$Frmr.CEO.or.its.Equiv.on.Bd=as.numeric(as.factor(data.reduced$Frmr.CEO.or.its.Equiv.on.Bd))
  
  data.reduced[[target]]=as.factor(data.reduced[[target]])
  colnames(data.reduced)[colnames(data.reduced) == target] <- 'target'
  
  len <- length(data.reduced[,1])
  sub <- sample(1:len,len*training.split)
  mfinal <- 100 #default is 100
  maxdepth <- 30 #30 is max
  data.reduced<-data.frame(data.reduced)
  data.adaboost <- boosting(target~., 
                           data=data.reduced[sub, ], 
                           boos=TRUE, 
                           mfinal=mfinal,
                           coeflearn="Zhu"
                           ,control=rpart.control(maxdepth=maxdepth) #not sure i need this 
  )
  data.adaboost.pred <- predict.boosting(data.adaboost,newdata=data.reduced[-sub, ])
  errorevol(data.adaboost,newdata=data.reduced[sub, ])->evol.train
  errorevol(data.adaboost,newdata=data.reduced[-sub, ])->evol.test
  
  result=list(
     "data.reduced"=data.reduced, 
     "model"=data.adaboost,
     "model.pred"=data.adaboost.pred,
     "confusion"=data.adaboost.pred$confusion,
     "error"=data.adaboost.pred$error,
     "evol.train"=evol.train,
     "evol.test"=evol.test
  )
  return(result)  
}
#call adaboost on each
spx.adaboost.tobin.results=adaboost(spx,"Tobins.Q.class")
spx.adaboost.altman.results=adaboost(spx,"AZS.class")
sxxp.adaboost.tobin.results=adaboost(sxxp,"Tobins.Q.class")
sxxp.adaboost.altman.results=adaboost(sxxp,"AZS.class")
eebp.adaboost.tobin.results=adaboost(eebp,"Tobins.Q.class")
eebp.adaboost.altman.results=adaboost(eebp,"AZS.class")


#analyze the results
#c(spx.adaboost.tobin.results$precision.class0, spx.adaboost.tobin.results$precision.class1,auc(spx.adaboost.tobin.results$roc.obj))
#c(sxxp.adaboost.tobin.results$precision.class0, sxxp.adaboost.tobin.results$precision.class1,auc(sxxp.adaboost.tobin.results$roc.obj))
#c(eebp.adaboost.tobin.results$precision.class0, eebp.adaboost.tobin.results$precision.class1,auc(eebp.adaboost.tobin.results$roc.obj))
# > errors in train and test
#plot.errorevol(spx.adaboost.tobin.results$evol.test,spx.adaboost.tobin.results$evol.train)
#plot.errorevol(sxxp.adaboost.tobin.results$evol.test,sxxp.adaboost.tobin.results$evol.train)
#plot.errorevol(eebp.adaboost.tobin.results$evol.test,eebp.adaboost.tobin.results$evol.train)



#**********************
#J48 [which is actually C5.0]
#**********************
# Runs an interpretation of the J48 algorithm on the given dataset
#
# Args:
#   dataset: A dataframe that contains the data to run the algorithm on 
#   
# Returns:
#   A number of elements of the results of the algorithm interpretation
j48 <- function(dataset){
  #we're actaully using the C5.0 algoritm, which is an improvement on the C4.5
  #+ algorithm. it is the latter than J48 is built on top of
  drops <- c("Ticker",
             "AZS.class",
             "Tobins.Q",
             "AZS")
  data.reduced<-dataset[ , !(names(dataset) %in% drops)] #remove unwanted columns
  data.reduced<-data.reduced[complete.cases(data.reduced[ , "Tobins.Q.class"]),]# we only want records with a class indicator
  
  len <- length(data.reduced[,1])
  sub <- sample(1:len,len*training.split)
  data.reduced$Tobins.Q.class=as.factor(data.reduced$Tobins.Q.class)
  data.reduced.train=data.reduced[sub,]
  data.reduced.test=data.reduced[-sub,]
  
  # > build model on train data
  tree.model <- C5.0(x = data.reduced.train[ , -which(names(data.reduced.train) %in% c("Tobins.Q.class"))], y = data.reduced.train$Tobins.Q.class)
  rule.model <- C5.0(Tobins.Q.class ~ ., data = data.reduced.train, rules = TRUE)
  
  # > predict on test data
  tree.model.predict <- predict(tree.model, data.reduced.test[ , -which(names(data.reduced.train) %in% c("Tobins.Q.class"))])
  rule.model.predict <- predict(rule.model, data.reduced.test[ , -which(names(data.reduced.train) %in% c("Tobins.Q.class"))])
  
  # > ROC
  roc.obj <- roc(data.reduced.test$Tobins.Q.class, as.numeric(tree.model.predict))
  
  result=list(
   "tree.model"=tree.model,
   "tree.model.predict"=tree.model.predict,
   "rule.model"=rule.model,
   "rule.model.predict"=rule.model.predict,
   "roc.obj"=roc.obj
  )
  return(result)  
  
}
#call J48[C5.0] on each
spx.j48.results=j48(spx)
sxxp.j48.results=j48(sxxp)
eebp.j48.results=j48(eebp)
#analyze the results
auc(spx.j48.results$roc.obj)
auc(sxxp.j48.results$roc.obj)
auc(eebp.j48.results$roc.obj)



















df <- data.frame(A=1:10, B=2:11, C=3:12)
fun1 <- function(x, column){
  x[,column]
}
fun1(df, "B")



