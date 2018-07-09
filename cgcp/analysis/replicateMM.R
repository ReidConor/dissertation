#!/usr/bin/env Rscript
library(RMySQL)
library(adabag)
library(pROC)
library(C50)
library(mice)
#get data
mydb <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_processed')
mydb.results <- dbConnect(MySQL(), user='root', password='', dbname='mm_results')
spx <- dbReadTable(conn=mydb,name='spx')
sxxp <- dbReadTable(conn=mydb,name='sxxp')
eebp <- dbReadTable(conn=mydb,name='eebp')

#spx <- dbReadTable(conn=mydb,name='spx_sub_app_a')
#sxxp <- dbReadTable(conn=mydb,name='sxxp_sub_app_a')
#eebp <- dbReadTable(conn=mydb,name='eebp_sub_app_a')

training.split=2/3 #proportion of data that will be dedicated to training data subsets 

#**********************
#ADABOOST
#**********************
# Runs the adaboost algorithm on the given dataset
#
# Args:
#   dataset: A dataframe that contains the data to run the algorithm on 
#   target:  The dependant variable
#
# Returns:
#   A number of elements of the results of adaboost
adaboost <- function(dataset, target) {
  set.seed(1)
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
  tcontrol <- trainControl(method="cv", number=10)
  data.adaboost <- boosting(target~., 
                           data=data.reduced[sub, ], 
                           boos=TRUE, 
                           mfinal=mfinal,
                           coeflearn="Freund" #used in the calc of alpha, mm uses freund i think
                           ,control=rpart.control(maxdepth=maxdepth) #not sure i need this 
                           ,trControl=train_control
  )
  data.adaboost.pred <- predict.boosting(data.adaboost,newdata=data.reduced[-sub, ])
  errorevol(data.adaboost,newdata=data.reduced[sub, ])->evol.train
  errorevol(data.adaboost,newdata=data.reduced[-sub, ])->evol.test
  
  var.importance <- data.frame(data.adaboost$importance)
  pred.error <- data.adaboost.pred$error
  accuracy <- 1 - pred.error
  test.length <- len-(len*training.split)
  training.length <- len*training.split
  error.CI.ninty.five <- c(
    pred.error + (1.96 * sqrt(pred.error * (1 - pred.error)) / test.length),
    pred.error - (1.96 * sqrt(pred.error * (1 - pred.error)) / test.length)
  )
  coverage.of.cases <- c()
  test.actuals <- data.reduced[-sub, ]$target
  confusion.caret <- confusionMatrix(
    data=data.adaboost.pred$class, 
    test.actuals
  )
  
  
  if (target == "AZS.class"){
    roc <- multiclass.roc(as.numeric(test.actuals), as.numeric(as.factor(data.adaboost.pred$class)),direction = "<")
  }else if (target == "Tobins.Q.class"){
    roc <- roc(test.actuals, as.numeric(data.adaboost.pred$class),direction = "<")
  }else{
    roc <- NULL
  }
  
  result=list(
     "data.reduced"=data.reduced, 
     "model"=data.adaboost,
     "model.pred"=data.adaboost.pred,
     "confusion"=data.adaboost.pred$confusion,
     "error"=data.adaboost.pred$error,
     "evol.train"=evol.train,
     "evol.test"=evol.test,
     "var.importance"=var.importance,
     "pred.error"=pred.error,
     "training.length"=training.length,
     "test.length"=test.length,
     "test.actuals"=test.actuals,
     "accuracy"=accuracy,
     "error.CI.ninty.five"=error.CI.ninty.five,
     "coverage.of.cases"=coverage.of.cases,
     "confusion.caret"=confusion.caret,
     "roc"=roc,
     "pred.class"=data.adaboost.pred$class
  )
  return(result)  
  
}
#call adaboost on each
spx.adaboost.tobin.results=adaboost(spx,"Tobins.Q.class")
sxxp.adaboost.tobin.results=adaboost(sxxp,"Tobins.Q.class")
eebp.adaboost.tobin.results=adaboost(eebp,"Tobins.Q.class")
spx.adaboost.altman.results=adaboost(spx,"AZS.class")
sxxp.adaboost.altman.results=adaboost(sxxp,"AZS.class")
eebp.adaboost.altman.results=adaboost(eebp,"AZS.class")



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
j48 <- function(dataset, target){
  #we're actaully using the C5.0 algoritm, which is an improvement on the C4.5
  #+ algorithm. it is the latter than J48 is built on top of
  drops <- c("Ticker",
             "AZS.class",
             "AZS",
             "Tobins.Q",
             "Tobins.Q.class")
  drops=drops[drops != target]#dont want to drop whatever is passed as the target
  data.reduced<-dataset[ , !(names(dataset) %in% drops)] #remove unwanted columns
  data.reduced<-data.reduced[complete.cases(data.reduced[ , target]),]# we only want records with a class indicator
  data.reduced$Feml.CEO.or.Equiv <- as.numeric(as.factor(data.reduced$Feml.CEO.or.Equiv))
  data.reduced$Prsdg.Dir <- as.numeric(as.factor(data.reduced$Prsdg.Dir))
  data.reduced$Clssfd.Bd.Sys <- as.numeric(as.factor(data.reduced$Clssfd.Bd.Sys))
  data.reduced$Indep.Lead.Dir <- as.numeric(as.factor(data.reduced$Indep.Lead.Dir))
  data.reduced$CEO.Duality <- as.numeric(as.factor(data.reduced$CEO.Duality))
  data.reduced$Indep.Chrprsn <- as.numeric(as.factor(data.reduced$Indep.Chrprsn))
  data.reduced$Frmr.CEO.or.its.Equiv.on.Bd <- as.numeric(as.factor(data.reduced$Frmr.CEO.or.its.Equiv.on.Bd))
  
  len <- length(data.reduced[,1])
  sub <- sample(1:len,len*training.split)
  
  data.reduced[[target]]=as.factor(data.reduced[[target]])
  colnames(data.reduced)[colnames(data.reduced) == target] <- 'target'
  
  data.reduced.train=data.reduced[sub,]
  data.reduced.test=data.reduced[-sub,]
  
  # > build model on train data
  tcontrol <- trainControl(method="cv", number=10)
  tree.model <- C5.0(
    x = data.reduced.train[ , -which(names(data.reduced.train) %in% c("target"))], 
    y = data.reduced.train$target,
    trcontrol=tcontrol
  )
  rule.model <- C5.0(
    target ~ ., 
    data = data.reduced.train, 
    rules = TRUE,
    trcontrol=tcontrol
  )
  
  # > predict on test data
  tree.model.predict <- predict(tree.model, data.reduced.test[ , -which(names(data.reduced.train) %in% c("target"))])
  rule.model.predict <- predict(rule.model, data.reduced.test[ , -which(names(data.reduced.train) %in% c("target"))])
  
  test.actuals <- data.reduced.test$target
  rule.confusion.caret <- confusionMatrix(
    data=rule.model.predict, 
    test.actuals
  )
  tree.confusion.caret <- confusionMatrix(
    data=tree.model.predict, 
    test.actuals
  )
  
  
  if (target == "AZS.class"){
    rule.roc <- multiclass.roc(test.actuals, as.numeric(as.factor(rule.model.predict)),direction = "<")
    tree.roc <- multiclass.roc(test.actuals, as.numeric(as.factor(tree.model.predict)),direction = "<")
  }else if (target == "Tobins.Q.class"){
    rule.roc <- roc(test.actuals, as.numeric(rule.model.predict),direction = "<")
    tree.roc <- roc(test.actuals, as.numeric(tree.model.predict),direction = "<")
  }else{
    rule.roc <- NULL
    tree.roc <- NULL
  }
  
  result=list(
   "tree.model"=tree.model,
   "summary.tree.model"=summary(tree.model),
   "tree.model.predict"=tree.model.predict,
   "rule.model"=rule.model,
   "summary.rule.model"=summary(rule.model),
   "rule.model.predict"=rule.model.predict,
   "data.reduced"=data.reduced,
   "data.reduced.train"=data.reduced.train,
   "data.reduced.test"=data.reduced.test,
   "rule.confusion.caret"=rule.confusion.caret,
   "tree.confusion.caret"=tree.confusion.caret,
   "rule.roc"=rule.roc,
   "tree.roc"=tree.roc
  )
  return(result)  
  
}
#call J48[C5.0] on each
spx.j48.tobin.results=j48(dataset=spx,target="Tobins.Q.class")
sxxp.j48.tobin.results=j48(sxxp,"Tobins.Q.class")
eebp.j48.tobin.results=j48(eebp,"Tobins.Q.class")
spx.j48.altman.results=j48(spx,"AZS.class")
sxxp.j48.altman.results=j48(sxxp,"AZS.class")
eebp.j48.altman.results=j48(eebp,"AZS.class")




#**********************
# Write Results
#**********************
#write important variables
library(data.table)
spx.tobin.imp <- data.frame(spx.adaboost.tobin.results$var.importance) 
spx.tobin.imp <- setDT(spx.tobin.imp, keep.rownames = TRUE)[]
colnames(spx.tobin.imp) <- c("Var","Imp")
spx.tobin.imp.head <- head(spx.tobin.imp[order(-spx.tobin.imp$Imp),],10)
dbWriteTable(mydb.results, value = spx.tobin.imp.head, name = "spx_tobin_q_imp_vars", overwrite = TRUE, row.names=FALSE)
spx.altman.imp <- data.frame(spx.adaboost.altman.results$var.importance) 
spx.altman.imp <- setDT(spx.altman.imp, keep.rownames = TRUE)[]
colnames(spx.altman.imp) <- c("Var","Imp")
spx.altman.imp.head <- head(spx.altman.imp[order(-spx.altman.imp$Imp),],10)
dbWriteTable(mydb.results, value = spx.altman.imp.head, name = "spx_altman_imp_vars", overwrite = TRUE, row.names=FALSE)

sxxp.tobin.imp <- data.frame(sxxp.adaboost.tobin.results$var.importance) 
sxxp.tobin.imp <- setDT(sxxp.tobin.imp, keep.rownames = TRUE)[]
colnames(sxxp.tobin.imp) <- c("Var","Imp")
sxxp.tobin.imp.head <- head(sxxp.tobin.imp[order(-sxxp.tobin.imp$Imp),],10)
dbWriteTable(mydb.results, value = sxxp.tobin.imp.head, name = "sxxp_tobin_q_imp_vars", overwrite = TRUE, row.names=FALSE)
sxxp.altman.imp <- data.frame(sxxp.adaboost.altman.results$var.importance) 
sxxp.altman.imp <- setDT(sxxp.altman.imp, keep.rownames = TRUE)[]
colnames(sxxp.altman.imp) <- c("Var","Imp")
sxxp.altman.imp.head <- head(sxxp.altman.imp[order(-sxxp.altman.imp$Imp),],10)
dbWriteTable(mydb.results, value = sxxp.altman.imp.head, name = "sxxp_altman_imp_vars", overwrite = TRUE, row.names=FALSE)

eebp.tobin.imp <- data.frame(eebp.adaboost.tobin.results$var.importance) 
eebp.tobin.imp <- setDT(eebp.tobin.imp, keep.rownames = TRUE)[]
colnames(eebp.tobin.imp) <- c("Var","Imp")
eebp.tobin.imp.head <- head(eebp.tobin.imp[order(-eebp.tobin.imp$Imp),],10)
dbWriteTable(mydb.results, value = eebp.tobin.imp.head, name = "eebp_tobin_q_imp_vars", overwrite = TRUE, row.names=FALSE)
eebp.altman.imp <- data.frame(eebp.adaboost.altman.results$var.importance) 
eebp.altman.imp <- setDT(eebp.altman.imp, keep.rownames = TRUE)[]
colnames(eebp.altman.imp) <- c("Var","Imp")
eebp.altman.imp.head <- head(eebp.altman.imp[order(-eebp.altman.imp$Imp),],10)
dbWriteTable(mydb.results, value = eebp.altman.imp.head, name = "eebp_altman_imp_vars", overwrite = TRUE, row.names=FALSE)


#write to mysql
to.write.spx <- data.frame(list (
  format(as.POSIXlt(Sys.time()),'%Y-%m-%d %H:%M:%S'),
  "Adaboost",
  "spx",
  "Tobins.Q.class", 
  spx.adaboost.tobin.results$accuracy, #correctly classified instances
  NA, #coverage of cases
  spx.adaboost.tobin.results$confusion.caret$byClass[1], #sensitivity / precision class 0
  spx.adaboost.tobin.results$confusion.caret$byClass[2], #specificity / precision class 1
  as.numeric(spx.adaboost.tobin.results$roc$auc) #roc area
))
colnames(to.write.spx) <- c(
  "DateStamp",
  "Algorithm",
  "DataSet",
  "Target",
  "Correctly Classified Instances", 
  "Coverage Of cases", 
  "Precision Class 0",
  "Precision Class 1",
  "ROC area"
)
to.write.sxxp <- data.frame(list (
  format(as.POSIXlt(Sys.time()),'%Y-%m-%d %H:%M:%S'),
  "Adaboost",
  "sxxp",
  "Tobins.Q.class", 
  sxxp.adaboost.tobin.results$accuracy, #correctly classified instances
  NA, #coverage of cases
  sxxp.adaboost.tobin.results$confusion.caret$byClass[1], #sensitivity / precision class 0
  sxxp.adaboost.tobin.results$confusion.caret$byClass[2], #specificity / precision class 1
  as.numeric(sxxp.adaboost.tobin.results$roc$auc) #roc area
))
colnames(to.write.sxxp) <- c(
  "DateStamp",
  "Algorithm",
  "DataSet",
  "Target",
  "Correctly Classified Instances", 
  "Coverage Of cases", 
  "Precision Class 0",
  "Precision Class 1",
  "ROC area"
)
to.write.eebp <- data.frame(list (
  format(as.POSIXlt(Sys.time()),'%Y-%m-%d %H:%M:%S'),
  "Adaboost",
  "eebp",
  "Tobins.Q.class", 
  eebp.adaboost.tobin.results$accuracy, #correctly classified instances
  NA, #coverage of cases
  eebp.adaboost.tobin.results$confusion.caret$byClass[1], #sensitivity / precision class 0
  eebp.adaboost.tobin.results$confusion.caret$byClass[2], #specificity / precision class 1
  as.numeric(eebp.adaboost.tobin.results$roc$auc) #roc area
))
colnames(to.write.eebp) <- c(
  "DateStamp",
  "Algorithm",
  "DataSet",
  "Target",
  "Correctly Classified Instances", 
  "Coverage Of cases", 
  "Precision Class 0",
  "Precision Class 1",
  "ROC area"
)
dbWriteTable(mydb.results, value = to.write.spx, name = "tobin_q_results", overwrite = TRUE, row.names=FALSE)
dbWriteTable(mydb.results, value = to.write.sxxp, name = "tobin_q_results", append = TRUE, row.names=FALSE)
dbWriteTable(mydb.results, value = to.write.eebp, name = "tobin_q_results", append = TRUE, row.names=FALSE)
to.write.spx <- data.frame(list (
  format(as.POSIXlt(Sys.time()),'%Y-%m-%d %H:%M:%S'),
  "Adaboost",
  "spx",
  "Altman.Z",
  spx.adaboost.altman.results$accuracy, #correctly classified instances
  NA, #coverage of cases
  spx.adaboost.altman.results$confusion.caret$byClass[1,3], #precision class 0
  spx.adaboost.altman.results$confusion.caret$byClass[2,3], #precision class 1
  spx.adaboost.altman.results$confusion.caret$byClass[3,3], #precision class 2
  as.numeric(spx.adaboost.altman.results$roc$auc) #roc area
))
colnames(to.write.spx) <- c(
  "DateStamp",
  "Algorithm",
  "DataSet",
  "Target",
  "Correctly Classified Instances", 
  "Coverage Of cases", 
  "Precision Class 0",
  "Precision Class 1",
  "Precision Class 2",
  "ROC area"
)
to.write.sxxp <- data.frame(list (
  format(as.POSIXlt(Sys.time()),'%Y-%m-%d %H:%M:%S'),
  "Adaboost",
  "sxxp",
  "Altman.Z",
  sxxp.adaboost.altman.results$accuracy, #correctly classified instances
  NA, #coverage of cases
  sxxp.adaboost.altman.results$confusion.caret$byClass[1,3], #precision class 0
  sxxp.adaboost.altman.results$confusion.caret$byClass[2,3], #precision class 1
  sxxp.adaboost.altman.results$confusion.caret$byClass[3,3], #precision class 2
  as.numeric(sxxp.adaboost.altman.results$roc$auc) #roc area
))
colnames(to.write.sxxp) <- c(
  "DateStamp",
  "Algorithm",
  "DataSet",
  "Target",
  "Correctly Classified Instances", 
  "Coverage Of cases", 
  "Precision Class 0",
  "Precision Class 1",
  "Precision Class 2",
  "ROC area"
)
to.write.eebp <- data.frame(list (
  format(as.POSIXlt(Sys.time()),'%Y-%m-%d %H:%M:%S'),
  "Adaboost",
  "eebp",
  "Altman.Z",
  eebp.adaboost.altman.results$accuracy, #correctly classified instances
  NA, #coverage of cases
  eebp.adaboost.altman.results$confusion.caret$byClass[1,3], #precision class 0
  eebp.adaboost.altman.results$confusion.caret$byClass[2,3], #precision class 1
  eebp.adaboost.altman.results$confusion.caret$byClass[3,3], #precision class 2
  as.numeric(eebp.adaboost.altman.results$roc$auc) #roc area
))
colnames(to.write.eebp) <- c(
  "DateStamp",
  "Algorithm",
  "DataSet",
  "Target",
  "Correctly Classified Instances", 
  "Coverage Of cases", 
  "Precision Class 0",
  "Precision Class 1",
  "Precision Class 2",
  "ROC area"
)
dbWriteTable(mydb.results, value = to.write.spx, name = "altman_z_results", overwrite = TRUE, row.names=FALSE)
dbWriteTable(mydb.results, value = to.write.sxxp, name = "altman_z_results", append = TRUE, row.names=FALSE)
dbWriteTable(mydb.results, value = to.write.eebp, name = "altman_z_results", append = TRUE, row.names=FALSE)

to.write.spx <- data.frame(list (
  format(as.POSIXlt(Sys.time()),'%Y-%m-%d %H:%M:%S'),
  "J48", #algo
  "spx",
  "Tobins.Q.class",  
  spx.j48.tobin.results$tree.confusion.caret$overall[1], #correctly classified instances
  NA, #coverage of cases
  spx.j48.tobin.results$tree.confusion.caret$byClass[1], #sensitivity / precision class 0
  spx.j48.tobin.results$tree.confusion.caret$byClass[2], #specificity / precision class 1
  as.numeric(spx.j48.tobin.results$tree.roc$auc) #roc area
))
colnames(to.write.spx) <- c(
  "DateStamp",
  "Algorithm",
  "DataSet",
  "Target",
  "Correctly Classified Instances", 
  "Coverage Of cases", 
  "Precision Class 0",
  "Precision Class 1",
  "ROC area"
)
to.write.sxxp <- data.frame(list (
  format(as.POSIXlt(Sys.time()),'%Y-%m-%d %H:%M:%S'),
  "J48", #algo
  "sxxp",
  "Tobins.Q.class", 
  sxxp.j48.tobin.results$tree.confusion.caret$overall[1], #correctly classified instances
  NA, #coverage of cases
  sxxp.j48.tobin.results$tree.confusion.caret$byClass[1], #sensitivity / precision class 0
  sxxp.j48.tobin.results$tree.confusion.caret$byClass[2], #specificity / precision class 1
  as.numeric(sxxp.j48.tobin.results$tree.roc$auc) #roc area
))
colnames(to.write.sxxp) <- c(
  "DateStamp",
  "Algorithm",
  "DataSet",
  "Target",
  "Correctly Classified Instances", 
  "Coverage Of cases", 
  "Precision Class 0",
  "Precision Class 1",
  "ROC area"
)
to.write.eebp <- data.frame(list (
  format(as.POSIXlt(Sys.time()),'%Y-%m-%d %H:%M:%S'),
  "J48", #algo
  "eebp",
  "Tobins.Q.class", 
  eebp.j48.tobin.results$tree.confusion.caret$overall[1], #correctly classified instances
  NA, #coverage of cases
  eebp.j48.tobin.results$tree.confusion.caret$byClass[1], #sensitivity / precision class 0
  eebp.j48.tobin.results$tree.confusion.caret$byClass[2], #specificity / precision class 1
  as.numeric(eebp.j48.tobin.results$tree.roc$auc) #roc area
))
colnames(to.write.eebp) <- c(
  "DateStamp",
  "Algorithm",
  "DataSet",
  "Target",
  "Correctly Classified Instances", 
  "Coverage Of cases", 
  "Precision Class 0",
  "Precision Class 1",
  "ROC area"
)
dbWriteTable(mydb.results, value = to.write.spx, name = "tobin_q_results", append = TRUE, row.names=FALSE)
dbWriteTable(mydb.results, value = to.write.sxxp, name = "tobin_q_results", append = TRUE, row.names=FALSE)
dbWriteTable(mydb.results, value = to.write.eebp, name = "tobin_q_results", append = TRUE, row.names=FALSE)


to.write.spx <- data.frame(list (
  format(as.POSIXlt(Sys.time()),'%Y-%m-%d %H:%M:%S'),
  "J48", #algo
  "spx",
  "Altman.Z",  
  spx.j48.altman.results$tree.confusion.caret$overall[1], #correctly classified instances
  NA, #coverage of cases
  spx.j48.altman.results$tree.confusion.caret$byClass[1,3], #precision class 0
  spx.j48.altman.results$tree.confusion.caret$byClass[2,3], #precision class 1
  spx.j48.altman.results$tree.confusion.caret$byClass[3,3], #precision class 2
  as.numeric(spx.j48.altman.results$tree.roc$auc) #roc area
))
colnames(to.write.spx) <- c(
  "DateStamp",
  "Algorithm",
  "DataSet",
  "Target",
  "Correctly Classified Instances", 
  "Coverage Of cases", 
  "Precision Class 0",
  "Precision Class 1",
  "Precision Class 2",
  "ROC area"
)
to.write.sxxp <- data.frame(list (
  format(as.POSIXlt(Sys.time()),'%Y-%m-%d %H:%M:%S'),
  "J48", #algo
  "sxxp",
  "Altman.Z", 
  sxxp.j48.altman.results$tree.confusion.caret$overall[1], #correctly classified instances
  NA, #coverage of cases
  sxxp.j48.altman.results$tree.confusion.caret$byClass[1,3], #precision class 0
  sxxp.j48.altman.results$tree.confusion.caret$byClass[2,3], #precision class 1
  sxxp.j48.altman.results$tree.confusion.caret$byClass[3,3], #precision class 2
  as.numeric(sxxp.j48.altman.results$tree.roc$auc) #roc area
))
colnames(to.write.sxxp) <- c(
  "DateStamp",
  "Algorithm",
  "DataSet",
  "Target",
  "Correctly Classified Instances", 
  "Coverage Of cases", 
  "Precision Class 0",
  "Precision Class 1",
  "Precision Class 2",
  "ROC area"
)
to.write.eebp <- data.frame(list (
  format(as.POSIXlt(Sys.time()),'%Y-%m-%d %H:%M:%S'),
  "J48", #algo
  "eebp",
  "Altman.Z", 
  eebp.j48.altman.results$tree.confusion.caret$overall[1], #correctly classified instances
  NA, #coverage of cases
  eebp.j48.altman.results$tree.confusion.caret$byClass[1,3], #precision class 0
  eebp.j48.altman.results$tree.confusion.caret$byClass[2,3], #precision class 1
  eebp.j48.altman.results$tree.confusion.caret$byClass[3,3], #precision class 2
  as.numeric(eebp.j48.altman.results$tree.roc$auc) #roc area
))
colnames(to.write.eebp) <- c(
  "DateStamp",
  "Algorithm",
  "DataSet",
  "Target",
  "Correctly Classified Instances", 
  "Coverage Of cases", 
  "Precision Class 0",
  "Precision Class 1",
  "Precision Class 2",
  "ROC area"
)
dbWriteTable(mydb.results, value = to.write.spx, name = "altman_z_results", append = TRUE, row.names=FALSE)
dbWriteTable(mydb.results, value = to.write.sxxp, name = "altman_z_results", append = TRUE, row.names=FALSE)
dbWriteTable(mydb.results, value = to.write.eebp, name = "altman_z_results", append = TRUE, row.names=FALSE)

