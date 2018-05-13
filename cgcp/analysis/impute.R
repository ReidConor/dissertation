#!/usr/bin/env Rscript
#get data
library(RMySQL)
mydb <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_processed')
spx <- dbReadTable(conn=mydb,name='spx')
summary(spx)
dim(spx) #500 61
spx.reduced <- spx[ , !(names(spx) %in% c("Bd.Age.Limit","Interest"))]
  #Bd.Age.Limit has 374 missing....very unimportant from classification task done previously 
  #ROC has 167 missing....fairly important
  #Interest has 73 missing....not very important


drops <- c("Ticker",
           "AZS", #one of these (the target) will be added back in
           "AZS.class", #one of these (the target) will be added back in
           "Tobins.Q", #one of these (the target) will be added back in
           "Tobins.Q.class", #one of these (the target) will be added back in
           "Bd.Age.Limit", #too many missing values, and not important
           "Interest", #too many missing values, and not important
           "Indep.Dir.Bd.Mtg.Att..", #cant impute correctly, also not important
           "Exec.Dir.Bd.Dur", #cant impute correctly, also not important
           "Board.Duration",
           "Sz.Aud.Cmte",
           "X..Empl.Reps.on.Bd"
)
target <- "Tobins.Q"
drops <- drops[drops != target]#dont want to remove whatever is passed as the target
data.reduced <- spx[ , !(names(spx) %in% drops)] #remove unwanted columns
dim(data.reduced)
colnames(data.reduced)[colnames(data.reduced) == target] <- 'target'
data.reduced <- data.frame(data.reduced)
data.reduced$Feml.CEO.or.Equiv <- as.numeric(as.factor(data.reduced$Feml.CEO.or.Equiv))
data.reduced$Prsdg.Dir <- as.numeric(as.factor(data.reduced$Prsdg.Dir))
data.reduced$Clssfd.Bd.Sys <- as.numeric(as.factor(data.reduced$Clssfd.Bd.Sys))
data.reduced$Indep.Lead.Dir <- as.numeric(as.factor(data.reduced$Indep.Lead.Dir))
data.reduced$CEO.Duality <- as.numeric(as.factor(data.reduced$CEO.Duality))
data.reduced$Indep.Chrprsn <- as.numeric(as.factor(data.reduced$Indep.Chrprsn))
data.reduced$Frmr.CEO.or.its.Equiv.on.Bd <- as.numeric(as.factor(data.reduced$Frmr.CEO.or.its.Equiv.on.Bd))


###############
#Impute
library(mice)
imputation.maxit <- 1
imputation.m <- 5
data.reduced.imputed <- 
  mice(
    data = data.reduced, 
    m = imputation.m, 
    method = "pmm", 
    maxit = imputation.maxit, 
    seed = 500
  )
data.reduced.imputed.stacked <- data.frame()
for(i in 1:data.reduced.imputed$m){
  data.reduced.imputed.stacked <- rbind(
    data.reduced.imputed.stacked, 
    complete(data.reduced.imputed, i)
  )
}
dim(data.reduced.imputed.stacked)
data.reduced.imputed.stacked.complete <-
  data.reduced.imputed.stacked[complete.cases(data.reduced.imputed.stacked), ]
dim(data.reduced.imputed.stacked.complete)


