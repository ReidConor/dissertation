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
library(mice)
linearRegression <- function(dataset, target) {
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
  drops=drops[drops != target]#dont want to remove whatever is passed as the target
  data.reduced<-dataset[ , !(names(dataset) %in% drops)] #remove unwanted columns
  data.reduced<-data.reduced[complete.cases(data.reduced[ , target]),]# we only want records with a class indicator
  data.reduced[[target]]=as.numeric(as.factor(data.reduced[[target]]))
  colnames(data.reduced)[colnames(data.reduced) == target] <- 'target'
  data.reduced<-data.frame(data.reduced)
  
  data.reduced$Feml.CEO.or.Equiv=as.numeric(as.factor(data.reduced$Feml.CEO.or.Equiv))
  data.reduced$Prsdg.Dir=as.numeric(as.factor(data.reduced$Prsdg.Dir))
  data.reduced$Clssfd.Bd.Sys=as.numeric(as.factor(data.reduced$Clssfd.Bd.Sys))
  data.reduced$Indep.Lead.Dir=as.numeric(as.factor(data.reduced$Indep.Lead.Dir))
  data.reduced$CEO.Duality=as.numeric(as.factor(data.reduced$CEO.Duality))
  data.reduced$Indep.Chrprsn=as.numeric(as.factor(data.reduced$Indep.Chrprsn))
  data.reduced$Frmr.CEO.or.its.Equiv.on.Bd=as.numeric(as.factor(data.reduced$Frmr.CEO.or.its.Equiv.on.Bd))
  
  #to impute
  #md.pattern(data.reduced)
  #library(VIM) #for visualizations of the magnitude of missing values
  data.reduced.imputed = mice(data = data.reduced, m = 5, method = "pmm", maxit = 1, seed = 500)
  #data.reduced.imputed.stacked = as.mids(data.reduced.imputed)
  
  #to check the balance of the target variable
  #plot(density(data.reduced$target), main="Density Plot", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(data.reduced$target), 2))) 
  #polygon(density(data.reduced$target), col="red")
  
  #build the model
  #len <- length(complete(data.reduced.imputed,1)[,1])
  #sub <- sample(1:len,len*training.split)
  #linearModel <- lm(target ~ ., data=data.reduced.imputed.stacked)
  #linearModel <- lm(target ~ ., data=data.reduced.imputed[sub,])
  n <- names(data.reduced)
  f <- as.formula(paste("target ~", paste(n[!n %in% "y"], collapse = " + ")))
  print(f)
  fit <- with(data.reduced.imputed,lm(f))
  ###analyse model
  #print(cor(data.reduced))
  #print(alias(linearModel))
  #summary(linearModel)
  #combine <- pool(fit)
  #summary(combine)
  
} 
linearRegression(spx,"Tobins.Q")



#2. apply classification on regression
#3. results analysis and discussion
