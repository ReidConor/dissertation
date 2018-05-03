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
linearRegressionWithImputationMice <- function(dataset, target) {
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
  
  #to impute
  #md.pattern(data.reduced)
  #library(VIM) #for visualizations of the magnitude of missing values
  data.reduced.imputed <- mice(data = data.reduced, m = 5, method = "pmm", maxit = 1, seed = 500)
  training.models <- list(analyses=vector("list", data.reduced.imputed$m))
  len <- length(complete(data.reduced.imputed,1)[,1])
  sub <- sample(1:len,len*training.split)
  
  #build the model
  for(i in 1:data.reduced.imputed$m){
    training.models$analyses[[i]] <- lm(target ~ Tax + Norm.NI.to.NI.for.Cmn.. + Interest + OPM.T12M + 
                              Asset + Fincl..l + Oper.ROE + X5Yr.Avg.Adj.ROE + Dvd.P.O + 
                              Sust.Gr.Rt + EBITDA.Sh + P.E + EPS + P.B + P.EBITDA + ROE + 
                              EV.EBITDA.T12M + Net.Debt.to.EBITDA + Cash.Gen.Cash.Reqd + 
                              Dvd.Yld + Board.Size + X..Empl.Reps.on.Bd + 
                              Clssfd.Bd.Sys + X..Non.Exec.Dir.on.Bd + X..NonExec.Dir.on.Bd + 
                              X..Indep.Directors + Indep.Directors + CEO.Duality + Indep.Chrprsn + 
                              Indep.Lead.Dir + Prsdg.Dir + Frmr.CEO.or.its.Equiv.on.Bd + 
                              X..Women.on.Bd + X..Wmn.on.Bd + X..Feml.Execs + X..Feml.Execs.1 + 
                              Feml.CEO.or.Equiv + X..Execs...Co.Mgrs + Age.Young.Dir + 
                              BOD.Age.Rng +  Bd.Avg.Age + Board.Duration + 
                              Board.Mtgs.. + Board.Mtg.Att.. + Indep.Dir.Bd.Mtg.Att.. + 
                              Sz.Aud.Cmte + X..Indep.Dir.on.Aud.Cmte + X..Indep.Dir.on.Comp.Cmte + 
                              X..Indep.Dir.on.Nom.Cmte, data=complete(data.reduced.imputed, i)[sub,])

  }
  class(training.models) <- "mira"
  combined.training.models <- pool(training.models)
  #summary(combined.training.models)
  
  #predict the test set
  #is this possible?
  #https://github.com/stefvanbuuren/mice/issues/32
  
} 
linearRegressionWithImputationMice(spx,"Tobins.Q")



#2. apply classification on regression
#3. results analysis and discussion
