library(reshape)
library(stringr)
library(rpart)
setwd("/Users/Conor/Google Drive/MSc/dissertation/data/processed")
#######functions
#function to give me the functionality of trimming string fields
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#american 500
spx=read.csv("spx.csv", sep=",", na.strings="#N/A N/A") 


View(spx)
spx$TickerID = str_split_fixed(spx$Ticker, " ", 2)[,1]
spx$Equity = trim(str_split_fixed(spx$Ticker, " ", 2)[,2])
summary(spx$Tobin.s.Q)
spx = data.frame(spx)
fit = rpart(Tobin.s.Q ~ ., data=spx, method="anova") 
printcp(fit)
plotcp(fit)
summary(fit)
plot(fit)
text(fit)


spx.sample=spx[c("Tobin.s.Q", "X..Women.on.Bd", "Tax", "Interest", "Asset")]
View(spx.sample)
fit <- rpart(Tobin.s.Q ~ . , 
             method="anova", data=spx.sample)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(pfit) # detailed summary of splits
plot(fit, uniform=TRUE, 
     main="Regression Tree for Tobins Q ")
text(fit, use.n=TRUE, all=TRUE, cex=.8)
pfit<- prune(fit, cp=0.042) # from cptable   
plot(pfit, uniform=TRUE, 
     main="Pruned Regression Tree for Tobins Q")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)






#data definition
library(lattice)
library(ggplot2)
spx_EDA = spx[c("Ticker", "Tobin.s.Q", "P.E", "EPS", "P.B", "P.EBITDA", 
                "Board.Size", "CEO.Duality", "X..Feml.Execs", 
                "X..Feml.Execs.1", "Bd.Avg.Age", "Board.Mtg.Att..")]
spx_EDA[ spx_EDA == "#N/A Field Not Applicable" ] <- NA
sapply(spx_EDA,class)
cols.num <- c("P.B","EPS","P.E","P.EBITDA")
spx_EDA[cols.num] <- sapply(spx_EDA[cols.num],as.numeric)
sapply(spx_EDA, class)



View(spx_EDA)
#exploration around Tobins Q 
#distrib
ggplot(data=spx_EDA) +
  geom_histogram( aes(Tobin.s.Q, ..density..), bins=40 ) +
  geom_density( aes(Tobin.s.Q, ..density..) ) +
  geom_rug( aes(Tobin.s.Q) ) +
  geom_vline(aes(xintercept=mean(Tobin.s.Q, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=median(Tobin.s.Q, na.rm=T)),   # Ignore NA values for mean
             color="blue", linetype="dashed", size=1)
#use log Q score
ggplot(data=spx_EDA) +
  geom_histogram( aes(log(Tobin.s.Q), ..density..), bins=40 ) +
  geom_density( aes(log(Tobin.s.Q), ..density..) ) +
  geom_rug( aes(log(Tobin.s.Q) )) +
  geom_vline(aes(xintercept=mean(log(Tobin.s.Q), na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=median(log(Tobin.s.Q), na.rm=T)),   # Ignore NA values for mean
             color="blue", linetype="dashed", size=1)

##P.B
ggplot(spx_EDA, aes(P.B, Tobin.s.Q) ) +
  geom_point()
  #with labels
  ggplot(spx_EDA, aes(P.B, Tobin.s.Q, label=TickerID) ) +
    geom_point() +geom_text(size = 3)

#no difference
ggplot(data=subset(spx_EDA, !is.na(CEO.Duality)),
  aes(CEO.Duality, Tobin.s.Q )) +
  geom_point() + geom_boxplot()

#board average age
ggplot(data=spx_EDA) +
  geom_histogram( aes(Bd.Avg.Age, ..density..) ) +
  geom_density( aes(Bd.Avg.Age, ..density..) ) +
  geom_rug( aes(Bd.Avg.Age) )

#female presence on board and success?
ggplot(spx_EDA, aes(Tobin.s.Q, X..Feml.Execs) ) +
  geom_point()

