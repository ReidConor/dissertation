library(reshape)
library(stringr)
library(rpart)
setwd("/Users/Conor/Google Drive/MSc/dissertation/data/processed")
#######functions
#function to give me the functionality of trimming string fields

#american 500
spx=read.csv("spx.csv", sep=",", na.strings="#N/A N/A") 
View(spx)
spx$TickerID = str_split_fixed(spx$Ticker, " ", 2)[,1]
summary(spx$Tobin.s.Q)


#get the median and create a class variable for Tobin Q, as per MM
m_TQ = median(subset(spx$Tobin.s.Q, !is.na(spx$Tobin.s.Q)))
m_AZ = median(subset(spx$AZS, !is.na(spx$AZS)))
spx$Tobin.s.Q.class = with(spx, ifelse(Tobin.s.Q >= m_TQ, 1, 2))
spx$AZS.class = with(spx, ifelse(AZS >= m_AZ, 1, 2))

table (spx$Tobin.s.Q.class) #248 248
table (spx$AZS.class)# 210 209
summary(spx$AZS.class)


View(spx)
ggplot(spx,
      aes(AZS.class, Indep.Directors )) +
      geom_point() + 
      geom_boxplot() +
      ggtitle("Indep.Directors and Altman Z score")

summary(spx$AZS.class)

boxplot(Indep.Directors~AZS.class,data=spx, main="Altman Z and Number of Indep Directors", 
        xlab="Altman Z Score class", ylab="Independant Directors")
