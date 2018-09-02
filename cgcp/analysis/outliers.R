#!/usr/bin/env Rscript
library(RMySQL)
#get data
mydb <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_processed')
mydb.raw <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov')
mydb.complete <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_complete_cases')
spx <- dbReadTable(conn=mydb.raw,name='spx')
sxxp <- dbReadTable(conn=mydb.raw,name='sxxp')
eebp <- dbReadTable(conn=mydb.raw,name='eebp')

eebp$AZS.class <- NULL
eebp$AZS <- NULL
eebp$Tobins.Q.class <- NULL
eebp$Ticker <- NULL

sxxp$AZS.class <- NULL
sxxp$AZS <- NULL
sxxp$Tobins.Q.class <- NULL
sxxp$Ticker <- NULL

summary(eebp)
View(eebp)
eebp$Feml.CEO.or.Equiv <- as.numeric(as.factor(eebp$Feml.CEO.or.Equiv))
eebp$Prsdg.Dir <- as.numeric(as.factor(eebp$Prsdg.Dir))
eebp$Clssfd.Bd.Sys <- as.numeric(as.factor(eebp$Clssfd.Bd.Sys))
eebp$Indep.Lead.Dir <- as.numeric(as.factor(eebp$Indep.Lead.Dir))
eebp$CEO.Duality <- as.numeric(as.factor(eebp$CEO.Duality))
eebp$Indep.Chrprsn <- as.numeric(as.factor(eebp$Indep.Chrprsn))
eebp$Frmr.CEO.or.its.Equiv.on.Bd <- as.numeric(as.factor(eebp$Frmr.CEO.or.its.Equiv.on.Bd))
eebp$Dvd.Yld <- NULL
eebp$X..Indep.Dir.on.Nom.Cmte <- NULL
summary(eebp)
seq(ncol(eebp))
sapply(eebp, function(x) is.na(x))


mod <- lm(Tobins.Q ~. , data = eebp)
summary(mod)

cooksd <- cooks.distance(mod)


plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

