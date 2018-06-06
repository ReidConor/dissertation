#!/usr/bin/env Rscript
library(RMySQL)
library(sqldf)
#get data
mydb_imputed  <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_imputed')
mydb_imputed_scaled  <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_imputed_scaled')
mydb_raw <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov')


spx.imputed <- dbReadTable(conn=mydb_imputed,name='spx')
spx.imputed$Feml.CEO.or.Equiv <- (as.numeric(as.factor(spx.imputed$Feml.CEO.or.Equiv)))
spx.imputed$AZS.class <- (as.numeric(as.factor(spx.imputed$AZS.class)))
spx.imputed$Frmr.CEO.or.its.Equiv.on.Bd <- (as.numeric(as.factor(spx.imputed$Frmr.CEO.or.its.Equiv.on.Bd)))
spx.imputed$Prsdg.Dir <- (as.numeric(as.factor(spx.imputed$Prsdg.Dir)))
spx.imputed$Indep.Lead.Dir <- (as.numeric(as.factor(spx.imputed$Indep.Lead.Dir)))
spx.imputed$Indep.Chrprsn <- (as.numeric(as.factor(spx.imputed$Indep.Chrprsn)))
spx.imputed$CEO.Duality <- (as.numeric(as.factor(spx.imputed$CEO.Duality)))
spx.imputed$Clssfd.Bd.Sys <- (as.numeric(as.factor(spx.imputed$Clssfd.Bd.Sys)))
#spx$Ticker <- NULL
num.ind <- sapply(spx.imputed, is.numeric)
f <-  function(dataset) {scale(dataset,center = TRUE, scale = TRUE)}
spx.imputed[num.ind] <- lapply(spx.imputed[num.ind], f)


spx.energy <- dbReadTable(conn=mydb_raw,name='spx_energy')
spx.energy.con <- spx.energy[ which(spx.energy$Variable=='ENERGY_CONSUMPTION'), ]
spx.energy.con <- data.frame(spx.energy.con$Ticker,spx.energy.con$X2014)
names <- c("Ticker", "Energy.Consumption")
colnames(spx.energy.con) <- names

dummy <- spx.energy.con
names <- c("Ticker", "Energy.Consumption.Again")
colnames(dummy) <- names
dummy.two <- merge(dummy, spx.energy.con, by="Ticker") 
dim(dummy.two)


spx.to.write <- merge(spx.imputed, spx.energy.con, by="Ticker", all=T) 
dim(spx.to.write)

dbWriteTable(mydb_imputed_scaled, value = spx, name = "spx", overwrite = TRUE,row.names=FALSE ) 
summary(spx$Ticker)
