#!/usr/bin/env Rscript
library(RMySQL)
library(sqldf)
library(DMwR)

#get data
mydb_imputed  <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_imputed')
mydb_imputed_scaled  <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_imputed_scaled')
mydb_raw <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov')

spx.imputed <- dbReadTable(conn=mydb_imputed,name='spx')
#spx.imputed$Feml.CEO.or.Equiv <- (as.numeric(as.factor(spx.imputed$Feml.CEO.or.Equiv)))
spx.imputed$AZS.class <- (as.numeric(as.factor(spx.imputed$AZS.class)))
spx.imputed$Frmr.CEO.or.its.Equiv.on.Bd <- (as.numeric(as.factor(spx.imputed$Frmr.CEO.or.its.Equiv.on.Bd)))
spx.imputed$Prsdg.Dir <- (as.numeric(as.factor(spx.imputed$Prsdg.Dir)))
spx.imputed$Indep.Lead.Dir <- (as.numeric(as.factor(spx.imputed$Indep.Lead.Dir)))
spx.imputed$Indep.Chrprsn <- (as.numeric(as.factor(spx.imputed$Indep.Chrprsn)))
spx.imputed$CEO.Duality <- (as.numeric(as.factor(spx.imputed$CEO.Duality)))
spx.imputed$Clssfd.Bd.Sys <- (as.numeric(as.factor(spx.imputed$Clssfd.Bd.Sys)))
spx.imputed$Ticker <- NULL

dontScale <- c(grep("Ticker", colnames(spx.imputed)), grep("Feml.CEO.or.Equiv", colnames(spx.imputed)))
spx.imputed[, -dontScale] <- scale(spx.imputed[, -dontScale])

spx.imputed$Feml.CEO.or.Equiv <- ifelse(spx.imputed$Feml.CEO.or.Equiv == 'Y', 1, 0)

summary(as.factor(spx.imputed$Feml.CEO.or.Equiv))
spx.imputed$Feml.CEO.or.Equiv <- as.factor(spx.imputed$Feml.CEO.or.Equiv)

spx.imputed.bal <- SMOTE(Feml.CEO.or.Equiv ~ ., spx.imputed, perc.over = 100, perc.under=200)
spx.imputed.bal$Feml.CEO.or.Equiv <- as.numeric(spx.imputed.bal$Feml.CEO.or.Equiv)
spx.imputed.bal$Feml.CEO.or.Equiv <- ifelse(spx.imputed.bal$Feml.CEO.or.Equiv == 2, 1, 0)

dbWriteTable(mydb_imputed_scaled, value = spx.imputed.bal, name = "spx", overwrite = TRUE,row.names=FALSE ) 
