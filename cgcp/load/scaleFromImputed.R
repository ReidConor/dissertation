#!/usr/bin/env Rscript
library(RMySQL)
library(sqldf)
library(DMwR)

#get data
mydb_imputed  <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_imputed')
mydb_imputed_scaled  <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_imputed_scaled')


spx.imputed <- dbReadTable(conn=mydb_imputed,name='spx')
spx.imputed$AZS.class <- (as.numeric(as.factor(spx.imputed$AZS.class)))
spx.imputed$Frmr.CEO.or.its.Equiv.on.Bd <- (as.numeric(as.factor(spx.imputed$Frmr.CEO.or.its.Equiv.on.Bd)))
spx.imputed$Prsdg.Dir <- (as.numeric(as.factor(spx.imputed$Prsdg.Dir)))
spx.imputed$Indep.Lead.Dir <- (as.numeric(as.factor(spx.imputed$Indep.Lead.Dir)))
spx.imputed$Indep.Chrprsn <- (as.numeric(as.factor(spx.imputed$Indep.Chrprsn)))
spx.imputed$CEO.Duality <- (as.numeric(as.factor(spx.imputed$CEO.Duality)))
spx.imputed$Clssfd.Bd.Sys <- (as.numeric(as.factor(spx.imputed$Clssfd.Bd.Sys)))
spx.imputed$Ticker <- NULL
summary(spx.imputed)
dontScale <- c(grep("Feml.CEO.or.Equiv", colnames(spx.imputed)))
spx.imputed[, -dontScale] <- scale(spx.imputed[, -dontScale])
summary(spx.imputed)


dbWriteTable(mydb_imputed_scaled, value = spx.imputed, name = "spx", overwrite = TRUE,row.names=FALSE ) 


#Female CEO as treatment
spx.imputed$Feml.CEO.or.Equiv <- ifelse(spx.imputed$Feml.CEO.or.Equiv == 'Y', 1, 0)
spx.imputed$Feml.CEO.or.Equiv <- as.factor(spx.imputed$Feml.CEO.or.Equiv)

spx.imputed.bal.fceo <- SMOTE(Feml.CEO.or.Equiv ~ ., spx.imputed, perc.over = 100, perc.under=200)
spx.imputed.bal.fceo$Feml.CEO.or.Equiv <- as.numeric(spx.imputed.bal.fceo$Feml.CEO.or.Equiv)
spx.imputed.bal.fceo$Feml.CEO.or.Equiv <- ifelse(spx.imputed.bal.fceo$Feml.CEO.or.Equiv == 2, 1, 0)

dbWriteTable(mydb_imputed_scaled, value = spx.imputed.bal.fceo, name = "spx_fceo", overwrite = TRUE,row.names=FALSE ) 


#ESG Disclosure Score
spx.cgcp.imputed <- dbReadTable(conn=mydb_imputed,name='spx_cgcp')
spx.cgcp.imputed$AZS.class <- (as.numeric(as.factor(spx.cgcp.imputed$AZS.class)))
spx.cgcp.imputed$Frmr.CEO.or.its.Equiv.on.Bd <- (as.numeric(as.factor(spx.cgcp.imputed$Frmr.CEO.or.its.Equiv.on.Bd)))
spx.cgcp.imputed$Prsdg.Dir <- (as.numeric(as.factor(spx.cgcp.imputed$Prsdg.Dir)))
spx.cgcp.imputed$Indep.Lead.Dir <- (as.numeric(as.factor(spx.cgcp.imputed$Indep.Lead.Dir)))
spx.cgcp.imputed$Indep.Chrprsn <- (as.numeric(as.factor(spx.cgcp.imputed$Indep.Chrprsn)))
spx.cgcp.imputed$CEO.Duality <- (as.numeric(as.factor(spx.cgcp.imputed$CEO.Duality)))
spx.cgcp.imputed$Clssfd.Bd.Sys <- (as.numeric(as.factor(spx.cgcp.imputed$Clssfd.Bd.Sys)))
spx.cgcp.imputed$Ticker <- NULL

dontScale <- c(grep("Feml.CEO.or.Equiv", colnames(spx.cgcp.imputed)),grep("esg_disc_score_bin", colnames(spx.cgcp.imputed)))
spx.cgcp.imputed[, -dontScale] <- scale(spx.cgcp.imputed[, -dontScale])

dbWriteTable(mydb_imputed_scaled, value = spx.cgcp.imputed, name = "spx_esg_disc", overwrite = TRUE,row.names=FALSE ) 
