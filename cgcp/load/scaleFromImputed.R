#!/usr/bin/env Rscript
library(RMySQL)
#get data
mydb_imputed  <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_imputed')
spx<-dbReadTable(conn=mydb_imputed,name='spx')

spx$Feml.CEO.or.Equiv <- (as.numeric(as.factor(spx$Feml.CEO.or.Equiv)))
spx$AZS.class <- (as.numeric(as.factor(spx$AZS.class)))
spx$Frmr.CEO.or.its.Equiv.on.Bd <- (as.numeric(as.factor(spx$Frmr.CEO.or.its.Equiv.on.Bd)))
spx$Prsdg.Dir <- (as.numeric(as.factor(spx$Prsdg.Dir)))
spx$Indep.Lead.Dir <- (as.numeric(as.factor(spx$Indep.Lead.Dir)))
spx$Indep.Chrprsn <- (as.numeric(as.factor(spx$Indep.Chrprsn)))
spx$CEO.Duality <- (as.numeric(as.factor(spx$CEO.Duality)))
spx$Clssfd.Bd.Sys <- (as.numeric(as.factor(spx$Clssfd.Bd.Sys)))
spx$Ticker <- NULL

spx.scale <- data.frame(scale(spx, center = TRUE, scale = TRUE))
dim(spx.scale)
mydb_imputed_scaled  <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_imputed_scaled')
dbWriteTable(mydb_imputed_scaled, value = spx.scale, name = "spx", overwrite = TRUE ) 
