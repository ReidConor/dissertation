#!/usr/bin/env Rscript

#get data in from mysql
library(RMySQL)
mydb_raw <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov')
spx<-dbReadTable(conn=mydb_raw,name='spx')
sxxp<-dbReadTable(conn=mydb_raw,name='sxxp')
eebp<-dbReadTable(conn=mydb_raw,name='eebp')

#discretise tobins Q into two classes based on median
spx$Tobins.Q.class <- ifelse(spx$Tobins.Q >= median(spx$Tobins.Q, na.rm=TRUE), 1, 0)
sxxp$Tobins.Q.class <- ifelse(sxxp$Tobins.Q >= median(sxxp$Tobins.Q, na.rm=TRUE), 1, 0)
eebp$Tobins.Q.class <- ifelse(eebp$Tobins.Q >= median(eebp$Tobins.Q, na.rm=TRUE), 1, 0)

#discretise altman z into three classes based on thresholds
safe_min<-2.99
safe_min_emerging<-2.6
grey_min<-1.81
grey_min_emerging<-1.1
spx$AZS.class<-cut(spx$AZS, breaks = c(-0.01, grey_min, safe_min, Inf), labels = c("distress","grey","safe"))
sxxp$AZS.class<-cut(sxxp$AZS, breaks = c(-0.01, grey_min, safe_min, Inf), labels = c("distress","grey","safe"))
eebp$AZS.class<-cut(eebp$AZS, breaks = c(-0.01, grey_min_emerging, safe_min_emerging, Inf), labels = c("distress","grey","safe"))
#see the split between safe/grey/distress for each dataset
azs.analysis<-data.frame(c(table(sxxp$AZS.class)),c(table(eebp$AZS.class)),c(table(spx$AZS.class)))
colnames(azs.analysis) <- c("sxxp", "eebp","spx")
#azs.analysis

#write back to mysql
mydb_processed <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_processed')
dbWriteTable(mydb_processed, value = spx, name = "spx", overwrite = TRUE ) 
dbWriteTable(mydb_processed, value = sxxp, name = "sxxp", overwrite = TRUE ) 
dbWriteTable(mydb_processed, value = eebp, name = "eebp", overwrite = TRUE ) 


spx.cgcp<-dbReadTable(conn=mydb_processed,name='spx_cgcp')
spx.cgcp["EightVarEq.class"]<-ifelse(spx.cgcp$EightVarEq >= -2.22, 1, 0)
spx.cgcp["FiveVarEq.class"]<-ifelse(spx.cgcp$FiveVarEq >= -2.22, 1, 0)
dbWriteTable(mydb_processed, value = spx.cgcp, name = "spx_cgcp", overwrite = TRUE ) 
