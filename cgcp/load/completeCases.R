#!/usr/bin/env Rscript
#
# creates dataframes of complete cases per index, then writes to the corp_gov_complete_cases db
#
library(RMySQL)
#get data
mydb <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_processed')
spx <- dbReadTable(conn=mydb,name='spx')
sxxp <- dbReadTable(conn=mydb,name='sxxp')
eebp <- dbReadTable(conn=mydb,name='eebp')

#get complete cases
spx.complete=spx[complete.cases(spx), ]
sxxp.complete=sxxp[complete.cases(sxxp), ]
eebp.complete=eebp[complete.cases(eebp), ]

#write back to mysql
mydb_complete_cases <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_complete_cases')
dbWriteTable(mydb_complete_cases, value = spx.complete, name = "spx", overwrite = TRUE ) 
dbWriteTable(mydb_complete_cases, value = sxxp.complete, name = "sxxp", overwrite = TRUE ) 
dbWriteTable(mydb_complete_cases, value = eebp.complete, name = "eebp", overwrite = TRUE ) 
