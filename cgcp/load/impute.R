#!/usr/bin/env Rscript

#get data in from mysql
library(RMySQL)
mydb_processed <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_processed')
spx <- dbReadTable(conn=mydb_processed,name='spx')
spx.mscore <- dbReadTable(conn=mydb_processed,name='spx_mscore')
sxxp <- dbReadTable(conn=mydb_processed,name='sxxp')
eebp <- dbReadTable(conn=mydb_processed,name='eebp')

#impute
impute <- function(dataset){
  set.seed(1)
  library(mice)
  imputation.maxit <- 15
  imputation.m <- 5
  imputation.method <- "pmm"

  dataset.imputed <- mice(
    data = dataset,
    m = imputation.m,
    method = imputation.method,
    maxit = imputation.maxit,
    seed = 500
  )
  dataset.imputed.stacked <- data.frame()
  for(i in 1:imputation.m){
    dataset.imputed.stacked <- rbind(dataset.imputed.stacked, complete(dataset.imputed, i))
  }
  return(dataset.imputed.stacked)
}
spx.imputed <- impute(spx)
spx.mscore.imputed <- impute(spx.mscore)
sxxp.imputed <- impute(sxxp)
eebp.imputed <- impute(eebp)


#get complete cases
spx.imputed.complete=spx.imputed[complete.cases(spx.imputed), ]
spx.mscore.imputed.complete=spx.mscore.imputed[complete.cases(spx.mscore.imputed), ]
sxxp.imputed.complet=sxxp.imputed[complete.cases(sxxp.imputed), ]
eebp.imputed.complete=eebp.imputed[complete.cases(eebp.imputed), ]

#write back to mysql
mydb_imputed <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_imputed')
dbWriteTable(mydb_imputed, value = spx.imputed.complete, name = "spx", overwrite = TRUE )
dbWriteTable(mydb_imputed, value = spx.mscore.imputed.complete, name = "spx_mscore", overwrite = TRUE )
dbWriteTable(mydb_imputed, value = sxxp.imputed.complet, name = "sxxp", overwrite = TRUE )
dbWriteTable(mydb_imputed, value = eebp.imputed.complete, name = "eebp", overwrite = TRUE )
