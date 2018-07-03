#!/usr/bin/env Rscript

#get data in from mysql
library(RMySQL)
mydb_processed <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_processed')
mydb_imputed <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_imputed')
spx <- dbReadTable(conn=mydb_processed,name='spx')
spx.cgcp <- dbReadTable(conn=mydb_processed,name='spx_cgcp')
spx.ceo.comp <- dbReadTable(conn=mydb_processed,name='spx_ceo_comp')
sxxp <- dbReadTable(conn=mydb_processed,name='sxxp')
eebp <- dbReadTable(conn=mydb_processed,name='eebp')

#impute
impute <- function(dataset){
  set.seed(1)
  library(mice)
  imputation.maxit <- 15
  imputation.m <- 5
  imputation.method <- "cart"

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
spx.cgcp.imputed <- impute(spx.cgcp)
spx.ceo.comp.imputed <- impute(spx.ceo.comp)
sxxp.imputed <- impute(sxxp)
eebp.imputed <- impute(eebp)

#handle eebp being a dick
eebp.imputed$Bd.Age.Limit <- NULL
eebp.imputed$Dvd.Yld <- NULL
eebp.imputed$Clssfd.Bd.Sys <- NULL
eebp.imputed$Prsdg.Dir <- NULL
eebp.imputed$CEO.Duality <- NULL

#get complete cases
spx.imputed.complete=spx.imputed[complete.cases(spx.imputed), ]
spx.cgcp.imputed.complete=spx.cgcp.imputed[complete.cases(spx.cgcp.imputed), ]
spx.ceo.comp.imputed.complete=spx.ceo.comp.imputed[complete.cases(spx.ceo.comp.imputed), ]
sxxp.imputed.complete=sxxp.imputed[complete.cases(sxxp.imputed), ]
eebp.imputed.complete=eebp.imputed[complete.cases(eebp.imputed), ]

#sort out the esg disc bin score
esg.disc.mean <- mean(spx.cgcp.imputed.complete$ESG_DISCLOSURE_SCORE)
spx.cgcp.imputed.complete["esg_disc_score_bin"] <- ifelse(spx.cgcp.imputed.complete$ESG_DISCLOSURE_SCORE > esg.disc.mean, 1, 0)
spx.cgcp.imputed.complete$ESG_DISCLOSURE_SCORE <- NULL


#write back to mysql
mydb_imputed <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_imputed')
dbWriteTable(mydb_imputed, value = spx.imputed.complete, name = "spx", overwrite = TRUE )
dbWriteTable(mydb_imputed, value = spx.cgcp.imputed.complete, name = "spx_cgcp", overwrite = TRUE )
dbWriteTable(mydb_imputed, value = spx.ceo.comp.imputed.complete, name = "spx_ceo_comp", overwrite = TRUE )
dbWriteTable(mydb_imputed, value = sxxp.imputed.complete, name = "sxxp", overwrite = TRUE )
dbWriteTable(mydb_imputed, value = eebp.imputed.complete, name = "eebp", overwrite = TRUE )
