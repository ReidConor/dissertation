#!/usr/bin/env Rscript
library(RMySQL)
library(reshape)
#get data
mydb <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov')
mydb.processed <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_processed')


# Beneish M-Score
#
#
#
#
#
#
spx.mscore <- dbReadTable(conn=mydb,name='spx_mscore')

spx.mscore.reduced <- spx.mscore[c("Ticker","Variable", "X12_2013", "X10_2014")]

variables <- unique(spx.mscore.reduced$Variable)
tickers <- unique(spx.mscore.reduced$Ticker)
spx.mscore.flattened <- data.frame(tickers)
colnames(spx.mscore.flattened) <- c("Ticker")

for (var in variables){
  data <- spx.mscore.reduced[spx.mscore.reduced$Variable == var,]
  data$Variable <- NULL
  colnames(data) <- c("Ticker",paste(var,"_2013", sep=""),paste(var,"_2014", sep=""))

  spx.mscore.flattened <- merge (x=spx.mscore.flattened, y=data, by.x='Ticker', by.y='Ticker')

}

#replace na's with average for now...might need to impute
for(i in 1:ncol(spx.mscore.flattened)){
  spx.mscore.flattened[is.na(spx.mscore.flattened[,i]), i] <- mean(spx.mscore.flattened[,i], na.rm = TRUE)
}


##************
# DSRI
##
DSRI <- data.frame (spx.mscore.flattened$Ticker, (spx.mscore.flattened$TRAIL_12M_NET_SALES_2013 / spx.mscore.flattened$TRAIL_12M_NET_SALES_2014) /
        (spx.mscore.flattened$BS_ACCTS_REC_EXCL_NOTES_REC_2013 / spx.mscore.flattened$BS_ACCTS_REC_EXCL_NOTES_REC_2014))
colnames(DSRI) <- c("Ticker","DSRI")
##************
# GMI
##
GMI <- data.frame(
  spx.mscore.flattened$Ticker, 
  ((spx.mscore.flattened$TRAIL_12M_NET_SALES_2014 - spx.mscore.flattened$ARD_COST_OF_GOODS_SOLD_2014) / spx.mscore.flattened$TRAIL_12M_NET_SALES_2014) /
  ((spx.mscore.flattened$TRAIL_12M_NET_SALES_2013 - spx.mscore.flattened$ARD_COST_OF_GOODS_SOLD_2013) / spx.mscore.flattened$TRAIL_12M_NET_SALES_2013)
)
colnames(GMI) <- c("Ticker","GMI")
##************
# AQI
##
AQI <- data.frame(
  spx.mscore.flattened$Ticker,
  ((spx.mscore.flattened$BS_TOT_ASSET_2014 - spx.mscore.flattened$ARD_PROPERTY_PLANT_EQUIP_NET_2014 - spx.mscore.flattened$BS_CUR_ASSET_REPORT_2014) / spx.mscore.flattened$BS_TOT_ASSET_2014 ) / 
  ((spx.mscore.flattened$BS_TOT_ASSET_2013 - spx.mscore.flattened$ARD_PROPERTY_PLANT_EQUIP_NET_2013 - spx.mscore.flattened$BS_CUR_ASSET_REPORT_2013) / spx.mscore.flattened$BS_TOT_ASSET_2013 )
)
colnames(AQI) <- c("Ticker","AQI")
##************
# SGI
##
SGI <- data.frame(
  spx.mscore.flattened$Ticker,
  (spx.mscore.flattened$TRAIL_12M_NET_SALES_2014 / spx.mscore.flattened$TRAIL_12M_NET_SALES_2013)
)
colnames(SGI) <- c("Ticker","SGI")
##************
# DEPI
##
DEPI <- data.frame(
  spx.mscore.flattened$Ticker,
  (spx.mscore.flattened$CF_DEPR_AMORT_2014 / (spx.mscore.flattened$CF_DEPR_AMORT_2014 + spx.mscore.flattened$ARD_PROPERTY_PLANT_EQUIP_NET_2014)) / 
  (spx.mscore.flattened$CF_DEPR_AMORT_2013 / (spx.mscore.flattened$CF_DEPR_AMORT_2013 + spx.mscore.flattened$ARD_PROPERTY_PLANT_EQUIP_NET_2013))
)
colnames(DEPI) <- c("Ticker","DEPI")
##************
# SGAI
##
SGAI <- data.frame(
  spx.mscore.flattened$Ticker,
  (spx.mscore.flattened$ARD_SELLING_GENERAL_ADMIN_EXP_2014 / spx.mscore.flattened$TRAIL_12M_NET_SALES_2014) /
  (spx.mscore.flattened$ARD_SELLING_GENERAL_ADMIN_EXP_2013 / spx.mscore.flattened$TRAIL_12M_NET_SALES_2013)
)
colnames(SGAI) <- c("Ticker","SGAI")
##************
# TATA
##
TATA <- data.frame(
  spx.mscore.flattened$Ticker,
  (spx.mscore.flattened$NET_INCOME_2013 - spx.mscore.flattened$CF_CASH_FROM_OPER_2013) / (spx.mscore.flattened$BS_TOT_ASSET_2013)
) 
colnames(TATA) <- c("Ticker","TATA")
##************
# LVGI
##
LVGI <- data.frame(
  spx.mscore.flattened$Ticker,
  ((spx.mscore.flattened$BS_LT_BORROW_2014+spx.mscore.flattened$BS_CUR_LIAB_2014)/(spx.mscore.flattened$BS_TOT_ASSET_2014)) /
  ((spx.mscore.flattened$BS_LT_BORROW_2013+spx.mscore.flattened$BS_CUR_LIAB_2013)/(spx.mscore.flattened$BS_TOT_ASSET_2013))
)  
colnames(LVGI) <- c("Ticker","LVGI")

spx.mscore.flattened.calc <- DSRI
spx.mscore.flattened.calc <- merge (x=spx.mscore.flattened.calc, y=GMI, by.x='Ticker', by.y='Ticker')  
spx.mscore.flattened.calc <- merge (x=spx.mscore.flattened.calc, y=AQI, by.x='Ticker', by.y='Ticker')  
spx.mscore.flattened.calc <- merge (x=spx.mscore.flattened.calc, y=SGI, by.x='Ticker', by.y='Ticker')  
spx.mscore.flattened.calc <- merge (x=spx.mscore.flattened.calc, y=DEPI, by.x='Ticker', by.y='Ticker')  
spx.mscore.flattened.calc <- merge (x=spx.mscore.flattened.calc, y=SGAI, by.x='Ticker', by.y='Ticker')  
spx.mscore.flattened.calc <- merge (x=spx.mscore.flattened.calc, y=TATA, by.x='Ticker', by.y='Ticker')  
spx.mscore.flattened.calc <- merge (x=spx.mscore.flattened.calc, y=LVGI, by.x='Ticker', by.y='Ticker')  

spx.mscore.flattened.calc["FiveVarEq"] <- -6.065 + 0.823*(spx.mscore.flattened.calc$DSRI) + 0.906*(spx.mscore.flattened.calc$GMI) + 0.593*(spx.mscore.flattened.calc$AQI) + 0.717*(spx.mscore.flattened.calc$SGI) + 0.107*(spx.mscore.flattened.calc$DEPI)
spx.mscore.flattened.calc["EightVarEq"] <- -4.84 + 0.920*(spx.mscore.flattened.calc$DSRI) + 0.528*(spx.mscore.flattened.calc$GMI) + 0.404*(spx.mscore.flattened.calc$AQI) + 0.892*(spx.mscore.flattened.calc$SGI) + 0.115*(spx.mscore.flattened.calc$DEPI) - 0.172*(spx.mscore.flattened.calc$SGAI) + 4.679*(spx.mscore.flattened.calc$TATA) - 0.327*(spx.mscore.flattened.calc$LVGI)
spx.mscore.flattened.calc$Ticker <- as.character(spx.mscore.flattened.calc$Ticker)
spx.mscore.flattened.calc$Ticker <- gsub(" ", "", spx.mscore.flattened.calc$Ticker, fixed = TRUE)







# CSR
#
#
#
#
#
#
spx.csr <- dbReadTable(conn=mydb,name='spx_csr')
spx.csr.reduced <- spx.csr[c("Ticker","Variable", "X2014")]
spx.csr.reduced <- spx.csr.reduced[spx.csr.reduced$Variable == "ESG_DISCLOSURE_SCORE",]
for(i in 1:ncol(spx.csr.reduced)){
  spx.csr.reduced[is.na(spx.csr.reduced[,i]), i] <- mean(spx.csr.reduced[,i], na.rm = TRUE)
}
spx.csr.reduced <- data.frame(spx.csr.reduced$Ticker, spx.csr.reduced$X2014) 
colnames(spx.csr.reduced) <- c("Ticker","ESG_DISCLOSURE_SCORE")
spx.csr.reduced$Ticker <- gsub(" ", "", spx.csr.reduced$Ticker, fixed = TRUE)
# Merge and write
#
#
#
#
#
#
spx <- dbReadTable(conn=mydb.processed,name='spx')
spx$Ticker <- gsub(" ", "", spx$Ticker, fixed = TRUE)

to.write <- merge(x=spx, y=spx.mscore.flattened.calc, by.x="Ticker", by.y="Ticker", all.x = TRUE)
to.write <- merge(x=to.write, y=spx.csr.reduced, by.x="Ticker", by.y="Ticker", all.x = TRUE)

summary(to.write)

dbWriteTable(
  mydb.processed, 
  value = to.write, 
  name = "spx_cgcp", 
  overwrite = TRUE,
  row.names=FALSE
) 
