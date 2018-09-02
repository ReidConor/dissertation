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

spx <-  dbReadTable(conn=mydb,name='spx')
spx$Ticker <- as.character(spx$Ticker)
spx$Ticker <- gsub(" ", "", spx$Ticker, fixed = TRUE)
spx.mscore <- merge (x=spx, y=spx.mscore.flattened.calc, by.x='Ticker', by.y='Ticker', all.x = TRUE)


#dbWriteTable(
#  mydb.processed, 
#  value = spx.mscore, 
#  name = "spx_mscore", 
#  overwrite = TRUE,
#  row.names=FALSE
#) 
remove(list = ls()[!c(grepl("spx.mscore", ls()))])
mydb <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov')
mydb.processed <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_processed')



# CSR
#
spx <- dbReadTable(conn=mydb.processed,name='spx')
spx.csr <- dbReadTable(conn=mydb,name='spx_csr')
spx.csr.reduced <- spx.csr[c("Ticker","Variable", "X2014")]

#esg disc
spx.csr.esg.disc <- spx.csr.reduced[spx.csr.reduced$Variable == "ESG_DISCLOSURE_SCORE",]
spx.csr.esg.disc <- data.frame(spx.csr.esg.disc$Ticker, spx.csr.esg.disc$X2014) 
colnames(spx.csr.esg.disc) <- c("Ticker","ESG_DISCLOSURE_SCORE")

#fair renum
spx.csr.fair.renum <- spx.csr.reduced[spx.csr.reduced$Variable == "FAIR_REMUNERATION_POLICY",]
spx.csr.fair.renum <- data.frame(spx.csr.fair.renum$Ticker, spx.csr.fair.renum$X2014) 
colnames(spx.csr.fair.renum) <- c("Ticker","FAIR_REMUNERATION_POLICY")

#social disc
spx.csr.social.disc <- spx.csr.reduced[spx.csr.reduced$Variable == "SOCIAL_DISCLOSURE_SCORE",]
spx.csr.social.disc <- data.frame(spx.csr.social.disc$Ticker, spx.csr.social.disc$X2014) 
colnames(spx.csr.social.disc) <- c("Ticker","SOCIAL_DISCLOSURE_SCORE")

#eq oppertunities
spx.csr.eq.opp <- spx.csr.reduced[spx.csr.reduced$Variable == "EQUAL_OPPORTUNITY_POLICY",]
spx.csr.eq.opp <- data.frame(spx.csr.eq.opp$Ticker, spx.csr.eq.opp$X2014) 
colnames(spx.csr.eq.opp) <- c("Ticker","EQUAL_OPPORTUNITY_POLICY")

#anti bribery
spx.csr.ant.brib <- spx.csr.reduced[spx.csr.reduced$Variable == "ANTI-BRIBERY_ETHICS_POLICY",]
spx.csr.ant.brib <- data.frame(spx.csr.ant.brib$Ticker, spx.csr.ant.brib$X2014) 
colnames(spx.csr.ant.brib) <- c("Ticker","ANTI-BRIBERY_ETHICS_POLICY")

spx$Ticker <- gsub(" ", "", spx$Ticker, fixed = TRUE)
spx.csr.esg.disc$Ticker <- gsub(" ", "", spx.csr.esg.disc$Ticker, fixed = TRUE)
spx.csr.fair.renum$Ticker <- gsub(" ", "", spx.csr.fair.renum$Ticker, fixed = TRUE)
spx.csr.social.disc$Ticker <- gsub(" ", "", spx.csr.social.disc$Ticker, fixed = TRUE)
spx.csr.eq.opp$Ticker <- gsub(" ", "", spx.csr.eq.opp$Ticker, fixed = TRUE)
spx.csr.ant.brib$Ticker <- gsub(" ", "", spx.csr.ant.brib$Ticker, fixed = TRUE)


spx.csr.final <- merge(x=spx, y=spx.csr.esg.disc, by.x="Ticker", by.y="Ticker", all.x = TRUE)
spx.csr.final <- merge(x=spx.csr.final, y=spx.csr.fair.renum, by.x="Ticker", by.y="Ticker", all.x = TRUE)
spx.csr.final <- merge(x=spx.csr.final, y=spx.csr.social.disc, by.x="Ticker", by.y="Ticker", all.x = TRUE)
spx.csr.final <- merge(x=spx.csr.final, y=spx.csr.eq.opp, by.x="Ticker", by.y="Ticker", all.x = TRUE)
spx.csr.final <- merge(x=spx.csr.final, y=spx.csr.ant.brib, by.x="Ticker", by.y="Ticker", all.x = TRUE)
summary(spx.csr.final)

#dbWriteTable(
#  mydb.processed, 
#  value = spx.csr.final, 
#  name = "spx_csr", 
#  overwrite = TRUE,
#  row.names=FALSE
#) 
#remove(list = ls()[!(grepl("mydb", ls()))])
spx.csr.final.mscore <- merge(x=spx.csr.final, y=spx.mscore[c("Ticker","EightVarEq","FiveVarEq","DSRI","GMI","AQI","SGI","DEPI","SGAI","TATA","LVGI")], by.x="Ticker", by.y="Ticker", all.x = TRUE)
nrow(spx.csr.final.mscore)
remove(list = ls()[!c(grepl("spx.csr.final.mscore", ls()))])
mydb <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov')
mydb.processed <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_processed')



# CEO COMP
spx <- dbReadTable(conn=mydb.processed,name='spx')
spx.ceo.comp <- dbReadTable(conn=mydb,name='spx_ceo_comp')

spx.ceo.comp <- data.frame(spx.ceo.comp$Ticker, spx.ceo.comp$X2014) 
colnames(spx.ceo.comp) <- c("Ticker","CEOPay")
spx.ceo.comp$Ticker <- gsub(" ", "", spx.ceo.comp$Ticker, fixed = TRUE)
spx$Ticker <- gsub(" ", "", spx$Ticker, fixed = TRUE)
spx.ceo.comp.final <- merge(x=spx, y=spx.ceo.comp, by.x="Ticker", by.y="Ticker", all.x = TRUE)
nrow(spx.ceo.comp.final)


spx.csr.final.mscore.ceo.comp <- merge(x=spx.csr.final.mscore, y=spx.ceo.comp.final[c("Ticker","CEOPay")], by.x="Ticker", by.y="Ticker", all.x = TRUE)
summary(spx.csr.final.mscore.ceo.comp)



dbWriteTable(
  mydb.processed, 
  value = spx.csr.final.mscore.ceo.comp, 
  name = "spx_cgcp", 
  overwrite = TRUE,
  row.names=FALSE
) 
remove(list = ls()[!(grepl("mydb", ls()))])
