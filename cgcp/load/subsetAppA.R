#!/usr/bin/env Rscript
library(RMySQL)

mydb <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_processed')
spx <- dbReadTable(conn=mydb,name='spx')
sxxp <- dbReadTable(conn=mydb,name='sxxp')
eebp <- dbReadTable(conn=mydb,name='eebp')

appendixA <- c(
  "Tax",
  "Interest",
  "Asset",
  "Fincl..l",
  "Oper.ROE",
  "Dvd.P.O",
  "Board.Size",
  "X..NonExec.Dir.on.Bd",
  "X..Indep.Directors",
  "CEO.Duality",
  "Indep.Chrprsn",
  "Indep.Lead.Dir",
  "Frmr.CEO.or.its.Equiv.on.Bd",
  "X..Women.on.Bd",
  "Indep.Dir.Bd.Mtg.Att..",
  "Unit.or.2.Tier.Bd.Sys",
  "Prsdg.Dir",
  "X..Feml.Execs",
  "Feml.CEO.or.Equiv", 
  "Age.Young.Dir",
  "Bd.Avg.Age",
  "Board.Duration",
  "Board.Mtgs..", 
  "Exec.Dir.Bd.Dur",
  "Tobins.Q",
  "Tobins.Q.class",
  "AZS",
  "AZS.class"
)
spx.reduced <- spx[ , (names(spx) %in% appendixA)]
sxxp.reduced <- sxxp[ , (names(sxxp) %in% appendixA)]
eebp.reduced <- eebp[ , (names(eebp) %in% appendixA)]

dbWriteTable(mydb, value = spx.reduced, name = "spx_sub_app_a", overwrite = TRUE, row.names=FALSE)
dbWriteTable(mydb, value = sxxp.reduced, name = "sxxp_sub_app_a", overwrite = TRUE, row.names=FALSE)
dbWriteTable(mydb, value = eebp.reduced, name = "eebp_sub_app_a", overwrite = TRUE, row.names=FALSE)



