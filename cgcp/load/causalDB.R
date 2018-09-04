#!/usr/bin/env Rscript
library(RMySQL)
library(sqldf)
library(DMwR)

#make connections
mydb_imputed <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_imputed')
mydb_causal <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_causal')

#**************************************************
#           S&P
#**************************************************
spx <- function(){
  spx.imputed <- dbReadTable(conn=mydb_imputed,name='spx')
  spx.imputed$AZS.class <- (as.numeric(as.factor(spx.imputed$AZS.class)))
  spx.imputed$Frmr.CEO.or.its.Equiv.on.Bd <- (as.numeric(as.factor(spx.imputed$Frmr.CEO.or.its.Equiv.on.Bd)))
  spx.imputed$Prsdg.Dir <- (as.numeric(as.factor(spx.imputed$Prsdg.Dir)))
  spx.imputed$Indep.Lead.Dir <- (as.numeric(as.factor(spx.imputed$Indep.Lead.Dir)))
  spx.imputed$Indep.Chrprsn <- (as.numeric(as.factor(spx.imputed$Indep.Chrprsn)))
  spx.imputed$CEO.Duality <- (as.numeric(as.factor(spx.imputed$CEO.Duality)))
  spx.imputed$Clssfd.Bd.Sys <- (as.numeric(as.factor(spx.imputed$Clssfd.Bd.Sys)))
  spx.imputed$Feml.CEO.or.Equiv <- (as.numeric(as.factor(spx.imputed$Feml.CEO.or.Equiv)))
  spx.imputed$Ticker <- NULL
  spx.imputed <- data.frame(scale(spx.imputed))
  
  dbWriteTable(mydb_causal, value = spx.imputed, name = "spx", overwrite = TRUE,row.names=FALSE )
  remove(list = ls()[!(grepl("mydb_", ls()))])
  
}
spx_female_ceo <- function (){
  #SP - Female CEO as treatment
  #
  #
  #
  spx.imputed <- dbReadTable(conn=mydb_imputed,name='spx')
  drops <- c(
    "Tobins.Q",
    "AZS"
  )#leaving tobins q class
  spx.imputed <- spx.imputed[ , !(names(spx.imputed) %in% drops)]
  spx.imputed$AZS.class <- (as.numeric(as.factor(spx.imputed$AZS.class)))
  spx.imputed["AZS.class.Binary"] <- ifelse(spx.imputed$AZS.class == 3, 1, 0)
  spx.imputed$AZS.class <- NULL
  spx.imputed$Frmr.CEO.or.its.Equiv.on.Bd <- (as.numeric(as.factor(spx.imputed$Frmr.CEO.or.its.Equiv.on.Bd)))
  spx.imputed$Prsdg.Dir <- (as.numeric(as.factor(spx.imputed$Prsdg.Dir)))
  spx.imputed$Indep.Lead.Dir <- (as.numeric(as.factor(spx.imputed$Indep.Lead.Dir)))
  spx.imputed$Indep.Chrprsn <- (as.numeric(as.factor(spx.imputed$Indep.Chrprsn)))
  spx.imputed$CEO.Duality <- (as.numeric(as.factor(spx.imputed$CEO.Duality)))
  spx.imputed$Clssfd.Bd.Sys <- (as.numeric(as.factor(spx.imputed$Clssfd.Bd.Sys)))
  spx.imputed$Ticker <- NULL
  dontScale <- c(grep("Feml.CEO.or.Equiv", colnames(spx.imputed)), grep("Tobins.Q.class", colnames(spx.imputed)), grep("AZS.class.Binary", colnames(spx.imputed)))
  spx.imputed[, -dontScale] <- data.frame(scale(spx.imputed[, -dontScale]))
  
  spx.imputed$Feml.CEO.or.Equiv <- ifelse(spx.imputed$Feml.CEO.or.Equiv == 'Y', 1, 0)
  spx.imputed$Feml.CEO.or.Equiv <- as.factor(spx.imputed$Feml.CEO.or.Equiv)
  spx.imputed <- SMOTE(Feml.CEO.or.Equiv ~ ., spx.imputed, perc.over = 900, perc.under=100)
  
  spx.imputed$Feml.CEO.or.Equiv <- ifelse(spx.imputed$Feml.CEO.or.Equiv == 0, as.double(0), as.double(1))

  dbWriteTable(mydb_causal, value = spx.imputed, name = "spx_fceo", overwrite = TRUE,row.names=FALSE ) 
  remove(list = ls()[!(grepl("mydb_", ls()))])
  
}
spx_female_board <- function (){
  #SP - female % on board as treatment
  #
  #
  #
  spx.imputed <- dbReadTable(conn=mydb_imputed,name='spx')
  drops <- c(
    "Tobins.Q",
    "AZS"
  )#leaving tobins q class
  spx.imputed <- spx.imputed[ , !(names(spx.imputed) %in% drops)]
  spx.imputed$AZS.class <- (as.numeric(as.factor(spx.imputed$AZS.class)))
  spx.imputed["AZS.class.Binary"] <- ifelse(spx.imputed$AZS.class == 3, 1, 0)
  spx.imputed$AZS.class <- NULL
  spx.imputed$Frmr.CEO.or.its.Equiv.on.Bd <- (as.numeric(as.factor(spx.imputed$Frmr.CEO.or.its.Equiv.on.Bd)))
  spx.imputed$Prsdg.Dir <- (as.numeric(as.factor(spx.imputed$Prsdg.Dir)))
  spx.imputed$Indep.Lead.Dir <- (as.numeric(as.factor(spx.imputed$Indep.Lead.Dir)))
  spx.imputed$Indep.Chrprsn <- (as.numeric(as.factor(spx.imputed$Indep.Chrprsn)))
  spx.imputed$CEO.Duality <- (as.numeric(as.factor(spx.imputed$CEO.Duality)))
  spx.imputed$Clssfd.Bd.Sys <- (as.numeric(as.factor(spx.imputed$Clssfd.Bd.Sys)))
  spx.imputed$Feml.CEO.or.Equiv <- (as.numeric(as.factor(spx.imputed$Feml.CEO.or.Equiv)))
  spx.imputed$Ticker <- NULL
  dontScale <- c(grep("X..Women.on.Bd", colnames(spx.imputed)), grep("Tobins.Q.class", colnames(spx.imputed)),  grep("AZS.class.Binary", colnames(spx.imputed)))
  spx.imputed[, -dontScale] <- data.frame(scale(spx.imputed[, -dontScale]))
  spx.imputed$X..Women.on.Bd <- ifelse(spx.imputed$X..Women.on.Bd > 20, 1, 0)
  spx.imputed$X..Women.on.Bd <- as.factor(spx.imputed$X..Women.on.Bd)
  spx.imputed <- SMOTE(X..Women.on.Bd ~ ., spx.imputed, perc.over = 100, perc.under=200)
  spx.imputed$X..Women.on.Bd <- ifelse(spx.imputed$X..Women.on.Bd == 0, as.double(0), as.double(1))

  dbWriteTable(mydb_causal, value = spx.imputed, name = "spx_fboard", overwrite = TRUE,row.names=FALSE ) 
  remove(list = ls()[!(grepl("mydb_", ls()))])
  
}
spx_indep_dir_fincl_lev <- function (){
  #SPX - independent lead director / financial leverage higher than 2.5
  #
  #
  #
  spx.imputed <- dbReadTable(conn=mydb_imputed,name='spx')
  drops <- c(
    "Tobins.Q"
    #"AZS"
  )#leaving azs class
  spx.imputed <- spx.imputed[ , !(names(spx.imputed) %in% drops)]
  spx.imputed$AZS.class <- (as.numeric(as.factor(spx.imputed$AZS.class)))
  spx.imputed$Frmr.CEO.or.its.Equiv.on.Bd <- (as.numeric(as.factor(spx.imputed$Frmr.CEO.or.its.Equiv.on.Bd)))
  spx.imputed$Prsdg.Dir <- (as.numeric(as.factor(spx.imputed$Prsdg.Dir)))
  spx.imputed$Indep.Lead.Dir <- ifelse(spx.imputed$Indep.Lead.Dir == 'Y', as.double(1), as.double(0))
  spx.imputed$Indep.Chrprsn <- (as.numeric(as.factor(spx.imputed$Indep.Chrprsn)))
  spx.imputed$CEO.Duality <- (as.numeric(as.factor(spx.imputed$CEO.Duality)))
  spx.imputed$Clssfd.Bd.Sys <- (as.numeric(as.factor(spx.imputed$Clssfd.Bd.Sys)))
  spx.imputed$Feml.CEO.or.Equiv <- (as.numeric(as.factor(spx.imputed$Feml.CEO.or.Equiv)))
  spx.imputed$Ticker <- NULL
  summary(spx.imputed)
  dontScale <- c(grep("Indep.Lead.Dir", colnames(spx.imputed)), grep("Fincl..l", colnames(spx.imputed)), grep("AZS.class", colnames(spx.imputed)),  grep("AZS", colnames(spx.imputed)), grep("Tobins.Q.class", colnames(spx.imputed)))
  spx.imputed[, -dontScale] <- data.frame(scale(spx.imputed[, -dontScale]))
  spx.imputed["Indep.Lead.Dir.Fincl..l"] <- ifelse(spx.imputed$Indep.Lead.Dir == 1 & spx.imputed$Fincl..l > 2.5, as.double(1), as.double(0))
  spx.imputed["AZS.class.Binary"] <- ifelse(spx.imputed$AZS.class == 3, 1, 0)
  spx.imputed$AZS.class <- NULL
  summary(spx.imputed)
  table(spx.imputed$Indep.Lead.Dir.Fincl..l)
  spx.imputed$Indep.Lead.Dir.Fincl..l <- as.factor(spx.imputed$Indep.Lead.Dir.Fincl..l)
  
  spx.imputed <- SMOTE(Indep.Lead.Dir.Fincl..l ~ ., spx.imputed, perc.over = 100, perc.under=200)
  spx.imputed$Indep.Lead.Dir.Fincl..l <- ifelse(spx.imputed$Indep.Lead.Dir.Fincl..l == 0, as.double(0), as.double(1))
  
  
  dbWriteTable(mydb_causal, value = spx.imputed, name = "spx_indepdirfincl", overwrite = TRUE,row.names=FALSE ) 
  remove(list = ls()[!(grepl("mydb_", ls()))])
  
}
spx_esg_disc <- function (){
  #SP - ESG Disclosure Score
  #
  #
  #
  spx.cgcp.imputed <- dbReadTable(conn=mydb_imputed,name='spx_cgcp')
  spx.cgcp.imputed$AZS.class <- (as.numeric(as.factor(spx.cgcp.imputed$AZS.class)))
  spx.cgcp.imputed$Frmr.CEO.or.its.Equiv.on.Bd <- (as.numeric(as.factor(spx.cgcp.imputed$Frmr.CEO.or.its.Equiv.on.Bd)))
  spx.cgcp.imputed$Prsdg.Dir <- (as.numeric(as.factor(spx.cgcp.imputed$Prsdg.Dir)))
  spx.cgcp.imputed$Indep.Lead.Dir <- (as.numeric(as.factor(spx.cgcp.imputed$Indep.Lead.Dir)))
  spx.cgcp.imputed$Indep.Chrprsn <- (as.numeric(as.factor(spx.cgcp.imputed$Indep.Chrprsn)))
  spx.cgcp.imputed$CEO.Duality <- (as.numeric(as.factor(spx.cgcp.imputed$CEO.Duality)))
  spx.cgcp.imputed$Clssfd.Bd.Sys <- (as.numeric(as.factor(spx.cgcp.imputed$Clssfd.Bd.Sys)))
  spx.cgcp.imputed$Feml.CEO.or.Equiv <- (as.numeric(as.factor(spx.cgcp.imputed$Feml.CEO.or.Equiv)))
  spx.cgcp.imputed$Ticker <- NULL
  dontScale <- c(grep("esg_disc_score_bin", colnames(spx.cgcp.imputed)))
  spx.cgcp.imputed[, -dontScale] <- data.frame(scale(spx.cgcp.imputed[, -dontScale]))
  summary(spx.cgcp.imputed)
  dbWriteTable(mydb_causal, value = spx.cgcp.imputed, name = "spx_esg_disc", overwrite = TRUE,row.names=FALSE ) 
  remove(list = ls()[!(grepl("mydb_", ls()))])
  
}  
spx_agerange <- function(){
  #SPX - age range of board members as treatment
  #
  #
  #
  spx.imputed <- dbReadTable(conn=mydb_imputed,name='spx')
  drops <- c(
    "Tobins.Q",
    "AZS"
  )#leaving tobins q class
  spx.imputed <- spx.imputed[ , !(names(spx.imputed) %in% drops)]
  spx.imputed$AZS.class <- (as.numeric(as.factor(spx.imputed$AZS.class)))
  spx.imputed["AZS.class.Binary"] <- ifelse(spx.imputed$AZS.class == 3, 1, 0)
  spx.imputed$AZS.class <- NULL
  spx.imputed$Indep.Lead.Dir <- (as.numeric(as.factor(spx.imputed$Indep.Lead.Dir)))
  spx.imputed$Indep.Chrprsn <- (as.numeric(as.factor(spx.imputed$Indep.Chrprsn)))
  spx.imputed$Feml.CEO.or.Equiv <- (as.numeric(as.factor(spx.imputed$Feml.CEO.or.Equiv)))
  spx.imputed$Frmr.CEO.or.its.Equiv.on.Bd <- (as.numeric(as.factor(spx.imputed$Frmr.CEO.or.its.Equiv.on.Bd)))
  spx.imputed$CEO.Duality <- (as.numeric(as.factor(spx.imputed$CEO.Duality)))
  spx.imputed$Prsdg.Dir <- (as.numeric(as.factor(spx.imputed$Prsdg.Dir)))
  spx.imputed$Clssfd.Bd.Sys <- (as.numeric(as.factor(spx.imputed$Clssfd.Bd.Sys)))
  spx.imputed$Ticker <- NULL
  summary(spx.imputed)
  
  dontScale <- c(grep("BOD.Age.Rng", colnames(spx.imputed)),grep("Tobins.Q.class", colnames(spx.imputed)),grep("AZS.class.Binary", colnames(spx.imputed)))
  spx.imputed[, -dontScale] <- data.frame(scale(spx.imputed[, -dontScale]))
  spx.imputed$BOD.Age.Rng <- ifelse(spx.imputed$BOD.Age.Rng > mean(spx.imputed$BOD.Age.Rng), 1, 0)
  #spx.imputed$BOD.Age.Rng <- as.factor(spx.imputed$BOD.Age.Rng)
  #spx.imputed.other <- SMOTE(BOD.Age.Rng ~ ., spx.imputed, perc.over = 100, perc.under=200)
  spx.imputed$BOD.Age.Rng <- ifelse(spx.imputed$BOD.Age.Rng == 0, as.double(0), as.double(1))
  
  dbWriteTable(mydb_causal, value = spx.imputed, name = "spx_agerange", overwrite = TRUE,row.names=FALSE ) 
  remove(list = ls()[!(grepl("mydb_", ls()))])
  
}
spx_indepChFmlCEO <- function(){
  #SPX - indep chairman or female ceo
  #
  #
  #
  spx.imputed <- dbReadTable(conn=mydb_imputed,name='spx')
  drops <- c(
    "Tobins.Q",
    "AZS"
  )#leaving altman class
  spx.imputed <- spx.imputed[ , !(names(spx.imputed) %in% drops)]
  spx.imputed$AZS.class <- (as.numeric(as.factor(spx.imputed$AZS.class)))
  spx.imputed$Indep.Lead.Dir <- (as.numeric(as.factor(spx.imputed$Indep.Lead.Dir)))
  spx.imputed$Frmr.CEO.or.its.Equiv.on.Bd <- (as.numeric(as.factor(spx.imputed$Frmr.CEO.or.its.Equiv.on.Bd)))
  spx.imputed$CEO.Duality <- (as.numeric(as.factor(spx.imputed$CEO.Duality)))
  spx.imputed$Prsdg.Dir <- (as.numeric(as.factor(spx.imputed$Prsdg.Dir)))
  spx.imputed$Clssfd.Bd.Sys <- (as.numeric(as.factor(spx.imputed$Clssfd.Bd.Sys)))
  spx.imputed$Ticker <- NULL
  
  spx.imputed$Indep.Chrprsn <- ifelse(spx.imputed$Indep.Chrprsn == "Y", 1, 0)
  spx.imputed$Feml.CEO.or.Equiv <- ifelse(spx.imputed$Feml.CEO.or.Equiv == "Y", 1, 0)
  spx.imputed["Indep.Chrprsn.Feml.CEO.or.Equiv"] <- ifelse(spx.imputed$Indep.Chrprsn == 1 | spx.imputed$Feml.CEO.or.Equiv == 1, 1, 0)
  spx.imputed["AZS.class.Binary"] <- ifelse(spx.imputed$AZS.class == 3, 1, 0)
  spx.imputed$AZS.class <- NULL
  summary(spx.imputed)
  
  dontScale <- c(grep("Indep.Chrprsn.Feml.CEO.or.Equiv", colnames(spx.imputed)), grep("AZS.class.Binary", colnames(spx.imputed)), grep("Tobins.Q.class", colnames(spx.imputed)))
  spx.imputed[, -dontScale] <- data.frame(scale(spx.imputed[, -dontScale]))
  
  #spx.imputed$AZS.class.Binary <- as.factor(spx.imputed$AZS.class.Binary)
  #spx.imputed <- SMOTE(AZS.class.Binary ~ ., spx.imputed, perc.over = 100, perc.under=200)
  #spx.imputed$AZS.class.Binary <- ifelse(spx.imputed$AZS.class.Binary == 0, as.double(0), as.double(1))
  
  dbWriteTable(mydb_causal, value = spx.imputed, name = "spx_indepChFmlCEO", overwrite = TRUE,row.names=FALSE ) 
  remove(list = ls()[!(grepl("mydb_", ls()))])
  
}
spx_fl <- function(){
  #SPX - financial leverage > 4 treatment
  #
  #
  #
  spx.imputed <- dbReadTable(conn=mydb_imputed,name='spx')
  drops <- c(
    "Tobins.Q",
    "AZS"
  )#leaving tobins q class
  spx.imputed <- spx.imputed[ , !(names(spx.imputed) %in% drops)]
  spx.imputed$AZS.class <- (as.numeric(as.factor(spx.imputed$AZS.class)))
  spx.imputed["AZS.class.Binary"] <- ifelse(spx.imputed$AZS.class == 3, 1, 0)
  spx.imputed$AZS.class <- NULL
  spx.imputed$Indep.Lead.Dir <- (as.numeric(as.factor(spx.imputed$Indep.Lead.Dir)))
  spx.imputed$Indep.Chrprsn <- (as.numeric(as.factor(spx.imputed$Indep.Chrprsn)))
  spx.imputed$Feml.CEO.or.Equiv <- (as.numeric(as.factor(spx.imputed$Feml.CEO.or.Equiv)))
  spx.imputed$Frmr.CEO.or.its.Equiv.on.Bd <- (as.numeric(as.factor(spx.imputed$Frmr.CEO.or.its.Equiv.on.Bd)))
  spx.imputed$CEO.Duality <- (as.numeric(as.factor(spx.imputed$CEO.Duality)))
  spx.imputed$Prsdg.Dir <- (as.numeric(as.factor(spx.imputed$Prsdg.Dir)))
  spx.imputed$Clssfd.Bd.Sys <- (as.numeric(as.factor(spx.imputed$Clssfd.Bd.Sys)))
  spx.imputed$Ticker <- NULL
  summary(spx.imputed)
  
  dontScale <- c(grep("Fincl..l", colnames(spx.imputed)), grep("Tobins.Q.class", colnames(spx.imputed)),  grep("AZS.class.Binary", colnames(spx.imputed)))
  spx.imputed[, -dontScale] <- data.frame(scale(spx.imputed[, -dontScale]))
  spx.imputed["Fincl.l.treatment"] <- ifelse(spx.imputed$Fincl..l < 4, 1, 0)
  spx.imputed$Fincl.l.treatment <- as.factor(spx.imputed$Fincl.l.treatment)
  spx.imputed$Fincl.l.treatment <- ifelse(spx.imputed$Fincl.l.treatment == 0, as.double(0), as.double(1))
  summary(spx.imputed)
  dbWriteTable(mydb_causal, value = spx.imputed, name = "spx_fl", overwrite = TRUE,row.names=FALSE ) 
  remove(list = ls()[!(grepl("mydb_", ls()))])
  
}
spx_indepDir_FormerCEOBoard <- function(){
  spx.imputed <- dbReadTable(conn=mydb_imputed,name='spx')
  drops <- c(
    "Tobins.Q",
    "AZS"
  )#leaving tobins q class
  spx.imputed <- spx.imputed[ , !(names(spx.imputed) %in% drops)]
  spx.imputed$AZS.class <- (as.numeric(as.factor(spx.imputed$AZS.class)))
  spx.imputed["AZS.class.Binary"] <- ifelse(spx.imputed$AZS.class == 3, 1, 0)
  spx.imputed$AZS.class <- NULL
  spx.imputed$Frmr.CEO.or.its.Equiv.on.Bd <- ifelse(spx.imputed$Frmr.CEO.or.its.Equiv.on.Bd == 'Y', as.double(1), as.double(0))
  spx.imputed$Prsdg.Dir <- (as.numeric(as.factor(spx.imputed$Prsdg.Dir)))
  spx.imputed$Indep.Lead.Dir <- ifelse(spx.imputed$Indep.Lead.Dir == 'Y', as.double(1), as.double(0))
  spx.imputed$Indep.Chrprsn <- (as.numeric(as.factor(spx.imputed$Indep.Chrprsn)))
  spx.imputed$CEO.Duality <- (as.numeric(as.factor(spx.imputed$CEO.Duality)))
  spx.imputed$Clssfd.Bd.Sys <- (as.numeric(as.factor(spx.imputed$Clssfd.Bd.Sys)))
  spx.imputed$Feml.CEO.or.Equiv <- ifelse(spx.imputed$Feml.CEO.or.Equiv == 'Y', as.double(1), as.double(0))
  spx.imputed$Ticker <- NULL
  summary(spx.imputed)
  
  dontScale <- c(grep("Indep.Lead.Dir", colnames(spx.imputed)), grep("Frmr.CEO.or.its.Equiv.on.Bd", colnames(spx.imputed)), grep("Tobins.Q.class", colnames(spx.imputed)), grep("AZS.class.Binary", colnames(spx.imputed)))
  spx.imputed[, -dontScale] <- data.frame(scale(spx.imputed[, -dontScale]))
  
  spx.imputed["Indep.Lead.Dir.Former.CEO.on.Board"] <- ifelse(spx.imputed$Indep.Lead.Dir == 1 & spx.imputed$Frmr.CEO.or.its.Equiv.on.Bd == 1, as.double(1), as.double(0))
  
  spx.imputed$Indep.Lead.Dir.Former.CEO.on.Board <- as.factor(spx.imputed$Indep.Lead.Dir.Former.CEO.on.Board)
  #sxxp.imputed <- SMOTE(Indep.Lead.Dir.Former.CEO.on.Board ~ ., sxxp.imputed, perc.over = 100, perc.under=200)
  spx.imputed$Indep.Lead.Dir.Former.CEO.on.Board <- ifelse(spx.imputed$Indep.Lead.Dir.Former.CEO.on.Board == 1, as.double(1), as.double(0))
  
  dbWriteTable(mydb_causal, value = spx.imputed, name = "spx_indepdirformerceo", overwrite = TRUE,row.names=FALSE ) 
  remove(list = ls()[!(grepl("mydb_", ls()))])
  
}
spx_ceo_comp <- function() {
  spx.ceo.imputed <- dbReadTable(conn=mydb_imputed,name='spx_cgcp')
  drops <- c(
    "Tobins.Q",
    "AZS"
  )#leaving tobins q class
  spx.ceo.imputed <- spx.ceo.imputed[ , !(names(spx.ceo.imputed) %in% drops)]
  spx.ceo.imputed$AZS.class <- (as.numeric(as.factor(spx.ceo.imputed$AZS.class)))
  spx.ceo.imputed["AZS.class.Binary"] <- ifelse(spx.ceo.imputed$AZS.class == 3, 1, 0)
  spx.ceo.imputed$AZS.class <- NULL
  spx.ceo.imputed$Frmr.CEO.or.its.Equiv.on.Bd <- ifelse(spx.ceo.imputed$Frmr.CEO.or.its.Equiv.on.Bd == 'Y', as.double(1), as.double(0))
  spx.ceo.imputed$Prsdg.Dir <- (as.numeric(as.factor(spx.ceo.imputed$Prsdg.Dir)))
  spx.ceo.imputed$Indep.Lead.Dir <- ifelse(spx.ceo.imputed$Indep.Lead.Dir == 'Y', as.double(1), as.double(0))
  spx.ceo.imputed$Indep.Chrprsn <- (as.numeric(as.factor(spx.ceo.imputed$Indep.Chrprsn)))
  spx.ceo.imputed$CEO.Duality <- (as.numeric(as.factor(spx.ceo.imputed$CEO.Duality)))
  spx.ceo.imputed$Clssfd.Bd.Sys <- (as.numeric(as.factor(spx.ceo.imputed$Clssfd.Bd.Sys)))
  spx.ceo.imputed$Feml.CEO.or.Equiv <- ifelse(spx.ceo.imputed$Feml.CEO.or.Equiv == 'Y', as.double(1), as.double(0))
  spx.ceo.imputed$Ticker <- NULL
  summary(spx.ceo.imputed)
  
  dontScale <- c(grep("CEOPay", colnames(spx.ceo.imputed)), grep("Tobins.Q.class", colnames(spx.ceo.imputed)), grep("AZS.class.Binary", colnames(spx.ceo.imputed)))
  spx.ceo.imputed[, -dontScale] <- data.frame(scale(spx.ceo.imputed[, -dontScale]))
  
  spx.ceo.imputed["CEOPayOverMedian"] <- ifelse(spx.ceo.imputed$CEOPay > median(spx.ceo.imputed$CEOPay), as.double(1), as.double(0))
  
  dbWriteTable(mydb_causal, value = spx.ceo.imputed, name = "spx_ceopay", overwrite = TRUE,row.names=FALSE ) 
  remove(list = ls()[!(grepl("mydb_", ls()))])
  
  
}
spx_csr <- function(){
  spx.csr <- dbReadTable(conn=mydb_imputed,name='spx_cgcp')
  drops <- c(
    "Tobins.Q",
    "AZS"
  )#leaving tobins q class
  spx.csr <- spx.csr[ , !(names(spx.csr) %in% drops)]
  spx.csr$AZS.class <- (as.numeric(as.factor(spx.csr$AZS.class)))
  spx.csr["AZS.class.Binary"] <- ifelse(spx.csr$AZS.class == 3, 1, 0)
  spx.csr$AZS.class <- NULL
  spx.csr$Frmr.CEO.or.its.Equiv.on.Bd <- ifelse(spx.csr$Frmr.CEO.or.its.Equiv.on.Bd == 'Y', as.double(1), as.double(0))
  spx.csr$Prsdg.Dir <- (as.numeric(as.factor(spx.csr$Prsdg.Dir)))
  spx.csr$Indep.Lead.Dir <- ifelse(spx.csr$Indep.Lead.Dir == 'Y', as.double(1), as.double(0))
  spx.csr$Indep.Chrprsn <- (as.numeric(as.factor(spx.csr$Indep.Chrprsn)))
  spx.csr$CEO.Duality <- (as.numeric(as.factor(spx.csr$CEO.Duality)))
  spx.csr$Clssfd.Bd.Sys <- (as.numeric(as.factor(spx.csr$Clssfd.Bd.Sys)))
  spx.csr$Feml.CEO.or.Equiv <- ifelse(spx.csr$Feml.CEO.or.Equiv == 'Y', as.double(1), as.double(0))
  spx.csr$Ticker <- NULL
  summary(spx.csr)

  spx.csr["esg_disc_over_avg"] <- ifelse(spx.csr$ESG_DISCLOSURE_SCORE > mean(spx.csr$ESG_DISCLOSURE_SCORE), as.double(1), as.double(0))
  spx.csr["social_disc_over_avg"] <- ifelse(spx.csr$SOCIAL_DISCLOSURE_SCORE > mean(spx.csr$SOCIAL_DISCLOSURE_SCORE), as.double(1), as.double(0))
  
  dbWriteTable(mydb_causal, value = spx.csr, name = "spx_csr", overwrite = TRUE,row.names=FALSE ) 
  remove(list = ls()[!(grepl("mydb_", ls()))])
  
}


spx()
spx_female_ceo()
spx_female_board()
spx_indep_dir_fincl_lev()
spx_esg_disc()
spx_agerange()
spx_indepChFmlCEO()
spx_fl()
spx_indepDir_FormerCEOBoard()
spx_ceo_comp()
spx_csr()

#**************************************************
#           S&P MScore
#**************************************************
spx_indep_dir_fincl_lev <- function (){
  #SPX - independent lead director / financial leverage higher than 2.5
  #
  #
  #
  spx.mscore.imputed <- dbReadTable(conn=mydb_imputed,name='spx_cgcp')
  drops <- c(
    "Tobins.Q",
    "Tobins.Q.class",
    "AZS",
    "AZS.class",
    "DSRI",
    "GMI",
    "SGI",
    "DEPI",
    "SGAI",
    "TATA",
    "LVGI",
    "AQI",
    "FiveVarEq"
  )
  spx.mscore.imputed <- spx.mscore.imputed[ , !(names(spx.mscore.imputed) %in% drops)]
  summary(spx.mscore.imputed)
  spx.mscore.imputed$Frmr.CEO.or.its.Equiv.on.Bd <- (as.numeric(as.factor(spx.mscore.imputed$Frmr.CEO.or.its.Equiv.on.Bd)))
  spx.mscore.imputed$Prsdg.Dir <- (as.numeric(as.factor(spx.mscore.imputed$Prsdg.Dir)))
  spx.mscore.imputed$Indep.Lead.Dir <- ifelse(spx.mscore.imputed$Indep.Lead.Dir == 'Y', as.double(1), as.double(0))
  spx.mscore.imputed$Indep.Chrprsn <- (as.numeric(as.factor(spx.mscore.imputed$Indep.Chrprsn)))
  spx.mscore.imputed$CEO.Duality <- (as.numeric(as.factor(spx.mscore.imputed$CEO.Duality)))
  spx.mscore.imputed$Clssfd.Bd.Sys <- (as.numeric(as.factor(spx.mscore.imputed$Clssfd.Bd.Sys)))
  spx.mscore.imputed$Feml.CEO.or.Equiv <- (as.numeric(as.factor(spx.mscore.imputed$Feml.CEO.or.Equiv)))
  spx.mscore.imputed$Ticker <- NULL
  summary(spx.mscore.imputed)
  dontScale <- c(grep("Indep.Lead.Dir", colnames(spx.mscore.imputed)), grep("Fincl..l", colnames(spx.mscore.imputed)), grep("EightVarEq", colnames(spx.mscore.imputed)))
  spx.mscore.imputed[, -dontScale] <- data.frame(scale(spx.mscore.imputed[, -dontScale]))
  spx.mscore.imputed["Indep.Lead.Dir.Fincl..l"] <- ifelse(spx.mscore.imputed$Indep.Lead.Dir == 1 & spx.mscore.imputed$Fincl..l > 2.5, as.double(1), as.double(0))
  
  spx.mscore.imputed["MScore"] <- ifelse(spx.mscore.imputed$EightVarEq > -2.22,as.double(1), as.double(0))
  
  dbWriteTable(mydb_causal, value = spx.mscore.imputed, name = "spx_mscore_indepdirfincl", overwrite = TRUE,row.names=FALSE ) 
  remove(list = ls()[!(grepl("mydb_", ls()))])
  
}
spx_fl <- function(){
  #SPX - financial leverage > 4 treatment
  #
  #
  #
  spx.mscore.imputed <- dbReadTable(conn=mydb_imputed,name='spx_cgcp')
  drops <- c(
    "Tobins.Q",
    "Tobins.Q.class",
    "AZS",
    "AZS.class",
    "DSRI",
    "GMI",
    "SGI",
    "DEPI",
    "SGAI",
    "TATA",
    "LVGI",
    "AQI",
    "FiveVarEq"
  )
  spx.mscore.imputed <- spx.mscore.imputed[ , !(names(spx.mscore.imputed) %in% drops)]
  spx.mscore.imputed$Indep.Lead.Dir <- (as.numeric(as.factor(spx.mscore.imputed$Indep.Lead.Dir)))
  spx.mscore.imputed$Indep.Chrprsn <- (as.numeric(as.factor(spx.mscore.imputed$Indep.Chrprsn)))
  spx.mscore.imputed$Feml.CEO.or.Equiv <- (as.numeric(as.factor(spx.mscore.imputed$Feml.CEO.or.Equiv)))
  spx.mscore.imputed$Frmr.CEO.or.its.Equiv.on.Bd <- (as.numeric(as.factor(spx.mscore.imputed$Frmr.CEO.or.its.Equiv.on.Bd)))
  spx.mscore.imputed$CEO.Duality <- (as.numeric(as.factor(spx.mscore.imputed$CEO.Duality)))
  spx.mscore.imputed$Prsdg.Dir <- (as.numeric(as.factor(spx.mscore.imputed$Prsdg.Dir)))
  spx.mscore.imputed$Clssfd.Bd.Sys <- (as.numeric(as.factor(spx.mscore.imputed$Clssfd.Bd.Sys)))
  spx.mscore.imputed$Ticker <- NULL

  dontScale <- c(grep("Fincl..l", colnames(spx.mscore.imputed)), grep("EightVarEq", colnames(spx.mscore.imputed)))
  spx.mscore.imputed[, -dontScale] <- data.frame(scale(spx.mscore.imputed[, -dontScale]))
  
  spx.mscore.imputed["Fincl.l.treatment"] <- ifelse(spx.mscore.imputed$Fincl..l < 2.5, as.double(1), as.double(0))
  spx.mscore.imputed["MScore"] <- ifelse(spx.mscore.imputed$EightVarEq > -2.22,as.double(1), as.double(0))
  spx.mscore.imputed$EightVarEq <- NULL
  
  summary(spx.mscore.imputed)
  
  dbWriteTable(mydb_causal, value = spx.mscore.imputed, name = "spx_mscore_fl", overwrite = TRUE,row.names=FALSE ) 
  remove(list = ls()[!(grepl("mydb_", ls()))])
  
}

spx_indep_dir_fincl_lev()
spx_fl()


#**************************************************
#           SXXP
#**************************************************
sxxp <- function(){
  sxxp.imputed <- dbReadTable(conn=mydb_imputed,name='sxxp')
  sxxp.imputed$AZS.class <- (as.numeric(as.factor(sxxp.imputed$AZS.class)))
  sxxp.imputed$Frmr.CEO.or.its.Equiv.on.Bd <- (as.numeric(as.factor(sxxp.imputed$Frmr.CEO.or.its.Equiv.on.Bd)))
  sxxp.imputed$Prsdg.Dir <- (as.numeric(as.factor(sxxp.imputed$Prsdg.Dir)))
  sxxp.imputed$Indep.Lead.Dir <- (as.numeric(as.factor(sxxp.imputed$Indep.Lead.Dir)))
  sxxp.imputed$Indep.Chrprsn <- (as.numeric(as.factor(sxxp.imputed$Indep.Chrprsn)))
  sxxp.imputed$CEO.Duality <- (as.numeric(as.factor(sxxp.imputed$CEO.Duality)))
  sxxp.imputed$Clssfd.Bd.Sys <- (as.numeric(as.factor(sxxp.imputed$Clssfd.Bd.Sys)))
  sxxp.imputed$Feml.CEO.or.Equiv <- (as.numeric(as.factor(sxxp.imputed$Feml.CEO.or.Equiv)))
  sxxp.imputed$Ticker <- NULL
  sxxp.imputed <- data.frame(scale(sxxp.imputed))
  summary(sxxp.imputed)
  dbWriteTable(mydb_causal, value = sxxp.imputed, name = "sxxp", overwrite = TRUE,row.names=FALSE ) 
  remove(list = ls()[!(grepl("mydb_", ls()))])
  
}
sxxp_indepDir_FormerCEOBoard <- function(){
  sxxp.imputed <- dbReadTable(conn=mydb_imputed,name='sxxp')
  drops <- c(
    "Tobins.Q",
    "AZS"
  )#leaving tobins q class
  sxxp.imputed <- sxxp.imputed[ , !(names(sxxp.imputed) %in% drops)]
  sxxp.imputed$AZS.class <- (as.numeric(as.factor(sxxp.imputed$AZS.class)))
  sxxp.imputed["AZS.class.Binary"] <- ifelse(sxxp.imputed$AZS.class == 3, 1, 0)
  sxxp.imputed$AZS.class <- NULL
  sxxp.imputed$Frmr.CEO.or.its.Equiv.on.Bd <- ifelse(sxxp.imputed$Frmr.CEO.or.its.Equiv.on.Bd == 'Y', as.double(1), as.double(0))
  sxxp.imputed$Prsdg.Dir <- (as.numeric(as.factor(sxxp.imputed$Prsdg.Dir)))
  sxxp.imputed$Indep.Lead.Dir <- ifelse(sxxp.imputed$Indep.Lead.Dir == 'Y', as.double(1), as.double(0))
  sxxp.imputed$Indep.Chrprsn <- (as.numeric(as.factor(sxxp.imputed$Indep.Chrprsn)))
  sxxp.imputed$CEO.Duality <- (as.numeric(as.factor(sxxp.imputed$CEO.Duality)))
  sxxp.imputed$Clssfd.Bd.Sys <- (as.numeric(as.factor(sxxp.imputed$Clssfd.Bd.Sys)))
  sxxp.imputed$Feml.CEO.or.Equiv <- ifelse(sxxp.imputed$Feml.CEO.or.Equiv == 'Y', as.double(1), as.double(0))
  sxxp.imputed$Ticker <- NULL
  summary(sxxp.imputed)
  
  dontScale <- c(grep("Indep.Lead.Dir", colnames(sxxp.imputed)), grep("Frmr.CEO.or.its.Equiv.on.Bd", colnames(sxxp.imputed)), grep("AZS.class.Binary", colnames(sxxp.imputed)))
  sxxp.imputed[, -dontScale] <- data.frame(scale(sxxp.imputed[, -dontScale]))

  sxxp.imputed["Indep.Lead.Dir.Former.CEO.on.Board"] <- ifelse(sxxp.imputed$Indep.Lead.Dir == 1 & sxxp.imputed$Frmr.CEO.or.its.Equiv.on.Bd == 1, as.double(1), as.double(0))
  
  sxxp.imputed$Indep.Lead.Dir.Former.CEO.on.Board <- as.factor(sxxp.imputed$Indep.Lead.Dir.Former.CEO.on.Board)
  sxxp.imputed <- SMOTE(Indep.Lead.Dir.Former.CEO.on.Board ~ ., sxxp.imputed, perc.over = 100, perc.under=200)
  sxxp.imputed$Indep.Lead.Dir.Former.CEO.on.Board <- ifelse(sxxp.imputed$Indep.Lead.Dir.Former.CEO.on.Board == 1, as.double(1), as.double(0))
  
  dbWriteTable(mydb_causal, value = sxxp.imputed, name = "sxxp_indepdirformerceo", overwrite = TRUE,row.names=FALSE ) 
  remove(list = ls()[!(grepl("mydb_", ls()))])
  
}
sxxp_agerange <- function(){
  #SXXP - age range of board members as treatment
  #
  #
  #
  sxxp.imputed <- dbReadTable(conn=mydb_imputed,name='sxxp')
  drops <- c(
    "Tobins.Q",
    "AZS"
  )#leaving tobins q class
  sxxp.imputed <- sxxp.imputed[ , !(names(sxxp.imputed) %in% drops)]
  sxxp.imputed$AZS.class <- (as.numeric(as.factor(sxxp.imputed$AZS.class)))
  sxxp.imputed["AZS.class.Binary"] <- ifelse(sxxp.imputed$AZS.class == 3, 1, 0)
  sxxp.imputed$AZS.class <- NULL
  sxxp.imputed$Indep.Lead.Dir <- (as.numeric(as.factor(sxxp.imputed$Indep.Lead.Dir)))
  sxxp.imputed$Indep.Chrprsn <- (as.numeric(as.factor(sxxp.imputed$Indep.Chrprsn)))
  sxxp.imputed$Feml.CEO.or.Equiv <- (as.numeric(as.factor(sxxp.imputed$Feml.CEO.or.Equiv)))
  sxxp.imputed$Frmr.CEO.or.its.Equiv.on.Bd <- (as.numeric(as.factor(sxxp.imputed$Frmr.CEO.or.its.Equiv.on.Bd)))
  sxxp.imputed$CEO.Duality <- (as.numeric(as.factor(sxxp.imputed$CEO.Duality)))
  sxxp.imputed$Prsdg.Dir <- (as.numeric(as.factor(sxxp.imputed$Prsdg.Dir)))
  sxxp.imputed$Clssfd.Bd.Sys <- (as.numeric(as.factor(sxxp.imputed$Clssfd.Bd.Sys)))
  sxxp.imputed$Ticker <- NULL
  summary(sxxp.imputed)
  
  dontScale <- c(grep("BOD.Age.Rng", colnames(sxxp.imputed)),grep("Tobins.Q.class", colnames(sxxp.imputed)), grep("AZS.class.Binary", colnames(sxxp.imputed)))
  sxxp.imputed[, -dontScale] <- data.frame(scale(sxxp.imputed[, -dontScale]))
  sxxp.imputed$BOD.Age.Rng <- ifelse(sxxp.imputed$BOD.Age.Rng > mean(sxxp.imputed$BOD.Age.Rng), 1, 0)
  sxxp.imputed$BOD.Age.Rng <- as.factor(sxxp.imputed$BOD.Age.Rng)
  sxxp.imputed$BOD.Age.Rng <- ifelse(sxxp.imputed$BOD.Age.Rng == 0, as.double(0), as.double(1))
  
  dbWriteTable(mydb_causal, value = sxxp.imputed, name = "sxxp_agerange", overwrite = TRUE,row.names=FALSE ) 
  remove(list = ls()[!(grepl("mydb_", ls()))])
  
}
sxxp_indepChFmlCEO <- function(){
  #SXXP - indep chairman or female ceo
  #
  #
  #
  sxxp.imputed <- dbReadTable(conn=mydb_imputed,name='sxxp')
  drops <- c(
    "Tobins.Q",
    "AZS"
  )#leaving altman class
  sxxp.imputed <- sxxp.imputed[ , !(names(sxxp.imputed) %in% drops)]
  sxxp.imputed$AZS.class <- (as.numeric(as.factor(sxxp.imputed$AZS.class)))
  sxxp.imputed$Indep.Lead.Dir <- (as.numeric(as.factor(sxxp.imputed$Indep.Lead.Dir)))
  sxxp.imputed$Frmr.CEO.or.its.Equiv.on.Bd <- (as.numeric(as.factor(sxxp.imputed$Frmr.CEO.or.its.Equiv.on.Bd)))
  sxxp.imputed$CEO.Duality <- (as.numeric(as.factor(sxxp.imputed$CEO.Duality)))
  sxxp.imputed$Prsdg.Dir <- (as.numeric(as.factor(sxxp.imputed$Prsdg.Dir)))
  sxxp.imputed$Clssfd.Bd.Sys <- (as.numeric(as.factor(sxxp.imputed$Clssfd.Bd.Sys)))
  sxxp.imputed$Ticker <- NULL
  
  sxxp.imputed$Indep.Chrprsn <- ifelse(sxxp.imputed$Indep.Chrprsn == "Y", 1, 0)
  sxxp.imputed$Feml.CEO.or.Equiv <- ifelse(sxxp.imputed$Feml.CEO.or.Equiv == "Y", 1, 0)
  sxxp.imputed["Indep.Chrprsn.Feml.CEO.or.Equiv"] <- ifelse(sxxp.imputed$Indep.Chrprsn == 1 | sxxp.imputed$Feml.CEO.or.Equiv == 1, 1, 0)
  sxxp.imputed["AZS.class.Binary"] <- ifelse(sxxp.imputed$AZS.class == 3, 1, 0)
  sxxp.imputed$AZS.class <- NULL
  summary(sxxp.imputed)
  
  dontScale <- c(grep("Indep.Chrprsn.Feml.CEO.or.Equiv", colnames(sxxp.imputed)), grep("AZS.class.Binary", colnames(sxxp.imputed)), grep("Tobins.Q.class", colnames(sxxp.imputed)))
  sxxp.imputed[, -dontScale] <- data.frame(scale(sxxp.imputed[, -dontScale]))
  
  dbWriteTable(mydb_causal, value = sxxp.imputed, name = "sxxp_indepChFmlCEO", overwrite = TRUE,row.names=FALSE ) 
  remove(list = ls()[!(grepl("mydb_", ls()))])
  
}
sxxp_fl <- function(){
  #SXXP - financial leverage > 4 treatment
  #
  #
  #
  sxxp.imputed <- dbReadTable(conn=mydb_imputed,name='sxxp')
  drops <- c(
    "Tobins.Q",
    "AZS"
  )#leaving tobins q class
  sxxp.imputed <- sxxp.imputed[ , !(names(sxxp.imputed) %in% drops)]
  sxxp.imputed$AZS.class <- (as.numeric(as.factor(sxxp.imputed$AZS.class)))
  sxxp.imputed["AZS.class.Binary"] <- ifelse(sxxp.imputed$AZS.class == 3, 1, 0)
  sxxp.imputed$AZS.class <- NULL
  sxxp.imputed$Indep.Lead.Dir <- (as.numeric(as.factor(sxxp.imputed$Indep.Lead.Dir)))
  sxxp.imputed$Indep.Chrprsn <- (as.numeric(as.factor(sxxp.imputed$Indep.Chrprsn)))
  sxxp.imputed$Feml.CEO.or.Equiv <- (as.numeric(as.factor(sxxp.imputed$Feml.CEO.or.Equiv)))
  sxxp.imputed$Frmr.CEO.or.its.Equiv.on.Bd <- (as.numeric(as.factor(sxxp.imputed$Frmr.CEO.or.its.Equiv.on.Bd)))
  sxxp.imputed$CEO.Duality <- (as.numeric(as.factor(sxxp.imputed$CEO.Duality)))
  sxxp.imputed$Prsdg.Dir <- (as.numeric(as.factor(sxxp.imputed$Prsdg.Dir)))
  sxxp.imputed$Clssfd.Bd.Sys <- (as.numeric(as.factor(sxxp.imputed$Clssfd.Bd.Sys)))
  sxxp.imputed$Ticker <- NULL
  summary(sxxp.imputed)
  
  dontScale <- c(grep("Fincl..l", colnames(sxxp.imputed)), grep("Tobins.Q.class", colnames(sxxp.imputed)), grep("AZS.class.Binary", colnames(sxxp.imputed)))
  sxxp.imputed[, -dontScale] <- data.frame(scale(sxxp.imputed[, -dontScale]))
  sxxp.imputed$Fincl.l.treatment <- ifelse(sxxp.imputed$Fincl..l < 2.5, 1, 0)
  sxxp.imputed$Fincl.l.treatment <- as.factor(sxxp.imputed$Fincl.l.treatment)
  sxxp.imputed$Fincl.l.treatment <- ifelse(sxxp.imputed$Fincl.l.treatment == 0, as.double(0), as.double(1))
  summary(sxxp.imputed)
  dbWriteTable(mydb_causal, value = sxxp.imputed, name = "sxxp_fl", overwrite = TRUE,row.names=FALSE ) 
  remove(list = ls()[!(grepl("mydb_", ls()))])
  
}
sxxp_indep_dir_fincl_lev <- function (){
  #SPX - independent lead director / financial leverage higher than 2.5
  #
  #
  #
  sxxp.imputed <- dbReadTable(conn=mydb_imputed,name='sxxp')
  drops <- c(
    "Tobins.Q",
    "AZS"
  )#leaving azs class
  sxxp.imputed <- sxxp.imputed[ , !(names(sxxp.imputed) %in% drops)]
  sxxp.imputed$AZS.class <- (as.numeric(as.factor(sxxp.imputed$AZS.class)))
  sxxp.imputed$Frmr.CEO.or.its.Equiv.on.Bd <- (as.numeric(as.factor(sxxp.imputed$Frmr.CEO.or.its.Equiv.on.Bd)))
  sxxp.imputed$Prsdg.Dir <- (as.numeric(as.factor(sxxp.imputed$Prsdg.Dir)))
  sxxp.imputed$Indep.Lead.Dir <- ifelse(sxxp.imputed$Indep.Lead.Dir == 'Y', as.double(1), as.double(0))
  sxxp.imputed$Indep.Chrprsn <- (as.numeric(as.factor(sxxp.imputed$Indep.Chrprsn)))
  sxxp.imputed$CEO.Duality <- (as.numeric(as.factor(sxxp.imputed$CEO.Duality)))
  sxxp.imputed$Clssfd.Bd.Sys <- (as.numeric(as.factor(sxxp.imputed$Clssfd.Bd.Sys)))
  sxxp.imputed$Feml.CEO.or.Equiv <- (as.numeric(as.factor(sxxp.imputed$Feml.CEO.or.Equiv)))
  sxxp.imputed$Ticker <- NULL
  summary(sxxp.imputed)
  dontScale <- c(grep("Indep.Lead.Dir", colnames(sxxp.imputed)), grep("Fincl..l", colnames(sxxp.imputed)), grep("AZS.class", colnames(sxxp.imputed)), grep("Tobins.Q.class", colnames(sxxp.imputed)))
  sxxp.imputed[, -dontScale] <- data.frame(scale(sxxp.imputed[, -dontScale]))
  sxxp.imputed["Indep.Lead.Dir.Fincl..l"] <- ifelse(sxxp.imputed$Indep.Lead.Dir == 1 & sxxp.imputed$Fincl..l > 2.5, as.double(1), as.double(0))
  sxxp.imputed["AZS.class.Binary"] <- ifelse(sxxp.imputed$AZS.class == 3, 1, 0)
  sxxp.imputed$AZS.class <- NULL
  
  dbWriteTable(mydb_causal, value = sxxp.imputed, name = "sxxp_indepdirfincl", overwrite = TRUE,row.names=FALSE ) 
  remove(list = ls()[!(grepl("mydb_", ls()))])
  
}
sxxp_female_board <- function (){
  #SXXP - female % on board as treatment
  #
  #
  #
  sxxp.imputed <- dbReadTable(conn=mydb_imputed,name='sxxp')
  drops <- c(
    "Tobins.Q",
    "AZS"
  )#leaving tobins q class
  sxxp.imputed <- sxxp.imputed[ , !(names(sxxp.imputed) %in% drops)]
  sxxp.imputed$AZS.class <- (as.numeric(as.factor(sxxp.imputed$AZS.class)))
  sxxp.imputed["AZS.class.Binary"] <- ifelse(sxxp.imputed$AZS.class == 3, 1, 0)
  sxxp.imputed$AZS.class <- NULL
  sxxp.imputed$Frmr.CEO.or.its.Equiv.on.Bd <- (as.numeric(as.factor(sxxp.imputed$Frmr.CEO.or.its.Equiv.on.Bd)))
  sxxp.imputed$Prsdg.Dir <- (as.numeric(as.factor(sxxp.imputed$Prsdg.Dir)))
  sxxp.imputed$Indep.Lead.Dir <- (as.numeric(as.factor(sxxp.imputed$Indep.Lead.Dir)))
  sxxp.imputed$Indep.Chrprsn <- (as.numeric(as.factor(sxxp.imputed$Indep.Chrprsn)))
  sxxp.imputed$CEO.Duality <- (as.numeric(as.factor(sxxp.imputed$CEO.Duality)))
  sxxp.imputed$Clssfd.Bd.Sys <- (as.numeric(as.factor(sxxp.imputed$Clssfd.Bd.Sys)))
  sxxp.imputed$Feml.CEO.or.Equiv <- (as.numeric(as.factor(sxxp.imputed$Feml.CEO.or.Equiv)))
  sxxp.imputed$Ticker <- NULL
  summary(sxxp.imputed)
  dontScale <- c(grep("X..Women.on.Bd", colnames(sxxp.imputed)), grep("Tobins.Q.class", colnames(sxxp.imputed)), grep("AZS.class.Binary", colnames(sxxp.imputed)))
  sxxp.imputed[, -dontScale] <- data.frame(scale(sxxp.imputed[, -dontScale]))
  sxxp.imputed$X..Women.on.Bd <- ifelse(sxxp.imputed$X..Women.on.Bd > 20, 1, 0)
  sxxp.imputed$X..Women.on.Bd <- as.factor(sxxp.imputed$X..Women.on.Bd)
  sxxp.imputed$X..Women.on.Bd <- ifelse(sxxp.imputed$X..Women.on.Bd == 0, as.double(0), as.double(1))
  
  dbWriteTable(mydb_causal, value = sxxp.imputed, name = "sxxp_fboard", overwrite = TRUE,row.names=FALSE ) 
  remove(list = ls()[!(grepl("mydb_", ls()))])
  
}
sxxp_female_ceo <- function (){
  #SXXP - Female CEO as treatment
  #
  #
  #
  sxxp.imputed <- dbReadTable(conn=mydb_imputed,name='sxxp')
  drops <- c(
    "Tobins.Q",
    "AZS"
  )#leaving tobins q class
  sxxp.imputed <- sxxp.imputed[ , !(names(sxxp.imputed) %in% drops)]
  sxxp.imputed$AZS.class <- (as.numeric(as.factor(sxxp.imputed$AZS.class)))
  sxxp.imputed["AZS.class.Binary"] <- ifelse(sxxp.imputed$AZS.class == 3, 1, 0)
  sxxp.imputed$AZS.class <- NULL
  sxxp.imputed$Frmr.CEO.or.its.Equiv.on.Bd <- (as.numeric(as.factor(sxxp.imputed$Frmr.CEO.or.its.Equiv.on.Bd)))
  sxxp.imputed$Prsdg.Dir <- (as.numeric(as.factor(sxxp.imputed$Prsdg.Dir)))
  sxxp.imputed$Indep.Lead.Dir <- (as.numeric(as.factor(sxxp.imputed$Indep.Lead.Dir)))
  sxxp.imputed$Indep.Chrprsn <- (as.numeric(as.factor(sxxp.imputed$Indep.Chrprsn)))
  sxxp.imputed$CEO.Duality <- (as.numeric(as.factor(sxxp.imputed$CEO.Duality)))
  sxxp.imputed$Clssfd.Bd.Sys <- (as.numeric(as.factor(sxxp.imputed$Clssfd.Bd.Sys)))
  sxxp.imputed$Ticker <- NULL
  dontScale <- c(grep("Feml.CEO.or.Equiv", colnames(sxxp.imputed)), grep("Tobins.Q.class", colnames(sxxp.imputed)), grep("AZS.class.Binary", colnames(sxxp.imputed)))
  sxxp.imputed[, -dontScale] <- data.frame(scale(sxxp.imputed[, -dontScale]))
  
  sxxp.imputed$Feml.CEO.or.Equiv <- ifelse(sxxp.imputed$Feml.CEO.or.Equiv == 'Y', 1, 0)
  sxxp.imputed$Feml.CEO.or.Equiv <- as.factor(sxxp.imputed$Feml.CEO.or.Equiv)
  sxxp.imputed <- SMOTE(Feml.CEO.or.Equiv ~ ., sxxp.imputed, perc.over = 900, perc.under=100)

  sxxp.imputed$Feml.CEO.or.Equiv <- ifelse(sxxp.imputed$Feml.CEO.or.Equiv == 0, as.double(0), as.double(1))
  #spx.imputed.bal.fceo$Feml.CEO.or.Equiv <- as.numeric(spx.imputed.bal.fceo$Feml.CEO.or.Equiv)
  #spx.imputed.bal.fceo$Feml.CEO.or.Equiv <- ifelse(spx.imputed.bal.fceo$Feml.CEO.or.Equiv == 2, 1, 0)
  
  dbWriteTable(mydb_causal, value = sxxp.imputed, name = "sxxp_fceo", overwrite = TRUE,row.names=FALSE ) 
  remove(list = ls()[!(grepl("mydb_", ls()))])
  
}

sxxp()
sxxp_indepDir_FormerCEOBoard()
sxxp_agerange()
sxxp_indepChFmlCEO()
sxxp_fl()
sxxp_indep_dir_fincl_lev()
sxxp_female_board()
sxxp_female_ceo()

#**************************************************
#           EEBP
#**************************************************
eebp <- function(){
  eebp.imputed <- dbReadTable(conn=mydb_imputed,name='eebp')
  eebp.imputed$AZS.class <- (as.numeric(as.factor(eebp.imputed$AZS.class)))
  eebp.imputed$Frmr.CEO.or.its.Equiv.on.Bd <- (as.numeric(as.factor(eebp.imputed$Frmr.CEO.or.its.Equiv.on.Bd)))
  eebp.imputed$Indep.Lead.Dir <- (as.numeric(as.factor(eebp.imputed$Indep.Lead.Dir)))
  eebp.imputed$Indep.Chrprsn <- (as.numeric(as.factor(eebp.imputed$Indep.Chrprsn)))
  eebp.imputed$Feml.CEO.or.Equiv <- (as.numeric(as.factor(eebp.imputed$Feml.CEO.or.Equiv)))
  eebp.imputed$Ticker <- NULL
  eebp.imputed <- data.frame(scale(eebp.imputed))
  summary(eebp.imputed)
  dbWriteTable(mydb_causal, value = eebp.imputed, name = "eebp", overwrite = TRUE,row.names=FALSE ) 
  remove(list = ls()[!(grepl("mydb_", ls()))])
  
}
eebp_agerange <- function(){
  #EEBP - age range of board members as treatment
  #
  #
  #
  eebp.imputed <- dbReadTable(conn=mydb_imputed,name='eebp')
  drops <- c(
    "Tobins.Q",
    "AZS"
  )#leaving tobins q class
  eebp.imputed <- eebp.imputed[ , !(names(eebp.imputed) %in% drops)]
  eebp.imputed$AZS.class <- (as.numeric(as.factor(eebp.imputed$AZS.class)))
  eebp.imputed["AZS.class.Binary"] <- ifelse(eebp.imputed$AZS.class == 3, 1, 0)
  eebp.imputed$AZS.class <- NULL
  eebp.imputed$Indep.Lead.Dir <- (as.numeric(as.factor(eebp.imputed$Indep.Lead.Dir)))
  eebp.imputed$Indep.Chrprsn <- (as.numeric(as.factor(eebp.imputed$Indep.Chrprsn)))
  eebp.imputed$Feml.CEO.or.Equiv <- (as.numeric(as.factor(eebp.imputed$Feml.CEO.or.Equiv)))
  eebp.imputed$Frmr.CEO.or.its.Equiv.on.Bd <- (as.numeric(as.factor(eebp.imputed$Frmr.CEO.or.its.Equiv.on.Bd)))
  eebp.imputed$Ticker <- NULL
  dontScale <- c(grep("BOD.Age.Rng", colnames(eebp.imputed)), grep("AZS.class.Binary", colnames(eebp.imputed)), grep("Tobins.Q.class", colnames(eebp.imputed)))
  eebp.imputed[, -dontScale] <- data.frame(scale(eebp.imputed[, -dontScale]))
  eebp.imputed$BOD.Age.Rng <- ifelse(eebp.imputed$BOD.Age.Rng > mean(eebp.imputed$BOD.Age.Rng), 1, 0)
  eebp.imputed$BOD.Age.Rng <- as.factor(eebp.imputed$BOD.Age.Rng)
  eebp.imputed$BOD.Age.Rng <- ifelse(eebp.imputed$BOD.Age.Rng == 0, as.double(0), as.double(1))
  
  dbWriteTable(mydb_causal, value = eebp.imputed, name = "eebp_agerange", overwrite = TRUE,row.names=FALSE ) 
  remove(list = ls()[!(grepl("mydb_", ls()))])
  
}
eebp_fl <- function(){
  #EEBP - financial leverage < 4 treatment
  #
  #
  #
  eebp.imputed <- dbReadTable(conn=mydb_imputed,name='eebp')
  drops <- c(
    "Tobins.Q",
    "AZS"
  )#leaving tobins q class
  eebp.imputed <- eebp.imputed[ , !(names(eebp.imputed) %in% drops)]
  eebp.imputed$AZS.class <- (as.numeric(as.factor(eebp.imputed$AZS.class)))
  eebp.imputed["AZS.class.Binary"] <- ifelse(eebp.imputed$AZS.class == 3, 1, 0)
  eebp.imputed$AZS.class <- NULL
  eebp.imputed$Indep.Lead.Dir <- (as.numeric(as.factor(eebp.imputed$Indep.Lead.Dir)))
  eebp.imputed$Indep.Chrprsn <- (as.numeric(as.factor(eebp.imputed$Indep.Chrprsn)))
  eebp.imputed$Feml.CEO.or.Equiv <- (as.numeric(as.factor(eebp.imputed$Feml.CEO.or.Equiv)))
  eebp.imputed$Frmr.CEO.or.its.Equiv.on.Bd <- (as.numeric(as.factor(eebp.imputed$Frmr.CEO.or.its.Equiv.on.Bd)))
  eebp.imputed$Ticker <- NULL
  dontScale <- c(grep("Fincl..l", colnames(eebp.imputed)), grep("Tobins.Q.class", colnames(eebp.imputed)), grep("AZS.class.Binary", colnames(eebp.imputed)))
  eebp.imputed[, -dontScale] <- data.frame(scale(eebp.imputed[, -dontScale]))
  summary(eebp.imputed$Fincl..l)
  eebp.imputed$Fincl.l.treatment <- ifelse(eebp.imputed$Fincl..l < 4, 1, 0)

  dbWriteTable(mydb_causal, value = eebp.imputed, name = "eebp_fl", overwrite = TRUE,row.names=FALSE ) 
  remove(list = ls()[!(grepl("mydb_", ls()))])
  
}
eebp_indepChFmlCEO <- function(){
  #EEBP - indep chairman or female ceo
  #
  #
  #
  eebp.imputed <- dbReadTable(conn=mydb_imputed,name='eebp')
  drops <- c(
    "Tobins.Q",
    "AZS"
  )#leaving altman class
  eebp.imputed <- eebp.imputed[ , !(names(eebp.imputed) %in% drops)]
  eebp.imputed$Indep.Lead.Dir <- (as.numeric(as.factor(eebp.imputed$Indep.Lead.Dir)))
  eebp.imputed$Frmr.CEO.or.its.Equiv.on.Bd <- (as.numeric(as.factor(eebp.imputed$Frmr.CEO.or.its.Equiv.on.Bd)))
  eebp.imputed$Ticker <- NULL
  
  eebp.imputed$Indep.Chrprsn <- ifelse(eebp.imputed$Indep.Chrprsn == "Y", 1, 0)
  eebp.imputed$Feml.CEO.or.Equiv <- ifelse(eebp.imputed$Feml.CEO.or.Equiv == "Y", 1, 0)
  eebp.imputed["Indep.Chrprsn.Feml.CEO.or.Equiv"] <- ifelse(eebp.imputed$Indep.Chrprsn == 1 | eebp.imputed$Feml.CEO.or.Equiv == 1, 1, 0)
  
  eebp.imputed["AZS.class.Binary"] <- ifelse(eebp.imputed$AZS.class == "safe", 1, 0)
  eebp.imputed$AZS.class <- NULL
  summary(eebp.imputed)
  
  dontScale <- c(grep("Indep.Chrprsn.Feml.CEO.or.Equiv", colnames(eebp.imputed)), grep("AZS.class.Binary", colnames(eebp.imputed)), grep("Tobins.Q.class", colnames(eebp.imputed)))
  eebp.imputed[, -dontScale] <- data.frame(scale(eebp.imputed[, -dontScale]))

  dbWriteTable(mydb_causal, value = eebp.imputed, name = "eebp_indepChFmlCEO", overwrite = TRUE,row.names=FALSE ) 
  remove(list = ls()[!(grepl("mydb_", ls()))])
  
}
eebp_indep_dir_fincl_lev <- function (){
  #EEBP - independent lead director / financial leverage higher than 2.5
  #
  #
  #
  eebp.imputed <- dbReadTable(conn=mydb_imputed,name='eebp')
  drops <- c(
    "Tobins.Q",
    "AZS"
  )#leaving azs class
  eebp.imputed <- eebp.imputed[ , !(names(eebp.imputed) %in% drops)]
  eebp.imputed$AZS.class <- (as.numeric(as.factor(eebp.imputed$AZS.class)))
  eebp.imputed$Frmr.CEO.or.its.Equiv.on.Bd <- (as.numeric(as.factor(eebp.imputed$Frmr.CEO.or.its.Equiv.on.Bd)))
  eebp.imputed$Indep.Lead.Dir <- ifelse(eebp.imputed$Indep.Lead.Dir == 'Y', as.double(1), as.double(0))
  eebp.imputed$Indep.Chrprsn <- (as.numeric(as.factor(eebp.imputed$Indep.Chrprsn)))
  eebp.imputed$Feml.CEO.or.Equiv <- (as.numeric(as.factor(eebp.imputed$Feml.CEO.or.Equiv)))
  eebp.imputed$Ticker <- NULL
  summary(eebp.imputed)
  dontScale <- c(grep("Indep.Lead.Dir", colnames(eebp.imputed)), grep("Fincl..l", colnames(eebp.imputed)), grep("AZS.class", colnames(eebp.imputed)),  grep("Tobins.Q.class", colnames(eebp.imputed)))
  eebp.imputed[, -dontScale] <- data.frame(scale(eebp.imputed[, -dontScale]))
  eebp.imputed["Indep.Lead.Dir.Fincl..l"] <- ifelse(eebp.imputed$Indep.Lead.Dir == 1 & eebp.imputed$Fincl..l > 2.5, as.double(1), as.double(0))
  eebp.imputed["AZS.class.Binary"] <- ifelse(eebp.imputed$AZS.class == 3, 1, 0)
  eebp.imputed$AZS.class <- NULL
  table(eebp.imputed$Indep.Lead.Dir.Fincl..l)

  dbWriteTable(mydb_causal, value = eebp.imputed, name = "eebp_indepdirfincl", overwrite = TRUE,row.names=FALSE ) 
  remove(list = ls()[!(grepl("mydb_", ls()))])
  
}
eebp_female_board <- function (){
  #SXXP - female % on board as treatment
  #
  #
  #
  eebp.imputed <- dbReadTable(conn=mydb_imputed,name='eebp')
  drops <- c(
    "Tobins.Q",
    "AZS"
  )#leaving tobins q class
  eebp.imputed <- eebp.imputed[ , !(names(eebp.imputed) %in% drops)]
  eebp.imputed$AZS.class <- (as.numeric(as.factor(eebp.imputed$AZS.class)))
  eebp.imputed["AZS.class.Binary"] <- ifelse(eebp.imputed$AZS.class == 3, 1, 0)
  eebp.imputed$AZS.class <- NULL
  eebp.imputed$Frmr.CEO.or.its.Equiv.on.Bd <- (as.numeric(as.factor(eebp.imputed$Frmr.CEO.or.its.Equiv.on.Bd)))
  eebp.imputed$Indep.Lead.Dir <- (as.numeric(as.factor(eebp.imputed$Indep.Lead.Dir)))
  eebp.imputed$Indep.Chrprsn <- (as.numeric(as.factor(eebp.imputed$Indep.Chrprsn)))
  eebp.imputed$Feml.CEO.or.Equiv <- (as.numeric(as.factor(eebp.imputed$Feml.CEO.or.Equiv)))
  eebp.imputed$Ticker <- NULL
  summary(eebp.imputed)
  dontScale <- c(grep("X..Women.on.Bd", colnames(eebp.imputed)), grep("Tobins.Q.class", colnames(eebp.imputed)), grep("AZS.class.Binary", colnames(eebp.imputed)))
  eebp.imputed[, -dontScale] <- data.frame(scale(eebp.imputed[, -dontScale]))
  eebp.imputed$X..Women.on.Bd <- ifelse(eebp.imputed$X..Women.on.Bd > 20, 1, 0)
  eebp.imputed$X..Women.on.Bd <- as.factor(eebp.imputed$X..Women.on.Bd)
  eebp.imputed$X..Women.on.Bd <- ifelse(eebp.imputed$X..Women.on.Bd == 0, as.double(0), as.double(1))
  eebp.imputed$X..Women.on.Bd <- as.factor(eebp.imputed$X..Women.on.Bd)
  summary(eebp.imputed$X..Women.on.Bd)
  
  eebp.imputed <- SMOTE(X..Women.on.Bd ~ ., eebp.imputed, perc.over = 300, perc.under=100)

  eebp.imputed$X..Women.on.Bd <- ifelse(eebp.imputed$X..Women.on.Bd == 0, as.double(0), as.double(1))
  dbWriteTable(mydb_causal, value = eebp.imputed, name = "eebp_fboard", overwrite = TRUE,row.names=FALSE ) 
  remove(list = ls()[!(grepl("mydb_", ls()))])
  
}
eebp_indepDir_FormerCEOBoard <- function(){
  eebp.imputed <- dbReadTable(conn=mydb_imputed,name='eebp')
  drops <- c(
    "Tobins.Q",
    "AZS"
  )#leaving tobins q class
  eebp.imputed <- eebp.imputed[ , !(names(eebp.imputed) %in% drops)]
  eebp.imputed$AZS.class <- (as.numeric(as.factor(eebp.imputed$AZS.class)))
  eebp.imputed["AZS.class.Binary"] <- ifelse(eebp.imputed$AZS.class == 3, 1, 0)
  eebp.imputed$AZS.class <- NULL
  eebp.imputed$Frmr.CEO.or.its.Equiv.on.Bd <- ifelse(eebp.imputed$Frmr.CEO.or.its.Equiv.on.Bd == 'Y', as.double(1), as.double(0))
  eebp.imputed$Indep.Lead.Dir <- ifelse(eebp.imputed$Indep.Lead.Dir == 'Y', as.double(1), as.double(0))
  eebp.imputed$Indep.Chrprsn <- (as.numeric(as.factor(eebp.imputed$Indep.Chrprsn)))
  eebp.imputed$Feml.CEO.or.Equiv <- ifelse(eebp.imputed$Feml.CEO.or.Equiv == 'Y', as.double(1), as.double(0))
  eebp.imputed$Ticker <- NULL
  summary(eebp.imputed)
  
  dontScale <- c(grep("Indep.Lead.Dir", colnames(eebp.imputed)), grep("Frmr.CEO.or.its.Equiv.on.Bd", colnames(eebp.imputed)), grep("Tobins.Q.class", colnames(eebp.imputed)), grep("AZS.class.Binary", colnames(eebp.imputed)))
  eebp.imputed[, -dontScale] <- data.frame(scale(eebp.imputed[, -dontScale]))
  
  eebp.imputed["Indep.Lead.Dir.Former.CEO.on.Board"] <- ifelse(eebp.imputed$Indep.Lead.Dir == 1 & eebp.imputed$Frmr.CEO.or.its.Equiv.on.Bd == 1, as.double(1), as.double(0))
  
  eebp.imputed$Indep.Lead.Dir.Former.CEO.on.Board <- as.factor(eebp.imputed$Indep.Lead.Dir.Former.CEO.on.Board)
  #sxxp.imputed <- SMOTE(Indep.Lead.Dir.Former.CEO.on.Board ~ ., sxxp.imputed, perc.over = 100, perc.under=200)
  eebp.imputed$Indep.Lead.Dir.Former.CEO.on.Board <- ifelse(eebp.imputed$Indep.Lead.Dir.Former.CEO.on.Board == 1, as.double(1), as.double(0))
  
  dbWriteTable(mydb_causal, value = eebp.imputed, name = "eebp_indepdirformerceo", overwrite = TRUE,row.names=FALSE ) 
  remove(list = ls()[!(grepl("mydb_", ls()))])
  
}
eebp_female_ceo <- function (){
  #EEBP - Female CEO as treatment
  #
  #
  #
  eebp.imputed <- dbReadTable(conn=mydb_imputed,name='eebp')
  drops <- c(
    "Tobins.Q",
    "AZS"
  )#leaving tobins q class
  eebp.imputed <- eebp.imputed[ , !(names(eebp.imputed) %in% drops)]
  eebp.imputed$AZS.class <- (as.numeric(as.factor(eebp.imputed$AZS.class)))
  eebp.imputed["AZS.class.Binary"] <- ifelse(eebp.imputed$AZS.class == 3, 1, 0)
  eebp.imputed$AZS.class <- NULL
  eebp.imputed$Frmr.CEO.or.its.Equiv.on.Bd <- (as.numeric(as.factor(eebp.imputed$Frmr.CEO.or.its.Equiv.on.Bd)))
  eebp.imputed$Indep.Lead.Dir <- (as.numeric(as.factor(eebp.imputed$Indep.Lead.Dir)))
  eebp.imputed$Indep.Chrprsn <- (as.numeric(as.factor(eebp.imputed$Indep.Chrprsn)))
  eebp.imputed$Ticker <- NULL
  dontScale <- c(grep("Feml.CEO.or.Equiv", colnames(eebp.imputed)), grep("Tobins.Q.class", colnames(eebp.imputed)), grep("AZS.class.Binary", colnames(eebp.imputed)))
  eebp.imputed[, -dontScale] <- data.frame(scale(eebp.imputed[, -dontScale]))
  
  eebp.imputed$Feml.CEO.or.Equiv <- ifelse(eebp.imputed$Feml.CEO.or.Equiv == 'Y', 1, 0)
  
  dbWriteTable(mydb_causal, value = eebp.imputed, name = "eebp_fceo", overwrite = TRUE,row.names=FALSE ) 
  remove(list = ls()[!(grepl("mydb_", ls()))])
  
}

eebp()
eebp_agerange()
eebp_fl()
eebp_indepChFmlCEO()
eebp_indep_dir_fincl_lev()
eebp_female_board()
eebp_indepDir_FormerCEOBoard()
eebp_female_ceo()
