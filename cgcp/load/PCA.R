#!/usr/bin/env Rscript

library(RMySQL)
mydb_processed <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_processed')
spx <- dbReadTable(conn=mydb_processed,name='spx')

drops <- c("Ticker",
           "AZS.class", 
           "AZS", 
           #"Tobins.Q",
           "Tobins.Q.class"
           )
spx.reduced <- spx[ , !(names(spx) %in% drops)] #remove unwanted columns
spx.reduced.complete <- na.omit(spx.reduced)

spx.target <- spx.reduced.complete$Tobins.Q
spx.vars <- spx.reduced.complete[ , !(names(spx.reduced.complete) %in% c("Tobins.Q"))]
summary(spx.vars)

spx.vars$X..Indep.Dir.on.Nom.Cmte <- NULL
spx.vars$X..Indep.Dir.on.Comp.Cmte.1 <- NULL
spx.vars$X..Indep.Dir.on.Aud.Cmte.1 <- NULL
spx.vars$Unit.or.2.Tier.Bd.Sys <- NULL
spx.vars$Feml.CEO.or.Equiv <- as.numeric(as.factor(spx.vars$Feml.CEO.or.Equiv))
spx.vars$Prsdg.Dir <- as.numeric(as.factor(spx.vars$Prsdg.Dir))
spx.vars$Clssfd.Bd.Sys <- as.numeric(as.factor(spx.vars$Clssfd.Bd.Sys))
spx.vars$Indep.Lead.Dir <- as.numeric(as.factor(spx.vars$Indep.Lead.Dir))
spx.vars$CEO.Duality <- as.numeric(as.factor(spx.vars$CEO.Duality))
spx.vars$Indep.Chrprsn <- as.numeric(as.factor(spx.vars$Indep.Chrprsn))
spx.vars$Frmr.CEO.or.its.Equiv.on.Bd <- as.numeric(as.factor(spx.vars$Frmr.CEO.or.its.Equiv.on.Bd))

spx.pca <- prcomp(spx.vars,
                 center = TRUE,
                 scale. = TRUE
                 )            
print(spx.pca)
plot(spx.pca, type = "l")
summary(spx.pca)
spx.pca$x[,1:4] # use the first 4 components for each record

