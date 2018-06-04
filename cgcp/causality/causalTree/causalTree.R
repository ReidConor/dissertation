library(causalTree)
library(RMySQL)

#get data
mydb.raw <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov')
mydb.processed <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_processed')
spx <- dbReadTable(conn=mydb.processed,name='spx')
spx.energy <- dbReadTable(conn=mydb.raw,name='spx_energy')

spx.energy.consumption <- spx.energy[ which(spx.energy$Variable=='ENERGY_CONSUMPTION'), c("Ticker","X2014")]
#replace NA's with mean (for now)
spx.energy.consumption[is.na(spx.energy.consumption[,"X2014"]), "X2014"] <- 
  mean(spx.energy.consumption[,"X2014"], na.rm = TRUE)

#this wont work....give up on merge for now.... 
spx.analysis <- merge(spx,spx.energy.consumption,by="Ticker")

spx$treatment <- ifelse(spx$X..Wmn.on.Bd >= 3, 1, 0)
spx[is.na(spx[,"treatment"]), "treatment"] <- 0

tree <- causalTree(Tobins.Q.class ~ ., data = spx, treatment = spx$treatment,
                   split.Rule = "CT", cv.option = "CT", split.Honest = T, cv.Honest = T, split.Bucket = F, 
                   xval = 5, cp = 0, minsize = 20, propensity = 0.5)


tree$cptable
opcp <- tree$cptable[,1][which.min(tree$cptable[,4])]

opfit <- prune(tree, opcp)

rpart.plot(opfit)




#---------------------------------


library(causalTree)
tree <- causalTree(y~ x1 + x2 + x3 + x4, data = simulation.1, treatment = simulation.1$treatment,
                   split.Rule = "CT", cv.option = "CT", split.Honest = T, cv.Honest = T, split.Bucket = F, 
                   xval = 5, cp = 0, minsize = 20, propensity = 0.5)
tree$cptable
opcp <- tree$cptable[,1][which.min(tree$cptable[,4])]

opfit <- prune(tree, opcp)

rpart.plot(opfit)
