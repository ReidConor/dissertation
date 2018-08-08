#!/usr/bin/env Rscript
library(RMySQL)

mydb <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov')
mydb.imputed <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_imputed')
spx <- dbReadTable(conn=mydb,name='spx')
eebp <- dbReadTable(conn=mydb,name='eebp')
sxxp <- dbReadTable(conn=mydb,name='sxxp')
spx.imputed <- dbReadTable(conn=mydb.imputed,name='spx')
eebp.imputed <- dbReadTable(conn=mydb.imputed,name='eebp')
sxxp.imputed <- dbReadTable(conn=mydb.imputed,name='sxxp')




summary(sxxp)

summary(spx$Fincl..l)[6]
summary(eebp$Fincl..l) 

summary(spx$Sust.Gr.Rt)
summary(eebp$Sust.Gr.Rt) 

boxplot(spx$Net.Debt.to.EBITDA)
summary(eebp$Net.Debt.to.EBITDA)

boxplot(spx$Fincl..l, eebp$Fincl..l, names=c("spx", "eebp"))
boxplot(spx$Sust.Gr.Rt, eebp$Sust.Gr.Rt, names=c("spx", "eebp"))
boxplot(spx$Net.Debt.to.EBITDA, eebp$Net.Debt.to.EBITDA, names=c("spx", "eebp"))

spx.model <- lm(Tobins.Q ~ Net.Debt.to.EBITDA, data = spx)
eebp.model <- lm(Tobins.Q ~ Net.Debt.to.EBITDA, data = eebp)
summary(spx.model)
summary(eebp.model)


model <- lm(Tobins.Q ~ Indep.Lead.Dir+Frmr.CEO.or.its.Equiv.on.Bd, data = spx.imputed)
summary(model)
