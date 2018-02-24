library(datasets)
library(RMySQL)
library(adabag)
data(iris)
#write to mysql
mydb = dbConnect(MySQL(), user='root', password='', dbname='corp_gov')
dbListTables(mydb)
dbWriteTable(mydb, value=as.data.frame(iris), name="iris", overwrite= TRUE)
