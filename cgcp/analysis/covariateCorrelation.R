#see https://stackoverflow.com/questions/21604997/how-to-find-significant-correlations-in-a-large-dataset
#trying to see whether we have significant correlations between predictor variables

library(RMySQL)
mydb.processed <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_processed')
mydb.analysis <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_analysis')
spx <- dbReadTable(conn=mydb.processed,name='spx')
eebp <- dbReadTable(conn=mydb.processed,name='eebp')
sxxp <- dbReadTable(conn=mydb.processed,name='sxxp')

correlations <- function(dataset){
  dataset.name <- deparse(substitute(dataset))
  drops <- c("Ticker",
             "AZS.class",
             "AZS", 
             "Tobins.Q", 
             "Tobins.Q.class"
  )
  data.reduced <- dataset[ , !(names(dataset) %in% drops)]
  
  data.reduced$Feml.CEO.or.Equiv <- as.numeric(as.factor(data.reduced$Feml.CEO.or.Equiv))
  data.reduced$Prsdg.Dir <- as.numeric(as.factor(data.reduced$Prsdg.Dir))
  data.reduced$Clssfd.Bd.Sys <- as.numeric(as.factor(data.reduced$Clssfd.Bd.Sys))
  data.reduced$Indep.Lead.Dir <- as.numeric(as.factor(data.reduced$Indep.Lead.Dir))
  data.reduced$CEO.Duality <- as.numeric(as.factor(data.reduced$CEO.Duality))
  data.reduced$Indep.Chrprsn <- as.numeric(as.factor(data.reduced$Indep.Chrprsn))
  data.reduced$Frmr.CEO.or.its.Equiv.on.Bd <- as.numeric(as.factor(data.reduced$Frmr.CEO.or.its.Equiv.on.Bd))
  number.of.columns <- ncol(data.reduced)
  
  library(Hmisc)
  correlations <- rcorr(as.matrix(data.reduced))
  
  corr.data.frame <- data.frame (
    Variable.1=character(), 
    Variable.2=character(), 
    P.Value=double(),
    data.stamp=as.Date(character())
  )
  current.time <- Sys.time()
  for (i in 1:number.of.columns){
    for (j in 1:number.of.columns){
      if ( !is.na(correlations$P[i,j])){
        if ( correlations$P[i,j] < 0.05 ) {
          new.details <- data.frame(
            toString(rownames(correlations$P)[i]),
            toString(colnames(correlations$P)[j]),
            correlations$P[i,j],
            toString(Sys.time())
          )
          corr.data.frame <- rbind(corr.data.frame,new.details)
        }
      }
    }
  }
  
  mysql.table.name <- c(dataset.name,"_variable_correlations")
  mysql.table.name.complete <- paste(mysql.table.name, collapse = '')
  names(corr.data.frame) <- c("var1", "var2","pvalue","datestamp")
  dbWriteTable(mydb.analysis, value = corr.data.frame, name = mysql.table.name.complete, overwrite = TRUE ) 
  #library(corrgram)
  #corrgram(the_data)

}
correlations(spx)
correlations(eebp)
correlations(sxxp)


#how do we deal with Multicollinearity?
#see http://blog.minitab.com/blog/understanding-statistics/handling-multicollinearity-in-regression-analysis
#talks about using partial least sqaures regression or principal component analysis 
