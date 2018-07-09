#!/usr/bin/env Rscript
library(causalTree)
library(RMySQL)

mydb.causal <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_causal')

#*********
#causalTree
#*********
# builds a binary regression tree model
# first, the tree is grown from root based on splitting rules
# splitting acts to min risk function
# secondly, estimates a treatment effect in each leaf
# +by taking diff of sample avg of treated group 
# +and the sample avg of control group within leaf
#
casual_tree <- function(dataset,target,treatment.var) {
  colnames(dataset)[colnames(dataset) == target] <- 'target'
  colnames(dataset)[colnames(dataset) == treatment.var] <- 'treatment'

  tree <- causalTree(
    target ~ ., 
    data = dataset,
    treatment = dataset$treatment, #must be binary
    split.Rule = "CT", #each corresponds to a diff risk function, each split aims to minmise the risk function 
    split.Honest = T, #use the honest split version, alternative is adaptive version
    split.Bucket = F, 
    cv.option = "CT", #cross validation option, 
    cv.Honest = T, #use honest cross validation
    xval = 5, #cross validation folds
    cp = 0, 
    minsize = 20, 
    propensity = 0.5
  )
  
  tree$cptable
  opcp <- tree$cptable[,1][which.min(tree$cptable[,4])] #opcp is the complexity param corressponding to the minimum cross validation error in the cptable
  opTree <- prune(tree, opcp)
  #rpart.plot(opfit)
  
  result=list(
    "opcp"=opcp,
    "opTree"=opTree,
    "tree"=tree
  )
  return(result)
}

#*********
#honest.causalTree
#*********
# supports one-step honest re-estimation 
# fits a causalTree model and gets honest estimation results
# +with tree structure built on training sample 
# +and leaf treatment effect estimates taken from estimation sample
#  
# 
honest_casual_tree <- function(dataset,target,treatment.var) {
  colnames(dataset)[colnames(dataset) == target] <- 'target'
  colnames(dataset)[colnames(dataset) == treatment.var] <- 'treatment'
  
  training.split <- 2/3
  len <- length(dataset[,1])
  sub <- sample(1:len,len*training.split)  
  dataset.train=dataset[sub,]
  dataset.est=dataset[-sub,]
  
  honestTree <- honest.causalTree(
    target ~ .,
    data = dataset.train,
    treatment = dataset.train$treatment,
    est_data = dataset.est,
    est_treatment = dataset.est$treatment,
    split.Rule = "CT", 
    split.Honest = T,
    split.Bucket = T, 
    HonestSampleSize = nrow(dataset.est),
    cv.option = "fit",
    cv.Honest = F
  )
  opcp <-  honestTree$cptable[,1][which.min(honestTree$cptable[,4])]
  opTree <- prune(honestTree, opcp)
  #rpart.plot(opTree)
  
  result=list(
    "opcp"=opcp,
    "opTree"=opTree,
    "honestTree"=honestTree
  )
  return(result)
  
  
}


# I think the "causal effect" is the diff in means 
# between the treated and non-treated populations
#
# so the bigger the diff, the more effect the treatment has
# if its minus, the treatment casues the target var to go down and visa versa
# in that population, defined by the splits in the decision tree



#*********
# SPX
#*********
spx.build <- function(){
  spx.fceo <- dbReadTable(conn=mydb.causal,name='spx_fceo')  
  spx.esg <- dbReadTable(conn=mydb.causal,name='spx_esg_disc')
  spx.fboard <- dbReadTable(conn=mydb.causal,name='spx_fboard')

}
spx.indepdirfincl <- dbReadTable(conn=mydb.causal,name='spx_indepdirfincl')
summary(spx.indepdirfincl)

spx.build.indepdirfincl <- function(){
  spx.indepdirfincl <- dbReadTable(conn=mydb.causal,name='spx_indepdirfincl')
  summary(spx.indepdirfincl)  
  drops <- c(
    "Tobins.Q",
    "Tobins.Q.class",
    #"AZS",
    "AZS.class"
  )
  spx.indepdirfincl <- spx.indepdirfincl[ , !(names(spx.indepdirfincl) %in% drops)]
  
  spx.indepdirfincl.results <- casual_tree(spx.indepdirfincl,'AZS','Indep.Lead.Dir.Fincl..l')
  rpart.plot(spx.indepdirfincl.results$opTree)
  spx.indepdirfincl.results$opTree
  summary(spx.indepdirfincl.results$opTree)
  
}
spx.build.indepdirfincl()



#*********
# SXXP
#*********
sxxp.fboard <- dbReadTable(conn=mydb.causal,name='sxxp_fboard')


#*********
# EEBP
#*********
eebp.build <- function(){
  eebp.agerange <- dbReadTable(conn=mydb.causal,name='eebp_agerange')
  eebp.fl <- dbReadTable(conn=mydb.causal,name='eebp_fl')
 
}
eebp.build.indepChFmlCEO <- function(){
  eebp.indepChFmlCEO <- dbReadTable(conn=mydb.causal,name='eebp_indepChFmlCEO')
  summary(eebp.indepChFmlCEO)  
  drops <- c(
    "Tobins.Q",
    "Tobins.Q.class",
    "AZS",
    "AZS.class"
  )
  eebp.indepChFmlCEO <- eebp.indepChFmlCEO[ , !(names(eebp.indepChFmlCEO) %in% drops)]
  
  eebp.indepChFmlCEO.results <- casual_tree(eebp.indepChFmlCEO,'AZS.class.Binary','Indep.Chrprsn.Feml.CEO.or.Equiv')
  rpart.plot(eebp.indepChFmlCEO.results$opTree)
  eebp.indepChFmlCEO.results$opTree
  summary(eebp.indepChFmlCEO.results$opTree)
  
}
eebp.build.indepChFmlCEO()
