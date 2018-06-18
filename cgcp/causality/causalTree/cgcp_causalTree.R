library(causalTree)
library(RMySQL)

c
mydb.imputed.scaled <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_imputed_scaled')
spx.fceo <- dbReadTable(conn=mydb.imputed.scaled,name='spx_fceo')
spx.esg <- dbReadTable(conn=mydb.imputed.scaled,name='spx_esg_disc')
spx.cgcp <- dbReadTable(conn=mydb.imputed,name='spx_cgcp')

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
# in that popualtion, defined by the splits in the decision tree


spx.fceo.results <- casual_tree(spx.fceo,'Tobins.Q','Feml.CEO.or.Equiv')
rpart.plot(spx.fceo.results$opTree)
summary(spx.fceo.results$opTree)

spx.fceo.honest.results <- honest_casual_tree(spx.fceo,'Tobins.Q','Feml.CEO.or.Equiv')
rpart.plot(spx.fceo.honest.results$opTree)

spx.esg.honest.results <- casual_tree(spx.esg,'Tobins.Q','esg_disc_score_bin')
rpart.plot(spx.esg.honest.results$opTree)
summary(spx.esg.honest.results$opTree)

#--------------
drops <- c(
  "Ticker",
  "Clssfd.Bd.Sys",
  "CEO.Duality",
  "Indep.Chrprsn",
  "Indep.Lead.Dir",
  "Prsdg.Dir",
  "AZS.class",
  "FiveVarEq",          
  "EightVarEq",
  "Tobins.Q.class",
  "AZS"
)
spx.cgcp <- spx.cgcp[ , !(names(spx.cgcp) %in% drops)]
spx.cgcp$Feml.CEO.or.Equiv <- ifelse(spx.cgcp$Feml.CEO.or.Equiv == 'Y', 1, 0)
spx.cgcp$Frmr.CEO.or.its.Equiv.on.Bd <- as.numeric(as.factor(spx.cgcp$Frmr.CEO.or.its.Equiv.on.Bd))
summary(spx.cgcp)
table(spx.cgcp$esg_disc_score_bin)

spx.cgcp.results <- casual_tree(spx.cgcp,'Tobins.Q','esg_disc_score_bin')
rpart.plot(spx.cgcp.results$opTree)
summary(spx.cgcp.results$opTree)
spx.cgcp.results$opTree
summary(spx.cgcp)
