library(causalTree)
library(RMySQL)

mydb.imputed.scaled <- dbConnect(MySQL(), user='root', password='', dbname='corp_gov_imputed_scaled')
spx <- dbReadTable(conn=mydb.imputed.scaled,name='spx')

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
  opfit <- prune(tree, opcp)
  #rpart.plot(opfit)
  
  result=list(
    "opcp"=opcp,
    "opfit"=opfit
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
    "opTree"=opTree
  )
  return(result)
  
  
}

spx.results <- casual_tree(spx,'Tobins.Q','Feml.CEO.or.Equiv')
rpart.plot(spx.results$opfit)

spx.honest.results <- honest_casual_tree(spx,'Tobins.Q','Feml.CEO.or.Equiv')
rpart.plot(spx.honest.results$opTree)

