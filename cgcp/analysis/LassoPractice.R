#from https://educationalresearchtechniques.com/2017/04/05/8601/
library(MASS)
library(corrplot)
library(glmnet)

data("nlschools")
View(nlschools)
str(nlschools)

nlschools$class<-NULL
p.cor<-cor(nlschools[,-5])#take off the COMB var
corrplot.mixed(p.cor)

ind<-sample(2,nrow(nlschools),replace=T,prob = c(0.7,0.3))
train<-nlschools[ind==1,]
test<-nlschools[ind==2,]

train$COMB<-model.matrix( ~ COMB - 1, data=train ) #convert to dummy variable
test$COMB<-model.matrix( ~ COMB - 1, data=test )
predictor_variables<-as.matrix(train[,2:4])
language_score<-as.matrix(train$lang)
lasso<-glmnet(predictor_variables,language_score,family="gaussian",alpha=1)
print(lasso)
plot(lasso,xvar="lambda",label=T)
View(train)
plot(lasso,xvar='dev',label=T)


#test
test.matrix<-as.matrix(test[,2:4])
lasso.y<-predict(lasso,newx = test.matrix,type = 'response',s=.02)
plot(lasso.y,test$lang)
lasso.resid<-lasso.y-test$lang
mean(lasso.resid^2)
