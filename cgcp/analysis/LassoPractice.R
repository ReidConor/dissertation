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



#--------------------------------------------
#with cross validation
# Load data set
data("mtcars")
View(mtcars)
# Prepare data set 
x   <- model.matrix(~.-1, data= mtcars[,-1])
mpg <- ifelse( mtcars$mpg < mean(mtcars$mpg), 0, 1)
y   <- factor(mpg, labels = c('notEfficient', 'efficient'))

library(glmnet)

# Run cross-validation
mod_cv <- cv.glmnet(x=x, y=y, family='binomial')
mod_cv$lambda.1se
coef(mod_cv, mod_cv$lambda.1se)
mod_cv$lambda.min
coef(mod_cv, mod_cv$lambda.min)



#--------------------------------------------
#from https://www4.stat.ncsu.edu/~post/josh/LASSO_Ridge_Elastic_Net_-_Examples.html
library(MASS)  # Package needed to generate correlated precictors
library(glmnet)  # Package to fit ridge/lasso/elastic net models
set.seed(19875)  # Set seed for reproducibility
n <- 1000  # Number of observations
p <- 5000  # Number of predictors included in model
real_p <- 15  # Number of true predictors
x <- matrix(rnorm(n*p), nrow=n, ncol=p)
y <- apply(x[,1:real_p], 1, sum) + rnorm(n)
train_rows <- sample(1:n, .66*n)
x.train <- x[train_rows, ]
x.test <- x[-train_rows, ]
y.train <- y[train_rows]
y.test <- y[-train_rows]
fit.lasso <- glmnet(x.train, y.train, family="gaussian", alpha=1)
fit.ridge <- glmnet(x.train, y.train, family="gaussian", alpha=0)
fit.elnet <- glmnet(x.train, y.train, family="gaussian", alpha=.5)
for (i in 0:10) {
  assign(paste("fit", i, sep=""), cv.glmnet(x.train, y.train, type.measure="mse", 
                                            alpha=i/10,family="gaussian"))
}
# Plot solution paths:
par(mfrow=c(3,2))
# For plotting options, type '?plot.glmnet' in R console
plot(fit.lasso, xvar="lambda")
plot(fit10, main="LASSO")
plot(fit.ridge, xvar="lambda")
plot(fit0, main="Ridge")
plot(fit.elnet, xvar="lambda")
plot(fit5, main="Elastic Net")


yhat0 <- predict(fit0, s=fit0$lambda.1se, newx=x.test)
yhat1 <- predict(fit1, s=fit1$lambda.1se, newx=x.test)
yhat2 <- predict(fit2, s=fit2$lambda.1se, newx=x.test)
yhat3 <- predict(fit3, s=fit3$lambda.1se, newx=x.test)
yhat4 <- predict(fit4, s=fit4$lambda.1se, newx=x.test)
yhat5 <- predict(fit5, s=fit5$lambda.1se, newx=x.test)
yhat6 <- predict(fit6, s=fit6$lambda.1se, newx=x.test)
yhat7 <- predict(fit7, s=fit7$lambda.1se, newx=x.test)
yhat8 <- predict(fit8, s=fit8$lambda.1se, newx=x.test)
yhat9 <- predict(fit9, s=fit9$lambda.1se, newx=x.test)
yhat10 <- predict(fit10, s=fit10$lambda.1se, newx=x.test)

mse0 <- mean((y.test - yhat0)^2)
mse1 <- mean((y.test - yhat1)^2)
mse2 <- mean((y.test - yhat2)^2)
mse3 <- mean((y.test - yhat3)^2)
mse4 <- mean((y.test - yhat4)^2)
mse5 <- mean((y.test - yhat5)^2)
mse6 <- mean((y.test - yhat6)^2)
mse7 <- mean((y.test - yhat7)^2)
mse8 <- mean((y.test - yhat8)^2)
mse9 <- mean((y.test - yhat9)^2)
mse10 <- mean((y.test - yhat10)^2)







