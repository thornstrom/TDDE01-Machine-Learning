rm(list=ls())
setwd("~/R/lab1")
data <- read.csv("tecator.csv", dec=".", sep=",")

#Question 1
plot(data$Moisture,data$Protein, col=c("blue", "red"))

#Question 2
#If moisture is a polynomial function of the power of i and protein is linear. 
#Then the most appropriate model would be a polynomial function of the power of i.

#Question 3
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]

order=6
MSEtrain=c(1:order)
MSEtest=c(1:order)
for (i in 1:order){
  model=lm(Moisture~poly(Protein,i), train)
  MSEtrain[i]=mean((train$Moisture - predict(model, train))^2)
  MSEtest[i]=mean((test$Moisture - predict(model, test))^2)
}

plot((1:order), MSEtrain, col="red", type="l", ylab="MSE", xlab="Order", ylim=c(min(MSEtrain,MSEtest),max(MSEtrain,MSEtest)))
lines((1:order),MSEtest, col="blue")

#Question 4
library(MASS)
values=data[,2:101]
fit=lm(data$Fat~., values)
step=stepAIC(fit, direction="both", trace=FALSE)
steporder=length(step$coefficients)
#step$anova
summary(step)

#Question 5
library(glmnet)
covariates=scale(values)
response=scale(data$Fat)

model0=glmnet(as.matrix(covariates), response, alpha=0, family="gaussian")
plot(model0, xvar="lambda", label=TRUE)

model=cv.glmnet(as.matrix(covarities), response, alpha=0, family="gaussian")
model$lambda.min
plot(model)
coef(model, s="lambda.min")

#Question 6
lasso=glmnet(as.matrix(covariates), response, alpha=1, family="gaussian")
plot(lasso, xvar="lambda", label=TRUE)

#Question 7
model1=cv.glmnet(as.matrix(covariates), response, alpha=1, family="gaussian")
model1$lambda.min
plot(model1)
coef(model1, s="lambda.min")

#skillnaden, stepAIC har 64 features, men lasso har 13 features