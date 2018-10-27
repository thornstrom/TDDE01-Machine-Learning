#min kod

rm(list=ls())
library("ggplot2")
#setwd("~/R/lab1")
data <- read.csv("machines.csv", dec=".", sep=",")

#Question 2
thetamodel=function(theta,x){
  return(theta*exp(-theta%*%t(x)))
}

theta = seq(0.5, 1.5, by=0.01)

likelyhood = log(apply(thetamodel(theta, data$Length),1,prod)) 
thetaMax = theta[which(likelyhood==max(likelyhood))]

#Question 3, same but [1:6]
likelyhood6 = log(apply(thetamodel(theta, data$Length[1:6]),1,prod)) 
plot(theta, likelyhood6, ylim = c(-200,00), type='l', ylab = "likelyhood6 (log)" )
lines(theta,likelyhood, col="blue")
#legend(x = "bottomright", c("All vals", "First six"), lty = c(1,1), lwd = c(1,1), col=c("red", "blue"))
thetaMax6 = theta[which(likelyhood6==max(likelyhood6))]

#Question 4
n = length(data$Length)
lambda=10
lambdalog = n*log(theta) - theta*sum(data) + log(lambda) - lambda*theta
plot(theta,lambdalog ,type='l', ylab = "lambda likelyhood (log)")
thetaMaxLlog1 = theta[which(lambdalog ==max(lambdalog))]

#Question 5
values = rexp(50,thetaMax)
p1 <- hist(data$Length)
p2 <- hist(values)

#gruppens kod
