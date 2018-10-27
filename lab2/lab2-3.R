rm(list=ls())
library(tree)
library(boot)
library(ggplot2)
set.seed(12345)
#setwd("~/R/lab2")
data <- read.csv2("State.csv")

#Question 5
data.order=data[order(data$MET),]
plot(data.order$MET, data.order$EX, ylab="EX", xlab="MET", pch=21, bg="dodgerblue4")

#Question 6
reg.tree=tree(EX~MET, data.order, control=tree.control(nobs=dim(data)[1], minsize=8))
cv.tree=cv.tree(reg.tree)
#plot(cv.tree$size, cv.tree$dev, ylab="Deviance")
optimal.depth=cv.tree$size[which.min(cv.tree$dev)]
final.tree=prune.tree(reg.tree, best=optimal.depth)
prediction.tree=predict(final.tree, newdata=data.order, type="vector")
plot(data.order$MET, prediction.tree, pch=21, bg="firebrick2", ylab="EX", xlab="MET", ylim=c(min(data.order$EX),max(data.order$EX)))
points(data.order$MET, data.order$EX, pch=21, bg="dodgerblue3")
prediction.hist=hist(resid(final.tree), breaks=40, xlab="Residuals")

#Question 7
f1=function(data, ind){
  data1=data[ind,]
  reg.tree=tree(EX~MET, data=data1, control=tree.control(nobs=dim(data)[1], minsize=8))
  return(predict(reg.tree, newdata=data))
}
temp.boot=boot(data.order, f1, R=1000)
env1=envelope(temp.boot)#, level=0.95)
plot(data.order$MET, data.order$EX, pch=21, bg="dodgerblue4")
lines(data.order$MET, env1$point[2,], col="firebrick3")
lines(data.order$MET, env1$point[1,], col="firebrick3")

#Question 8
#mle=lm(EX~MET, data=data.order)
rng=function(data, mle){
  data1=data.frame(EX = data.order$EX, MET = data.order$MET)
  n=length(data.order$EX)
  pred=predict(mle, newdata=data1)
  ##This line of code looks good but I don???t know why you cannot get the smooth line for confidence band.
  data1$EX=rnorm(n, pred, sd(resid(mle)))
  ##
  return(data1)
}
f2=function(data1){
  #test=lm(EX~MET, data=data1)
  test=tree(EX~MET, data1, control=tree.control(nobs=dim(data1)[1], minsize=8))
  return(predict(test, newdata=data.order))
}
reg.tree=tree(EX~MET, data.order, control=tree.control(nobs=dim(data)[1], minsize=8))
res=boot(data=data.order, statistic=f2, R=1000, mle=reg.tree, ran.gen=rng, sim="parametric")
env2=envelope(res)

plot(data.order$MET, data.order$EX, pch=21, bg="dodgerblue4")
lines(data.order$MET, env2$point[2,], col="chartreuse4")
lines(data.order$MET, env2$point[1,], col="chartreuse4")
#FEL 
#This is not the way to do with the prediction band. 
#You should modify another function like f2/f1 to get the predictions.
lines(data.order$MET, env2$overall[2,], col="firebrick3")
lines(data.order$MET, env2$overall[1,], col="firebrick3")
#

res=boot(data, statistic=f2, R=1000, mle=reg.tree, ran.gen=rng, sim="parametric")
e=envelope(res)
df<-data.frame(data.met=data$MET, data.ex=data$EX, pred.tree = prediction.tree,
                     ep1=e$point[1,], ep2=e$point[2,], eo1=e$overall[1,], eo2=e$overall[2,])
ggplot(df, aes(x=data.met)) + geom_point(aes(y=data.ex)) + geom_line(aes(y=pred.tree, color="dodgerblue4")) + geom_line(aes(y=ep1, color="chartreuse4")) + geom_line(aes(y=ep2, color="chartreuse4")) + geom_line(aes(y=eo1, color="firebrick3")) + geom_line(aes(y=eo2, color="firebrick3")) + xlab("MET") + ylab("EX")
  