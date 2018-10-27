rm(list=ls())
library(tree)
library(MASS)
library(e1071)
set.seed(12345)
setwd("~/R/lab2")
data <- read.csv("creditscoring.csv", dec=".", sep=",")

#Question 1
n=dim(data)[1]
id=sample(1:n, floor(n*0.5))
train=data[id,]
id1=setdiff(1:n, id)
id2=sample(id1, floor(0.25*n))
validation=data[id2,]
id3=setdiff(id1,id2)
test=data[id3,]

#Question 2
mcr=function(matrix){
  return(1-(sum(diag(matrix))/ sum(matrix)))
}

fit.d=tree(good_bad~., train, split="deviance")
fit.g=tree(good_bad~., train, split="gini")

Dfit_train=predict(fit.d, train, type="class")
Dfit_test=predict(fit.d, test, type="class")
Dconf_train=table(Dfit_train, train$good_bad)
Dconf_test=table(Dfit_test, test$good_bad)
Dtrain_rate=mcr(Dconf_train)
Dtest_rate=mcr(Dconf_test)

Gfit_train=predict(fit.g, train, type="class")
Gfit_test=predict(fit.g, test, type="class")
Gconf_train=table(Gfit_train, train$good_bad)
Gconf_test=table(Gfit_test, test$good_bad)
Gtrain_rate=mcr(Gconf_train)
Gtest_rate=mcr(Gconf_test)

#Question 3
#fit=tree(good_bad~., train, split="deviance")
fit.o=fit.d
cv.res=cv.tree(fit.o)
plot(cv.res$size, cv.res$dev, type="b", col="red")
plot(log(cv.res$k), cv.res$dev, type="b", col="red")

tree_size=summary(fit.o)$size
trainScore=rep(0:tree_size)
testScore=rep(0:tree_size)

for (i in 2:tree_size){
  prunedTree=prune.tree(fit.o, best=i)
  pred=predict(prunedTree, newdata=validation, type="tree")
  trainScore[i]=deviance(prunedTree)
  validScore[i]=deviance(pred)
}

plot(2:tree_size, trainScore[2:tree_size], ylab="Deviance", xlab="Number of leaves", type="b", col="red", ylim=c(250,max(trainScore,testScore)))
points(2:tree_size, validScore[2:tree_size], type="b", col="blue")

#hardcoded+1, should fix
startNode = 1
nodeDepth=which.min(testScore[2:tree_size])+startNode

finalTree=prune.tree(fit.o, best=nodeDepth)
hej =summary(finalTree)
plot(finalTree, col="#888888")
text(finalTree, pretty=0, col="#000000", cex=1)
finalTree_rate = summary(finalTree)$misclass[1]/summary(finalTree)$misclass[2]

Yfit=predict(finalTree, validation, type="class")
temp=table(Yfit,validation$good_bad)
Yfit1=predict(finalTree, test, type="class")
temp2=table(Yfit1,test$good_bad)
#node depth: 4, missclass: 0.26, classes used: duration,savings and history

#Question 4
fit.b=naiveBayes(good_bad~., train)
Yfit_train=predict(fit.b, train)
Yfit_test=predict(fit.b, test)
bTableTrain=table(Yfit_train, train$good_bad)
bTableTest=table(Yfit_test, test$good_bad)

#Question 5
fit.train=naiveBayes(good_bad~., train)
loss_matrix=matrix(c(0,10,1,0), nrow=2)
Yfit_loss=predict(fit.train, train, type="raw")
#ska det vara transponat?
Yfit_loss=Yfit_loss%*%t(loss_matrix)
Yfit_loss=apply(Yfit_loss, 1, FUN=function(m){
  if(m[1] < m[2]){
    return('bad')
  }
  else{
    return('good')
  }
})
Yfit_table_train=table(Yfit_loss,train$good_bad)
Yfit_rate_train=mcr(Yfit_table_train)


fit.test=naiveBayes(good_bad~., train)
loss_matrix=matrix(c(0,10,1,0), nrow=2)
Yfit_loss=predict(fit.test, test, type="raw")
#ska det vara transponat?
Yfit_loss=Yfit_loss%*%t(loss_matrix)
Yfit_loss=apply(Yfit_loss, 1, FUN=function(m){
  if(m[1] < m[2]){
    return('bad')
  }
  else{
    return('good')
  }
})
Yfit_table_test=table(Yfit_loss,test$good_bad)
Yfit_rate_test=mcr(Yfit_table_test)

