rm(list=ls())
setwd("~/R/lab1")
library(kknn)
data <- read.csv("spambase.csv", dec=".", sep=",")

#Divide data into train and test
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]

#knearest(train,int,test,int)
knearest=function(data,k,newdata) {
  n1=dim(data)[1]
  n2=dim(newdata)[1]
  p=dim(data)[2]
  Prob=numeric(n2)
  X=as.matrix(data[,-p])
  Xn=as.matrix(newdata[,-p])
  X=X/matrix(sqrt(rowSums(X^2)), nrow=n1, ncol=p-1)
  Xn=Xn/matrix(sqrt(rowSums(Xn^2)), nrow=n2, ncol=p-1)

  C=X%*%t(Xn)
  D=1-C
  
  indexlist = list()
  Prob = c(1:dim(newdata)[1])
  
  for (i in 1:n2 ){
    indexlist[[i]] = order(D[,i])[1:k]
    prediction = 0
    for(j in 1:k){
      prediction = prediction+data[indexlist[[i]][j],49]
    }
    Prob[i] = prediction/k
  }
  return(Prob)
}

modelfit=function(data,threshold){
  for (i in 1:length(data)){
    if(data[i] > threshold){
      data[i] = 1;
    }
    else{
      data[i] = 0;
    }
  }
  return(data)
}

#Y spam not spam
#Yfit probabilities
#p threshold
ROC=function(Y, Yfit, p){
  m=length(p)
  TPR=numeric(m)
  FPR=numeric(m)
  for(i in 1:m){
    t=table(Yfit>p[i], Y)
    TP=t[2,2]
    FP=t[2,1]
    totalpositives=sum(t[,2])
    totalnegatives=sum(t[,1])
    TPR[i]=TP/totalpositives
    FPR[i]=FP/totalnegatives
  }
  return (list(TPR=TPR,FPR=FPR))
}

#misclassification rate
mcr=function(matrix){
  return(1-(sum(diag(matrix))/ sum(matrix)))
}

# MCR & CT for Knearest
result = function(data,k,newdata){
  prob_vector <- knearest(data,k,newdata)
  binary_vals <- modelfit(prob_vector, 0.5)
  ct <- table(binary_vals,newdata[,49])
  mcr <- mcr(ct)
  return(list(ct, mcr))
}

# MCR & CT for kknn
prob_vector_kknn = function(data,k,newdata){
  data[,ncol(data)] = as.factor(data[,ncol(data)])
  newdata[,ncol(newdata)] = as.factor(newdata[,ncol(newdata)])
  kknn_pred <- kknn(Spam~., data, newdata, k = k)
  prob_vector <- kknn_pred$prob[,2]
  return(prob_vector)
}

result_kknn = function(data,k,newdata){
  prob_vector <- prob_vector_kknn(data,k,newdata)
  round_kknn <- round(prob_vector)
  binary_vals <- modelfit(round_kknn, 0.5)
  ct <- table(binary_vals,newdata[,49])
  mcr <- mcr(ct)
  return(list(ct, mcr))
}

result_kknn(train,5,test)
pi = seq(from = 0.05, to = 0.95, by =0.05)
ROC_kknn <- ROC(c(test[,49]),prob_vector_kknn(train,5,test),pi)
ROC_knearest <- ROC(c(test[,49]),knearest(train,5,test),pi)

plot(ROC_knearest$FPR, ROC_knearest$TPR, type="l", xlim=c(0,1), ylim=c(0,1), xlab="FPR", ylab="TPR", col="blue")
lines(ROC_kknn$FPR, ROC_kknn$TPR, col="red")
legend(x = "bottomright", c("knearest", "kknn"), lty = c(1,1), lwd = c(1,1), col=c("blue", "Red"))

knear1 = knearest(train,1,test)
knear5 = knearest(train,5,test)
knear5 = modelfit(knear5,0.5)
knear1 = table(knear1,test[,49])
knear5 = table(knear5,test[,49])
res1 = mcr(knear1)
res5 = mcr(knear5)