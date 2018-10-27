library(fastICA)
library(pls)
set.seed(12345)
setwd("~/R/lab2")
rm(list=ls())

data=read.csv2("NIRSpectra.csv")

#Question 1

principal_comps=prcomp(data[,-ncol(data)])
lambda=principal_comps$sdev^2
sprintf("%2.3f", lambda/sum(lambda)*100)

df1=data.frame(comp=(1:length(lambda)), lambda=lambda/sum(lambda))
plot(df1[,1],lambda, type="h", ylab="Variance", xlab="Component")

df2=data.frame(PC1<-principal_comps$x[,1], PC2<-principal_comps$x[,2], viscosity=data$Viscosity)
plot(PC1, PC2, pch=21, bg="dodgerblue2", xlab="PC1", ylab="PC2")

#Question 2
U = principal_comps$rotation
df3=data.frame(x=1:nrow(U), y=U[,1])
plot(df3[,1],df3[,2], type="l", ylim=c(0,0.15), xlab="index", ylab="PC1")

df4=data.frame(x=1:nrow(U), y=U[,2])
plot(df4[,1],df4[,2], type="l", xlab="index", ylab="PC2")

#Question 3

ica=fastICA(data[,-ncol(data)], 2)
w_t=ica$K%*%ica$W
df5=data.frame(w_t)
plot(1:nrow(w_t), w_t[,1], type="l", ylab="IC", xlab="Index")
plot(1:nrow(w_t), w_t[,2], type="l", ylab="IC", xlab="Index")

df6=data.frame(IC1<-ica$S[,1], IC2<-ica$S[,2], viscosity=data$Viscosity)
plot(df6[,2], df6[,1], pch=21, bg="dodgerblue2", xlab="IC1", ylab="IC2")

#Question 4
pcr_fit=pcr(Viscosity ~., data=data, validation="CV")
summary(pcr_fit)
validationplot(pcr_fit, val.type="MSEP")