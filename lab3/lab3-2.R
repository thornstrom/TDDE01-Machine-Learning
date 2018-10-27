rm(list=ls())
set.seed(1234567890)
library(neuralnet)
setwd("~/R/lab3")

Var <- runif(50, 0, 10) # Random data
trva <- data.frame(Var, Sin=sin(Var))
train <- trva[1:25,] # Training
validation <- trva[26:50,] # Validation

# Random initialization of the weights in the interval [-1, 1]
winit=runif(50, -1, +1)
error=1000
threshold=0
for (i in 1:10) {
  nn=neuralnet(Sin ~ Var, train, hidden=10, threshold = i/1000, startweights = winit)
  predicted=compute(nn, validation$Var)
  mse=sum((validation$Sin - predicted$net.result)^2)
  if(mse < error){
    error=mse
    threshold=i/1000
  }
}

# Plot optimal neuralnet
plot(nn.opt <- neuralnet(Sin ~ Var, train, hidden=10, threshold = threshold, startweights = winit))

#predicted <- compute(nn.opt, validation$Var)

# Plot of the predictions (black dots) and the data (red dots)
plot(prediction(nn.opt)$rep1)
points(trva, col = "firebrick3")