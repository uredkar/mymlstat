rm(list(ls))
library(ggvis)
library(caret)
library(gmodels)
library(class)
library(reshape2)
library(R.matlab)
library(matlab)

set.seed(1234)



heightWeight.data = readMat("data/heightWeight/heightWeight.mat")
hw.data = heightWeight.data[[1]]
X = cbind(hw.data[,2],hw.data[,3])
plot(X[,1],X[,2],col="blue", xlab = "height", ylab = "weight" )
colnames(X) = c("x","y")
X %>% head
cl2 = kmeans(X,2)
plot(cl2$centers,col=1:2, pch=8, cex = 2,xlab = "height", ylab = "weight")
plot(X[,1],X[,2], col = cl2$cluster,pch=8:9,xlab = "height", ylab = "weight")


cl3 = kmeans(X,3)
plot(cl3$centers,col=1:3, pch=8, cex = 2,xlab = "height", ylab = "weight")
plot(X[,1],X[,2], col = cl3$cluster,pch=8:10,xlab = "height", ylab = "weight")

