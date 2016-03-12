rm(list=ls())
library(ggvis)
library(caret)
library(gmodels)
library(class)
library(reshape2)
library(R.matlab)
library(matlab)


#set.seed(1234)

################################ using built in kmeans

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
plot(X[,1],X[,2], col = cl3$cluster,pch=8,xlab = "height", ylab = "weight")

X = cbind(X,cluster = cl3$cluster)
colnames(X) = c("height","weight","cluster")
X = X %>% as.data.frame
X %>% head
densityplot( ~ height | cluster, data = X, layout = c(2, 2),  
             xlab = "Height/Cluster", bw = 5)|

histogram( ~ weight | cluster, data = X,  layout = c(2, 2),
             xlab = "Weight/cluster", bw = 5)
histogram( ~ weight | cluster, data = X[X$cluster == 1,],  
             xlab = "Weight for Cluster 1", bw = 5)

histogram( ~ weight | cluster, data = X[X$cluster == 2,],  
           xlab = "Weight for Cluster 2", bw = 5)

histogram( ~ weight | cluster, data = X[X$cluster == 3,],  
           xlab = "Weight for Cluster 3", bw = 5)

################### now for code in https://github.com/jtan189/kMeansClustering/blob/master/kmeans.R
#rm(list=ls())
source("utils.r")
K = 3
X = matrix(X,ncol=2, byrow = FALSE)
n = dim(X)[1]
d = dim(X)[2]


#mu = kmeansRandomMu(n = nrow(X),x = X,K = K)

## not using this from kevin murphy, sounds like magic
#xdist = sqDistance(X,mu) 

#### this is easy to understand 
#xdist = centroidDists(X,mu)

ids = sample(1:n, K)
centroids = X[ids, ]

plot(X[,1],X[,2],col="blue", xlab = "height", ylab = "weight" )

for(iter in 1:maxIter) {
  
  clusters = assignClusters(X, centroids)
  points(X,col= clusters)
  points(centroids,col= "black", cex=5)
  
  #partMu = partitionedMean(x = X, y = clusters)
  
  newCentroids = t(sapply(1:K, function(c) colMeans(X[which(clusters == c), ])))
  points(newCentroids,col= "orange", cex=5, lwd="2")
  
  ## check if change in centroids is significant
  delta = abs(newCentroids - centroids) 
  delta_avg = (abs(newCentroids) + abs(centroids) + epsilon)/2
  
  cat("Delta:", delta,iter, "\n")
  if (any(delta / delta_avg > epsilon)) {
    ## use new centroids for next iteration
    centroids = newCentroids
    
  } 
  else {
    break
  }
}

plot(X[,1],X[,2],col=centroids, xlab = "height", ylab = "weight" )
plot(X[,1],X[,2],col=clusters, xlab = "height", ylab = "weight" )

