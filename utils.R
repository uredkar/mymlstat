library(dplyr)
library(pracma)
library(matlab)
library(MASS)
# convert functions from  kevin murphy book code check github
set.seed(0)
epsilon = 1e-4
maxIter = 1000
########### rotate a 2d matrix
rotate <- function(x) {
  t(apply(x, 2, rev))
} 

########### get sq of distance from kevin murphy code
sqDistance = function(p,q)
{
  pSOS = apply(p^2,MARGIN = 2, FUN = sum)
  qSOS = apply(q^2,MARGIN = 2, FUN = sum)
  a = bsxfun("+",pSOS,qSOS)
  b = - 2 * (p %*% t(q))
  a+b
}

##https://github.com/jtan189/kMeansClustering/blob/master/kmeans.R
centroidDists <- function(X, c) {
  ## For each row in the input data, find its Euclidean distance to
  ## each of the centroids.
  ##
  ## Args:
  ##   X: input data matrix, where each row represents a point
  ##   c: centroid matrix, where each row represents a centroid
  ## Returns:
  ##   A matrix with the same number of rows as X and the same number
  ##   of columns as c. The rows correspond to the points in
  ##   X and the columns correspond to the centroids. The value
  ##   for a particular (row, col) is the Euclidean distance
  ##   for the corresponding (point, centroid).
  dists = apply(X, 1, function(point)
                          sapply(1:nrow(c), function(dim)
                                                dist(rbind(point, c[dim, ]))))
  return(t(dists))
}

##https://github.com/jtan189/kMeansClustering/blob/master/kmeans.R
assignClusters <- function(X, c) {
  ## Assign the points in the input data to its closest centroid,
  ## using Euclidean distances.
  ##
  ## Args:
  ##   X: input data matrix, where each row represents a point
  ##   c: centroid matrix, where each row represents a centroid 
  ## Returns:
  ##   A vector with the same number of rows as X. The value for
  ##   a given index in this vector is indicates the cluster
  ##   centered at at c[index, ].
  
  n = dim(X)[1]
  d = dim(X)[2]
  
  cDists = centroidDists(X, c)
  clusterAndDist = sapply(1:n, function(x) which.min(cDists[x, ]))
  return(clusterAndDist)
}

### returns a list with index of vector for which the value in the vector is the minimum
minidx = function(x)
{
  
  idx = apply(x,MARGIN = 2, FUN = which.min)
  minvalue = x[idx]
  list(idx = idx, minvalue = minvalue)
}

# x input vector, y the class labels, returns a list of class tables and mean value in each class 
partitionedMean = function(x,y)
{
  grp_by = dplyr::group_by(x,y)
  dplyr::summarise(grp_by,mean(x))
}


kmeansRandomMu = function(n,x,K=2)
{
  perm = pracma::randperm(n)
  rv = apply(x,MARGIN = 2,FUN =  stats::var)  # var of each col
  rmu = matlab::zeros(1,length(rv))
  noise = MASS::mvrnorm(n = K, mu = rmu , Sigma= 0.01 * diag(rv))
  t(x[perm[1:K],]) + t(noise)
}



poly_data_make = function(sampling="sparse", deg=3, n=21)
{
  if (sampling == "irregular")
    xtrain = c(seq(-1, -0.5, 0.1), seq(3, 3.5, 0.1))
  else if(sampling == "sparse")
    xtrain = c(-3, -2, 0, 2, 3)
  else if(sampling == "dense")
    xtrain = seq(from = -5, to = 5,by = 0.6)
  else if (sampling == "thibaux")
  {
    xtrain = seq(from=0, to=20, length.out =  n)
    xtest = seq(from = 0,to= 20.1, by = 0.1)
    sigma2 = 4
    w = c(-1.5, 1/9.)
    fun = function(x)  w[1]*x + w[2]*(x^2)
    print("sampling.")
    print(sampling)
  }
  else 
  {
    if (sampling != "thibaux")
      stopifnot(deg < 4)
    
    xtest = seq(-7, 7, 0.1)
    if (deg == 2)
    {
      fun = function(x) (10 + x + (x^2))
    }
    else
    {
      fun = function(x) (10 + x + (x^3))
    }
    sigma2 = 5*5
  }
  
  ytrain = fun(xtrain) + rnorm(mean = 0,sd = 1, n = length(xtrain)) * sqrt(sigma2)
  
  ytestNoisefree = fun(xtest)
  ytestNoisy = ytestNoisefree + rnorm(mean = 0,sd = 1,n = length(xtest)) * sqrt(sigma2)
  
  list(xtrain = xtrain, ytrain = ytrain, xtest = xtest,ytestNoisefree = ytestNoisefree, 
       ytestNoisy = ytestNoisy, sigma2 = sigma2)
  
  
}


