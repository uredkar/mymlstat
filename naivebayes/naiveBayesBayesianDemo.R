rm(list = ls())
library(parma)
library(matlab)
library(dplyr)
## from kevin murphy's book code
#%% Illustrate benefit of using T distribution instead of plugin estimate
#% We use a naive Bayes classifier with 2 Gaussian features
#%%
  
#% This file is from pmtk3.googlecode.com



normalize = function(A, dim)
{
  if (nargs() < 2)
  {
    z = sum(A)
    z[z==0] = 1;
    
    A / z
    
  }
  else
  {
    z = sum(A, dim)
    z[z==0] = 1;
    bsxfun("/", A, z);
    
  }
}




randn = function(m, n)
{
  matrix(rnorm(m*n),nrow=m, ncol=n)
}

makeDataHelper = function(nPts_n)
{
  
  mu  = matrix(c(0,0.6,-0.3,-0.4,1,-0.5),ncol=3,nrow=2, byrow = FALSE)  
  var = matrix(c(0.6,0.4,0.6), nrow = 1, ncol=3)
  
  N   = sum(nPts_n[,])
  dim = size(mu,1)
  X   = matlab::zeros(2,N)
  Y   = matlab::zeros(1,N)
  curidx = 1
  for(j in 1:3)
  {
    X[,seq(curidx,curidx+nPts_n[j]-1)] = repmat(mu[,j], 1, nPts_n[j])+var[j] * randn(dim, nPts_n[j]);
    Y[,seq(curidx,curidx+nPts_n[j]-1)] = j
    curidx = curidx + nPts_n[j];
  }
  X = t(X); # N by 2
  Y = t(Y);
  list(X = X, Y = Y)
}


C = c("blue","red","black")
m = matrix(c(2,5,8), nrow = 1, ncol=3)
train = makeDataHelper(m * 1) # also try m * 5
test = makeDataHelper(m * 20)


plot(train$X[,1], train$X[,2], type="n");
NC = 3 # number of cluster
for(c in 1:NC)
{
  ndx = train$Y == c;
  points(train$X[ndx, 1], train$X[ndx,2],pch=c,col = C[c]);
}
title('training data')

plot(test$X[,1], test$X[,2], type="n");
for(c in 1:3)
{
  ndx = test$Y == c;
  points(test$X[ndx, 1], test$X[ndx,2],pch=c,col = C[c]);
}
title('test data')


#% train
post.nu = post.mu =  post.kappa = post.pi = post.s2 = mle.mu = mle.s2 = mle.pi = cell(3,1)


for(c in 1:NC)
{

  ndx = train$Y == c;
  
  X =  train$X[ndx,]
  n = length(ndx)
  post.nu[c] = n;
  post.mu[c] = list(apply(X, MARGIN = 1, FUN=mean))
  post.kappa[c] = n;
  post.pi[c] = n;
  post.s2[c] = list(apply(X, MARGIN = 1, FUN=var)); #% divide by n, across rows
  mle.mu[c] = list(apply(X, MARGIN = 1, FUN=mean))
  mle.s2[c] = list(apply(X, MARGIN = 1, FUN=var))
  mle.pi[c] = n;
}


normalize(unlist(post.pi) + 1)
#normalize(unlist(post.mle) + 1)


ndims = 2;
logprobBayesC = zeros(size(test$X, 1) ,NC, ndims);
logprobPluginC = zeros(size(test$X, 1) ,NC, ndims);

################ work in progress beyon this ##################
