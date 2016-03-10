library(dplyr)
# convert from utils.py, kevin murphy book
set.seed(0)

rotate <- function(x) t(apply(x, 2, rev))

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


