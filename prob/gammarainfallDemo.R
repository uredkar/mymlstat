rm(list=ls())
library(matlab)
library(VGAM)
source("utils.r")
source("prob/prob.r")
gammaMle = function(x)
{
  g = fitdistr(x,"gamma") 
  g$estimate
}

# method of moment

gammaMOM = function (X)
{
  xbar = mean(X);
  s2hat = var(X);
  a = xbar^2/s2hat;
  b  = xbar/s2hat;
  c(a = a, b = b)
}

rainfall.data = readMat("data/rainfall/rainfall.mat")
X = rainfall.data[[1]]
X = t(X);
X = na.omit(as.vector(X))
ldata = data.frame(y = rlgamma(100, k = exp(1)))

gmom = gammaMOM(X);
gmle = gammaMle(X);
rh = hist(X,probability = TRUE,col="grey")
lines(density(X),col="blue",lwd=2)
lines( sort(X) , y = exp(dgamma( sort(X) , shape = gmom["a"], log = TRUE )) , col = "blue" , lty = 2 , lwd = 2 )
lines( sort(X) , y = exp(dgamma( sort(X) , shape = gmle["shape"],rate = gmle["rate"], log = TRUE )) , col = "green" , lty = 2 , lwd = 2 )

