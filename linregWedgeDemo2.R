rm(list=ls())

library(matlab)
source("utils.r")
data = poly_data_make(sampling = "thibaux")
xtrain = data$xtrain
ytrain = data$ytrain
xtest  = data$xtest
ytestNoisefree = data$ytestNoisefree
ytestNoisy = data$ytestNoisy
sigma2 = data$sigma2


n = length(xtrain);
Xtrain = cbind(ones(n,1), xtrain);
dim(Xtrain)
dim(ytrain)


w = qr.solve(Xtrain,ytrain)
ntest = length(xtest);
Xtest = cbind(ones(ntest,1),xtest);


ypredTest = Xtest %*% w



plot(xtrain,ytrain, col = "blue",  pch=16)
abline(lm(ytrain~xtrain), col="black",lwd = 2) # regression line (y~x) 
lines(lowess(xtrain,ytrain), col="red",lwd = 2) # lowess line (x,y)
lines(xtest, as.vector(ypredTest), col="green") #, 'k', 'linewidth', 3)


sigma = 1;
a = w[1];
b = w[2]; 
stepSize = 0.1;

mg = matlab::meshgrid(seq(from = min(xtest), to = max(xtest), length.out = 300), 
              seq(from = min(ypredTest),to = max(ypredTest),length.out = 300))
x = mg$x
y = mg$y
mgsize = size(x)
r = mgsize[1]
c = mgsize[2]
r
c
colvec = function(X)
{
  as.vector(X)  
}

uniGaussPdf = function(X, mu, sigma2)
{
# Univariate Gaussian PDF vectorized w.r.t. mu and sigma2
#% p(i) = p(X(i) | mu(i), sigma2(i))
#% Use this function when you want to evaluate each data case under a
#% different distribution, i.e. different mu and sigma2 values, otherwise
#% use gaussProb, which works in the uni and multivariate case. 
#%%
  
#% This file is from pmtk3.googlecode.com

X      = colvec(X);
mu     = colvec(mu);
sigma2 = colvec(sigma2);
logZ   = log(sqrt(2*pi * sigma2));
logp   = -0.5 * (((X-mu)^2)/sigma2);
p      = exp(logp - logZ); 
p
}

sigma = matrix(sigma)  

func = function(X,Y,sigma) {
  uniGaussPdf(Y,a + b * X,sigma ^2)
}

p = func(colvec(x),
         colvec(y),sigma);

p = matlab::reshape(as.array(p), r, c);



X <- x
Y <- y
Z <- p
cc <- colorRamp(rev(rainbow(10)))
Zsc <- (Z-min(Z))/diff(range(Z))
rgbvec2col <- function(x) do.call(rgb,c(as.list(x),list(max=255)))
colvec <- apply(cc(Zsc),1,rgbvec2col)

library(rgl)
open3d()

surface3d(x,y,p * 20,col=colvec) 
bbox3d(color=c("white","black"))



