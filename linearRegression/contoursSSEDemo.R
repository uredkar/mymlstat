rm(list=ls())
# kevin murphy's book octave to r

library(ggvis)
library(caret)
library(gmodels)
library(class)
library(reshape2)
library(dplyr)
library(R.matlab)
library(matlab)
library(plot3D)
library(limSolve)
#library(scatterplot3d)
#set.seed(1234) # try many runs by commenting this line
#%% Error surface for linear regression model
#% Based on code by John D'errico

#% This file is from pmtk3.googlecode.com




n = 20;
x = rnorm(n);
#%x = (-3:0.5:3)';
#%n = length(x);
wtrue = c(1,2); # change this for experiments
sigma = 3;
y = wtrue[1] + wtrue[2]*x + sigma * rnorm(n);

X = matrix(cbind(ones(n,1),x),nrow=n)


w = limSolve::Solve(X,y)  # % least squares soln

v = seq(from = -1,to = 3,by = .1)
#%v = -5:.5:5;
nv = length(v);
W = meshgrid(v,v);
w0 = t(W$x)
w1 = t(W$y)

w0 = matrix(as.vector(w0),nrow = 1)
w1 = matrix(as.vector(w1),nrow = 1)
m = length(w0);

SS = ((ones(n,1) %*% w0 + x %*% w1) - repmat(y,1,m))^2;
size(SS)
dim(SS)
SS = apply(SS,MARGIN =  2, FUN=sum)
size(SS)
SS = matrix(SS, nrow = nv, ncol = nv)
SS = reshape(SS,nv,nv);


#persp(v,v,SS,   box = TRUE,
      #axes = TRUE, nticks =5,ticktype = "detailed", zlab = "test",
      #theta =  -40,phi = 10,expand = 0.75, main="Test Data")

plot3D::persp3D(x = v,y = v,z = SS, box = TRUE,shade = 0.15,
                axes = TRUE, nticks =5,ticktype = "detailed", zlab = "test",
                theta =  -40,phi = 20,expand = 0.75, main="test")

plot3D::persp3D(x = v,y = v,z = SS, box = TRUE,shade = 0.15,
                axes = TRUE, nticks =5,ticktype = "detailed", zlab = "test",
                theta =  -140,phi = 20,expand = 0.75, main="test")


contour(v,v,SS,col=SS)
points(w[1],w[2],lwd= 3, col = "red")
points(wtrue[1],wtrue[2], col= "blue", lwd=3)
abline(h = wtrue[1], v = wtrue[2], col="green")
title("Sum of squares Errs linear regression Blue=True Red=MLE", xlab="w0", ylab="w1")
