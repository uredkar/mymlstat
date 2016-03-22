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
#library(scatterplot3d)
set.seed(1234)


mote.data = readMat("data/moteData/moteData.mat")
with(mote.data, { X = X
                  y = y
                  })

X = mote.data$X
y = mote.data$y
N = size(X,1)
for (i in 0:1)
{
  if (i == 0)
  {
    Phi = matrix(cbind(ones(N,1),X),nrow=N)  
  }
  else
  {
    Phi = matrix(cbind(ones(N,1),X,X ^ 2),nrow=N)
  }
  
  w = limSolve::Solve(Phi,y)
  
  
  
  x1 = linspace(min(X[,1]),max(X[,1]),10);
  y1 = linspace(min(X[,2]),max(X[,2]),10);
  g = meshgrid(x1, y1);
  X1 = g$x
  Y1 = g$y
  if (i == 0)
  {
    Z = w[1] + w[2]*X1 + w[3]*Y1;  
  }
  else
  {
    Z = w[1] + w[2]*X1 + w[3]*Y1 + w[4] * (X1^2) + w[5] * Y1^2;
  }
    
  
  mat3d = plot3D::persp3D(X1, Y1, Z, colvar = Z, colkey = FALSE, box = TRUE,
                          axes = TRUE, nticks =5,ticktype = "detailed", zlab = "temperature",
                          theta =  -40,phi = 20,expand = 0.75, main="x and y are location within room")
  #mat3d = plot3D::surf3D(X1, Y1, Z, colvar = Z, colkey = FALSE, box = TRUE)
  p = plot3D::trans3D(X[,1],X[,2],y,mat3d)
  points(p,pch=1,col="black",lwd=2)
}

