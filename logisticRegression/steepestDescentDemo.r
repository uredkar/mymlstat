rm(list = ls())
# kevin murphy's book octave to r
library(digest)
library(scales)
#install.packages("scales")
library(ggvis)
library(caret)

library(gmodels)
library(class)
library(reshape2)
library(data.table)
library(dplyr)
library(R.matlab)
library(matlab)
library(plot3D)
library(limSolve)
library(mboost)
library(Boom)
library(animation)
library(Rcpp)

###########  first : animation r package example ##################
ani.options(nmax = 70)
par(mar = c(4, 4, 2, 0.1))
f2 = function(x, y) sin(1 / 2 * x ^ 2 - 1 / 4 * y ^ 2 + 3) * cos(2 * x + 1 -
  exp(y))
grad.desc(f2, c(-2, -2, 2, 2), c(-1, 0.5), gamma = 0.3, tol = 1e-04)
grad.desc(f2, c(-2, -2, 2, 2), c(-1, 0.5), gamma = 0.03, tol = 1e-04)


#%% Visualize steepest descent optimization on a 2d function
#% We try fixed step size, and also line searching.

#% This file is from pmtk3.googlec


# from kevin murphys book
aokiFn = function(x)
{
    if (is.vector(x) == TRUE)
    {
        f = 0.5 %*% (x[1] ^ 2 - x[2]) ^ 2 + 0.5 %*% (x[1] - 1) ^ 2;
    } 
    else
    {
        #% each row is a param vector
        f = 0.5 * (x[, 1] ^ 2 - x[, 2]) ^ 2 + 0.5 * (x[, 1] - 1) ^ 2;
    }

    g = cbind(2 * x[1] * (x[1] ^ 2 - x[2]) + x[1] - 1, x[2] - x[1] ^ 2);
    H = cbind(6 * x[1] ^ 2 - 2 * x[2] + 1, -2 * x[1], -2 * x[1], 1)
    if (nargs() >= 2)
        stop("not implemented for arg 2 or more")

   list(f = f,g = g, H = H)
}

## from aokiFn above

f1 = function(x, y) 0.5 * (x ^ 2 - y) ^ 2 + 0.5 * (x - 1) ^ 2
#library(Deriv)
#Deriv(f, "x")
#Deriv(f, "y")

## this is a deriv of the above
g1 = function(x, y) c(2 * x * (x ^ 2 - y) + x - 1,
                      y - x ^ 2)

xx = grad.desc(FUN = f1, gr = g1, rg = c(0, -0.5, 2, 3), init = c(0.5, 0.5), gamma = 0.3,len = 50,tol = 0.0000001 )
xx$persp(col = "lightblue", theta = 130, phi = 30, axes = TRUE, nticks = 5)

#plot3D::points3D(1, 1, 1, pch = 19, col = "red", add = TRUE)
##########################################################################



steepestDescent = function(f, x0, stepSize = 0.03, ftol = 1e-3, outputFn = NULL, maxIter = 500, exactLineSearch = FALSE) {
    #% f is the function of the form[fx, gx] = f(x) where gx is a column vector
    #% outputfn has the form stop = outputfn(x, optimValues, state)

    #% This file is from pmtk3.googlecode.com
    k = 1;
    reldiff = NaN;
    x = as.vector(x0);
    stop = FALSE;
    f1 = f(x);
    while (stop == FALSE) {


        k = k + 1;
        d = -f1$g;
        #% steepest descent direction
        if (is.null(stepSize)) {
            #ls = linesearch(f, x, f1$f, f$g, d, ~ exactLineSearch);
        } else {
            xnew = x + stepSize * d;
            f1 = f(xnew);
        }

        #%reldiff = norm(xnew - x) / norm(xnew);
        #%stop = (reldiff < ftol) | (k > maxIter);
        stop = norm(f1$g) < ftol | (k > maxIter);
        if (is.null(outputFn) == FALSE) {
            optimValues.iteration = k;
            optimValues.fval = f1$g;
            optimValues.funccount = k;
            state = 'iter';
            stop = stop | outputFn(x, optimValues, state);


        }
        plot3D::points3D(xnew[1], xnew[2], 1, pch = 19, col = "yellow", add = TRUE)
        #print(xnew)
        x = xnew;


    }
    x
}


x1 = seq(0, 2, by = 0.1)
y1 = seq(-0.5, 3, by = 0.1)
g = meshgrid(x1, y1);

#x = matrix(g$x,size(g$x))
#y = matrix(g$y, size(g$y))
x = g$x
y = g$y
dfg = cbind(x = as.vector(x), y = as.vector(y))
size(dfg)
ak = aokiFn(dfg)
Z1 = ak$f %>% as.vector

z = matrix(Z1, size(g$x))
par(mfrow = c(1,1))


#plot3D::persp3D(x = x, y = y, z = z, box = TRUE, shade = 0.15,
                    #axes = TRUE, nticks = 5, ticktype = "detailed", zlab = "aokiFn",
                    #theta = 140, phi = 20, expand = 0.75, main = "test")



# 0.1 step size works but slow , cpp implementation could be nice here
stepSizes = list( 0.3, 0.6 ); #% [] means use line search
for (m in 1:length(stepSizes)) {

    plot3D::persp3D(x = x, y = y, z = z, box = TRUE, shade = 0.15,
                        axes = TRUE, nticks = 5, ticktype = "detailed", zlab = "aokiFn",
                        theta = 140, phi = 20, expand = 0.75, main = "test")

    plot3D::points3D(1, 1, 1, pch = 19, col = "red", add = TRUE)
    x0 = c(0, 0);
    #global xhist fhist %updated by optimstore
    xhist = c();
    fhist = c();
    stepsize = as.numeric(stepSizes[m]);
    x = steepestDescent(aokiFn, x0, stepSize = stepsize);
    #print(m)
    #print(x)

    
}
x

