rm(list = ls())
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
library(mboost)

seed = 0;
N = 100
#set.seed(1234)
x = sort(rnorm(N));

y = 1 + 2 * x + rnorm(size(x)) - .5;

#% add some outliers
outlier = c(0.1, 0.5, 0.9)
x = c(x, outlier);
k = -5;
y = c(y, k,  k ,k);

n = length(x);
m = length(y);
m
n
Xtrain = as.vector(x);
linear.model = lm(y ~ Xtrain)

Xtest = sort(rnorm(n)) #seq(from = 0, by = 0.1, length.out = n)

resid = residuals(linear.model)
pred <- predict(linear.model, newdata = data.frame(x = Xtest))
cof = coefficients(linear.model)
wt = weights(linear.model)
yhat = fitted(linear.model)

#plot(x = x, y = y, pch = 19)
#abline(coef = cof, col = "red",lwd=2)
#points(Xtest, pred,col="red")

plot(x = Xtrain, y = y, type = "n")
abline(h = 0, lty = 3, col = "gray")

#points(Xtest,pred, col = "green")

#gm.model = glm(y ~ Xtrain)
#gm.yhat = fitted(gm.model)
#gm.cof = coefficients(gm.model)
#gm.pred = predict(gm.model, newdata = data.frame(x = Xtest))
#points(Xtest, gm.pred, col = "green",pch=3) # same as lm

points(x = Xtrain, y = y, pch = 18, col = "black") # original train
points(Xtest, pred, col = "red", pch = 19)
lines(x = Xtest, y = pred, col = "red",pch=2)



boost.model <- glmboost(y ~ x, data = data.frame(x = x, y = y), family = Gaussian())
boost.cof = coef(boost.model)
boost.pred = predict(boost.model, newdata = data.frame(x = Xtest))
points(Xtest, boost.pred, col = "purple", pch = 19)
abline(coef = boost.cof, col = "purple", lwd = 2)

boost.model <- glmboost(y ~ x, data = data.frame(x = x, y = y), family = Laplace())
boost.cof = coef(boost.model)
boost.pred = predict(boost.model, newdata = data.frame(x = Xtest))
points(Xtest, boost.pred, col = "blue",pch=19)
abline(coef = boost.cof, col = "blue", lwd = 2)
legend("bottomright", c("Red = lm", "blue laplace"), col = c("red", "blue"), lwd = 2,cex = 0.70)

points(x = outlier, y = tail(y, 3), pch = 18, cex = 2, col = "black")