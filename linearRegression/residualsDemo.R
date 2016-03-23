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
#library(scatterplot3d)
#%% Residuals Demo
#%
#%%

#% This file is from pmtk3.googlecode.com

#set.seed(1234)
#xTrainRaw = (-10:1:10) ';
n = 200; # try n = 20
xTrainRaw = rnorm(n); # randn(n,1);

#%xTrainRaw = (-3:0.5:3)';
Ntrain = length(xTrainRaw);
xTrain = cbind(ones(Ntrain, 1), xTrainRaw);

wtrue = c(1, 1);
sigma = 1;
yTrain = wtrue[1] + wtrue[2]  * xTrainRaw + sigma %*% rnorm(Ntrain);

X = xTrain;
y = yTrain;
#w = pinv(X '*X)*X' * y;
#% OLS estimate
w = limSolve::Solve(X, y)

yPredTrain = xTrain %*% w;

#% Performance on test set;
#%xTestRaw = (-10.5:1:10.5) ';
xTestRaw = seq(from = -3.5, to = 3.5, by = 0.5);

Ntest = length(xTestRaw);
xTest = cbind(ones(Ntest, 1) , xTestRaw);

# true weights based on train data, applied on test data
yTestOpt = wtrue[1] + wtrue[2] * xTestRaw;

# predicted weights applied on test on test data
yPredTest = xTest %*% w; 

hh1 = plot(xTestRaw, yPredTest,lwd=2, type="l", col = "red");
hh2 = lines(xTestRaw, yTestOpt, lwd=2,col="blue");
hh2 = points(xTestRaw, yTestOpt,, lwd = 2, col = "black", pch="x")


h = points(xTrainRaw, yTrain, col = "red", pch = 19); # pch 19 does solid circle
h = points(xTrainRaw, yPredTrain,  col = "black", pch= c(1,2));

for (i in 1:Ntrain) {
    h = lines(c(xTrainRaw[i], xTrainRaw[i]),
              c(yPredTrain[i], yTrain[i]),col = "blue",lwd=3);
    
}
legend("topleft", c("Truth On Test", "Prediction On Test"), col = c("blue", "red"), lwd = 2)


