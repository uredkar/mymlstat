# kevin murphy's book
#%% Ridge regression:visualize effect of changing lambda and selecting it with CV and EB

#% This file is from pmtk3.googlecode.com
#%ns = [21 50];
#%for n = ns(:) '
rm(list = ls())
library(pracma)
library(matlab)
library(caret)
mycolor <- c("#60FFFF", "#B52000", "#FF99FF", "#20B500", "#FFCA00",
            "red", "green", "blue", "grey75", "#FFB2B2", "#928F52")

n = 21;
deg = 3;

set.seed(1234)
source("utils.r")
sampling = "thibaux"
data = poly_data_make(sampling = sampling, n = n, deg = deg)
xtrain = data.frame(data$xtrain)
xtest = data.frame(data$xtest)
############ preprocessing ##################
preprocessParams = preProcess(x = xtrain, method = c("center","scale","expoTrans"))
xpptrain = predict(preprocessParams, xtrain)

preprocessParams = preProcess(x = xtest, method = c("center", "scale", "expoTrans"))
xpptest = predict(preprocessParams, xtest)


pptraindata = data.frame(x = xpptrain, y = data$ytrain)
names(pptraindata) = c("x","y")

lm.model = lm(y ~ x, data = pptraindata)
lm.polymodel = lm(y ~ x + poly(x, deg * 2), data = pptraindata)
dtest = data.frame(x = xpptest, y = data$ytest)
names(dtest) = c("x", "y")

ypredTest = predict(lm.model, newdata = dtest)

plot(pptraindata$x, pptraindata$y, pch = 16, col = "blue")

lines(dtest$x, dtest$y, col = "black")
lines(dtest$x, ypredTest, col = "red")

poly.ypredTest = predict(lm.polymodel, newdata = dtest)
lines(dtest$x, poly.ypredTest, col = "red",lwd=3)
title(main="dark red line is fitted with polynomial deg * 2")


library(glmnet)
xtrain = data.frame(cbind(as.numeric(pptraindata$x), as.numeric(rep(1, n)))) # add ones
names(xtrain) = c("x1","x2")
xtest = data.frame(cbind(as.numeric(dtest[, 1]), as.numeric(rep(1, length(dtest[, 1])))))
names(xtest) = c("x1", "x2")
head(xtrain)
head(xtest)

#%% compute train/test error for each  lambda using ridge
logLambdas = logspace(-10,1.3,10);
NL = length(logLambdas);
printNdx = round(linspace(2, NL-1, 3));
testMse = zeros(1, NL);
trainMse = zeros(1,NL);
for (k in 1:NL) {
    lambda = logLambdas[k];

    model <- glmnet(x = as.matrix(xtrain),y = pptraindata$y, alpha = 1, lambda = lambda, thresh = 1e-12)
    train_pred <- predict(model, newx = as.matrix(xtrain))
    test_pred <- predict(model, newx = as.matrix(xtest))
    trainMse[k] = mean((train_pred - pptraindata$y) ^ 2) ## MSE
    testMse[k] = mean((test_pred - dtest$y) ^ 2) ## MSE
}
    



ndx = log(logLambdas)
length(ndx)
length(trainMse)
length(testMse)

plot(ndx, trainMse, type = "l", lwd = 2, col = "blue", pch = '46', ylim = c(14,50), xlab = "log lambda");
points(ndx, trainMse, lwd = 2, col = "blue", pch = 46, cex = 12)
lines(ndx, testMse, type = "l", lwd = 2, col = "red", pch = 2);
points(ndx, testMse, lwd = 2, col = "red", pch = 46, cex = 12)

plot(xtrain[, 1], pptraindata$y)
#plot(xtest[, 1], dtest$y)

model <- glmnet(as.matrix(xtrain), pptraindata$y, alpha = 1, thresh = 1e-12)
model$lambda


lambdas = seq(from = min(model$lambda), to = max(model$lambda), by = .50)
plot(xtest[, 1], dtest$y, type = "p", lwd = 2)
for (lambda in model$lambda) {
    print(lambda)
    pfit = predict(model, s = lambda, newx = as.matrix(xtest), type = "response")
    #str(pfit)
    lines(y = pfit, x = dtest$x, col = lambda)
    

    #title(main = paste(lambda))
 

}

pfit = predict(model, s = 0.01, newx = as.matrix(xtest), type = "class")
str(pfit)

plot(xtest[, 1], dtest$y, type = "p", lwd = 2, col="black")
colno = 1
for (lambda in lambdas[1:6]) {
    print(lambda)
    pfit = predict(model, s = lambda, newx = as.matrix(xtest)) #, type = "response")
    #str(pfit)
    lines(y = pfit, x = dtest$x, col = mycolor[colno])
    colno = colno + 1

    #title(main = paste(lambda))


}

library(ridge)
deg = 5;
LR = lm.ridge(y ~ x + poly(x, deg), data = pptraindata, lambda = logspace(-10, 1.3, 10))

LM = lm(y ~ x + poly(x, deg), data = pptraindata)
ypredTest = predict(LM, newdata = data.frame(x = xtest$x1))
plot(pptraindata$x, pptraindata$y, type = "p", lwd = 2, col = "black", pch = 19)
lines(xtest$x1, ypredTest, col = "black", lwd = 2)

ypredTest <- scale(xtest[, 1], center = F, scale = LR$scales[1]) %*% LR$coef[, which.min(LR$GCV)] + LR$ym

lines(xtest[, 1], ypredTest[, 1], type = "l", lwd = 2, col="red")
lines(xtest[, 1], ypredTest[, 2], type = "l", lwd = 2, col = "blue")
lines(xtest[, 1], ypredTest[, 3], type = "l", lwd = 2, col = "green")
lines(xtest[, 1], ypredTest[, 4], type = "l", lwd = 2, col = "yellow")
lines(xtest[, 1], ypredTest[, 5], type = "l", lwd = 2)
lines(xtest[, 1], ypredTest[, 6], type = "l", lwd = 2)


