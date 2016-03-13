rm(list=ls())
set.seed(1234)
library(R.matlab)
library(matlab)
library(ggplot2)
library(arm)
source("utils.r")
data = poly_data_make(sampling = "thibaux")
xtrain = data$xtrain
ytrain = data$ytrain
xtest  = data$xtest


ytestNoisefree = data$ytestNoisefree
ytestNoisy = data$ytestNoisy
sigma2 = data$sigma2

plot(ytestNoisefree,type="l")
plot(ytestNoisy,type="l")
plot(ytrain,type="l")

plot(xtrain,type="l")

#bglmGaussModel = bayesglm(ytrain ~ xtrain + I(xtrain^deg), family = "gaussian")

#model = lm(ytrain ~ poly(xtrain,1))
#plot(model)
#model = lm(ytrain ~ poly(xtrain,5))
#plot(model)
#p1  = predict(model, newdata = xtest)
#p2  = predict(model, newdata = xtrain)

degs = 1:20
Nm = length(degs)

mseTrain = zeros(1,Nm); 
mseTest = zeros(1,Nm);

for(m in 1:length(degs))
{
    deg = degs[m]
    #pp = preprocessorCreate('rescaleX', true, 'poly', deg, 'addOnes', true);
    model = lm(ytrain ~ xtrain + poly(xtrain, deg)) 
    
    ypredTrain = predict(model,newdata=data.frame(xtrain=xtrain))
    ypredTest  = predict(model,newdata=data.frame(xtrain=xtest))
    
    mseTrain[m] = mean((ytrain - ypredTrain)^2);
    mseTest[m] = mean((ytestNoisy - ypredTest)^2);
    
    
    plot(xtrain ,ytrain, pch=10, main = deg)
    lines(xtrain, ypredTrain, col="blue",lwd=2)
    lines(xtest,ypredTest, col = "black",lwd=2)
    lines(xtest,ypredTest + sd(ypredTest), col = "red")
    lines(xtest,ypredTest - sd(ypredTest), col = "red")
    
}



plot(x = degs, y =  mseTest,  cex=1.9, pch=22, col="blue",  main = "mean prediction error")
lines(x = degs, y = mseTrain, col="red", lwd=2)



