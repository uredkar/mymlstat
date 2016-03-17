rm(list=ls())
# kevin murphy's book octave to r

library(ggvis)
library(caret)
library(gmodels)
library(class)
library(reshape2)
set.seed(1234)

library(R.matlab)
library(matlab)

rotate <- function(x) t(apply(x, 2, rev))

mnist.data = readMat("data/mnistAll/mnistAll.mat")
mnist.data = mnist.data[[1]]
mnist.data %>% str
mnist.data['train.images']

x = matrix(mnist.data['train.images',,],28,28)
#x %>% head
#x == train.images

# there could be better way to do this, 
# but this works thanks to : https://www.kaggle.com/c/seizure-prediction/forums/t/10453/load-the-mat-file-in-r
# extract training images
mnist.data %>% str
matTrain = do.call(rbind, mnist.data['train.images',,])
ntrain = length(matTrain)/28/28
train.images = matlab::reshape(matTrain,28*28,ntrain)
train.labels = do.call(rbind, (mnist.data['train.labels',,]))

x = matrix(train.images[,110],28,28)
imagesc(x)
image(x)
?image


# find index of the all the images for digit 7
ndx = find(train.labels == 7)
NTrain = 1000
ndx = ndx[1:NTrain]
str(matTrain)
matTrain = train.images[,ndx]
str(matTrain)


Xtrain = matTrain
str(Xtrain)
m = mean(as.vector(Xtrain))
m
x = as.numeric(Xtrain>m)
Xtrain = matrix(x,size(matTrain))
str(Xtrain)
img = matrix(Xtrain[,1],nrow=28,ncol = 28)
str(img)
imagesc(img)


x <- 1:10
y <- 1:10
plot(x,y, type='n',axes = FALSE)
for(i in 1:5)
{
  for(j in 1:5)  
  {
    img = matrix(Xtrain[,i*j],28,28)
    imagesc(img)
  }
  
}

 

#% Fit model

Non = apply(Xtrain == TRUE,MARGIN = 1,sum)
Noff = apply(Xtrain == FALSE,MARGIN = 1,sum)

a = 1; b = 1; #% Laplace smoothing
pOn = (Non + a) / (Non + Noff + a + b); #% posterior mean

img = matrix(pOn,nrow = 28, ncol = 28)
imagesc(img)

#% Generate samples from posterior predictive distribution
Nsim = 100;
Npixels = size(Xtrain,1);


repx = repmat(pOn,Nsim,1)
size(repx)
Xsamp = matrix(runif(Nsim * Npixels),
               ncol=Npixels,nrow=Nsim) 
size(Xsamp)
x = as.numeric(runif(Nsim * Npixels) < as.vector(repx))
Xsamp2 = matrix(x,size(Xsamp))
str(Xsamp2)
img = matrix(Xsamp2[24,],nrow = 28,ncol = 28)
imagesc(img) # this looks like 7

x <- 1:10
y <- 1:10
plot(x,y, type='n',axes = FALSE)
#Xsamp2[1,] %>% length()
#28*28
for(i in 1:10)  
{
  for(j in 1:10)  
  {
    img = matrix(Xsamp2[i*j,],nrow = 28,ncol = 28)
    img = rot90(img,k=3) # rotation is required 
    subplot(image(img,axes=FALSE),x[i] , y[j], size=c(0.3,0.3)) 
    #imagesc(img)
  }
}  
