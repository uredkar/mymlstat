rm(list=ls())
library(mda)
library(matlab)
library(R.matlab)
library(pracma)
library(mixtools) 
library(rethinking)
library(dplyr)
library(ElemStatLearn)
library(rgl)

doMDA = function(data)
{
  names(model.data)  = c("x1","x2","y")
  model.data$x1
  model.data$y
  #options(error=recover)
  #options(error=NULL)
  mixfit = mda(y ~ x1 + x2, data = model.data)
  plot(mixfit, data=model.data, group="pred")
}

# change discrimAnalysisDboundariesDemo.m file in kevin murphy book code
#for i = 1:numel(model)
#[X, y] = mixGaussSample(model(i).mu,  model(i).Sigma, model(i).classPrior,  nsamples); 
#uncomment to generate files
#m =  mixGaussSample(model(i).mu,  model(i).Sigma, model(i).classPrior,  nsamples); 
#f1 = sprintf("data/mixgauss/disrimAnalysisDbound%d.csv",i); this dir may have to be changed for you
#f1
#csvwrite(f1, [X,y]);
# then use the 

for(i in 1:4)
{

  
  filename = sprintf("data/mixgauss/disrimAnalysisDbound%d.csv",i)
  model.data = read.csv(filename)
  doMDA(model.data)
  
}

model2.data = read.csv("data/mixgauss/disrimAnalysisDbound2.csv")
names(model2.data)  = c("x1","x2","y")


model3.data = read.csv("data/mixgauss/disrimAnalysisDbound3.csv")
names(model3.data)  = c("x1","x2","y")

model4.data = read.csv("data/mixgauss/disrimAnalysisDbound4.csv")
names(model4.data)  = c("x1","x2","y")







########### also good for learning get latest from this link
############### http://biostat.mc.vanderbilt.edu/wiki/pub/Main/CourseBios362/mixture-data-complete.R

library(rgl)

data(ESL.mixture)
dat = ESL.mixture
## create 3D graphic, rotate to view 2D x1/x2 projection
par3d(FOV=1,userMatrix=diag(4))
plot3d(dat$xnew[,1], dat$xnew[,2], dat$prob, type="n",
       xlab="x1", ylab="x2", zlab="",
       axes=FALSE, box=TRUE, aspect=1)

## plot points and bounding box
x1r <- range(dat$px1)
x2r <- range(dat$px2)
pts <- plot3d(dat$x[,1], dat$x[,2], 1,
              type="p", radius=0.5, add=TRUE,
              col=ifelse(dat$y, "orange", "blue"))
lns <- lines3d(x1r[c(1,2,2,1,1)], x2r[c(1,1,2,2,1)], 1)

## draw Bayes (True) classification boundary
dat$probm <- with(dat, matrix(prob, length(px1), length(px2)))
dat$cls <- with(dat, contourLines(px1, px2, probm, levels=0.5))
pls <- lapply(dat$cls, function(p) lines3d(p$x, p$y, z=1))

## plot marginal probability surface and decision plane
sfc <- surface3d(dat$px1, dat$px2, dat$prob, alpha=1.0,
                 color="gray", specular="gray")
qds <- quads3d(x1r[c(1,2,2,1)], x2r[c(1,1,2,2)], 0.5, alpha=0.4,
               color="gray", lit=FALSE)

## clear the surface, decision plane, and decision boundary
par3d(userMatrix=diag(4)); pop3d(id=sfc); pop3d(id=qds)
for(pl in pls) pop3d(id=pl)

## fit additive natural cubic spline model
library(splines)
ddat <- data.frame(y=dat$y, x1=dat$x[,1], x2=dat$x[,2])
form.add <- y ~ ns(x1, df=3)+
  ns(x2, df=3)
fit.add  <- glm(form.add, data=ddat, family=binomial(link="logit"))

## compute probabilities plot classification boundary
probs.add <- predict(fit.add, type="response",
                     newdata = data.frame(x1=dat$xnew[,1], x2=dat$xnew[,2]))
dat$probm.add <- with(dat, matrix(probs.add, length(px1), length(px2)))
dat$cls.add <- with(dat, contourLines(px1, px2, probm.add, levels=0.5))
pls <- lapply(dat$cls.add, function(p) lines3d(p$x, p$y, z=1))

## plot probability surface and decision plane
sfc <- surface3d(dat$px1, dat$px2, probs.add, alpha=1.0,
                 color="gray", specular="gray")
qds <- quads3d(x1r[c(1,2,2,1)], x2r[c(1,1,2,2)], 0.5, alpha=0.4,
               color="gray", lit=FALSE)

## clear the surface, decision plane, and decision boundary
par3d(userMatrix=diag(4)); pop3d(id=sfc); pop3d(id=qds)
for(pl in pls) pop3d(id=pl)

## fit tensor product natural cubic spline model
form.tpr <- y ~ 0 + ns(x1, df=4, intercept=TRUE):
  ns(x2, df=4, intercept=TRUE)
fit.tpr  <- glm(form.tpr, data=ddat, family=binomial(link="logit"))

## compute probabilities plot classification boundary
probs.tpr <- predict(fit.tpr, type="response",
                     newdata = data.frame(x1=dat$xnew[,1], x2=dat$xnew[,2]))
dat$probm.tpr <- with(dat, matrix(probs.tpr, length(px1), length(px2)))
dat$cls.tpr <- with(dat, contourLines(px1, px2, probm.tpr, levels=0.5))
pls <- lapply(dat$cls.tpr, function(p) lines3d(p$x, p$y, z=1))

## plot probability surface and decision plane
sfc <- surface3d(dat$px1, dat$px2, probs.tpr, alpha=1.0,
                 color="gray", specular="gray")
qds <- quads3d(x1r[c(1,2,2,1)], x2r[c(1,1,2,2)], 0.5, alpha=0.4,
               color="gray", lit=FALSE)


## clear the surface, decision plane, and decision boundary
par3d(userMatrix=diag(4)); pop3d(id=sfc); pop3d(id=qds)
for(pl in pls) pop3d(id=pl)

## compute probabilities plot classification boundary
## associated with local linear logistic regression
probs.loc <- 
  apply(dat$xnew, 1, function(x0) {
    ## smoothing parameter
    l <- 1
    ## compute (Gaussian) kernel weights
    d <- colSums((rbind(ddat$x1, ddat$x2) - x0)^2)
    k <- exp(-d/2/l^2)
    ## local fit at x0
    fit <- suppressWarnings(glm(y ~ x1 + x2, data=ddat, weights=k,
                                family=binomial(link="logit")))
    ## predict at x0
    as.numeric(predict(fit, type="response", newdata=as.data.frame(t(x0))))
  })

dat$probm.loc <- with(dat, matrix(probs.loc, length(px1), length(px2)))
dat$cls.loc <- with(dat, contourLines(px1, px2, probm.loc, levels=0.5))
pls <- lapply(dat$cls.loc, function(p) lines3d(p$x, p$y, z=1))

## plot probability surface and decision plane
sfc <- surface3d(dat$px1, dat$px2, probs.loc, alpha=1.0,
                 color="gray", specular="gray")
qds <- quads3d(x1r[c(1,2,2,1)], x2r[c(1,1,2,2)], 0.5, alpha=0.4,
               color="gray", lit=FALSE)

## clear the surface, decision plane, and decision boundary
par3d(userMatrix=diag(4)); pop3d(id=sfc); pop3d(id=qds)
for(pl in pls) pop3d(id=pl)

## kernel density classification
## compute kernel density estimates for each class
dens.kde <- 
  lapply(unique(ddat$y), function(uy) {
    apply(dat$xnew, 1, function(x0) {
      ## subset to current class
      dsub <- subset(ddat, y==uy)
      ## smoothing parameter
      l <- 1/2
      ## kernel density estimate at x0
      mean(dnorm(dsub$x1-x0[1], 0, l)*dnorm(dsub$x2-x0[2], 0, l))
    })
  })

## compute prior for each class (sample proportion)
prir.kde <- table(ddat$y)/length(dat$y)

## compute posterior probability Pr(y=1|x)
probs.kde <- prir.kde[2]*dens.kde[[2]]/
  (prir.kde[1]*dens.kde[[1]]+prir.kde[2]*dens.kde[[2]])

## plot classification boundary associated
## with kernel density classification
dat$probm.kde <- with(dat, matrix(probs.kde, length(px1), length(px2)))
dat$cls.kde <- with(dat, contourLines(px1, px2, probm.kde, levels=0.5))
pls <- lapply(dat$cls.kde, function(p) lines3d(p$x, p$y, z=1))

## plot probability surface and decision plane
sfc <- surface3d(dat$px1, dat$px2, probs.kde, alpha=1.0,
                 color="gray", specular="gray")
qds <- quads3d(x1r[c(1,2,2,1)], x2r[c(1,1,2,2)], 0.5, alpha=0.4,
               color="gray", lit=FALSE)

## draw Bayes (True) classification boundary
dat$probm <- with(dat, matrix(prob, length(px1), length(px2)))
dat$cls <- with(dat, contourLines(px1, px2, probm, levels=0.5))
pls <- lapply(dat$cls, function(p) lines3d(p$x, p$y, z=1, lty=2, color="blue"))
pls
