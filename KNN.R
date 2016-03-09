#
# this file has two test of knn3, first on using iris , second one using mnistAll.mat from kevin murphy dataset



#https://www.datacamp.com/community/tutorials/machine-learning-in-r
rm(list=ls())


library(ggvis)
library(caret)
library(gmodels)
library(class)
library(reshape2)
library(ipred)
set.seed(1234)




normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

iris %>% ggvis(~Sepal.Length, ~Sepal.Width, fill = ~Species) %>% layer_points()

iris %>% ggvis(~Petal.Length, ~Petal.Width, fill = ~Species) %>% layer_points()

table(iris$Species)
round(prop.table(table(iris$Species)) * 100, digits = 1)
summary(iris)
summary(iris[c("Petal.Width", "Sepal.Width")])


ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))
iris.training <- iris[ind==1, 1:4]
iris.test <- iris[ind==2, 1:4]
iris.test.check <- iris[ind==2,]

iris_norm <- as.data.frame(lapply(iris.training, normalize))
summary(iris_norm)


iris.trainLabels <- iris[ind==1, 5]
iris.testLabels <- iris[ind==2, 5]

iris_pred <- knn(train = iris_norm, test = iris.test, cl = iris.trainLabels, k=3)
attributes(.Last.value)

CrossTable(x = iris.testLabels, y = iris_pred, prop.chisq=FALSE)


irisKnnFit <- knn3(x=iris.training,y = iris.trainLabels, k = 5)
class(irisKnnFit)
saveRDS(irisKnnFit,file = "irisKnnFit.rds")
prediction = predict.knn3(irisKnnFit, newdata = iris.test[10:20,],type = "class")
prediction
iris.test.check[10:20,]


######################################################## now for something completely different i.e convert octave to R
#now we try to convert kevin murphy's matlab code
rm(list=ls())


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

x = matrix(train.images[,12],28,28,byrow = FALSE)
imagesc(x)

# extract test images
matTest = do.call(rbind, (mnist.data['test.images',,]))
ntest = length(matTest)/28/28
test.images = matlab::reshape(matTest,28*28,ntest)
test.labels = do.call(rbind, (mnist.data['test.labels',,]))

x = matrix(test.images[,12],28,28,byrow = FALSE)
imagesc(x)

x = matrix(train.images[,sample(1:200,1)],28,28,byrow = FALSE)
imagesc(x)

test.labels %>% head

nrow(t(train.images)) 
names(train.labels) = c("Name")

y = as.vector(train.labels) %>% factor

mnistKnnFit <- knn3(x=t(train.images),y = y, k = 5)
class(mnistKnnFit)
saveRDS(mnistKnnFit,file = "mnistKnnFit.rds")
prediction = predict.knn3(mnistKnnFit, newdata = t(test.images[,10:20]),type = "class")
prediction
###### check image it should match prediction
x = matrix(test.images[,12],28,28,byrow = FALSE)
imagesc(x)




