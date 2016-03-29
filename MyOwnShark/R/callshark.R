rm(list=ls())
#library (devtools)
#load_all (".")

# tbd make it a proper r interface to so that this r code can
# be called out side this package
library(MyOwnShark)
# convert iris to matrix
data = as.matrix(iris[,1:4])

model = SharkKMeansTrain (data, 3)
labels = SharkKMeansPredict (data, model$centroids)
model
