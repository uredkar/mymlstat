# file to test R code
#rm(list=ls())
getwd()

data(PimaIndiansDiabetes, package = "mlbench")
data(BostonHousing, package = "mlbench")

set.seed(12)

rpid <- runif(nrow(PimaIndiansDiabetes))

pid.train <- PimaIndiansDiabetes[rpid >= 0.33,]
pid.test <- PimaIndiansDiabetes[rpid < 0.33,]
length(pid.train)
head(pid.train)
length(pid.test)
head(pid.test)

rbh <- runif(nrow(BostonHousing))
bh.train <- BostonHousing[rbh >= 0.33, -4]
bh.test <- BostonHousing[rbh < 0.33, -4]

pid.stdm <- std.all(diabetes ~ ., pid.train)
pid.std.train <- predict.std(pid.stdm, pid.train)
pid.std.test <- predict.std(pid.stdm, pid.test)

bh.stdm <- std.all(medv ~ ., bh.train)
bh.std.train <- predict.std(bh.stdm, bh.train)
bh.std.test <- predict.std(bh.stdm, bh.test)
