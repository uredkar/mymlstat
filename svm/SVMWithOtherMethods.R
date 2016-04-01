# http://www.public.iastate.edu/~vardeman/stat602/602x_hw6_sol.pdf
rm(list = ls())
library(MASS)
library(e1071)
library(ada)
library(randomForest)
da = MASS::synth.tr
#da <- read.table("synth.tr", sep = "", quote = "", header = TRUE)
da$yc <- da$yc * 2 - 1
set.seed(1234)
ret.svm <- svm(as.factor(yc) ~ ., data = da)
ret.ada <- ada(as.factor(yc) ~ ., data = da)
ret.rf <- randomForest(as.factor(yc) ~ ., data = da)
### Estabilish grids for classification.
N.grid <- 25
x.grid <- seq(-1.5, 1.0, length = N.grid)
y.grid <- seq(-0.2, 1.3, length = N.grid)
da.grid <- data.frame(xs = rep(x.grid, N.grid),
                      ys = rep(y.grid, rep(N.grid, N.grid)))
f.hat.svm <- as.numeric(predict(ret.svm, da.grid)) * 2 - 3
f.hat.adaBoost <- as.numeric(predict(ret.ada, da.grid)) * 2 - 3
f.hat.random.forest <- as.numeric(predict(ret.rf, da.grid)) * 2 - 3
par(mfrow = c(1, 3))
for (i.method in c("svm", "adaBoost", "random.forest")) {
    eval(parse(text = paste("f.hat <- f.hat.", i.method, sep = "")))
    plot(da.grid$xs, da.grid$ys, col = f.hat + 3, pch = 19, cex = 0.3,
    xlab = "xs", ylab = "ys", main = i.method)
    contour(x.grid, y.grid, matrix(f.hat, nrow = N.grid),
    nlevels = 1, labels = "", add = TRUE)
    points(da$xs, da$ys, pch = (da$yc + 1) / 2 + 1)
}
dev.off()
library(ElemStatLearn)
vowel = ElemStatLearn::vowel.train
da <- vowel[, -1]
Y <- da[, 1]
X <- da[, -1]
color <- c("#60FFFF", "#B52000", "#FF99FF", "#20B500", "#FFCA00",
"red", "green", "blue", "grey75", "#FFB2B2", "#928F52")
### LDA to get first two coordinates
library(MASS)
LDA <- lda(y ~ ., data = vowel.train)
X.pred <- predict(LDA, newdata = vowel.test)
X.1 <- X.pred$x[, 1]
X.2 <- X.pred$x[, 2]

(error.LDA <- mean(as.numeric(X.pred$class) != Y))
### Make grids
X.new <- X.pred$x
x.lim <- range(X.new[, 1])
y.lim <- range(X.new[, 2])
x.grid <- 100
y.grid <- 100
grid <- data.frame(X.1 = seq(x.lim[1], x.lim[2], length = x.grid),
X.2 = rep(seq(y.lim[1], y.lim[2], length = y.grid), each = x.grid))
### adaBoost.mh
library(ada)
set.seed(1234)
ret.ada <- NULL
Y = vowel.test$y
length(Y)
Y.new <- (Y == 3) * 2 - 1
length(Y.new)
length(X.1)
for (i in 1:11) {
    Y.new <- (Y == i) * 2 - 1
    ret <- ada(as.factor(Y.new) ~ X.1 + X.2)
    pred.ada <- predict(ret, newdata = grid, type = "F")
    ret.ada <- cbind(ret.ada, pred.ada)
}




#table(Y, f.hat.adaBoost)
#table(Y, f.hat.random.forest)

grid.class <- apply(ret.ada, 1, which.max)
plot(X.new[, 1], X.new[, 2], col = color[Y], type = "n", main = "Classified by adaBoost",
xlab = "Coordinate 1", ylab = "Coordinate 2")
points(grid[, 1], grid[, 2], col = color[grid.class], pch = 19, cex = 0.3)
points(X.new[, 1], X.new[, 2], pch = Y + 1, cex = 0.8)
### random forest
library(randomForest)
set.seed(1234)
ret.rf <- randomForest(as.factor(Y) ~ X.1 + X.2)
grid.class <- as.numeric(predict(ret.rf, grid))
plot(X.new[, 1], X.new[, 2], col = color[Y], type = "n", main = "Classified by randomForest",
xlab = "Coordinate 1", ylab = "Coordinate 2")
points(grid[, 1], grid[, 2], col = color[grid.class], pch = 19, cex = 0.3)
points(X.new[, 1], X.new[, 2], pch = Y + 1, cex = 0.8)
### compare
par(mfrow = c(2, 2))
plot(X.1, X.2, col = color[Y],
    pch = Y + 1, main = "true",
    xlab = "Coordinate 1 for Training Data", ylab = "Coordinate 2 for Training Data")
plot(X.1, X.2, col = color[f.hat.adaBoost],
        pch = f.hat.adaBoost + 1, main = "adaBoost",
        xlab = "Coordinate 1 for Training Data", ylab = "Coordinate 2 for Training Data")
plot(X.1, X.2, col = color[f.hat.random.forest],
        pch = f.hat.random.forest + 1, main = "random forest",
        xlab = "Coordinate 1 for Training Data", ylab = "Coordinate 2 for Training Data")
### show error
#table(y, f.hat.adaBoost)
#table(y, f.hat.random.forest)
