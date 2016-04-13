#http://stackoverflow.com/questions/8017427/plotting-data-from-an-svm-fit-hyperplane
# there is some tribal knowledge here

require(e1071) # for svm()                                                                                                                                                          
require(rgl) # for 3d graphics.                                                                                                                                                                                    
set.seed(12345)
seed <- .Random.seed
t <- data.frame(x = runif(100), y = runif(100), z = runif(100), cl = NA)
t$cl <- 2 * t$x + 3 * t$y - 5 * t$z
train1 = t
train1[1:10,]

matplot(train1[1:10,])

t$cl <- as.factor(ifelse(t$cl > 0, 1, -1))
train = t[1:4,]
with(train,plot3d(x , y , z ))

svm_model <- svm(cl ~ x + y + z, t, type = 'C-classification', kernel = 'linear', scale = FALSE)



w <- t(svm_model$coefs) %*% svm_model$SV

detalization <- 100
grid <- expand.grid(seq(from = min(t$x), to = max(t$x), length.out = detalization),
                    seq(from = min(t$y), to = max(t$y), length.out = detalization))

z <- (svm_model$rho - w[1, 1] * grid[, 1] - w[1, 2] * grid[, 2]) / w[1, 3]

plot3d(grid[, 1], grid[, 2], z) # this will draw plane.
# adding of points to the graphics.
points3d(t$x[which(t$cl == -1)], t$y[which(t$cl == -1)], t$z[which(t$cl == -1)], col = 'red',lwd=2)
points3d(t$x[which(t$cl == 1)], t$y[which(t$cl == 1)], t$z[which(t$cl == 1)], col = 'blue',lwd=2)

