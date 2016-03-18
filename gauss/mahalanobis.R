# code from samples of the package and my additions
# not from kevin murphys code
rm(list=ls())
require(graphics)
library(mvnfast)


set.seed(434)
# Simulating multivariate normal data
N <- 1000
mu <- c(4, 2) # 2d 
sigma <- matrix(c(1, 0.5, 0.5, 1), 2, 2)

X <- rmvn(N, mu = mu, sigma = sigma)

# Plotting the true density function
steps <- 100
range1 <- seq(min(X[ , 1]), max(X[ , 1]), length.out = steps)
range2 <- seq(min(X[ , 2]), max(X[ , 2]), length.out = steps)

grid <- expand.grid(range1, range2)
vals <- dmvn(as.matrix(grid), mu, sigma)
contour(z = matrix(vals, steps, steps), 
        x = range1, 
        y = range2, 
        xlab = "X1", 
        ylab = "X2")
title(main="Bivariate Gaussian")
points(X[ , 1], X[ , 2], pch = '.')

# Estimating the mode from "nrep" starting points
nrep <- 10 
index <- sample(1:N, nrep) # take nrep samples between 1 to N

# estimate by simulation random points from sample statement above
for(ii in 1:nrep) {
  start <- X[index[ii], ] # Take the first point in the sample
  out <- ms(X, init = start, H = 0.1 * sigma, store = TRUE) # each run will result in different lines
  lines(out$traj[ , 1], out$traj[ , 2], col = "red", lwd = 2)
  points(out$final[1], out$final[2], col = "blue", pch = 3, lwd = 3) # Estimated mode (blue)
  points(start[1], start[2], col = "green", pch = 4, lwd = 3) # ii-th starting value
}

eg = eigen(sigma)

ctr = c(out$final[1], out$final[2])
angles <- seq(0, 2*pi, length.out=200) 
eigVal  <- eg$values
eigVec  <- eg$vectors
eigScl  <- eigVec %*% diag(sqrt(eigVal))  # scale eigenvectors to length = square-root
xMat    <- rbind(ctr[1] + eigScl[1, ], ctr[1] - eigScl[1, ])
yMat    <- rbind(ctr[2] + eigScl[2, ], ctr[2] - eigScl[2, ])

ellBase <- cbind(sqrt(eigVal[1])*cos(angles), sqrt(eigVal[2])*sin(angles)) # normal ellipse
ellRot  <- eigVec %*% t(ellBase)                                          # rotated ellipse
lines((ellRot+ctr)[1, ], (ellRot+ctr)[2, ], asp=1, type="l", lwd=2)
matlines(xMat, yMat, lty=1, lwd=2, col="blue")

D2 <- mahalanobis(X, colMeans(X), cov(X))
str(D2)
str(X)

plot(X[,1],D2,col="blue")
points(X[,2],D2,col="red")
abline(v=mu[1],col="blue")
abline(v=mu[2],col="red")
matlines(xMat, yMat, lty=1, lwd=2, col="blue")


plot(density(D2, bw = 0.5),
     main="Squared Mahalanobis distances, n=100, p=3") ; rug(D2)
qqplot(qchisq(ppoints(100), df = 3), D2,
       main = expression("Q-Q plot of Mahalanobis" * ~D^2 *
                           " vs. quantiles of" * ~ chi[3]^2))
abline(0, 1, col = 'gray')
