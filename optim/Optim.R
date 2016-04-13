#http://zoonek.free.fr/blosxom/R/2012-06-01_Optimization.html

library(quadprog)

##
## Assume we want to minimize: -(0 5 0) %*% b + 1/2 b^T b
## under the constraints: A^T b >= b0
## with b0 = (-8,2,0)^T
## and (-4 2 0)
## A = (-3 1 -2)
## ( 0 0 1)
## we can use solve.QP as follows:
##
Dmat <- matrix(0, 3, 3)
diag(Dmat) <- 1
dvec <- c(0, 5, 0)
Amat <- matrix(c(-4, -3, 0, 2, 1, 0, 0, -2, 1), 3, 3)
bvec <- c(-8, 2, 0)
solve.QP(Dmat, dvec, Amat, bvec = bvec)##
## Assume we want to minimize: -(0 5 0) %*% b + 1/2 b^T b
## under the constraints: A^T b >= b0
## with b0 = (-8,2,0)^T
## and (-4 2 0)
## A = (-3 1 -2)
## ( 0 0 1)
## we can use solve.QP.compact as follows:
##
Dmat <- matrix(0, 3, 3)
diag(Dmat) <- 1
dvec <- c(0, 5, 0)
Aind <- rbind(c(2, 2, 2), c(1, 1, 2), c(2, 2, 3))
Amat <- rbind(c(-4, 2, -2), c(-3, 1, 1))
bvec <- c(-8, 2, 0)
solve.QP.compact(Dmat, dvec, Amat, Aind, bvec = bvec)




#Find x
#To maximize f(x)
#Such that g(x) = 0
#and h(x) >= 0

# Optimization problem with many extrema: non-linear regression
#   y = sin( a * x + b ) + noise
n <- 100
x <- seq(0, 1, length = n)
f <- function(x, a, b) sin(a * x + b)
y <- f(x, 4 * pi * runif(1), 2 * pi * runif(1)) + rnorm(n)
g <- function(a, b) sum((f(x, a, b) - y) ^ 2)
N <- 200
a <- seq(0, 10 * pi, length = N)
b <- seq(0, 2 * pi, length = N)
z <- outer(a, b, Vectorize(g))
image(a, b, z,
  las = 1,
  main = "Multiple extrema",
  xlab = "Frequency",
  ylab = "Phase")
# Compute the extrema visible on the plot
shift <- function(z, i, j) {
    n <- nrow(z)
    m <- ncol(z)
    u <- NA * z
    if (i >= 0 && j >= 0) {
        u[1:(n - i), 1:(m - j)] <- z[(i + 1):n, (j + 1):m]
    } else if (i <= 0 && j >= 0) {
        u[( - i + 1):n, 1:(m - j)] <- z[1:(n + i), (j + 1):m]
    } else if (i >= 0 && j <= 0) {
        u[1:(n - i), ( - j + 1):m] <- z[(i + 1):n, 1:(m + j)]
    } else if (i <= 0 && j <= 0) {
        u[( - i + 1):n, ( - j + 1):m] <- z[1:(n + i), 1:(m + j)]
    }
    u
}
u <- v <- z == z
for (i in c(-1, 0, 1))
    for (j in c(-1, 0, 1))
        if (abs(i) + abs(j) > 0) {
            tmp <- shift(z, i, j)
            u <- u & z >= tmp
            v <- v & z <= tmp
        }
v <- which(v, arr.ind = TRUE)
v <- t(apply(v, 1, function(u) c(a[u[1]], b[u[2]])))
v <- t(apply(v, 1, function(r) optim(r, function(p) g(p[1], p[2]))$par))
points(v, pch = "+", cex = 2)
u <- which(u, arr.ind = TRUE)
u <- t(apply(u, 1, function(u) c(a[u[1]], b[u[2]])))
u <- t(apply(u, 1, function(r) optim(r, function(p) - g(p[1], p[2]))$par))
points(u, pch = "-", cex = 2)
# Not much to see on the 3D plot
library(rgl)
persp3d(a, b, z)

##################


############### require(graphics)

fr <- function(x) {
    ## Rosenbrock Banana function
    x1 <- x[1]
    x2 <- x[2]
    100 * (x2 - x1 * x1) ^ 2 + (1 - x1) ^ 2
}
grr <- function(x) {
    ## Gradient of 'fr'
    x1 <- x[1]
    x2 <- x[2]
    c(-400 * x1 * (x2 - x1 * x1) - 2 * (1 - x1),
       200 * (x2 - x1 * x1))
}
optim(c(-1.2, 1), fr)
(res <- optim(c(-1.2, 1), fr, grr, method = "BFGS"))
optimHess(res$par, fr, grr)
optim(c(-1.2, 1), fr, NULL, method = "BFGS", hessian = TRUE)
## These do not converge in the default number of steps
optim(c(-1.2, 1), fr, grr, method = "CG")
optim(c(-1.2, 1), fr, grr, method = "CG", control = list(type = 2))
optim(c(-1.2, 1), fr, grr, method = "L-BFGS-B")

flb <- function(x) {
    p <- length(x);
    sum(c(1, rep(4, p - 1)) * (x - c(1, x[ - p]) ^ 2) ^ 2)
}
## 25-dimensional box constrained
optim(rep(3, 25), flb, NULL, method = "L-BFGS-B",
      lower = rep(2, 25), upper = rep(4, 25)) # par[24] is *not* at boundary

## "wild" function , global minimum at about -15.81515
fw <- function(x)
10 * sin(0.3 * x) * sin(1.3 * x ^ 2) + 0.00001 * x ^ 4 + 0.2 * x + 80
plot(fw, -50, 50, n = 1000, main = "optim() minimising 'wild function'")

res <- optim(50, fw, method = "SANN",
             control = list(maxit = 20000, temp = 20, parscale = 20))
res
## Now improve locally {typically only by a small bit}:
(r2 <- optim(res$par, fw, method = "BFGS"))
points(r2$par, r2$value, pch = 8, col = "red", cex = 2)

## Combinatorial optimization: Traveling salesman problem
library(stats) # normally loaded

eurodistmat <- as.matrix(eurodist)

distance <- function(sq) {
    # Target function
    sq2 <- embed(sq, 2)
    sum(eurodistmat[cbind(sq2[, 2], sq2[, 1])])
}

genseq <- function(sq) {
    # Generate new candidate sequence
    idx <- seq(2, NROW(eurodistmat) - 1)
    changepoints <- sample(idx, size = 2, replace = FALSE)
    tmp <- sq[changepoints[1]]
    sq[changepoints[1]] <- sq[changepoints[2]]
    sq[changepoints[2]] <- tmp
    sq
}

sq <- c(1:nrow(eurodistmat), 1) # Initial sequence: alphabetic
distance(sq)
# rotate for conventional orientation
loc <- -cmdscale(eurodist, add = TRUE)$points
x <- loc[, 1];
y <- loc[, 2]
s <- seq_len(nrow(eurodistmat))
tspinit <- loc[sq,]

plot(x, y, type = "n", asp = 1, xlab = "", ylab = "",
     main = "initial solution of traveling salesman problem", axes = FALSE)
arrows(tspinit[s, 1], tspinit[s, 2], tspinit[s + 1, 1], tspinit[s + 1, 2],
       angle = 10, col = "green")
text(x, y, labels(eurodist), cex = 0.8)

set.seed(123) # chosen to get a good soln relatively quickly
res <- optim(sq, distance, genseq, method = "SANN",
             control = list(maxit = 30000, temp = 2000, trace = TRUE,
                            REPORT = 500))
res # Near optimum distance around 12842

tspres <- loc[res$par,]
plot(x, y, type = "n", asp = 1, xlab = "", ylab = "",
     main = "optim() 'solving' traveling salesman problem", axes = FALSE)
arrows(tspres[s, 1], tspres[s, 2], tspres[s + 1, 1], tspres[s + 1, 2],
       angle = 10, col = "red")
text(x, y, labels(eurodist), cex = 0.8)


y = a * x1 + b * x2 + c * x3 + noise
a + b + c = 1
a, b, c >= 0.


# Parameters
k <- 3
a <- diff(c(0, sort(runif(k - 1)), 1))
# Data
n <- 1e4
x <- matrix(rnorm(k * n), nc = k)
y <- x %*% a + rnorm(n)
# Log-Likelihood
f <- function(u) sum((y - x %*% u) ^ 2)
# Transform the parameters: we just have
# to find a bijection between R^2 
# and {(a,b,c) \in [0,1]^3 : a+b+c=1}.
g <- function(v) {
    # Ensure they in [0,1]
    v <- 1 / (1 + exp( - v))
    # Ensure they add up to 1
    v <- c(v[1], v[2] * (1 - v[1]))
    u <- c(v, 1 - sum(v))
    u
}
# Minimize the log-likelihood
r <- optim(rnorm(k - 1), function(v) f(g(v)))
g(r$par);
a # Reasonably similar