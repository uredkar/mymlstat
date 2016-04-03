# ------------------------------------------------------------------------------
# Book:       	MVA3
# ------------------------------------------------------------------------------
# Quantlet:   	MVASVMspiral
# ------------------------------------------------------------------------------
# Description:	MVASVMspiral plots the area of two different groups of 
#		        spiral data using anisotropic Gaussian kernel.
# ------------------------------------------------------------------------------
# See also:     MVA3svm01, MVAppsib, ppsib, ppsibexample
# ------------------------------------------------------------------------------
# Keywords:     SVM
# ------------------------------------------------------------------------------
# Usage:      	-
# ------------------------------------------------------------------------------
# Inputs:     	-
# ------------------------------------------------------------------------------
# Output:     	2-DIm Plot of a svm classification
#		        for spiral data
# ------------------------------------------------------------------------------
# Example:    	Copy the code and paste in R
#		        Change value of r in anisotropic gaussian kernel for
#		        different parameter values
# ------------------------------------------------------------------------------
# Author:     	Wolfgang H?rdle, Dedy Dwi Prastyo, 20110707
# ------------------------------------------------------------------------------

## with some changes by me, just cleaning up
rm(list=ls())
library(kernlab)
library(tseries)
library(quadprog)
library(zoo)
library(grid)
library(e1071)
library(MASS)
library(ggplot2)

# Generating spiral data
tetha = seq(length = 100, from = 0, to = 3)
a = 1
b = 0.5 # b is near to zero, spiral approaches a circle
r = a * exp(b * tetha)
plot(tetha,r, type = "l")

# X(+1) members of first group, centering in (c1p,c2p)
c1p = 0
c2p = 0
X1p = c1p + r * cos(tetha * pi)
X2p = c2p + r * sin(tetha * pi)

plot(tetha, X1p, type = "l")
plot(tetha, X2p, type = "l")
plot(X1p, X2p, type = "l")
plot(X2p, X1p, type = "l")
points(c1p, c2p, pch = "x", lwd = "2", col = "red")

# X(-1) members of second group, centering in (c1n,c2n)
c1n = 1
c2n = 0
X1n = 1 - (c1n + r * cos(tetha * pi))
X2n = -(c2n + r * sin(tetha * pi))
plot(tetha, X1n, type = "l")
plot(tetha, X2n, type = "l")
plot(X1n, X2n, type = "l")
points(c1n, c2n, pch = "x", lwd = "2", col = "red")

# Aggregating data
X1 = c(X1p, X1n)
X2 = c(X2p, X2n)

# Generating indicator variable

yp = array(1:100, dim = c(100, 1))
yn = array(1:100, dim = c(100, 1))
for (i in 1:100) {
    yp[i] = 1
    yn[i] = -1
}
Y = c(yp, yn)

# Generating noise, N(0,0.01)
e = rnorm(200, mean = 0, sd = 0.1)
X1 = X1 + e
X2 = X2 + e

Spiral.noise = cbind(X2, X1)

plot(Spiral.noise[, 1])
plot(Spiral.noise[, 2])

plot(Spiral.noise[, 1], Spiral.noise[, 2],pch=19, col="blue")

# define gaussian kernel function

gaussian = function(xi, xj) {
    exp( - sum((xi - xj) * RB * (xi - xj)))
    }

class(gaussian) = "kernel"

# compute Radial Basis (RB)

r = 0.1 # parameter r in anisotropic gaussian kernel

X = cbind(X2, X1)
RB = solve(cov(X)) / (r) ^ 2

# compute SVM score value

SpiralModel = ksvm(Spiral.noise, Y, type = "C-svc",
                        kernel = gaussian,
                        kpar = list(RB), C = 10 / 200,
                        prob.model = TRUE, cross = 4)


# C-svc : SVM for classification
# C : cost of constraints violation in the term of Lagrangian formulation

# create SVM classification plot

plot(SpiralModel, data = Spiral.noise, xlim = c(-4, 4), ylim = c(-4, 4))


# ------------------------------------------------------------------------------
# Book:       	MVA3
# ------------------------------------------------------------------------------
# Quantlet:   	MVASVMspiral
# ------------------------------------------------------------------------------
# Description:	MVASVMspiral plots the area of two different groups of 
#		        spiral data using anisotropic Gaussian kernel.
# ------------------------------------------------------------------------------
# See also:     MVA3svm01, MVAppsib, ppsib, ppsibexample
# ------------------------------------------------------------------------------
# Keywords:     SVM
# ------------------------------------------------------------------------------
# Usage:      	-
# ------------------------------------------------------------------------------
# Inputs:     	-
# ------------------------------------------------------------------------------
# Output:     	2-DIm Plot of a svm classification
#		        for spiral data
# ------------------------------------------------------------------------------
# Example:    	Copy the code and paste in R
#		        Change value of r in anisotropic gaussian kernel for
#		        different parameter values
# ------------------------------------------------------------------------------
# Author:     	Wolfgang H?rdle, Dedy Dwi Prastyo, 20110707
# ------------------------------------------------------------------------------

# Generating spiral data
tetha = seq(length = 100, from = 0, to = 3)
a = 1
b = 0.5 # b is near to zero, spiral approaches a circle
r = a * exp(b * tetha)

# X(+1) members of first group, centering in (c1p,c2p)
c1p = 0
c2p = 0
X1p = c1p + r * cos(tetha * pi)
X2p = c2p + r * sin(tetha * pi)

# X(-1) members of second group, centering in (c1n,c2n)
c1n = 1
c2n = 0
X1n = 1 - (c1n + r * cos(tetha * pi))
X2n = -(c2n + r * sin(tetha * pi))

# Aggregating data
X1 = c(X1p, X1n)
X2 = c(X2p, X2n)

# Generating indicator variable

yp = array(1:100, dim = c(100, 1))
yn = array(1:100, dim = c(100, 1))
for (i in 1:100) {
    yp[i] = 1
    yn[i] = -1
}
Y = c(yp, yn)

# Generating noise, N(0,0.01)
e = rnorm(200, mean = 0, sd = 0.1)
X1 = X1 + e
X2 = X2 + e

Spiral.noise = cbind(X2, X1)

# ---------------------------------------------------------------------------------------

# Main program for SVM classification plot
# read simulated data


# define gaussian kernel function

gaussian = function(xi, xj) {
    exp( - sum((xi - xj) * RB * (xi - xj)))
}
class(gaussian) = "kernel"

# compute Radial Basis (RB)

r = 0.1 # parameter r in anisotropic gaussian kernel

X = cbind(X2, X1)
RB = solve(cov(X)) / (r) ^ 2

# compute SVM score value

SpiralModel = ksvm(Spiral.noise, Y, type = "C-svc", kernel = gaussian, kpar = list(RB), C = 10 / 200, prob.model = TRUE, cross = 4)


# C-svc : SVM for classification
# C : cost of constraints violation in the term of Lagrangian formulation

# create SVM classification plot

plot(SpiralModel, data = Spiral.noise, xlim = c(-4, 4), ylim = c(-4, 4))


###########################33

#install.packages("e1071")
#SVM

data(iris)
sample = iris[sample(nrow(iris)),]
train = sample[1:105,]
test = sample[106:150,]
tune = tune(svm, Species ~ ., data = train, kernel = "radial", scale = FALSE, ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
tune$bestmodel
model = svm(Species ~ ., data = train, kernel = "radial", cost = 10, scale = FALSE)
pred = predict(model, test)
pred


################################3

####################################################
# Some Plotting Functions
####################################################


getLineParameters <- function(p1, p2) {
    gradient <- (p2[2] - p1[2]) / (p2[1] - p1[1])
    intercept <- p1[2] - gradient * p1[1]
    return(list(gradient = gradient, intercept = intercept))
}

getEuclideanDistance <- function(x1, x2, y1, y2) {
    return(sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2))
}

getMarginSegmentCoordinates <- function(xvar, yvar, intercept, gradient) {
    # 1.Compute all the coordinates of the perpendicular line segments
    # from your data points on the line
    a <- intercept
    b <- gradient
    xnorm <- (xvar + b * yvar - a * b) / (1 + b ^ 2)
    ynorm <- a + b * xnorm

    distances <- mapply(getEuclideanDistance, xvar, xnorm, yvar, ynorm)
    selector <- distances == min(distances)

    # Return a list with the input points you kept and the 
    return(list(xvar = xvar[selector], yvar = yvar[selector], xnorm = xnorm[selector], ynorm = ynorm[selector], margin = min(distances)))
}

getAllSegmentCoordinates <- function(xvar, yvar, intercept, gradient) {
    # 1.Compute all the coordinates of the perpendicular line segments
    # from your data points on the line
    a <- intercept
    b <- gradient
    xnorm <- (xvar + b * yvar - a * b) / (1 + b ^ 2)
    ynorm <- a + b * xnorm

    # Return a list with the input points you kept and the 
    return(list(xvar = xvar, yvar = yvar, xnorm = xnorm, ynorm = ynorm))
}

convertCoeffsToLineParameters <- function(coeffs) {
    b0 <- coeffs[1]
    b1 <- coeffs[2]
    b2 <- coeffs[3]
    return(list(intercept = (b0 / -b2), gradient = b1 / -b2))
}

####################################################
# The Plots Themselves
####################################################


set.seed(808881)
n <- 100
x1 <- mvrnorm(n, mu = c(13, 45), Sigma = matrix(c(2, 0.5, 05, 2), nrow = 2))
set.seed(675682)
x2 <- mvrnorm(n, mu = c(19, 39), Sigma = matrix(c(2, 0.5, 05, 2), nrow = 2))
f1 <- c(x1[, 1], x2[, 1])
f2 <- c(x1[, 2], x2[, 2])
z <- c(rep.int(1, n), rep.int(-1, n))

l1 <- getLineParameters(c(12, 40.8), c(20, 42.9))
l2 <- getLineParameters(c(15, 36), c(18, 48))


df = data.frame(f1, f2, y = as.factor(z))
svmfit <- svm(y ~ ., data = df, kernel = "linear", cost = 100, scale = FALSE)
w <- t(svmfit$coefs) %*% svmfit$SV
coefs <- c( - svmfit$rho, w)
lineparams <- convertCoeffsToLineParameters(coefs)

####################################################
# Plot 1 showing different separating lines
####################################################


p <- ggplot(data = NULL, aes(x = f1, y = f2, shape = ifelse(z > 0, "Class 1", "Class -1")))
p <- p + geom_point()
p <- p + ggtitle("Linearly Separable Classes in 2D Feature Space")
p <- p + theme(plot.title = element_text(lineheight = .8, face = "bold", vjust = 2), legend.position = "bottom")
p <- p + xlab("f1")
p <- p + ylab("f2")
p <- p + scale_shape_manual(name = "Class Labels", values = c(1, 15))
p <- p + geom_abline(intercept = l1$intercept, slope = l1$gradient, aes(linetype = "General Separating Line"), size = 0.5, show_guide = T)
p <- p + geom_abline(intercept = l2$intercept, slope = l2$gradient, aes(linetype = "General Separating Line"), size = 0.5, show_guide = T)
p <- p + geom_abline(intercept = lineparams$intercept, slope = lineparams$gradient, aes(linetype = "Line of Maximum Margin Separation"), size = 1, show_guide = T)
p <- p + scale_linetype_manual(name = "Separating Lines", values = c("dashed", "solid"))
p <- p + guides(shape = guide_legend(override.aes = list(linetype = 0)),
       linetype = guide_legend())
p

####################################################
# Plot 2 showing margin
####################################################

marlist <- getAllSegmentCoordinates(f1, f2, lineparams$intercept, lineparams$gradient)
distances <- mapply(getEuclideanDistance, marlist$xvar[svmfit$index], marlist$xnorm[svmfit$index], marlist$yvar[svmfit$index], marlist$ynorm[svmfit$index])


p <- ggplot()
p <- p + geom_point(data = NULL, aes(x = f1, y = f2, shape = ifelse(z > 0, "Class 1", "Class -1")))
p <- p + ggtitle("Computing the Margin of a Decision Boundary in 2D Feature Space")
p <- p + theme(plot.title = element_text(lineheight = .8, face = "bold", vjust = 2), legend.position = "bottom")
p <- p + xlab("f1")
p <- p + ylab("f2")
p <- p + scale_shape_manual(name = "Class Labels", values = c(1, 15))
p <- p + geom_abline(intercept = lineparams$intercept, slope = lineparams$gradient, size = 1, show_guide = T)
p <- p + guides(shape = guide_legend(override.aes = list(linetype = 0)),
       linetype = guide_legend())
p <- p + geom_segment(data = NULL, aes(x = marlist$xvar[svmfit$index], y = marlist$yvar[svmfit$index], xend = marlist$xnorm[svmfit$index], yend = marlist$ynorm[svmfit$index]), arrow = arrow(ends = "both", length = unit(0.3, "cm")))
p <- p + annotate("text", x = marlist$xnorm[svmfit$index[2]], y = marlist$ynorm[svmfit$index[2]], label = paste("Margin = ", round(distances[1], 2)), hjust = 1.7, vjust = -1.5)
p

####################################################
# Plot 3
####################################################

newf1 <- c(f1, 16)
newf2 <- c(f2, 40)
newz <- c(z, 1)
newdf = data.frame(f1 = newf1, f2 = newf2, y = as.factor(newz))
svmfit2 <- svm(y ~ ., data = newdf, kernel = "linear", cost = 100, scale = FALSE)
w2 <- t(svmfit2$coefs) %*% svmfit2$SV
coefs2 <- c( - svmfit2$rho, w2)
lineparams2 <- convertCoeffsToLineParameters(coefs2)
marlist2 <- getAllSegmentCoordinates(newf1, newf2, lineparams2$intercept, lineparams2$gradient)
distances2 <- mapply(getEuclideanDistance, marlist2$xvar[svmfit2$index], marlist2$xnorm[svmfit2$index], marlist2$yvar[svmfit2$index], marlist2$ynorm[svmfit2$index])


p <- ggplot()
p <- p + geom_point(data = NULL, aes(x = newf1, y = newf2, shape = ifelse(newz > 0, "Class 1", "Class -1")))
p <- p + ggtitle("Computing the Margin of a Decision Boundary in 2D Feature Space")
p <- p + theme(plot.title = element_text(lineheight = .8, face = "bold", vjust = 2), legend.position = "bottom")
p <- p + xlab("f1")
p <- p + ylab("f2")
p <- p + scale_shape_manual(name = "Class Labels", values = c(1, 15))
p <- p + geom_abline(intercept = lineparams2$intercept, slope = lineparams2$gradient, size = 1, show_guide = T)
p <- p + guides(shape = guide_legend(override.aes = list(linetype = 0)),
       linetype = guide_legend())
p <- p + geom_segment(data = NULL, aes(x = marlist2$xvar[svmfit2$index], y = marlist2$yvar[svmfit2$index], xend = marlist2$xnorm[svmfit2$index], yend = marlist2$ynorm[svmfit2$index]), arrow = arrow(ends = "both", length = unit(0.2, "cm")))
p <- p + annotate("text", x = marlist2$xnorm[svmfit2$index[2]], y = marlist2$ynorm[svmfit2$index[2]], label = paste("Margin = ", round(distances2[1], 2)), hjust = 1.7, vjust = -1.5)
p

