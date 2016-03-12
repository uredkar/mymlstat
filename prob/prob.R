# probability

set.seed(1234)
# 


plotQuantile = function(x)
{

dens <- density(draws)
plot(dens)

q = quantile(x)

q2     <- q["100%"]
q75    <- q["75%"]
qn25   <- q["25%"]
qn0   <- q["0%"]

x1 <- min(which(dens$x >= max(dens$x)))  
x2 <- max(which(dens$x <  q2))

x3 <- min(which(dens$x >= q2))  
x4 <- max(which(dens$x <  q75))

x5 <- min(which(dens$x >= q75))  
x6 <- max(which(dens$x <  qn25))

x7 <- min(which(dens$x >= qn25))  
x8 <- max(which(dens$x <  qn0))

x9 <- max(which(dens$x <  qn0))
x10 <- min(which(dens$x >= min(dens$x)))  



with(dens, polygon(x = c(x[c(x1,x1:x2,x2)]), y = c(0, y[x1:x2], 0), col="red"))
with(dens, polygon(x = c(x[c(x3,x3:x4,x4)]), y = c(0, y[x3:x4], 0), col="gray"))
with(dens, polygon(x = c(x[c(x5,x5:x6,x6)]), y = c(0, y[x5:x6], 0), col="green"))
with(dens, polygon(x = c(x[c(x7,x7:x8,x8)]), y = c(0, y[x7:x8], 0), col="yellow"))
with(dens, polygon(x = c(x[c(x9,x9:x10,x10)]), y = c(0, y[x9:x10], 0), col="red"))
abline(v=mean(x),lwd=2,col = "blue")
abline(v=median(x),lwd=3, lty = 3)
abline(v=sd(x),lwd=4, lty = 2)
abline(v=-sd(x),lwd=4, lty = 2)

}


#draws <- rnorm(1000)^2
#plotQuantile(draws)

#draws <- rnorm(100)^3
#plotQuantile(draws)
#rm(list=ls())

rgamma(1000, shape = 2)
