
list(rm = ls())
library(rgl)
library(matlab)
library(pracma)

n = 2000;
w = seq(from = 2 * pi / 3, to = 9 * pi / 2, length.out = 1000)
plot(sin(w))
cor(w,sin(w))
y = sapply(sin(w), FUN = function(x) rnorm(mean = x, n = 10))
size(y)
size(w)
matplot(w, t(y), pch=19)
#cor(w, y[1,])
#library(Hmisc)
#c = rcorr(y, type = "pearson") # type can be pearson or spearman
#str(c)
#plot(c)
#library(corrgram)
#corrgram(y)