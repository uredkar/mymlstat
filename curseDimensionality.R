# kevin murphy curseDimensionality
rm(list=ls())
library(ggplot2)
library(ggvis)
library(caret)
library(gmodels)
library(class)
library(reshape2)

ds = c(1.0,3.0,5.0,7.0, 10.0) # original 
ds = seq(1,10,by=2)

s = seq(from = 0,to = 1,length.out = 100)


p = ggplot() 

for(d in ds)
{
  y = s ^ (1/d)
  data = do.call(cbind,list(d,s,y))
  colnames(data) = c("dimension","change","volume")
  data = as.data.frame(data)
  print(d)
  
  p = p + geom_line(data = data,aes(x = change,y = volume, color= factor(dimension))) 
}
p + xlab("Fraction of data in neighbourhood") +
    ylab("Edge length of cube") +
    ggtitle("As the (9) Dimension increases the data is same (.25) % of data is (.80) further away")