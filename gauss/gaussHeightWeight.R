rm(list=ls())
library(matlab)
library(R.matlab)
library(ggplot2)
library(ggvis)
library(ellipse)

wtht.data = readMat("data/heightWeight/heightWeight.mat")
wtht.data = wtht.data[1]$heightWeightData


Y = wtht.data[,1]
wtht.data = as.data.frame(cbind("ind" = wtht.data[,1],"height" = as.numeric(wtht.data[,2]),
                                "weight" = as.numeric(wtht.data[,3])))
wtht.data %>% head

maleNdx = find(wtht.data$ind == 1)
femaleNdx = find(wtht.data$ind == 2)
classNdx = c(maleNdx, femaleNdx)


wtht.data %>% ggvis(x = ~height, y = ~weight,fill = ~ind) %>% layer_points()

mu = apply(wtht.data[,c("height","weight")],MARGIN = 2,FUN = mean) # sum of columns

sigma = cov(wtht.data$height,wtht.data$weight)

#### but I will use ellipse package in insted of the book code
#http://stackoverflow.com/questions/27659921/ggvis-creating-a-plot-with-multiple-layers-on-different-dataset

df_ellipse <- data.frame()

for(g in unique(wtht.data$ind))
{
  df_ellipse <- rbind(df_ellipse,
                      cbind(as.data.frame(
                        with(wtht.data[wtht.data$ind==g,],
                             ellipse(cor(height, weight),
                                     level=0.7,
                                     scale=c(sd(height),sd(weight)),
                                     centre=c(mean(height),mean(weight))))),
                        ind=g))
}


myPlot <- ggplot(data=wtht.data, aes(x=height, y=weight,colour=ind)) +
            geom_point(size=1.5, alpha=.6) +
            geom_path(data=df_ellipse, aes(x=x, y=y,colour=ind), size=0.5, linetype=1)

myPlot


myFullPlot <- ggvis(data = df_ellipse, x = ~x, y = ~ y) %>% group_by(ind) %>%
  
  layer_paths(stroke = ~ind, strokeWidth := 1) %>%
  
  layer_points(x = ~height, y= ~weight, size := 15, fill= ~ind, data = wtht.data)

myFullPlot


