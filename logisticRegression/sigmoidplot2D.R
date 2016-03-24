rm(list = ls())
# kevin murphy's book octave to r

library(ggvis)
library(caret)
library(gmodels)
library(class)
library(reshape2)
library(dplyr)
library(R.matlab)
library(matlab)
library(plot3D)
library(limSolve)
library(mboost)

#%% Plot 2 d sigmoid functions for
#    various values of w1, w2.
#% i.e. sigmoid(w1 * x1 + %w2*x2)
#%%

#% This file is from pmtk3.googlecode.com

#% Plot sigmoids with these parameters
#%w_1 = [-2; -2;0;1;1;2;2;3;5;5];
#%w_2 = [-1;3;2;4;0;2;-2;0;4;1];

w_1 = c(-3,-3, 3,3,0  ,0, 0,3,-3,0.2,0.02);
w_2 = c(-3, 3,-3,3,0.5,3,-3,0, 0,0.1,0.01);


lowRes = 0;
#% Set to 1 for black and white printing
fullscreen = 0;
#% Set to 1 to enlarge figure window

left = min(w_1) - 2;
right = max(w_1) + 2;
bottom = min(w_2) - 2;
top = max(w_2) + 2;


x1 = seq(from = -10, to = 10, by = 0.1)
y1 = x1
g = meshgrid(x1, y1);
x = g$x
y = g$y
m = n = length(g$x)

for (i in 1:length(w_1))
{
    
    title = paste0("w(", w_1[i],",", w_2[i],")")
    wtsum = x * w_1[i] + y * w_2[i]
    z = pracma::sigmoid(wtsum, a = 1, b = 1)
    plot3D::persp3D(x = x, y = y, z = z, box = TRUE, shade = 0.15,
                    axes = TRUE, nticks = 5, ticktype = "detailed", zlab = "sigmoid",
                    theta = 40, phi = 20, expand = 0.75, main = title)
    
}

