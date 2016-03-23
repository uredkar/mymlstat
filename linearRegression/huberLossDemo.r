#%% Plot the Huber loss fn compared to L1 and L2
#% cf Hastie book 2e p350
#%%

#% This file is from pmtk3.googlecode.com

err = seq(from = -3,to = 3,by=0.1);
L1 = abs(err);
L2 = err ^ 2;
delta = 1.5;
ind = abs(err) <= delta;
huber =  0.5 * ind * (err ^ 2) + (1 - ind)  * (delta * (abs(err) - delta / 2));
vapnik = ind * 0 + (1 - ind)  * (abs(err) - delta);

plot(err, L1, col = "blue",type="l",lwd=2);
lines(err, L2, col = "red", lwd = 2);
lines(err, vapnik, col = "purple", lwd = 2);
lines(err, huber, col = "green", lwd = 2);
legend("topright", c("L2", "L1", "Vapnik", "Huber"), col = c("red", "blue","purple","green"), lwd = 2, cex = 0.70)