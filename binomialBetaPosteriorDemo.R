rm(list = ls())
#%% Example of parameter updating in a Beta-Binomial model
#%
#%%
  
#  % This file is from pmtk3.googlecode.com

library(matlab)

data = data.frame(data = zeros(4,4))
names(data)  = c("a","b","N1","N0")
data[1,] = c(a=2,b=2,N1=4 ,N0 = 1)
data[2,] = c(a=2,b=2,N1=40,N0 = 10)
data[3,] = c(a=1,b=1,N1=4 ,N0 = 1)
data[4,] = c(a=1,b=1,N1=40,N0 = 10)

X = data[1,]

flatPrior.a = 1;
flatPrior.b = 1;

#%%    
figname = c('betaPostInfSmallSample', 'betaPostInfLargeSample', 
            'betaPostUninfSmallSample', 'betaPostUninfLargeSample');

x = linspace(0.001, 0.999, 50); 
for (i in 1:numel(data))
{
  
  #%% Update
  prior.a = data[i,"a"];
  prior.b = data[i,"b"];
  N = data[i,"N0"] + data[i,"N1"];
  nsucc = data[i,"N1"];
  nfail = N - nsucc;
  post.a = prior.a + nsucc;
  post.b = prior.b + nfail;
  lik.a = flatPrior.a + nsucc;
  lik.b = flatPrior.b + nfail; 

  Pprior = exp(dbeta(shape1 = prior.a, shape2 = prior.b, x = x, log = TRUE))
  name1 = sprintf('prior Beta Be(%2.1f, %2.1f)', prior.a, prior.b)
  
  Plik = exp(dbeta(shape1 = lik.a, shape2 = lik.b, x = x, log = TRUE));
  name2 = sprintf('lik Be(%2.1f, %2.1f)', lik.a, lik.b);
  
  Ppost = exp(dbeta(shape1 = post.a, shape2 = post.b, x = x, log = TRUE));
  name3 = sprintf('post Be(%2.1f, %2.1f)', post.a, post.b);
  
  ylim = max(unlist(list(max(Pprior),max(Plik),max(Ppost))))
  
  plot(x, Pprior,type="l", col="red",ylim=c(0,ylim),lwd=2,lty=1,pch=21, ann=FALSE) 
  title(main="Bayesian Beta")
  title(xlab="x")
  title(ylab="Distribution")  
  
  
  
  lines(x = x, y = Plik, col="blue", type = "l", lty=2,pch=22)    
  lines(x, Ppost, col="green", type = "l", lty=3, pch=23)  
  
  legend(0, ylim, c(name1,name2,name3), cex=0.8, 
         col=c("red","blue","green"), pch=21:23, lty=1:3);

}




