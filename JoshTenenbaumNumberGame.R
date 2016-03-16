library(matlab)
library(R.matlab)
library(dplyr)
print("Not yet done, this is work in progess!!!!!")
first = 1; last = 100; #Range for the hypothesis space


# generate even and odd numbers
evenOdd = function(first, last)
{
  even = seq(from = first + 1, to = last, by = 2)
  odd  = seq(from = first, to = last, by = 2)
  hyps = list(even,odd)
  #sizes = c(size(even,2), size(odd,2))
  #list(hyps = hyps, names = names(hyps), sizes = sizes)
  list(hyps  = hyps, names = c("even","odd"))
}

#testing
#x = evenOdd(first, last)
#x$hyps
#x$names

predicateBased = function(FUN, name, first, last)
{
  range = seq(from = first, to = last, by = 1)
  logicalNDX = FUN(range)
  size = sum(logicalNDX)
  hyps = range[logicalNDX]
  list(hyps = list(hyps), names = list(name))
}

pred = function(x)
{
  x == round(sqrt(x))^2
}
#testing
y = predicateBased(FUN = pred, name = 'squares',first, last)
y$hyps
y$names
multiples = function(ints,first,last)
{
  n     = length(ints)
  hyps  = list(n)
  names = list(n)
  
  for(i in 1:n)
  {
    m = ints[i]
    xmax = ceil(last/m)
    hyps [[i]] = intersect(seq(first,last), m * (seq(first,xmax)))
    names[[i]] = sprintf('mult of %d', m);
    
  }
  list(hyps = hyps, names = names)
}

#testing 
#m = multiples(c(3,9,10), 1,100)
#m$hyps
#m$names
endingIn = function(ints,first,last)
{
  fst = first - 1
  lst = last - 10
  n = length(ints)
  hyps =  list(n) 
  names = list(n)
  
  for(i in 1:n)
  {
    m = ints[i]
    if (m==0)
    {
        hyps[[i]] = seq(fst+10,lst+10, by = 10) + m;
    }
    else
    {
        hyps[[i]] = seq(fst,lst, by = 10) + m;
    }
    names[[i]] =  sprintf('ends in %d', m)
  
  }
  
  list(hyps = hyps, names = names)
}

#e = endingIn(seq(1,3),1,1000)
#e$hyps
#e$names
#%If range = 1:100 and pows = 4:7, hypotheses = [4 16 64] ; [5 25];
#%[6:36]; [7;49]
powers = function(pows,first,last)
{
  n = length(pows)
  hyps = list(n)
  names = list(n)
  sizes = cell(n,1)
  for (i in 1:n)  
  {
    m = pows[i]
    xmax = ceil(log(last)/log(m))
    hyps[[i]] = intersect(seq(first,last),m^seq(first,xmax))
    names[[i]] = sprintf('powers of %d', m)
    #sizes[[i]] = size(hyps[[i]],2)
  }
  list(hyps = hyps, names = names)
}

x = powers(seq(4,7), 1,100)
#x$hyps
#x$names
#http://stackoverflow.com/questions/24556468/r-eqivalent-to-matlab-cell2mat-function
cell2mat<-function(m)
{
  do.call(rbind, apply(m, 1, function(x) do.call(cbind,x)))
}

compose = function(hyp1,hyp2,operator,str)
{
  n = size(hyp1,1)
  hyps = list(n)
  names = list(n)
  
  
  for(i in 1:n)
  {
    hyps[[i]] = t(matrix(operator(hyp1$hyps[[i]] %>% matrix(nrow = 1) %>% t,
                                  hyp2$hyps[[i]] %>% matrix(nrow = 1) %>% t),nrow = 1));
    names[[i]] = paste0(hyp1$names, str, hyp2$names)
  
  }
  list(hyps = hyps, names = names)

}

explicitSet = function(set)
{
  
  n = 1
  hyps  = list(n)
  names = list(n)
  
  
  hyps[[1]] = set
  names[[1]] = as.character(set)
  
  
  list(hyps = hyps,names = names)
}

mathHypothesesSmall = function(first, last)
{
  hypSpace = list(
               evenOdd(first,last),
               predicateBased(FUN = function(x) x == round(sqrt(x))^2, name = 'squares',first, last),
               multiples(3:10,first,last),
               endingIn(1:9,first, last),
               powers(2:10,first, last),
               predicateBased(FUN = function(x)(x==x),'all',first, last),
               compose(powers(2,first,last),explicitSet(list(37)),union,'+'),
               compose(powers(2,first,last),explicitSet(list(32)),setdiff,'-')
  )
  
  list(hypSpace = hypSpace)
  
}

allContiguousIntervals = function(first,last)
{
  i = 1;
  n = last*(last+1)/2;
  hyps = cell(n,1);
  names = cell(n,1);
  sizes = cell(n,1);
  for(s in first:last)
  {
    for(e in s:last)
    {
      hyps[[i]]  = s:e;
      names[[i]] = sprintf('interval %d..%d', s, e);
      sizes[[i]] = size(hyps[i],2);
      i = i + 1;
    }
  }
  list(hyps = hyps, names = names, sizes = sizes);
  
}



tenenbaumHypSpace = function(first, last)
{
  hypSpace = list(
    evenOdd(first,last),
    predicateBased(FUN = function(x) x == round(sqrt(x))^2, name = 'squares',first, last),
    predicateBased(FUN = function(x) x == round(x ^ (1/3))^3, name = 'cubes',first, last),
    predicateBased(FUN = isprime, name = 'primes',first, last),
    multiples(3:12,first,last),
    powers(2:10,first, last),
    endingIn(0:9,first, last),
    allContiguousIntervals(first,last)
    
  )
  
  list(hypSpace = hypSpace)
  
}

mathPriorSmall = function(hypSpace)
{
  prior = c(0.5, 0.5, 0.1*ones(1,28), 0.001 ,0.001)
}


buildExtensionTable = function(hypSpace,last,first)
{
  hypSpaceSize = size(hypSpace$hyps,1);
  inconcept = zeros(last-first+1,hypSpaceSize);
  for(h in 1:hypSpaceSize)
  {
    hyp = hypSpace$hypSpace[h]; 
    inconcept[hyp,h] = 1;
  }  
}



#x = powers(2,first,last)
#p = explicitSet(37)

#y = compose(powers(2,first,last),explicitSet(list(37)),union,'+')
#y$hyps[1,]

hypSpace = mathHypothesesSmall(first = first, last = last)

sizhyper = 0
for(i in 1:2) #length(hypSpace$hypSpace))
{
  print("------------------------x-----------------x-------------------x")
  x1 = hypSpace$hypSpace[[i]]$hyps
  y1 = hypSpace$hypSpace[[i]]$names
  str(x1)
  print(y1)
  l = length(x1)
  for (j in 1:l)
  {
    x2 = x1[j]
    print("len")
    print(length(x2))
    print("values")
    print(x2)
  }
  sizhyper = sizhyper + l
  
  print("------------------------x-----------------x-------------------x")
  
}
print(sizhyper)



