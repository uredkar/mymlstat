library(matlab)
library(R.matlab)
print("Not yet done, this is work in progess!!!!!")
first = 1; last = 100; #Range for the hypothesis space


# generate even and odd numbers
evenOdd = function(first, last)
{
  even = seq(from = first + 1, to = last, by = 2)
  odd  = seq(from = first, to = last, by = 2)
  hyps = list("even" = even,"odd" = odd)
  sizes = c(size(even,2), size(odd,2))
  list(hyps = hyps, names = names(hyps), sizes = sizes)
  
}

#testing
#x = evenOdd(first, last)
#x$hyps


predicateBased = function(FUN, name, first, last)
{
  range = seq(from = first, to = last, by = 1)
  logicalNDX = FUN(range)
  size = sum(logicalNDX)
  hyp = range[logicalNDX]
  list(hyp = hyp, name = name, size = size)
}

pred = function(x)
{
  x == round(sqrt(x))^2
}
#testing
#predicateBased(FUN = pred, name = 'squares',first, last)

multiples = function(ints,first,last)
{
  n     = length(ints)
  hyps  = cell(n,1)
  names = cell(n,1)
  sizes = cell(n,1)
  for(i in 1:n)
  {
    m = ints[i]
    xmax = ceil(last/m)
    hyps [[i]] = intersect(seq(first,last), m * (seq(first,xmax)))
    names[[i]] = sprintf('mult of %d', m);
    sizes[[i]] = size(hyps[[i]],2);
  }
  list(hyps = hyps, names = names, sizes = sizes)
}

#testing 
#m = multiples(c(3,9,10), 1,100)
endingIn = function(ints,first,last)
{
  fst = first - 1
  lst = last - 10
  n = length(ints)
  hyps =  cell(n,1) 
  names = cell(n,1)
  sizes = cell(n,1)
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
    sizes[[i]] = size(hyps[[i]],2)
  }
  
  list(hyps = hyps, names = names, sizes = sizes)
}
#endingIn(seq(1,3),1,1000)
