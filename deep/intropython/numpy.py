import numpy as np
from numpy import poly1d

def quicksort(arr):
    print(arr)
    l = len(arr)
    if l <= 1:
        return arr
    
    pivot = arr[l // 2 ] # in python 3 use // instead of /
    left = [x for x in arr if x < pivot]
    middle = [x for x in arr if x == pivot]
    right = [x  for x in arr if x > pivot]
    return quicksort(left) + middle + quicksort(right)

y = [3,6, 8 , 1,2 , 1]
x = quicksort(y)
print(x)
a = np.r_[3,[0]*5,-1:1:10j] # row concatenation

np.mgrid[0:5,0:5]

p = poly1d([3,4,5])
print(p)

np.cast['f'](np.pi)

x  = np.r_[-2:4]
print(x)
x = np.array([1,2,3,4,5])
y = np.select([x >= 3, x >= 5],[10,x])

a = np.arange(10)
print(a)
v1 = a[1:2] # view without fancy indexing can be updated
print(v1)
a[1] = 2
print(v1)
v2 = a[1::3]
print(v2)
a[7] = 10
print(v2)
