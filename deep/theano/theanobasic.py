import theano
import GPy
from theano import tensor as T
import numpy as np

X = T.matrix(dtype=theano.config.floatX)
f = T.sum(T.square(X))
f
my_func = theano.function([X], f) 

# Generate some values for X and evaluate
X_values = np.random.randn(3,4,dtype=numpy.float64)
my_func(X_values) 

# Define gradients
g = theano.grad(f,X)

# Compute the derivarives
mu_new_func = theano.function([X], [f,g],allow_input_downcast=True) 

# Theano derivatives
mu_new_func(X_values)

# Exact derivatives
X_values*2 