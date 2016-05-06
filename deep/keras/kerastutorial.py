"""
[global]
device = cpu
floatX = float32

[gcc]
cxxflags = -shared -IC:\Anaconda3\MinGW\x86_64-w64-mingw32\include -LC:\Anaconda3\libs -lpython35 -DMS_WIN64

[nvcc]
compiler_bindir=C:\MinGW\bin
Oldcompiler_bindir=C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\bin
"""
from __future__ import print_function
import numpy as np
np.random.seed(1337)  # for reproducibility
import theano
from keras.datasets import mnist
from keras.models import Sequential
from keras.layers.core import Dense, Dropout, Activation, Flatten
from keras.layers.convolutional import Convolution2D, MaxPooling2D
from keras.utils import np_utils

from keras import callbacks

remote = callbacks.ProgbarLogger()

batch_size = 128
nb_classes = 10
nb_epoch = 12

# input image dimensions
img_rows, img_cols = 28, 28
# number of convolutional filters to use
nb_filters = 32
# size of pooling area for max pooling
nb_pool = 2
# convolution kernel size
nb_conv = 3


# the data, shuffled and split between tran and test sets
(X_train, y_train), (X_test, y_test) = mnist.load_data()

X_train = X_train.reshape(X_train.shape[0], 1, img_rows, img_cols)
X_test = X_test.reshape(X_test.shape[0], 1, img_rows, img_cols)
X_train = X_train.astype('float32')
X_test = X_test.astype('float32')
X_train /= 255
X_test /= 255
print('X_train shape:', X_train.shape)
print(X_train.shape[0], 'train samples')
print(X_test.shape[0], 'test samples')

# convert class vectors to binary class matrices
Y_train = np_utils.to_categorical(y_train, nb_classes)
Y_test = np_utils.to_categorical(y_test, nb_classes)



model = Sequential()

model.add(Convolution2D(nb_filters, nb_conv, nb_conv,
                        border_mode='valid', activation='relu',
                        input_shape=(1, img_rows, img_cols)))
model.add(Convolution2D(nb_filters, nb_conv, nb_conv, activation='relu'))
model.add(MaxPooling2D(pool_size=(nb_pool, nb_pool)))
model.add(Dropout(0.25))
model.add(Flatten())
model.add(Dense(128, activation='relu'))
model.add(Dropout(0.5))
model.add(Dense(nb_classes, activation='softmax'))

model.compile(loss='categorical_crossentropy', optimizer='adadelta',  metrics=['accuracy'])


model.fit(X_train, Y_train, batch_size=batch_size, nb_epoch=nb_epoch,
          show_accuracy=True, verbose=1, validation_data=(X_test, Y_test), 
          callbacks=[remote])
score = model.evaluate(X_test, Y_test,verbose=0)

print(model.metrics_names,score)

model.save_weights('../data/mnistmodelweights.h5')


json_string = model.to_json()
open('../data/mnistmodelarchitecture.json', 'w').write(json_string)


from keras.models import model_from_json
model2 = model_from_json(open('../data/mnistmodelarchitecture.json').read())
model2.load_weights('../data/mnistmodelweights.h5')

test2Y = model.predict_classes(X_test, verbose=2)

test2Y[1:10]
Y_test[1:10]