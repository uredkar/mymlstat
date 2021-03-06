#!/usr/bin/python

# Processes an image to extract the text portions. Primarily
# used for pre-processing for performing OCR.

# Based on the paper "Font and Background Color Independent Text Binarization" by
# T Kasar, J Kumar and A G Ramakrishnan
# http://www.m.cs.osakafu-u.ac.jp/cbdar2007/proceedings/papers/O1-1.pdf

# Copyright (c) 2012, Jason Funk <jasonlfunk@gmail.com>
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software
# and associated documentation files (the "Software"), to deal in the Software without restriction,
# including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all copies or substantial
# portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
# LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
# WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

import cv2
import numpy as np
import sys
import os.path

#if len(sys.argv) != 3:
 #   print("%s input_file output_file" % (sys.argv[0]))
    #sys.exit()
#else:
    #input_file = sys.argv[1]
    #output_file = sys.argv[2]

input_file = "C:/sources/Quantnet/data/noisy.jpg"
#input_file = "C:/sources/Quantnet/data/imagewithtext.jpg"

output_file = "C:/sources/Quantnet/data/denoisy.png"
if not os.path.isfile(input_file):
    print("No such file '%s'" % input_file)
    sys.exit()

DEBUG = 1

image = cv2.imread(input_file)

hsv = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)


# construct the argument parse and parse the arguments

# define the list of boundaries
boundaries = [
	([17, 15, 100], [50, 56, 200]),
	([86, 31, 4], [220, 88, 50]),
	([25, 146, 190], [62, 174, 250]),
	([103, 86, 65], [145, 133, 128])
]

for (lower, upper) in boundaries:
	# create NumPy arrays from the boundaries
	lower = np.array(lower, dtype = "uint8")
	upper = np.array(upper, dtype = "uint8")

	# find the colors within the specified boundaries and apply
	# the mask
	mask = cv2.inRange(image, lower, upper)
	output = cv2.bitwise_and(image, image, mask = mask)

	# show the images
	cv2.imshow("images", np.hstack([image, output]))
	cv2.waitKey(0)


cv2.imwrite('C:/sources/Quantnet/data/changed.jpg',hsv)


gray = cv2.cvtColor(image,cv2.COLOR_BGR2GRAY) # grayscale
cv2.imwrite("C:/sources/Quantnet/data/gray.jpg", gray) 
_,thresh = cv2.threshold(gray,150,255,cv2.THRESH_BINARY_INV) # threshold
kernel = cv2.getStructuringElement(cv2.MORPH_CROSS,(3,3))
dilated = cv2.dilate(thresh,kernel,iterations = 13) # dilate
img2, contours, hierarchy = cv2.findContours(dilated,cv2.RETR_EXTERNAL,cv2.CHAIN_APPROX_NONE) # get contours
#img2
cv2.imwrite("C:/sources/Quantnet/data/contoured2.jpg", img2) 
# for each contour found, draw a rectangle around it on original image
for contour in contours:
    # get rectangle bounding contour
    [x,y,w,h] = cv2.boundingRect(contour)

    # discard areas that are too large
    if h>300 and w>300:
        continue

    # discard areas that are too small
    if h<40 or w<40:
        continue

    # draw rectangle around contour on original image
    cv2.rectangle(image,(x,y),(x+w,y+h),(255,0,255),2)

# write original image with added contours to disk  
cv2.imwrite("C:/sources/Quantnet/data/contoured.jpg", image) 

# Determine pixel intensity
# Apparently human eyes register colors differently.
# TVs use this formula to determine
# pixel intensity = 0.30R + 0.59G + 0.11B
def ii(xx, yy):
    global img, img_y, img_x
    if yy >= img_y or xx >= img_x:
        #print "pixel out of bounds ("+str(y)+","+str(x)+")"
        return 0
    pixel = img[yy][xx]
    return 0.30 * pixel[2] + 0.59 * pixel[1] + 0.11 * pixel[0]


# A quick test to check whether the contour is
# a connected shape
def connected(contour):
    first = contour[0][0]
    last = contour[len(contour) - 1][0]
    return abs(first[0] - last[0]) <= 1 and abs(first[1] - last[1]) <= 1


# Helper function to return a given contour
def c(index):
    global contours
    return contours[index]


# Count the number of real children
def count_children(index, h_, contour):
    # No children
    if h_[index][2] < 0:
        return 0
    else:
        #If the first child is a contour we care about
        # then count it, otherwise don't
        if keep(c(h_[index][2])):
            count = 1
        else:
            count = 0

            # Also count all of the child's siblings and their children
        count += count_siblings(h_[index][2], h_, contour, True)
        return count


# Quick check to test if the contour is a child
def is_child(index, h_):
    return get_parent(index, h_) > 0


# Get the first parent of the contour that we care about
def get_parent(index, h_):
    parent = h_[index][3]
    while not keep(c(parent)) and parent > 0:
        parent = h_[parent][3]

    return parent


# Count the number of relevant siblings of a contour
def count_siblings(index, h_, contour, inc_children=False):
    # Include the children if necessary
    if inc_children:
        count = count_children(index, h_, contour)
    else:
        count = 0

    # Look ahead
    p_ = h_[index][0]
    while p_ > 0:
        if keep(c(p_)):
            count += 1
        if inc_children:
            count += count_children(p_, h_, contour)
        p_ = h_[p_][0]

    # Look behind
    n = h_[index][1]
    while n > 0:
        if keep(c(n)):
            count += 1
        if inc_children:
            count += count_children(n, h_, contour)
        n = h_[n][1]
    return count


# Whether we care about this contour
def keep(contour):
    return keep_box(contour) and connected(contour)


# Whether we should keep the containing box of this
# contour based on it's shape
def keep_box(contour):
    xx, yy, w_, h_ = cv2.boundingRect(contour)

    # width and height need to be floats
    w_ *= 1.0
    h_ *= 1.0

    # Test it's shape - if it's too oblong or tall it's
    # probably not a real character
    if w_ / h_ < 0.1 or w_ / h_ > 10:
        if DEBUG:
            print("\t Rejected because of shape: (" + str(xx) + "," + str(yy) + "," + str(w_) + "," + str(h_) + ")" + \
                  str(w_ / h_))
        return False
    
    # check size of the box
    if ((w_ * h_) > ((img_x * img_y) / 5)) or ((w_ * h_) < 15):
        if DEBUG:
            print("\t Rejected because of size")
        return False

    return True


def include_box(index, h_, contour):
    if DEBUG:
        print(str(index) + ":")
        if is_child(index, h_):
            print("\tIs a child")
            print("\tparent " + str(get_parent(index, h_)) + " has " + str(
                count_children(get_parent(index, h_), h_, contour)) + " children")
            print("\thas " + str(count_children(index, h_, contour)) + " children")

    if is_child(index, h_) and count_children(get_parent(index, h_), h_, contour) <= 2:
        if DEBUG:
            print("\t skipping: is an interior to a letter")
        return False

    if count_children(index, h_, contour) > 2:
        if DEBUG:
            print("\t skipping, is a container of letters")
        return False

    if DEBUG:
        print("\t keeping")
    return True

# Load the image
orig_img = cv2.imread(input_file)

# Add a border to the image for processing sake
img = cv2.copyMakeBorder(orig_img, 50, 50, 50, 50, cv2.BORDER_CONSTANT)

# Calculate the width and height of the image
img_y = len(img)
img_x = len(img[0])

if DEBUG:
    print("Image is " + str(len(img)) + "x" + str(len(img[0])))

#Split out each channel
blue, green, red = cv2.split(img)

# Run canny edge detection on each channel
blue_edges = cv2.Canny(blue, 200, 250)
green_edges = cv2.Canny(green, 200, 250)
red_edges = cv2.Canny(red, 200, 250)

# Join edges back into image
edges = blue_edges | green_edges | red_edges

# Find the contours
img2, contours, hierarchy = cv2.findContours(edges.copy(), cv2.RETR_TREE, cv2.CHAIN_APPROX_NONE)

hierarchy = hierarchy[0]

if DEBUG:
    processed = edges.copy()
    rejected = edges.copy()

# These are the boxes that we are determining
keepers = []

# For each contour, find the bounding rectangle and decide
# if it's one we care about
for index_, contour_ in enumerate(contours):
    if DEBUG:
        print("Processing #%d" % index_)

    x, y, w, h = cv2.boundingRect(contour_)

    # Check the contour and it's bounding box
    if keep(contour_) and include_box(index_, hierarchy, contour_):
        # It's a winner!
        keepers.append([contour_, [x, y, w, h]])
        if DEBUG:
            cv2.rectangle(processed, (x, y), (x + w, y + h), (100, 100, 100), 1)
            cv2.putText(processed, str(index_), (x, y - 5), cv2.FONT_HERSHEY_PLAIN, 1, (255, 255, 255))
    else:
        if DEBUG:
            cv2.rectangle(rejected, (x, y), (x + w, y + h), (100, 100, 100), 1)
            cv2.putText(rejected, str(index_), (x, y - 5), cv2.FONT_HERSHEY_PLAIN, 1, (255, 255, 255))

# Make a white copy of our image
new_image = edges.copy()
new_image.fill(255)
boxes = []

# For each box, find the foreground and background intensities
for index_, (contour_, box) in enumerate(keepers):

    # Find the average intensity of the edge pixels to
    # determine the foreground intensity
    fg_int = 0.0
    for p in contour_:
        fg_int += ii(p[0][0], p[0][1])

    fg_int /= len(contour_)
    if DEBUG:
        print("FG Intensity for #%d = %d" % (index_, fg_int))

    # Find the intensity of three pixels going around the
    # outside of each corner of the bounding box to determine
    # the background intensity
    x_, y_, width, height = box
    bg_int = [# bottom left corner 3 pixels
                ii(x_ - 1, y_ - 1),
                ii(x_ - 1, y_),
                ii(x_, y_ - 1),

                # bottom right corner 3 pixels
                ii(x_ + width + 1, y_ - 1),
                ii(x_ + width, y_ - 1),
                ii(x_ + width + 1, y_),

                # top left corner 3 pixels
                ii(x_ - 1, y_ + height + 1),
                ii(x_ - 1, y_ + height),
                ii(x_, y_ + height + 1),

                # top right corner 3 pixels
                ii(x_ + width + 1, y_ + height + 1),
                ii(x_ + width, y_ + height + 1),
                ii(x_ + width + 1, y_ + height)
            ]

    # Find the median of the background
    # pixels determined above
    bg_int = np.median(bg_int)

    if DEBUG:
        print("BG Intensity for #%d = %s" % (index_, repr(bg_int)))

    # Determine if the box should be inverted
    if fg_int >= bg_int:
        fg = 255
        bg = 0
    else:
        fg = 0
        bg = 255

        # Loop through every pixel in the box and color the
        # pixel accordingly
    for x in range(x_, x_ + width):
        for y in range(y_, y_ + height):
            if y >= img_y or x >= img_x:
                if DEBUG:
                    print("pixel out of bounds (%d,%d)" % (y, x))
                continue
            if ii(x, y) > fg_int:
                new_image[y][x] = bg
            else:
                new_image[y][x] = fg

# blur a bit to improve ocr accuracy
new_image = cv2.blur(new_image, (2, 2))
cv2.imwrite(output_file, new_image)
cv2.imwrite('"C:/sources/Quantnet/data/edges.png', edges)
cv2.imwrite('"C:/sources/Quantnet/data/processed.png', processed)
cv2.imwrite('"C:/sources/Quantnet/data/rejected.png', rejected)