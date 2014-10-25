#lang python
cpyimport numpy as np
from "racket" import time

def add_arrays(n):
    result = np.zeros((100,100))
    for i in range(n):
	    result += np.random.randint(0, 100000, (100,100))
    return result

print time(add_arrays(10000))
