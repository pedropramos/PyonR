import numpy as np

def add_arrays(n):
    result = np.zeros((100,100))
    for i in range(n):
	    result += np.random.randint(0, 100000, (100,100))
    return result

from time import time
a = time()
print add_arrays(10000)
b = time()
print b-a, 'seconds'