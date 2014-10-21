import sys
sys.setrecursionlimit(5000)

def ackermann(m,n):
    if m == 0: return n+1
    elif m > 0 and n == 0: return ackermann(m-1,1)
    else: return ackermann(m-1, ackermann(m,n-1))

import time
a = time.time()
print ackermann(3,9)
b=time.time()
print b-a, 'seconds'
