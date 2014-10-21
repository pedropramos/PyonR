#lang python
from "racket" import time

def mandelbrot(limit, c):
    z = 0+0j
    for i in range(limit+1):
        if abs(z) > 2:
            return i
        z = z*z + c
    return i+1

print time(mandelbrot(1000000, .2+.3j))