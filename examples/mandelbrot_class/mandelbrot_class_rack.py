#lang python
from "racket" import time

class MandelbrotComplex:
    def __init__(self):
        self.z = 0+0j

def mandelbrot(limit, c):
    mc = MandelbrotComplex()
    for i in range(limit+1):
        if abs(mc.z) > 2:
            return i
        mc.z = mc.z * mc.z + c
    return i+1

print time(mandelbrot(1000000, .2+.3j))