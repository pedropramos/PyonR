#lang python

def square(n):
    """Returns the square of a number."""
    squared = n**2
    print "%d squared is %d." % (n, squared)
    return squared

def favorite_actors(*args):
    """Prints out your favorite actorS (plural!)"""
    print "Your favorite actors are:" , args
favorite_actors("Michael Palin", "John Cleese", "Graham Chapman")

def cube(number):
    return number**3
def by_three(number):
    if number%3 == 0:
        return cube(number)
    else:
        return False
by_three(9)

cpyimport math
print math.sqrt(25)
from math cpyimport sqrt
print sqrt(25)
#from math cpyimport *
print sqrt(25)

print max(-10, -5, 5, 10)
print min(-10, -5, 5, 10)
print abs(-10)

print type(42) # => integer
print type(4.2) # => float
print type('spam') # => unicode
print type({'Name':'John Cleese'}) # => dict
print type((1,2)) # => tuple