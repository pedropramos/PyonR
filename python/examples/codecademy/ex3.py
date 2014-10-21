#lang python

# / and * are evaluated before + and -
bool_one = 17 < 118 % 100
bool_two = 100 == (33 * 3) + 1
bool_three = 19 <= 2**4
bool_four = -22 >= -18
bool_five = 99 != 98 + 1

# not is evaluated first, and is evaluated next, or is evaluated last
"""
True and True is True
False or False is False
Not True is False
Not False is True
"""

answer=7
if 5 <= answer:
    print 1
elif answer < 5:
    print -1
else:
    print 0