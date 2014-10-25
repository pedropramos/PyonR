#lang python
from datetime cpyimport date

ilc = date.today()
print ilc
print ilc.year
print ilc.isoweekday()

xmas = date(2014,12,25)
interval = xmas - ilc
print interval
print interval.days

print ilc.isocalendar()