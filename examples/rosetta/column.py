#lang python

from '(planet aml/rosetta)' import *
from xyz import XYZ

backend(rhino5)

def shaft(p, shaft_h, base_r, top_r):
  cone_frustum(p, base_r, shaft_h, top_r)

def echinus(p, echinus_h, base_r, top_r):
  cone_frustum(p, base_r, echinus_h, top_r)

def abacus(p, abacus_h, abacus_l):
  box(p + xyz(-abacus_l / 2, -abacus_l / 2, 0), \
      abacus_l, \
      abacus_l, \
      abacus_h)

def column(p, shaft_h, shaft_base_r, \
              echinus_h, echinus_base_r, \
              abacus_h, abacus_l):
  shaft(p, shaft_h, shaft_base_r, echinus_base_r)
  echinus(p + z(shaft_h), echinus_h, echinus_base_r, abacus_l/2)
  abacus(p + z(shaft_h + echinus_h), abacus_h, abacus_l)