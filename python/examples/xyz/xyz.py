#lang python
from '(planet aml/rosetta)' import *
import predicates

class XYZ(object):

    x = property(cx)
    y = property(cy)
    z = property(cz)

    rho = property(cyl_rho)
    phi = property(cyl_phi)
    
    def __add__(self, other):
        return PLUS_xyz(self, cx(other), cy(other), cz(other))

    def __repr__(self):
        return "<" + str(cx(self)) + ", " \
                   + str(cy(self)) + ", " \
                   + str(cz(self)) + ">"


predicates.set_predicate(position_QUERY, XYZ)