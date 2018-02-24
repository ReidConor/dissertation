#!/usr/bin/env python

import numpy
import pandas as pd

from causality.inference.search import IC
from causality.inference.independence_tests import RobustRegressionTest


#gen some toy data
SIZE=2000
x1 = numpy.random.normal(size=SIZE)
x2 = x1 + numpy.random.normal(size=SIZE)
x3 = x1 + numpy.random.normal(size=SIZE)
x4 = x2 + x3 + numpy.random.normal(size=SIZE)
x5 = x4 + numpy.random.normal(size=SIZE)
# see below for example of the above
#numpy.random.normal(size=4)
#>>> array([ 0.23287152,  0.50037316, -0.68046787, -0.53118273])

X = pd.DataFrame({'x1' : x1, 'x2' : x2, 'x3' : x3, 'x4' : x4, 'x5' : x5})
variable_types = {'x1' : 'c', 'x2' : 'c', 'x3' : 'c', 'x4' : 'c', 'x5' : 'c'}
#c for continuous. these are the variables that are searched over, not every ssconvert

ic_algorithm = IC(RobustRegressionTest)
graph = ic_algorithm.search(X, variable_types)

print(graph.edges(data=True))
