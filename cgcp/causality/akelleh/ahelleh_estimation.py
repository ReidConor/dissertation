import pandas as pd
import numpy as np
from causality.estimation.parametric import PropensityScoreMatching

N = 10000
z1 = np.random.normal(size=N)
z2 = np.random.normal(size=N)
z3 = np.random.normal(size=N)

p_d = 1. / (1. + np.exp(-(z1 + z2 + z3)/4.))
d = np.random.binomial(1, p=p_d)

y0 = np.random.normal()
y1 = y0 + z1 + z2 + z3

y = (d==1)*y1 + (d==0)*y0
X = pd.DataFrame({'d': d, 'z1': z1, 'z2': z2, 'z3': z3, 'y': y, 'y0': y0, 'y1': y1, 'p': p_d})

print(X.head(10))

matcher = PropensityScoreMatching()
print(matcher.estimate_ATE(X, 'd', 'y', {'z1': 'c', 'z2': 'c', 'z3': 'c'}))

#matcher.check_support(X, 'd', {'z1': 'c', 'z2': 'c', 'z3': 'c'})

print(matcher.assess_balance(X, 'd', {'z1': 'c', 'z2': 'c', 'z3': 'c'}))


