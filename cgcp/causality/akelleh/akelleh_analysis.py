import numpy as np
import pandas as pd
import matplotlib.pyplot as pp
from causality.analysis.dataframe import CausalDataFrame

N = 1000

z = np.random.normal(1., size=N)
x = np.random.binomial(1, p=1./(1. + np.exp(-z/.1)))
y = x + z + np.random.normal(size=N)

# It's easy to create a data frame
df = CausalDataFrame({'x': x, 'y': y, 'z': z})

df.zplot(x='x', y='y', z_types={'z': 'c'}, z=['z'], kind='bar', bootstrap_samples=500); pp.ylabel("$E[Y|do(X=x)]$"); pp.show()
