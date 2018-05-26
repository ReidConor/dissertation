import numpy
import pandas as pd
import MySQLdb
import sys
from builtins import any as b_any
from pprint import pprint

from causality.inference.search import IC
from causality.inference.independence_tests import RobustRegressionTest

#read in data from mysql
conn = MySQLdb.connect(host="localhost",
                     user="root",
                     passwd="",
                     db="corp_gov_complete_cases")

spx = pd.read_sql("SELECT * FROM spx", conn)
spx_types = pd.read_sql("describe spx", conn)

desiredDataTypes = ['double']
variable_types = {}
for index, row in spx_types.iterrows():
    if b_any(row["Type"] in x for x in desiredDataTypes):
         variable_types[row["Field"]]="c"

conn.close()

# run the search
ic_algorithm = IC(RobustRegressionTest)
graph = ic_algorithm.search(spx, variable_types)

results = graph.edges(data=True)
pprint(results)
