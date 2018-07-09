#!/usr/bin/env python3
import numpy
import pandas as pd
import MySQLdb
import sys
from builtins import any as b_any
from pprint import pprint
import os

from causality.inference.search import IC
from causality.inference.independence_tests import RobustRegressionTest

#read in data from mysql
def getData(table):
    conn = MySQLdb.connect(host="localhost",
                         user="root",
                         passwd="",
                         db="corp_gov_causal")

    data = pd.read_sql("SELECT * FROM " + table, conn)
    data_types = pd.read_sql("describe " + table, conn)
    conn.close()
    #spx.dropna(inplace=True)
    #print(spx.head(30))
    return (data, data_types)


def stageAlgo(types,target):
    desiredDataTypes = ['double']
    targetVars = ['AZS','Tobins.Q','Tobins.Q.class','AZS.class']

    variable_types = {}
    for index, row in types.iterrows():
        if b_any(row["Type"] in x for x in desiredDataTypes):
            if row["Field"] not in targetVars:
                variable_types[row["Field"]]="c"
    #add in the target
    variable_types[target]="c"
    return (variable_types)


def runSearch(data,algo_var_types):
    # run the search
    ic_algorithm = IC(RobustRegressionTest)
    graph = ic_algorithm.search(data, algo_var_types)

    results = graph.edges(data=True)
    #pprint(results)
    return (results)

if __name__ == "__main__":
    os.system('clear')

    data, types = getData("spx_fceo")

    algo_var_types = stageAlgo(types,"Tobins.Q")
    print(len(algo_var_types))
    print(algo_var_types)

    try:
        algo_var_types = {'Tobins.Q':'c', 'P.EBITDA': 'c', 'P.B': 'c', 'Asset':'c', 'Tax':'c', 'P.E':'c'}
        algo_results = runSearch(data,algo_var_types)
        pprint(algo_results)
        #os.system('say "akelleh : success "')
    except:
        print("failed")
        #os.system('say "akelleh : failure"')
