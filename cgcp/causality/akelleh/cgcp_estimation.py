import pandas as pd
import numpy as np
import os
import MySQLdb
from builtins import any as b_any
from causality.estimation.parametric import PropensityScoreMatching

#read in data from mysql
def getData(table):
    conn = MySQLdb.connect(host="localhost",
                         user="root",
                         passwd="",
                         db="corp_gov_imputed_scaled")
                         #db="corp_gov_imputed")

    data = pd.read_sql("SELECT * FROM " + table, conn)
    data_types = pd.read_sql("describe " + table, conn)
    conn.close()
    return (data, data_types)


def stageAlgo(types):
    desiredDataTypes = ['double']
    exclude = ['AZS','Tobins.Q','Tobins.Q.class','AZS.class','Feml.CEO.or.Equiv']

    variable_types = {}
    for index, row in types.iterrows():
        if b_any(row["Type"] in x for x in desiredDataTypes):
            if row["Field"] not in exclude:
                variable_types[row["Field"]]="c"

    return (variable_types)


if __name__ == "__main__":
    os.system('clear')

    data, types = getData("spx_esg_disc")
    controlFor = stageAlgo(types)

    matcher = PropensityScoreMatching()
    print(matcher.estimate_ATE(data, 'esg_disc_score_bin', 'AZS', {'P.EBITDA': 'c', 'P.B': 'c', 'Asset':'c', 'Tax':'c', 'P.E':'c'}, bootstrap=True))
    #of the form matcher.estimate_ATE (data.frame, treatment, target, dict{vars to control for})

    matcher.check_support(data, 'esg_disc_score_bin', {'P.EBITDA': 'c', 'P.B': 'c','Asset':'c', 'Tax':'c', 'P.E':'c'})

    print("")
    print("Balance before matching")
    print(matcher.assess_balance(data, 'esg_disc_score_bin', {'P.EBITDA': 'c', 'P.B': 'c','Asset':'c', 'Tax':'c', 'P.E':'c', 'propensity score': 'c'}))
    print ("")

    data = matcher.score(data, assignment='esg_disc_score_bin', confounder_types={'P.EBITDA': 'c', 'P.B': 'c','Asset':'c', 'Tax':'c', 'P.E':'c'})
    treated, control = matcher.match(data, assignment='esg_disc_score_bin')
    print("")
    print("Balance after matching")
    print(matcher.assess_balance(treated.append(control), 'esg_disc_score_bin', {'P.EBITDA': 'c', 'P.B': 'c','Asset':'c', 'Tax':'c', 'P.E':'c', 'propensity score': 'c'}))
    print ("")
