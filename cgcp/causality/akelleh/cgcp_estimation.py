import pandas as pd
import numpy as np
import os
import MySQLdb
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


if __name__ == "__main__":
    os.system('clear')

    data, types = getData("spx")

    matcher = PropensityScoreMatching()
    print(matcher.estimate_ATE(data, 'Feml.CEO.or.Equiv', 'Tobins.Q', {'P.EBITDA': 'c', 'P.B': 'c', 'Asset':'c'}, bootstrap=True))
    #of the form matcher.estimate_ATE (data.frame, treatment, target, dict{vars to control for})

    matcher.check_support(data, 'Feml.CEO.or.Equiv', {'P.EBITDA': 'c', 'P.B': 'c','Asset':'c'})

    print(matcher.assess_balance(data, 'Feml.CEO.or.Equiv', {'P.EBITDA': 'c', 'P.B': 'c','Asset':'c'}))
