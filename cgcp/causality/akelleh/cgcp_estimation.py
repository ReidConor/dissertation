#!/usr/bin/env python3

import pandas as pd
import numpy as np
import os
import MySQLdb
from builtins import any as b_any
from causality.estimation.parametric import PropensityScoreMatching
import datetime

now = str(datetime.datetime.now())

def stageDbLayer():
    conn = MySQLdb.connect(host="localhost",
                         user="root",
                         passwd="",
                         db="causal_results")

    cur = conn.cursor()
    drop_table = "drop table if exists `akelleh_results`;"
    create_table = "create table akelleh_results (datestamp timestamp, dataset varchar(10), treatment varchar(100), target varchar(100), results varchar(100), mm varchar(1000))"
    cur.execute(drop_table)
    cur.execute(create_table)
    conn.close()

#read in data from mysql
def getData(table, database):
    conn = MySQLdb.connect(host="localhost",
                         user="root",
                         passwd="",
                         db=database)
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

#
#   S&P - America
#
def spx_wmOnBoard_tobin():
    '''
    Test the effect of having women on the board of directors on the tobins Q score
    M&M:
        For the American companies inside the S&P 500 index,
        we found a positive correlation between the percentage higher
        than 20 % of women in the board and the Tobin’s Q ratio
    Latest Results:
        (-0.094388682158789469, -0.04962212239013655, -0.0068850052276448973)
    Comments:
        So no real causal influence here, wrong sign
    '''
    data, types = getData("spx_fboard","corp_gov_causal")
    controlFor = stageAlgo(types)
    treatment = 'X..Women.on.Bd'
    target = 'Tobins.Q'

    matcher = PropensityScoreMatching()
    ATE_results = matcher.estimate_ATE(data, treatment, target, {'P.EBITDA': 'c', 'P.B': 'c', 'Asset':'c', 'Tax':'c', 'P.E':'c'}, bootstrap=True)

    #matcher.check_support(data, 'X..Women.on.Bd', {'P.EBITDA': 'c', 'P.B': 'c','Asset':'c', 'Tax':'c', 'P.E':'c'})

    #print("")
    #print("Balance before matching")
    #print(matcher.assess_balance(data, 'X..Women.on.Bd', {'P.EBITDA': 'c', 'P.B': 'c','Asset':'c', 'Tax':'c', 'P.E':'c', 'propensity score': 'c'}))
    #print ("")

    #data = matcher.score(data, assignment='X..Women.on.Bd', confounder_types={'P.EBITDA': 'c', 'P.B': 'c','Asset':'c', 'Tax':'c', 'P.E':'c'})
    #treated, control = matcher.match(data, assignment='X..Women.on.Bd')
    #print("")
    #print("Balance after matching")
    #print(matcher.assess_balance(treated.append(control), 'X..Women.on.Bd', {'P.EBITDA': 'c', 'P.B': 'c','Asset':'c', 'Tax':'c', 'P.E':'c', 'propensity score': 'c'}))
    #print ("")

    #now write results to mysql
    conn = MySQLdb.connect(host="localhost",
                         user="root",
                         passwd="",
                         db="causal_results")
    cur = conn.cursor()
    query = """ insert into akelleh_results values ('%s','%s','%s','%s','%s','%s');  """ % (now,"spx",treatment,target,str(ATE_results),"For the American companies inside the S and P 500 index, we found a positive correlation between the percentage higher than 20pct of women in the board and the Tobins Q ratio")
    cur.execute(query)
    conn.commit()
    conn.close()
    print("Done")

def spx_indepDirFinlL_azs():
    '''
    Test the effect of having a lead indep director and fincl leverage > 2.5
    M&M:
        ...but also the presence of an independent lead director in the company
        along with a financial leverage higher than 2.5 incur a higher risk of bankruptcy.
    Latest Results:
        (-0.47855609106276292, -0.4343301267327499, -0.3864914259963988)
    Comments:
        So this 'treatment' causes quite a dip in AZS, which is what MM are saying
    '''
    data, types = getData("spx_indepdirfincl","corp_gov_causal")
    controlFor = stageAlgo(types)
    treatment = 'Indep.Lead.Dir.Fincl..l'
    target = 'AZS'

    matcher = PropensityScoreMatching()
    ATE_results = matcher.estimate_ATE(data, treatment, target, {'P.EBITDA': 'c', 'P.B': 'c', 'Asset':'c', 'Tax':'c', 'P.E':'c'}, bootstrap=True)

    #now write results to mysql
    conn = MySQLdb.connect(host="localhost",
                         user="root",
                         passwd="",
                         db="causal_results")
    cur = conn.cursor()
    query = """ insert into akelleh_results values ('%s','%s','%s','%s','%s','%s');  """ % (now,"spx",treatment,target,str(ATE_results),"...but also the presence of an independent lead director in the company along with a financial leverage higher than 2.5 incur a higher risk of bankruptcy.")
    cur.execute(query)
    conn.commit()
    conn.close()
    print("Done")

def spx_fceo_tobin():
    '''
    Test the effect of having a female ceo on tobins
    M&M:
        This is my own
    Latest Results:
        (-0.48556936715099608, -0.3878325746547393, -0.29127847792553346)
    Comments:
        Non-zero influence?
    '''
    data, types = getData("spx_fceo","corp_gov_causal")
    controlFor = stageAlgo(types)
    treatment = 'Feml.CEO.or.Equiv'
    target = 'Tobins.Q'

    matcher = PropensityScoreMatching()
    ATE_results = matcher.estimate_ATE(data, treatment, target, {'P.EBITDA': 'c', 'P.B': 'c', 'Asset':'c', 'Tax':'c', 'P.E':'c'}, bootstrap=True)

    #now write results to mysql
    conn = MySQLdb.connect(host="localhost",
                         user="root",
                         passwd="",
                         db="causal_results")
    cur = conn.cursor()
    query = """ insert into akelleh_results values ('%s','%s','%s','%s','%s','%s');  """ % (now,"spx",treatment,target,str(ATE_results),"This is my own")
    cur.execute(query)
    conn.commit()
    conn.close()
    print("Done")


#
#   SXXP - Western Europe
#
def sxxp_indepDirFormerCEOBoard_tobin():
    '''
    Test the effect of having a lead indep director or former ceo on board on tobins Q
    M&M:
        the presence of an independent lead director or a former CEO in the board could be a sign of weaker performances, being negatively correlated with Tobin’s Q
    Latest Results:
        (0.012699075182737657, 0.05961530907139483, 0.099317124249211436)
    Comments:
        Nothing much, but is positive contrary to what MM say

    '''
    data, types = getData("sxxp_indepdirfceo","corp_gov_causal")
    controlFor = stageAlgo(types)
    treatment = 'Indep.Lead.Dir.Feml.CEO.or.Equiv'
    target = 'Tobins.Q'

    matcher = PropensityScoreMatching()
    ATE_results = matcher.estimate_ATE(data, treatment, target, {'P.B': 'c', 'Asset':'c', 'Tax':'c', 'P.E':'c'}, bootstrap=True)

    #now write results to mysql
    conn = MySQLdb.connect(host="localhost",
                         user="root",
                         passwd="",
                         db="causal_results")
    cur = conn.cursor()
    query = """ insert into akelleh_results values ('%s','%s','%s','%s','%s','%s');  """ % (now,"sxxp",treatment,target,str(ATE_results),"the presence of an independent lead director or a former CEO in the board could be a sign of weaker performances, being negatively correlated with Tobins Q")
    cur.execute(query)
    conn.commit()
    conn.close()
    print("Done")

def sxxp_womenBoard_tobin():
    '''
    Test the effect of having a large % of women on the board on tobins
    M&M:
        A large percentage of women in the board could also affect negatively the performance.
    '''
    data, types = getData("sxxp_fboard","corp_gov_causal")
    controlFor = stageAlgo(types)
    treatment = 'X..Women.on.Bd'
    target = 'Tobins.Q'

    matcher = PropensityScoreMatching()
    ATE_results = matcher.estimate_ATE(data, treatment, target, {'P.B': 'c', 'Asset':'c', 'Tax':'c', 'P.E':'c'}, bootstrap=True)

    #now write results to mysql
    conn = MySQLdb.connect(host="localhost",
                         user="root",
                         passwd="",
                         db="causal_results")
    cur = conn.cursor()
    query = """ insert into akelleh_results values ('%s','%s','%s','%s','%s','%s');  """ % (now,"sxxp",treatment,target,str(ATE_results),"A large percentage of women in the board could also affect negatively the performance.")
    cur.execute(query)
    conn.commit()
    conn.close()
    print("Done")


#
#   EEBP - Eastern Europe
#
def eebp_ageRange_tobins():
    '''
    Test the effect of age range in board on tobins
    M&M:
        we found that a smaller age range for the board members is positively related with the companies’ performance
    '''
    data, types = getData("eebp_agerange","corp_gov_causal")
    controlFor = stageAlgo(types)
    treatment = 'BOD.Age.Rng'
    target = 'Tobins.Q'

    matcher = PropensityScoreMatching()
    ATE_results = matcher.estimate_ATE(data, treatment, target, {'P.B': 'c', 'Asset':'c', 'Tax':'c', 'P.E':'c'}, bootstrap=True)

    #now write results to mysql
    conn = MySQLdb.connect(host="localhost",
                         user="root",
                         passwd="",
                         db="causal_results")
    cur = conn.cursor()
    query = """ insert into akelleh_results values ('%s','%s','%s','%s','%s','%s');  """ % (now,"eebp",treatment,target,str(ATE_results),"we found that a smaller age range for the board members is positively related with the companies performance")
    cur.execute(query)
    conn.commit()
    conn.close()
    print("Done")

def eebp_finlL_tobins():
        '''
        Test the effect of financial leverage on tobins
        M&M:
            ...and that a financial leverage less than 4 is needed in order to be on the upper side of the Tobin’s Q ratio

        '''
        data, types = getData("eebp_fl","corp_gov_causal")
        controlFor = stageAlgo(types)
        treatment = 'Fincl.l.treatment'
        target = 'Tobins.Q'

        matcher = PropensityScoreMatching()
        ATE_results = matcher.estimate_ATE(data, treatment, target, {'P.B': 'c', 'Asset':'c', 'Tax':'c', 'P.E':'c'}, bootstrap=True)

        #now write results to mysql
        conn = MySQLdb.connect(host="localhost",
                             user="root",
                             passwd="",
                             db="causal_results")
        cur = conn.cursor()
        query = """ insert into akelleh_results values ('%s','%s','%s','%s','%s','%s');  """ % (now,"eebp",treatment,target,str(ATE_results),"...and that a financial leverage less than 4 is needed in order to be on the upper side of the Tobins Q ratio")
        cur.execute(query)
        conn.commit()
        conn.close()
        print("Done")

def eebp_indepChaFCEO_azs():
        '''
        Test the effect of financial leverage on tobins
        M&M:
            ...to be on the “safe” zone of the Altman Z-score it is important to have an independent chairperson or even a woman as CEO.
        '''
        data, types = getData("eebp_indepChFmlCEO","corp_gov_causal")
        #controlFor = stageAlgo(types)
        controlFor = {
            'P.B': 'c',
            'Fincl..l':'c',
            'Asset':'c',
            'Tax':'c',
            'P.E':'c',
            'OPM.T12M':'c',
            'P.EBITDA':'c',
            'EV.EBITDA.T12M':'c',
            'ROC':'c',
            'ROE':'c',
            'BOD.Age.Rng':'c',
            'Norm.NI.to.NI.for.Cmn..':'c',
            'Cash.Gen.Cash.Reqd':'c',
            'Bd.Avg.Age':'c',
            'X5Yr.Avg.Adj.ROE':'c',
            #'Dvd.Yld':'c',
            'EBITDA.Sh':'c',
            'Net.Debt.to.EBITDA':'c'
        }
        treatment = 'Indep.Chrprsn.Feml.CEO.or.Equiv'
        target = 'AZS.class.Binary'

        matcher = PropensityScoreMatching()
        ATE_results = matcher.estimate_ATE(data, treatment, target, controlFor, bootstrap=True)

        #now write results to mysql
        conn = MySQLdb.connect(host="localhost",
                             user="root",
                             passwd="",
                             db="causal_results")
        cur = conn.cursor()
        query = """ insert into akelleh_results values ('%s','%s','%s','%s','%s','%s');  """ % (now,"eebp",treatment,target,str(ATE_results),"...to be on the safe zone of the Altman Z-score it is important to have an independent chairperson or even a woman as CEO.")
        cur.execute(query)
        conn.commit()
        conn.close()
        print("Done")



if __name__ == "__main__":
    os.system('clear')

    #stageDbLayer()
    #spx_wmOnBoard_tobin()
    #spx_indepDirFinlL_azs()
    #spx_fceo_tobin()
    #sxxp_indepDirFormerCEOBoard_tobin()
    #sxxp_womenBoard_tobin()
    #eebp_ageRange_tobins()
    #eebp_finlL_tobins()
    eebp_indepChaFCEO_azs()
