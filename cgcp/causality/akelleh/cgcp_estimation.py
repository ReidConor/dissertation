#!/usr/bin/env python3

import pandas as pd
import numpy as np
import os
import MySQLdb
from builtins import any as b_any
from causality.estimation.parametric import PropensityScoreMatching
import datetime

now = str(datetime.datetime.now())

def createLatestResultsTable():
    conn = MySQLdb.connect(host="localhost",
                         user="root",
                         passwd="",
                         db="causal_results")

    cur = conn.cursor()
    drop_table = "drop table if exists `akelleh_results_latest`;"
    create_table = "create table akelleh_results_latest as \
    select \
      a.dataset, \
      a.treatment, \
      a.target, \
      a.results, \
      a.mm \
    from akelleh_results a \
    inner join ( \
      select \
        dataset, \
        treatment, \
        target, \
        max(datestamp) as max_datestamp \
      from akelleh_results \
      group by \
        dataset, \
        treatment, \
        target \
    ) b \
      on a.dataset = b.dataset \
      and a.treatment = b.treatment \
      and a.target = b.target \
      and a.datestamp = b.max_datestamp;"
    cur.execute(drop_table)
    cur.execute(create_table)
    conn.close()

def getData(data_table, imp_vars_table):
    conn = MySQLdb.connect(host="localhost",
                         user="root",
                         passwd="",
                         db="corp_gov_causal")

    data = pd.read_sql("SELECT * FROM " + data_table, conn)
    data_types = pd.read_sql("describe " + data_table, conn)
    conn.close()

    conn = MySQLdb.connect(host="localhost",
                     user="root",
                     passwd="",
                     db="mm_results")
    imp_vars = pd.read_sql("SELECT Var, Imp FROM " + imp_vars_table, conn)
    conn.close()

    return (data, data_types, imp_vars)

def stageVarTypes(imp_vars_table, imp_limit):
    variable_types = {}
    imp_vars_table = imp_vars_table.sort_values(by='Imp', ascending=False)
    for index, row in imp_vars_table.iterrows():
        variable_types[row["Var"]]="c"
        if index == (imp_limit-1):
            break

    return (variable_types)


#####
def ageRange(dataset, target, data_table, imp_vars_table, imp_limit=10):
    '''
    Test the effect of age range in board on tobins
    M&M:
        we found that a smaller age range for the board members is positively related with the companies’ performance
    '''
    data, types, imp_vars = getData(data_table,imp_vars_table)
    controlFor = stageVarTypes(imp_vars, imp_limit)
    treatment = 'BOD.Age.Rng'
    #target = target

    matcher = PropensityScoreMatching()

    ATE_results = matcher.estimate_ATE(data, treatment, target, controlFor, bootstrap=True)
    print(ATE_results)

    matcher.check_support(data, treatment, controlFor)

    controlForPS = controlFor.copy()
    controlForPS['propensity score'] = 'c'

    print(controlFor)
    print("")
    print("Balance before matching")
    print(matcher.assess_balance(data, treatment, controlForPS))
    print ("")

    data = matcher.score(data, assignment=treatment, confounder_types=controlFor)
    treated, control = matcher.match(data, assignment=treatment)
    print("")
    print("Balance after matching")
    print(matcher.assess_balance(treated.append(control), treatment, controlForPS))
    print ("")

    #now write results to mysql
    conn = MySQLdb.connect(host="localhost",
                         user="root",
                         passwd="",
                         db="causal_results")
    cur = conn.cursor()
    query = """ insert into akelleh_results values ('%s','%s','%s','%s','%s','%s');  """ % (now,dataset,treatment,target,str(ATE_results),"we found that a smaller age range for the board members is positively related with the companies performance")
    cur.execute(query)
    conn.commit()
    conn.close()
    print("Done")

def indepChaFCEO(dataset,target, data_table, imp_vars_table, imp_limit=10):
        '''
        Test the effect indep chair or female ceo
        M&M:
            ...to be on the “safe” zone of the Altman Z-score it is important to have an independent chairperson or even a woman as CEO.
        '''
        data, types, imp_vars = getData(data_table,imp_vars_table)
        controlFor = stageVarTypes(imp_vars, imp_limit)
        treatment = 'Indep.Chrprsn.Feml.CEO.or.Equiv'
        #target = 'AZS.class.Binary'

        matcher = PropensityScoreMatching()
        ATE_results = matcher.estimate_ATE(data, treatment, target, controlFor, bootstrap=True)

        matcher.check_support(data, treatment, controlFor)

        controlForPS = controlFor.copy()
        controlForPS['propensity score'] = 'c'

        print(controlFor)
        print("")
        print("Balance before matching")
        print(matcher.assess_balance(data, treatment, controlForPS))
        print ("")

        data = matcher.score(data, assignment=treatment, confounder_types=controlFor)
        treated, control = matcher.match(data, assignment=treatment)
        print("")
        print("Balance after matching")
        print(matcher.assess_balance(treated.append(control), treatment, controlForPS))
        print ("")


        #now write results to mysql
        conn = MySQLdb.connect(host="localhost",
                             user="root",
                             passwd="",
                             db="causal_results")
        cur = conn.cursor()
        query = """ insert into akelleh_results values ('%s','%s','%s','%s','%s','%s');  """ % (now,dataset,treatment,target,str(ATE_results),"...to be on the safe zone of the Altman Z-score it is important to have an independent chairperson or even a woman as CEO.")
        cur.execute(query)
        conn.commit()
        conn.close()
        print("Done")

def wmOnBoard(dataset, target, data_table, imp_vars_table, imp_limit=10):
    '''
    Test the effect of having women on the board of directors on the tobins Q score
    M&M:
        For the American companies inside the S&P 500 index,
        we found a positive correlation between the percentage higher
        than 20 % of women in the board and the Tobin’s Q ratio
    '''
    data, types, imp_vars = getData(data_table,imp_vars_table)
    controlFor = stageVarTypes(imp_vars, imp_limit)
    treatment = 'X..Women.on.Bd'
    #target = 'Tobins.Q.class'

    matcher = PropensityScoreMatching()
    ATE_results = matcher.estimate_ATE(data, treatment, target, controlFor, bootstrap=True)
    print(ATE_results)
    matcher.check_support(data, treatment, controlFor)

    controlForPS = controlFor.copy()
    controlForPS['propensity score'] = 'c'

    print(controlFor)
    print("")
    print("Balance before matching")
    print(matcher.assess_balance(data, treatment, controlForPS))
    print ("")

    data = matcher.score(data, assignment=treatment, confounder_types=controlFor)
    treated, control = matcher.match(data, assignment=treatment)
    print("")
    print("Balance after matching")
    print(matcher.assess_balance(treated.append(control), treatment, controlForPS))
    print ("")

    #now write results to mysql
    conn = MySQLdb.connect(host="localhost",
                         user="root",
                         passwd="",
                         db="causal_results")
    cur = conn.cursor()
    query = """ insert into akelleh_results values ('%s','%s','%s','%s','%s','%s');  """ % (now,dataset,treatment,target,str(ATE_results),"For the American companies inside the S and P 500 index, we found a positive correlation between the percentage higher than 20pct of women in the board and the Tobins Q ratio")
    cur.execute(query)
    conn.commit()
    conn.close()
    print("Done")

def fceo(dataset, target, data_table, imp_vars_table, imp_limit=10):
    '''
    Test the effect of having a female ceo on tobins
    M&M:
        This is my own
    '''
    data, types, imp_vars = getData(data_table, imp_vars_table)
    controlFor = stageVarTypes(imp_vars, imp_limit)
    treatment = 'Feml.CEO.or.Equiv'
    #target = 'Tobins.Q.class'

    matcher = PropensityScoreMatching()
    ATE_results = matcher.estimate_ATE(data, treatment, target, controlFor, bootstrap=True)

    #now write results to mysql
    #conn = MySQLdb.connect(host="localhost",
    #                     user="root",
    #                     passwd="",
    #                     db="causal_results")
    #cur = conn.cursor()
    #query = """ insert into akelleh_results values ('%s','%s','%s','%s','%s','%s');  """ % (now,"spx",treatment,target,str(ATE_results),"This is my own")
    #cur.execute(query)
    #conn.commit()
    #conn.close()
    print("Done")

def indepDirFinlL(dataset, target, data_table, imp_vars_table, imp_limit=10):
    '''
    Test the effect of having a lead indep director and fincl leverage > 2.5
    M&M:
        ...but also the presence of an independent lead director in the company
        along with a financial leverage higher than 2.5 incur a higher risk of bankruptcy.
    '''
    data, types, imp_vars = getData(data_table, imp_vars_table)
    controlFor = stageVarTypes(imp_vars, imp_limit)
    treatment = 'Indep.Lead.Dir.Fincl..l'
    #target = 'AZS.class.Binary'
    #target = 'AZS'

    matcher = PropensityScoreMatching()
    ATE_results = matcher.estimate_ATE(data, treatment, target, controlFor, bootstrap=True)
    print(ATE_results)

    matcher.check_support(data, treatment, controlFor)

    controlForPS = controlFor.copy()
    controlForPS['propensity score'] = 'c'

    print(controlFor)
    print("")
    print("Balance before matching")
    print(matcher.assess_balance(data, treatment, controlForPS))
    print ("")

    data = matcher.score(data, assignment=treatment, confounder_types=controlFor)
    treated, control = matcher.match(data, assignment=treatment)
    print("")
    print("Balance after matching")
    print(matcher.assess_balance(treated.append(control), treatment, controlForPS))
    print ("")




    #now write results to mysql
    conn = MySQLdb.connect(host="localhost",
                         user="root",
                         passwd="",
                         db="causal_results")
    cur = conn.cursor()
    query = """ insert into akelleh_results values ('%s','%s','%s','%s','%s','%s');  """ % (now,dataset,treatment,target,str(ATE_results),"...but also the presence of an independent lead director in the company along with a financial leverage higher than 2.5 incur a higher risk of bankruptcy.")
    cur.execute(query)
    conn.commit()
    conn.close()
    print("Done")

def finlL(dataset, target, data_table, imp_vars_table, imp_limit=10):
        '''
        Test the effect of financial leverage on tobins
        M&M:
            ...and that a financial leverage less than 4 is needed in order to be on the upper side of the Tobin’s Q ratio
        '''
        #data, types = getData("eebp_fl","corp_gov_causal")
        #controlFor = stageAlgo(types)
        data, types, imp_vars = getData(data_table, imp_vars_table)
        controlFor = stageVarTypes(imp_vars, imp_limit)
        treatment = 'Fincl.l.treatment'
        #target = 'Tobins.Q.class'

        matcher = PropensityScoreMatching()
        ATE_results = matcher.estimate_ATE(data, treatment, target, controlFor, bootstrap=True)
        print(ATE_results)

        matcher.check_support(data, treatment, controlFor)

        controlForPS = controlFor.copy()
        controlForPS['propensity score'] = 'c'

        print(controlFor)
        print("")
        print("Balance before matching")
        print(matcher.assess_balance(data, treatment, controlForPS))
        print ("")

        data = matcher.score(data, assignment=treatment, confounder_types=controlFor)
        treated, control = matcher.match(data, assignment=treatment)
        print("")
        print("Balance after matching")
        print(matcher.assess_balance(treated.append(control), treatment, controlForPS))
        print ("")

        #now write results to mysql
        conn = MySQLdb.connect(host="localhost",
                             user="root",
                             passwd="",
                             db="causal_results")
        cur = conn.cursor()
        query = """ insert into akelleh_results values ('%s','%s','%s','%s','%s','%s');  """ % (now,dataset,treatment,target,str(ATE_results),"...and that a financial leverage less than 4 is needed in order to be on the upper side of the Tobins Q ratio")
        cur.execute(query)
        conn.commit()
        conn.close()
        print("Done")

def indepDirFormerCEOBoard(dataset, target, data_table, imp_vars_table, imp_limit=10):
    '''
    Test the effect of having a lead indep director or former ceo on board on tobins Q
    M&M:
        the presence of an independent lead director or a former CEO in the board could be a sign of weaker performances, being negatively correlated with Tobin’s Q
    '''
    data, types, imp_vars = getData(data_table, imp_vars_table)
    controlFor = stageVarTypes(imp_vars, imp_limit)
    treatment = 'Indep.Lead.Dir.Former.CEO.on.Board'

    matcher = PropensityScoreMatching()
    ATE_results = matcher.estimate_ATE(data, treatment, target, controlFor, bootstrap=True)
    print(ATE_results)
    matcher.check_support(data, treatment, controlFor)

    controlForPS = controlFor.copy()
    controlForPS['propensity score'] = 'c'

    print(controlFor)
    print("")
    print("Balance before matching")
    print(matcher.assess_balance(data, treatment, controlForPS))
    print ("")

    data = matcher.score(data, assignment=treatment, confounder_types=controlFor)
    treated, control = matcher.match(data, assignment=treatment)
    print("")
    print("Balance after matching")
    print(matcher.assess_balance(treated.append(control), treatment, controlForPS))
    print ("")

    #now write results to mysql
    conn = MySQLdb.connect(host="localhost",
                         user="root",
                         passwd="",
                         db="causal_results")
    cur = conn.cursor()
    query = """ insert into akelleh_results values ('%s','%s','%s','%s','%s','%s');  """ % (now,dataset,treatment,target,str(ATE_results),"the presence of an independent lead director or a former CEO in the board could be a sign of weaker performances, being negatively correlated with Tobins Q")
    cur.execute(query)
    conn.commit()
    conn.close()
    print("Done")

def ceoPay (dataset, target, data_table, imp_vars_table, imp_limit=10):
    '''
    Test the effect of above average ceo pay
    M&M:
        NA
    '''
    data, types, imp_vars = getData(data_table, imp_vars_table)
    controlFor = stageVarTypes(imp_vars, imp_limit)
    treatment = 'CEOPayOverMedian'
    matcher = PropensityScoreMatching()
    ATE_results = matcher.estimate_ATE(data, treatment, target, controlFor, bootstrap=True)

    matcher.check_support(data, treatment, controlFor)

    controlForPS = controlFor.copy()
    controlForPS['propensity score'] = 'c'

    print(controlFor)
    print("")
    print("Balance before matching")
    print(matcher.assess_balance(data, treatment, controlForPS))
    print ("")

    data = matcher.score(data, assignment=treatment, confounder_types=controlFor)
    treated, control = matcher.match(data, assignment=treatment)
    print("")
    print("Balance after matching")
    print(matcher.assess_balance(treated.append(control), treatment, controlForPS))
    print ("")

    #now write results to mysql
    conn = MySQLdb.connect(host="localhost",
                         user="root",
                         passwd="",
                         db="causal_results")
    cur = conn.cursor()
    query = """ insert into akelleh_results values ('%s','%s','%s','%s','%s','%s');  """ % (now,dataset,treatment,target,str(ATE_results),"my own")
    cur.execute(query)
    conn.commit()
    conn.close()
    print("Done")

def esg (dataset, target, treatment, data_table, imp_vars_table, imp_limit=10):
    '''
    Test the effect of esg disclosure
    M&M:
        NA
    '''
    data, types, imp_vars = getData(data_table, imp_vars_table)
    controlFor = stageVarTypes(imp_vars, imp_limit)

    matcher = PropensityScoreMatching()
    ATE_results = matcher.estimate_ATE(data, treatment, target, controlFor, bootstrap=True)
    print(ATE_results)

    matcher.check_support(data, treatment, controlFor)

    controlForPS = controlFor.copy()
    controlForPS['propensity score'] = 'c'

    print(controlFor)
    print("")
    print("Balance before matching")
    print(matcher.assess_balance(data, treatment, controlForPS))
    print ("")

    data = matcher.score(data, assignment=treatment, confounder_types=controlFor)
    treated, control = matcher.match(data, assignment=treatment)
    print("")
    print("Balance after matching")
    print(matcher.assess_balance(treated.append(control), treatment, controlForPS))
    print ("")
    conn = MySQLdb.connect(host="localhost",
                         user="root",
                         passwd="",
                         db="causal_results")
    cur = conn.cursor()
    query = """ insert into akelleh_results values ('%s','%s','%s','%s','%s','%s');  """ % (now,dataset,treatment,target,str(ATE_results),"my own")
    cur.execute(query)
    conn.commit()
    conn.close()
    print("Done")


################################################################


if __name__ == "__main__":
    os.system('clear')


    #matching in general is good here
    #ageRange("eebp", "Tobins.Q.class", "eebp_agerange", "eebp_tobin_q_imp_vars")
    #ageRange("eebp", "AZS.class.Binary", "eebp_agerange", "eebp_altman_imp_vars")
    #ageRange("spx", "Tobins.Q.class", "spx_agerange", "spx_tobin_q_imp_vars", imp_limit=7)
    #ageRange("spx", "AZS.class.Binary", "spx_agerange", "spx_altman_imp_vars", imp_limit=7)
    #ageRange("sxxp", "Tobins.Q.class", "sxxp_agerange", "sxxp_tobin_q_imp_vars")
    #ageRange("sxxp", "AZS.class.Binary", "sxxp_agerange", "sxxp_altman_imp_vars")

    #matching not great for first 2/3, good after that
    #indepChaFCEO("eebp","Tobins.Q.class", "eebp_indepChFmlCEO", "eebp_tobin_q_imp_vars", imp_limit=6)
    #indepChaFCEO("eebp","AZS.class.Binary", "eebp_indepChFmlCEO", "eebp_altman_imp_vars", imp_limit=8)
    #indepChaFCEO("spx", "Tobins.Q.class", "spx_indepChFmlCEO", "spx_tobin_q_imp_vars")
    #indepChaFCEO("spx", "AZS.class.Binary", "spx_indepChFmlCEO", "spx_altman_imp_vars")
    #indepChaFCEO("sxxp", "Tobins.Q.class", "sxxp_indepChFmlCEO", "sxxp_tobin_q_imp_vars", imp_limit=9)
    #indepChaFCEO("sxxp", "AZS.class.Binary", "sxxp_indepChFmlCEO", "sxxp_altman_imp_vars", imp_limit=9)

    #matching not great for first 2/3, good after that
    #wmOnBoard("eebp","Tobins.Q.class","eebp_fboard","eebp_tobin_q_imp_vars", imp_limit=9)
    #wmOnBoard("eebp","AZS.class.Binary","eebp_fboard","eebp_altman_imp_vars", imp_limit=7)
    #wmOnBoard("spx","Tobins.Q.class", "spx_fboard","spx_tobin_q_imp_vars", imp_limit=9)
    #wmOnBoard("spx","AZS.class.Binary", "spx_fboard","spx_altman_imp_vars")
    #wmOnBoard("sxxp","Tobins.Q.class", "sxxp_fboard","sxxp_tobin_q_imp_vars", imp_limit=8)
    #wmOnBoard("sxxp","AZS.class.Binary", "sxxp_fboard","sxxp_altman_imp_vars", imp_limit=7)

    #come back to
    ##fceo("eebp", "eebp_fceo", "eebp_tobin_q_imp_vars") #there are only 4 feml CEO's in the org dataset. leave off
    #fceo("spx","Tobins.Q.class" ,"spx_fceo", "spx_tobin_q_imp_vars", imp_limit=8)
    #fceo("spx","AZS.class.Binary" ,"spx_fceo", "spx_altman_imp_vars", imp_limit=7)
    #fceo("sxxp", "Tobins.Q.class", "sxxp_fceo", "sxxp_tobin_q_imp_vars", imp_limit=9)
    #fceo("sxxp", "AZS.class.Binary", "sxxp_fceo", "sxxp_altman_imp_vars", imp_limit=9)

    #matching not great for first 2/3, good after that
    ##indepDirFinlL("eebp", "eebp_indepdirfincl", "eebp_altman_imp_vars") #not enough samples are treated
    #indepDirFinlL("spx","AZS.class.Binary", "spx_indepdirfincl", "spx_altman_imp_vars")
    #indepDirFinlL("spx","Tobins.Q.class", "spx_indepdirfincl", "spx_tobin_q_imp_vars",imp_limit=8)
    #indepDirFinlL("sxxp", "Tobins.Q.class", "sxxp_indepdirfincl", "sxxp_tobin_q_imp_vars", imp_limit=9)
    #indepDirFinlL("sxxp", "AZS.class.Binary", "sxxp_indepdirfincl", "sxxp_altman_imp_vars",imp_limit=8)
    #indepDirFinlL("spx_mscore","MScore", "spx_mscore_indepdirfincl", "spx_tobin_q_imp_vars", imp_limit=3)

    #very few variables actually being used here, overall not great but might be usable
    #finlL("eebp", "Tobins.Q.class" , "eebp_fl", "eebp_tobin_q_imp_vars")
    #finlL("eebp", "AZS.class.Binary" , "eebp_fl", "eebp_altman_imp_vars", imp_limit=4)
    #finlL("spx", "Tobins.Q.class", "spx_fl", "spx_tobin_q_imp_vars", imp_limit=5)
    #finlL("spx", "AZS.class.Binary", "spx_fl", "spx_altman_imp_vars", imp_limit=2)
    #finlL("sxxp", "Tobins.Q.class", "sxxp_fl", "sxxp_tobin_q_imp_vars", imp_limit=1)
    #finlL("sxxp", "AZS.class.Binary", "sxxp_fl", "sxxp_altman_imp_vars", imp_limit=5) #this is decent
    #finlL("spx_mscore", "MScore", "spx_mscore_fl", "spx_tobin_q_imp_vars", imp_limit=1)
    #finlL("spx_mscore", "EightVarEq", "spx_cgcp", "spx_tobin_q_imp_vars", imp_limit=1)


    #not very good
    ##indepDirFormerCEOBoard("eebp", "Tobins.Q.class" , "eebp_indepdirformerceo", "eebp_tobin_q_imp_vars", imp_limit=1 )
    ##indepDirFormerCEOBoard("eebp", "AZS.class.Binary" , "eebp_indepdirformerceo", "eebp_altman_imp_vars")
    #indepDirFormerCEOBoard("spx", "Tobins.Q.class" , "spx_indepdirformerceo", "spx_tobin_q_imp_vars")
    #indepDirFormerCEOBoard("spx", "AZS.class.Binary" , "spx_indepdirformerceo", "spx_altman_imp_vars")
    #indepDirFormerCEOBoard("sxxp", "Tobins.Q.class" , "sxxp_indepdirformerceo", "sxxp_tobin_q_imp_vars")
    #indepDirFormerCEOBoard("sxxp", "AZS.class.Binary" , "sxxp_indepdirformerceo", "sxxp_altman_imp_vars")

    #good
    #ceoPay("spx_ceopay", "Tobins.Q.class" , "spx_ceopay", "spx_tobin_q_imp_vars")
    #ceoPay("spx_ceopay", "AZS.class.Binary" , "spx_ceopay", "spx_altman_imp_vars")

    #some good, some not so good
    #esg("spx", "Tobins.Q.class" , "esg_disc_over_avg", "spx_csr", "spx_tobin_q_imp_vars",imp_limit=9 )
    #esg("spx", "AZS.class.Binary" , "esg_disc_over_avg", "spx_csr", "spx_altman_imp_vars")
    #esg("spx", "Tobins.Q.class" , "FAIR_REMUNERATION_POLICY", "spx_csr", "spx_tobin_q_imp_vars",imp_limit=6)
    #esg("spx", "AZS.class.Binary" , "FAIR_REMUNERATION_POLICY", "spx_csr", "spx_altman_imp_vars",imp_limit=5)
    #esg("spx", "Tobins.Q.class" , "social_disc_over_avg", "spx_csr", "spx_tobin_q_imp_vars") #pretty good matching
    esg("spx", "AZS.class.Binary" , "social_disc_over_avg", "spx_csr", "spx_altman_imp_vars",imp_limit=9) #pretty good matching
    #esg("spx", "Tobins.Q.class" , "EQUAL_OPPORTUNITY_POLICY", "spx_csr", "spx_tobin_q_imp_vars")
    #esg("spx", "AZS.class.Binary" , "EQUAL_OPPORTUNITY_POLICY", "spx_csr", "spx_altman_imp_vars")
    #esg("spx", "Tobins.Q.class" , "ANTI.BRIBERY_ETHICS_POLICY", "spx_csr", "spx_tobin_q_imp_vars")
    #esg("spx", "AZS.class.Binary" , "ANTI.BRIBERY_ETHICS_POLICY", "spx_csr", "spx_altman_imp_vars")



    createLatestResultsTable()
