#!/bin/bash
#/
#/ JOB NAME : load
#/
#/ Purpose : ETL proceedure for loading raw data into mysql for analysis
#/ Create date : 20-01-2018
#/
export jobDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
export dissertationDir=`cd "$jobDir"; cd ../../; pwd`
export dataDir=`cd "$dissertationDir"/data/; pwd;`

errorCheck(){
  if [ $1 -ne 0 ]
  then
    echo "An error has occured. Error code: " $1
    exit 99
  fi

}

ddl(){
  echo "Performing DDL."
  cd "$jobDir"/ddl
  mysql -u root < corp_gov.sql
  errorCheck $?
  echo "DDL'd."

}


cleanData(){
  echo "Cleaning Data."
  cd "$jobDir"
  ./cleanData.sh
  errorCheck $?
  echo "Cleaned."

}


load(){
  echo "Load Data."
  cd "$dataDir"/clean
  mysqlimport \
    -u root \
    --local \
    --ignore-lines=1 \
    --fields-terminated-by=';' \
    --fields-enclosed-by='"' \
    --lines-terminated-by='\r\n' \
    corp_gov \
    $1
  errorCheck $?
  echo "Loaded."
}


main(){
  clear
  ddl
  cleanData

  #theres a better way of doing the below
  #but want the easy ability to comment out / in any of these for ad-hoc runs
  load eebp.csv
  load spx.csv
  load sxxp.csv

  load spx_esg.csv
  load spx_emissions.csv
  load spx_energy.csv
  load spx_actual_new_income_per_employee.csv
  load spx_cash_flow_per_employee.csv
  load spx_ghg_scope_1.csv

  load spx_csr.csv
  load spx_mscore.csv

  load spx_ceo_comp.csv

}
main
