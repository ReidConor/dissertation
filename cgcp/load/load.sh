#!/bin/bash
#/
#/ JOB NAME : load
#/
#/ Purpose : ETL proceedure for loading raw data into mysql for analysis
#/ Create date : 20-01-2018
#/
export jobDir=`pwd`
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
  mysql -u root < ddl.sql
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

  echo "Loaded."
}


manipulations(){
  cd "$jobDir"
  ./dataManipulationsMM.R
  errorCheck $?

}


main(){
  clear
  ddl
  cleanData
  load eebp.csv
  load spx.csv
  load sxxp.csv
  manipulations

}
main
