#!/bin/bash

jobDir=`pwd`
rawDataDir=`cd $jobDir; cd ../data/processed; pwd;`
ddlDir=`cd $jobDir; cd ../data/ddl; pwd;`


ddl(){
  echo "Performing DDL."
  cd "$ddlDir"
  mysql -u root corp_gov < ddl.sql
  returnCode=$?
  echo "DDL ran. The return code is: " $returnCode

}


load(){
  echo "Loading data."
  cd "$rawDataDir"

  mysqlimport -u root --local corp_gov sxxp.csv  --fields-terminated-by=',' --lines-terminated-by='\n'  --ignore-lines=1
  errorCheck $? sxxp

  mysqlimport -u root --local corp_gov spx.csv  --fields-terminated-by=',' --lines-terminated-by='\n'  --ignore-lines=1
  errorCheck $? spx

  cp eebp.csv eebp.csv.bk
  sed -i -e 's/\?/\\N/g' eebp.csv
  mysqlimport -u root --local corp_gov eebp.csv  --fields-terminated-by=',' --lines-terminated-by='\n'  --ignore-lines=1
  rm eebp.csv eebp.csv-e
  mv eebp.csv.bk eebp.csv
  errorCheck $? eebp

}


errorCheck(){
  if [ $1 -ne 0 ]
  then
    echo "An error has occured with "$2". Error code: " $1
  fi

}

clear
ddl
load
