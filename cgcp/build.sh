#!/bin/bash
#/
#/ JOB NAME : build
#/
#/ Purpose : builds this project
#/ Create date : 02-03-2018
#/
export jobDir=`pwd`
export loadDir=`cd "$jobDir"; cd load/; pwd`

filePermissions(){
  cd "$loadDir"
  chmod 755 load.sh cleanData.sh replaceBlanks.py

}

filePermissions
