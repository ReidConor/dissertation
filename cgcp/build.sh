#!/bin/bash
#/
#/ JOB NAME : build
#/
#/ Purpose : builds this project
#/ Create date : 02-03-2018
#/
export jobDir=`pwd`
export loadDir=`cd "$jobDir"; cd load/; pwd`
export analysisDir=`cd analysis; pwd`

filePermissions(){
  cd "$loadDir"
  chmod 755 load.sh cleanData.sh replaceBlanks.py

  cd "$analysisDir"
  chmod 755 *.R

}

filePermissions
