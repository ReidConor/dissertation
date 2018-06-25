#!/bin/bash
#/
#/ JOB NAME : build
#/
#/ Purpose : builds this project
#/ Create date : 02-03-2018
#/

export jobDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
export loadDir=`cd "$jobDir"; cd load/; pwd`
export analysisDir=`cd "$jobDir"; cd analysis; pwd`
export causalDir=`cd "$jobDir"; cd causality/akelleh; pwd`

filePermissions(){
  cd "$loadDir"
  chmod 755 load.sh cleanData.sh replaceBlanks.py
  chmod 755 *.R

  cd "$analysisDir"
  chmod 755 *.R

  cd "$causalDir"
  chmod 755 *.py

}

filePermissions
