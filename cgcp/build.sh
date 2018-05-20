#!/bin/bash
#/
#/ JOB NAME : build
#/
#/ Purpose : builds this project
#/ Create date : 02-03-2018
#/

#try this https://stackoverflow.com/questions/59895/getting-the-source-directory-of-a-bash-script-from-within?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
export jobDir=/Users/Conor/Google\ Drive/MSc/_dissertation/cgcp
export loadDir=`cd "$jobDir"; cd load/; pwd`
export analysisDir=`cd "$jobDir"; cd analysis; pwd`

filePermissions(){
  cd "$loadDir"
  chmod 755 load.sh cleanData.sh replaceBlanks.py

  cd "$analysisDir"
  chmod 755 *.R

}

filePermissions
