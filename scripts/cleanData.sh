#!/bin/bash
#/
#/ JOB NAME : cleanData
#/
#/ Purpose : clean data such that it can be loaded into mysql
#/ Create date : 20-01-2018
#/
# Help Message
usage() {
  clear
  grep '^#/' "$0" | cut -c4-
  exit 0
}
expr "$*" : ".*--help" > /dev/null && usage
# ----------

eebp(){
  echo "  Cleaning eebp."
  cd "$dataDir"/raw
  cp eebp_091214.csv "$dataDir"/processing

  cd "$dataDir"/processing
  mv eebp_091214.csv eebp.csv
  sed -i '' 's/?//g' eebp.csv

  cd "$jobDir"
  python replaceBlanks.py "$dataDir"/processing/eebp.csv "$dataDir"/processing/eebpNoBlanks.csv

  cd "$dataDir"/processing
  rm eebp.csv; mv eebpNoBlanks.csv eebp.csv
  mv eebp.csv "$dataDir"/clean
  echo "  Cleaned."

}

spx(){
  echo "  Cleaning spx."
  echo "  Cleaned."

}

sxxp(){
  echo "  Cleaning sxxp."
  echo "  Cleaned."

}


main(){
  eebp
  spx
  sxxp

}
main
