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
  cd "$dataDir"/raw
  cp spx_* "$dataDir"/processing

  cd "$dataDir"/processing
  ssconvert spx_091214.xlsx spx.csv
  rm spx_091214.xlsx

  #sort out some weird stuff in the files
  for filename in *.csv; do
    sed -i '' 's/#N\/A\ N\/A//g' $filename
    sed -i '' 's/,/;/g' $filename
  done

  #run the replaceBlanks file on each csv
  #replaces blanks with nulls
  #then move in to the clean dir
  cd "$jobDir"
  for filename in "$dataDir"/processing/*.csv; do
    python replaceBlanks.py "$filename" "$filename".bak
    mv "$filename".bak "$filename"
    mv "$filename" "$dataDir"/clean
  done

  echo "  Cleaned."

}

sxxp(){
  echo "  Cleaning sxxp."
  cd "$dataDir"/raw
  cp sxxp_091214.xlsx "$dataDir"/processing

  cd "$dataDir"/processing
  ssconvert sxxp_091214.xlsx sxxp.csv
  rm sxxp_091214.xlsx
  sed -i '' 's/#N\/A\ N\/A//g' sxxp.csv
  sed -i '' 's/#N\/A\ Field\ Not\ Applicable//g' sxxp.csv
  sed -i '' 's/,/;/g' sxxp.csv

  cd "$jobDir"
  python replaceBlanks.py "$dataDir"/processing/sxxp.csv "$dataDir"/processing/sxxpNoBlanks.csv

  cd "$dataDir"/processing
  rm sxxp.csv; mv sxxpNoBlanks.csv sxxp.csv
  mv sxxp.csv "$dataDir"/clean
  echo "  Cleaned."

}


main(){
  eebp
  spx
  sxxp

}
main
