require(reshape)
library(stringr)
library(rpart)
setwd("/Users/Conor/Google Drive/MSc/dissertation/data/processed")

#######Analysis
#eastern europe 300
eebp = read.csv("eebp.csv", sep=";", na.strings="?")
summary(eebp)
