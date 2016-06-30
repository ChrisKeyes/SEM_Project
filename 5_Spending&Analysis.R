# This script will take the cleaned data (PARKsem) and variables generated from script #4 
# and create spending profiles and analysize the summary statistics

setwd("~/SEM_Project")

source("4_Variable-Generation.R")

# Clear the environment except for the data we want
rm(list=setdiff(ls(), c("CHECKvars", "PARKbads", "PARKsegments", "PARKsem", "PARKshares_table")))  # NOTE: once script is finalized, delete this line 

expVARS <- colnames(PARKbads[grepl(paste("^", "exp*", sep = ""),colnames(PARKbads))])
nightsVARS <- colnames(PARKbads[grepl(paste("^", "nights*", sep = ""), colnames(PARKbads))])

PARKexp <- as.character(strsplit(expVARS, "_1"))
PARKnights <- as.character(strsplit(nightsVARS, "_1"))

# Use the bads matrix to fix any incomplete observations where accomodation type is NA
# ex. Respondent said campIN=2, but left all other accomodation types bland (e.g. nights*_1 == 1)

# NOTE: this is a quick solution below

# For all PARKnights and PARKexp variables, convert NA's to zero (assumes respondents refused to answer 
# accomodation and  expenditure types which did not apply to them)
for (x in PARKnights){
  PARKsegments[,x] <- ifelse(is.na(PARKsegments[,x]),0,PARKsegments[,x])
}

for (x in PARKexp){
  PARKsegments[,x] <- ifelse(is.na(PARKsegments[,x]),0,PARKsegments[,x])
}


a <- NULL
b <- NULL
c <- NULL

for(x in PARKnights){
  v <- paste(substring(x, 7), "nonlocal", sep = "_")
            a <- append(a, v)
  
      for (y in PARKexp){
            z <- mean(PARKsegments[with(PARKsegments, PARKsegments[,v]==1 & PARKsegments[,y] > 0) , y], na.rm = TRUE)
                b <- append(b, z)
                c <- append(c, y)
      }
  
      assign(v, data.frame(EXP = c, MEAN = b))
}




# almost there with this loop, should convert "NA"'s to 0 using BADS before running this and 
# need to verify values being produced.
