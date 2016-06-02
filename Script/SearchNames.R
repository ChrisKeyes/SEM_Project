#This function renames the survey variable names to the search variable names

SearchNames <- function(PARKsegments) {
  colnames(PARKsegments) <- SearchVars[c(which(Matches == TRUE))]
  }

#Have not got this to work as function, but line 4 works as single code