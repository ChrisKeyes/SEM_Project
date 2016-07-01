
# create vector of variable names which define which variable names to keep
MATCHvars <- levels(SEMVars[,c(PARKname)])
      MATCHvars <- MATCHvars[which(MATCHvars != c(""))]  # remove any NULL values

SEARCHvars <- NULL
for (x in MATCHvars){
SEARCHvars <- append(SEARCHvars, c(grep(x, colnames(PARKdata))))
}


# subset data on the selected variable names 

PARKsem <- PARKdata[,match(c(MATCHvars), colnames(PARKdata))]  
  
#Rename the variables to SEMvars for consistancy
colIndex <- NULL
for (r in MATCHvars){
  i <- which(SEMVars[c(PARKname)] == r)
  colIndex <- append(colIndex, i)
}
  
colnames(PARKsem) <- SEMVars$SEMvars[c(colIndex)]
  
# End of script - Move to cleaning script
setwd("~/SEM_Project")


