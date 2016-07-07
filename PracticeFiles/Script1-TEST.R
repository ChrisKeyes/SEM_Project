
# create vector of variable names which define which variable names to keep
MATCHvars <- levels(SEMvars[,c(PARKname)])
      MATCHvars <- MATCHvars[which(MATCHvars != c(""))]  # remove any NULL values

SEARCHvars <- NULL
for (x in MATCHvars){
SEARCHvars <- append(SEARCHvars, c(grep(x, colnames(PARKdata))))
}


# subset data on the selected variable names 
for (x in MATCHvars){
  y <- subset(PARKdata, select = x )
}

    rm(y)
    
PARKsem <- subset(PARKdata, select = c(MATCHvars))

# PARKsem <- PARKdata[,match(c(MATCHvars), colnames(PARKdata))]  
  
#Rename the variables to SEMvars for consistancy
colIndex <- NULL
for (r in MATCHvars){
  i <- which(SEMvars[c(PARKname)] == r)
  colIndex <- append(colIndex, i)
}
  
colnames(PARKsem) <- SEMvars$SEMvars[c(colIndex)]
  
# End of script - Move to cleaning script
setwd("~/SEM_Project")


