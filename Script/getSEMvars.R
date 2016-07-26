# This script changes the variable names in PARKdata to the corresponding SEM variable names.
# In doing so, all variables will have consistant names for analysis

################################################################################################

# Set working directory
setwd("~/SEM_Project")

# Generate a vector of variable names which are specific to the PARK and needed for the
# the SEM analysis.
PARK_SEMvars <- levels(SEMvars[,c(PARKname)])

# Drop the blank (NULL) values from the vector
PARK_SEMvars <- PARK_SEMvars[which(PARK_SEMvars!= c(""))]  # remove any NULL values

# Subset PARKdata by selecting thse variables that match those in PARK_SEMvars. Name this
# new data frame "PARKsem"
PARKsem <- PARKdata[c(PARK_SEMvars)]  

# Rename the variables to SEMvars for consistancy.  Generate a vector of indicies which
# matches the original variable names in PARK_SEMvars to those in the column SEMvars. 
# Use the column index to assign the correct SEM variable names to PARKsem.
rowINDEX <- NULL

for (VAR in PARK_SEMvars){
  i <- which(SEMvars[c(PARKname)] == VAR) # i: row number
      rowINDEX <- append(rowINDEX, i)     # add row number to vector of ids
}

# Assign the correct variable names to PARKsem
colnames(PARKsem) <- SEMvars$SEMvars[c(rowINDEX)]

################################################################################################
