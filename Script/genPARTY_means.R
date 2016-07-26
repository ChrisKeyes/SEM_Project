# This script writes average party size estimates for each visitor segment
# The script outputs the dataframe PARKparty_MEANS
###########################################################################################

# Set working directory
setwd("~/SEM_Project")

# Build an empty dataframe that has relevant segments as columns (SEGvars).
PARKparty_MEANS <- data.frame(matrix(ncol = length(SEGvars), nrow=0))
      colnames(PARKparty_MEANS)<-SEGvars

# Generate a vector of the variable names needed for party size analysis. Name the
# vector "VARS"
VARS <- c("ID","grpSize","adultsCovered","childrenCovered","numSplit", SEGvars)

# Generate a temporary data.frame from PARKsegments which is subset on VARS and 
# named "tempDF"
tempDF <- subset(PARKsegments, select = VARS)

# Drop any observations from tempDF where adultsCovered is NA
tempDF <- subset(tempDF, !is.na(tempDF$adultsCovered))

# Create a new variable named "totalCovered", which is the sum of 
# adultsCovered + childrenCovered.  If childrenCovered is NA, then assume 
# childrenCovered is zero.
tempDF$totalCovered <- rowSums(tempDF[,c("adultsCovered","childrenCovered")], na.rm=TRUE)

# Loop through segment types (SEGvars) to calculate average party size values by segment 
# and print them to the PARKparty_MEANS dataframe.
for (VAR in SEGvars){
  PARKparty_MEANS["n",VAR] <- sum(tempDF[,VAR])
  PARKparty_MEANS["GroupSize",VAR] <- round(mean(tempDF[tempDF[,VAR]==1,"grpSize"]),2)
  PARKparty_MEANS["TotalCovered",VAR] <- round(mean(tempDF[tempDF[,VAR]==1,"totalCovered"]),2)
  PARKparty_MEANS["NumberSplit",VAR] <- round(mean(tempDF[tempDF[,VAR]==1,"numSplit"],na.rm=T),2)
}


