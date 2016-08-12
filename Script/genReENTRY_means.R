# This script will generate the re-entry rates for PARK

# These are the checks from script 2:

        # entries_1: check for entries == NA & DKentries == NA
        # These respondents refused to answer the question
                
        # DKentries_1: check if respondent only answered "Dont Know"

# Drop observations where 
        # PARKbads$entries_1 = 1
        # PARKbads$DKentries_1 = 1
# Also drop observations where entries == 0  

###########################################################################################

# Set working directory
setwd("~/SEM_Project")

# This script runs within the conditional statement that verifies "entries" variable is
# present in the PARKsem data.frame
if(exists("entries", where = PARKsem) == TRUE){ 

# Generate a temporary data frame from PARKsegments named tempDF
tempDF <- data.frame(PARKsegments)

# Add a column to tempDF named "sumBADSentries" which is the sum of the check variables
# explained above.  These check variables reflect the minimum criteria for an observation
# to be used in the ReENTRY analysis. See "genBADS.R" for additional information.
tempDF$sumBADSentries <- PARKbads_seg$entries_1 + PARKbads_seg$DKentries_1

# Subset tempDF by dropping observations where sumBADSentries is greater than zero
tempDF <- subset(tempDF,
                 sumBADSentries == 0 & entries > 0,
                 select = c("entries",  c(SEGvars)))
      
    colnames(tempDF) <- c("entries", SEGvars)

    
# Generate a table named "PARKreEntry_MEANS" and find the mean re-entry rate by 
# party per visit to local area. 
PARKreEntry_MEANS <- data.frame(matrix(
                                    ncol = length(SEGvars),
                                    nrow = 0))

    colnames(PARKreEntry_MEANS) <- SEGvars
    
# Loop through the segment variables and generate the mean re-entry rates by 
# segment type.  Fill in PARKreEntry_MEANS with these values.
for (VAR in SEGvars){
  PARKreEntry_MEANS["n", VAR] <- sum(tempDF[,VAR])
  PARKreEntry_MEANS["Party_reEntry", VAR] <- 
            round(mean(tempDF$entries[tempDF[,VAR]==1]),2)}

}


