# This script will use the PARKbads matrix to generate 
# variables for spending analysis.

# Use the preceeding scripts to get the data
setwd("~/SEM_Project")

source("2_Cleaning.R")

# Set data frames to park of interest 
PARKbads <- CUVAbads
PARKsem <- CUVAsem

###########################################################################################
#***************** SEGMENT VARIABLE CREATION & ANALYSIS ***********************************

# The park specific data frame will be called "PARKsegments" and will be used for 
# segment analysis

# Subset data by droping observations which do not match rules:

# RULE_1: Must have answered local (from PARKbads, local_1 == 0)
       
      # NOTE: we could write additional check to fill in missing "local" values. 
      # For example, if local == NA, but zip == "some country" then we can assign 
      # observation to non-local category
       
# RULE_2: Must have answered overnight or have sufficient data to assign day or ON trip

            # (e.g. within PARKbads, overnight_1 == 0 & daysPark_1 == 0)
            # IDpassR1 <- PARKbads[PARKbads$local_1 == 0 , "ID"]  #rule 1
            #     PARKsegments <- PARKsem[c(IDpassR1),] 
            # 
            # IDpassR2 <- PARKbads[PARKbads$overnight_1 == 0 &
            #                      PARKbads$daysPark_1 == 0 , "ID"] #rule 2
            # 
            # PARKsegments <- PARKsegments[na.omit(match(IDpassR2, PARKsegments$ID)),]
            
            # Try to drop observations, rather than keep


# Create PARKsegments data frame from PARKsem data 
PARKsegments <- PARKsem

# Rule 1: create a vector of "ID"'s which do not satisfy rule 1:
r1Fail <- PARKbads[PARKbads$local_1 == 1 , "ID"]  #rule 1

# Rule 2: create a vector of "ID"'s which do not satisfy rule 2. 
# First, use overnight_1 and hoursPark_1 to fill in incomplete observations:
r2Check <- PARKbads[PARKbads$overnight_1 == 1 &
                      PARKbads$hoursPark_1 == 1 , "ID"]

# The vector r2Check is a list of "ID"'s which are incomplete but can be assigned
# day trip category.  Change overnight == NA to overnight == 0
PARKsegments$overnight[c(r2Check)] <- 0

# Collect "ID"s which do not satisfy rule 2
r2Fail <- PARKbads[PARKbads$overnight_1 == 1, "ID"]

# Remove from r2Fail the ID's which have been corrected using r2Check
r2Fail <- as.factor(setdiff(r2Fail, r2Check))

# Drop the observations from PARKsegments which fail Rule 1 and Rule 2:
IDdrop <- as.factor(union(r1Fail, r2Fail))

PARKsegments <- PARKsegments[ -c(match(IDdrop, PARKsegments$ID)),]


# CODE ABOVE WORKS - CREATE SHARES NEXT 



