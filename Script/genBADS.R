# This script will use a series of conditional statements to identify missing, incomplete, and 
# outlying observations. The conditional statements are outlined in "CHECKvars.csv". If additional
# checks are to be added for future use, they must be added to this .csv file. 

###################################################################################################
setwd("~/SEM_Project")

Bads <- subset(PARKsem, select = ID)

####################################################################################################
# PERFORM CHECKS ON SEM VARIABLES -----------------------------------------------------------------
# Location variables ------------------------------------------------------------------------------

# Local:
# Local_1; check for NA
        # check <- PARKsem[is.na(PARKsem$local),"ID"]
        #     Bads$local_1[c(check)] <- 1
Bads$local_1 <- ifelse(is.na(PARKsem$local)==TRUE, 1, 0)

# zip:
# Factor variable with 306 levels
# The "as.numeric" function forces the factors into numbers with range 1:number of levels
# For zip, as.numeric identifies each a_Zip as a number 1:306; "levels()" lists levels
# levels(PARKsem$zip)   

# zip_1; Identify NULL (blank zip codes)
Bads$zip_1 <- ifelse(is.na(PARKsem$zip), 1, 0)

#***************************************************************************************************
# Expenditure Variables ----------------------------------------------------------------------------

# No checks as of now

#***************************************************************************************************
# Segment Variables --------------------------------------------------------------------------------

# segment_1:
# Identify observations where there are duplicate nights by accomodation type (duplicates > 0).
# These observations need to be assigned a segment type based on predetermined rules which assign
# the party the higher of the two expenditure profiles.

Bads$segment_1 <- 0

for (x in 1:length(PARKsem$ID)){
if (as.integer(PARKsem$nightsLocalArea[x]) > 1 &
      (PARKsem$nightsLocalArea[x] > max(PARKsem[x, PARK_SegmentVars], na.rm = T)) ){  

    maxNightsType <- colnames(PARKsem[c(x),PARK_SegmentVars])[max.col(PARKsem[c(x),PARK_SegmentVars],
                                                                           ties.method="first")]
    maxNightsType.last <- colnames(PARKsem[c(x),PARK_SegmentVars])[max.col(PARKsem[c(x),PARK_SegmentVars],
                                                                           ties.method="last")]
if (maxNightsType != maxNightsType.last){ 

    Bads[Bads$ID == PARKsem$ID[x], "segment_1"] <- 1
}}}

# NOTE: The observations identified in "segment_1" will be automatically assigned a segment 
# category in the "genSEGMENTS.R" script.  This category assignment is random and may be incorrect.
# The user must manually change these assignments.

#***************************************************************************************************
# Overnight -----------------------------------------------------------------------------------------

# overnight_1: 
# Check to see if respondant refused to answer overnight (or if RSG did not fill in).  
# If overnight is NULL, see hoursPark_1 and daysPark_1 to fill in incomplete data

# check <- PARKsem[is.na(PARKsem$overnight) ,"ID"]
#       Bads$overnight_1[c(check)] <- 1
      
Bads$overnight_1 <- ifelse(is.na(PARKsem$overnight), 1, 0)

# overnight_2:
# Check to identify observations with overnight==NA, but with positive sum of overnight stays
# in the lodging sectors (e.g. sum(nights*) >= 1)
Bads$overnight_2 <- 0
Bads["overnight_2"][is.na(PARKsem["overnight"]) & PARKsem$nightsLocalArea >0] <- 1
          
# overnight_3:
# Check to verify that for overnight==1, at least one "nights*" variable is >= 1 
Bads$overnight_3 <- 0
Bads["overnight_3"][PARKsem["overnight"]==1 & PARKsem["nightsLocalArea"]==0] <- 1      

#***************************************************************************************************
# hoursPark & daysPark -----------------------------------------------------------------------------

if (exists("hoursPark", where = PARKsem)==TRUE){
  
# hoursPark_1:
#   not currently being used
  
# hoursPark_2:        
# Check to see if respondant answered >24 hours. Assumed to be "cheap talk" response.
Bads$hoursPark_2 <- ifelse(is.na(PARKsem$hoursPark)==FALSE & PARKsem$hoursPark > 24, 1, 0)

# daysPark_1:
# Check for observations with daysPark == NA and hoursPark == NA
# If both daysPark and hoursPark are blank, assume daysPark == 1 unless overnight suggests otherwise
Bads$daysPark_1 <- ifelse(is.na(PARKsem$daysPark) & 
                          is.na(PARKsem$hoursPark), 1, 0)

}

#***************************************************************************************************
# Entries ------------------------------------------------------------------------------------------

if (exists("entries", where = PARKsem) == TRUE){
  
# entries_1: 
# Check for entries == NA & DKentries == NA
# These respondents refused to answer the question
Bads$entries_1 <- ifelse(is.na(PARKsem$entries) & is.na(PARKsem$DKentries), 1, 0)

# DKentries_1: check if respondent only answered "Dont Know"
Bads$DKentries_1 <- ifelse(is.na(PARKsem$entries) & is.na(PARKsem$DKentries)==FALSE &
                            as.integer(PARKsem$DKentries)== 99, 1, 0)

# check <- PARKsem[is.na(PARKsem$entries) &
#                    as.integer(PARKsem$DKentries)==99 ,"ID"]    
#     check <- na.omit(check)
# 
# Bads$DKentries_1[c(check)] <- 1

}

#***************************************************************************************************
# Append column for summation of checks for each ID
# Bads$Sum <- rowSums(Bads[,c(which(colnames(Bads) != c("ID")))])    




