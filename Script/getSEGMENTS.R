# This script categorizes each party into their respective segment category. Each segment is 
# represented as column in PARKsegments and is binary in its value:
# 1 == Respondant is this segment type
# 0 == Repsondant is not this segment type

###########################################################################################

# Set working directory
setwd("~/SEM_Project")

# Add columns to PARKsegments to classify each observation, column name is segment type 
# (i.e. ON_CampIn). Each column is initially a vector of zeros
PARKsegments[,c(SEGvars)] <- 0

# Identify local day trip and local overnight observations
PARKsegments$day_local <- ifelse(PARKsegments$local == 1 & PARKsegments$overnight == 0, 1, 0)
PARKsegments$day_nonlocal <- ifelse(PARKsegments$local == 0 & PARKsegments$overnight == 0, 1,0)

PARKsegments$overnight_local <- ifelse(PARKsegments$local == 1 & PARKsegments$overnight == 1, 1,0 )
PARKsegments$overnight_nonlocal <- ifelse(PARKsegments$local == 0 & PARKsegments$overnight == 1, 1 ,0)


# Categorize all overnight parties by maxiumum nights over accomodation types. While looping through 
#  observations, collect ID's which are either unusuable (slipped through previous cleaning scripts)
# or their maximum nights by accomodation type are shared by two types of accomodation (ex. ID= 0001,
# nightsCampOut = 3, nightsCampIn = 3). 

IDs_badNights <- as.character(NULL) # Will capture "ID"s with incomplete data

for(x in 1:length(PARKsegments$ID)){
  if(as.integer(PARKsegments$overnight[x]) == 1){  # Confine loop to only overnight
    maxNightsType <- colnames(PARKsegments[c(x),PARK_SegmentVars])[max.col(PARKsegments[c(x),PARK_SegmentVars],
                                                                           ties.method="first")]
    maxNightsType.last <- colnames(PARKsegments[c(x),PARK_SegmentVars])[max.col(PARKsegments[c(x),PARK_SegmentVars],
                                                                            ties.method="last")]
    
    # maxNightsType: vector specifying the type of accomodation with the largest number of nights provided
    maxNights <- max(PARKsegments[c(x), PARK_SegmentVars], na.rm = T)
    minNights <- min(PARKsegments[c(x), PARK_SegmentVars], na.rm = T)
    # maxNights/minNights: vector specifying the largest/smallest integer value of nights given.
    
    if (is.na(maxNightsType) | 
        as.character(maxNights) == 0 |
        as.character(maxNights) == as.character(minNights) |
        maxNightsType != maxNightsType.last){ 
      
        IDs_badNights <- append(IDs_badNights, as.character(PARKsegments$ID[x])) 
          
          v <- paste("ON", substring(maxNightsType, 7), sep = "_")
      
          PARKsegments[x,v] <- 1
}
              # IDs_badNights: vector of ID's which need to be addressed due to 
              # equal nights in multiple segment categories or missing data that 
              # was not caught by checks
    else { # If the observation is clean, categorize it accordingly
      v <- paste("ON", substring(maxNightsType, 7), sep = "_")
      
          PARKsegments[x,v] <- 1
}}}
# If any observations were not able to be categorized by segment, stop the script 
# and give user warning message to check the ID's which are problematic

if(length(IDs_badNights) > 0){ 
  message("The ID's in IDs_badNights, listed below, may not have been assigned the correct segment")
  print(IDs_badNights)
}
###########################################################################################
