# This script categorizes each party into their respective segment category. Each segment is 
# represented as column in PARKsegments and is boolean in its value:
# 1 == Respondant is this segment type
# 0 == Repsondant is not this segment type

# Add columns to PARKsegments to classify each observation, column name is segment type (i.e. ON_CampIn)
# Each column is initially a vector of zeros

for (VAR in PARK_SegmentVars){
  
  v <- paste("ON", substring(VAR, 7), sep = "_")
  
  PARKsegments[,v] <- 0   # for each SegmentVar that is present in the PARKsegments data, create a column
  # named "v" and fill it with zeros
}

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

for (x in 1:length(PARKsegments$ID)){
  if (as.integer(PARKsegments$overnight[x]) == 1){  # Confine loop to only overnight
    
    
    maxNightsType <- colnames(PARKsegments[c(x),PARK_SegmentVars])[max.col(PARKsegments[c(x),PARK_SegmentVars],
                                                                           ties.method="first")]
    # maxNightsType: vector specifying the type of accomodation with the largest number of nights provided
    
    # NOTE: ties.method = "first" returns the first value in the vector, problem when all accomodation 
    # types are equal (most likely all zeros)
    
    maxNights <- max(PARKsegments[c(x), PARK_SegmentVars])
    minNights <- min(PARKsegments[c(x), PARK_SegmentVars])
    # maxNights/minNights: vector specifying the largest/smallest integer value of nights given.
    
    if (is.na(maxNightsType) | 
        as.character(maxNights) == 0 |
        as.character(maxNights) == as.character(minNights)){ 
      
            IDs_badNights <- append(IDs_badNights, as.character(PARKsegments$ID[x])) 
              # IDs_badNights: vector of ID's which need to be addressed due to 
              # equal nights in multiple segment categories or missing data that 
              # was not caught by checks
    }
    
    else { # If the observation is clean, categorize it accordingly
      v <- paste("ON", substring(maxNightsType, 7), sep = "_")
      
          PARKsegments[x,v] <- 1
    }
  }
}

# If any observations were not able to be categorized by segment, stop the script 
# and give user warning message to check the ID's which are problematic

if(length(IDs_badNights) > 0) stop(
  "There are ID's which cannot be put into segments. Review IDs_badNights")
