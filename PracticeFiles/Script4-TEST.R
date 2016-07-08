setwd("~/SEM_Project")

# Create PARKsegments data frame from PARKsem data (copy of PARKsem data) 
PARKsegments <- PARKsem

PARKsegments$sumBADS <- PARKbads$local_1 + PARKbads$overnight_1 + PARKbads$overnight_3

PARKsegments <- subset(PARKsegments, sumBADS == 0)

row.names(PARKsegments) <- NULL   

###########################################################################################
#***************** Categorize Observations by Accomodation ********************************

# Add columns to PARKsegments to classify each observation
# Each column is initially a vector of zeros

# PARKsegments$day_local <- 0

SegmentVars <- c("nightsBackcountry", "nightsCampIn", "nightsCampOut",
                 "nightsLodgeIn", "nightsLodgeOut", "nightsCruise", "nightsOther")    

PARK_SegmentVars <- NULL
PARK_nonlocalVars <- NULL
for (y in SegmentVars){
  if (exists(y, where = PARKsegments) == TRUE){
    
    PARK_SegmentVars <- append(PARK_SegmentVars, y)   # vector of segments specific to PARK for use later
    
    v <- paste(substring(y,7),
               "nonlocal", sep = "_") 
    PARK_nonlocalVars <- append(PARK_nonlocalVars, v)
    
    # This code reads:
    # take the SegmentVar and remove the first six letter (e.g. "nights")
    # then combine the remaining string with "_nonlocal" and store as variable "v"
    
    PARKsegments[,v] <- 0   # for each SegmentVar that is present in the PARKsegments data, create a column
    # named "v" and fill it with zeros
  }
}

# Identify local day trip and local overnight observations
PARKsegments$day_local <- ifelse(PARKsegments$local == 1 & PARKsegments$overnight == 0, 1, 0)
PARKsegments$overnight_local <- ifelse(PARKsegments$local == 1 & PARKsegments$overnight == 1, 1,0 )
PARKsegments$day_nonlocal <- ifelse(PARKsegments$local == 0 & PARKsegments$overnight == 0, 1 , 0)

tempDF <- subset(PARKsegments, PARKsegments$overnight_local == 0 &
                               PARKsegments$day_local == 0 &
                               PARKsegments$day_nonlocal == 0, 
                 select = c("ID", "overnight", "local", c(PARK_SegmentVars) , c(PARK_nonlocalVars)) )


for (night in PARK_SegmentVars){
  tempDF[,night] <- ifelse(is.na(tempDF[,night]), 0, tempDF[,night])
}


tempDF$NIGHTsegment <- paste(substring(c(PARK_SegmentVars[max.col(tempDF[,PARK_SegmentVars])]),7),
      "nonlocal", sep = "_")

for (night in PARK_nonlocalVars){
  tempDF[,night] <- ifelse(night == tempDF$NIGHTsegment, tempDF[,night] <- 1, tempDF[,night] <- 0)
}

for (night in PARK_nonlocalVars){
  IDs <- tempDF[tempDF$NIGHTsegment==night, "ID"]
    PARKsegments[c(match(IDs, PARKsegments$ID )),night] <- 1
    
}

      # badNights <- as.character(NULL) # Will capture "ID"s with incomplete data
      # 
      # # for (x in 1:length(PARKsegments$local)){
      # for (x in 1:length(PARKsegments$local)){
      #   if (as.integer(PARKsegments$local[x])== 0 &
      #       as.integer(PARKsegments$overnight[x])== 1){  # Confine loop to only nonlocal & overnight 
      #     
      #     maxNightsType <- colnames(PARKsegments[c(x),PARK_SegmentVars])[max.col(PARKsegments[c(x),PARK_SegmentVars],
      #                                                                            ties.method="first")]
      #     # maxNightsType: vector specifying the type of accomodation with the largest number of nights provided
      #     # NOTE: maxNightsType = NA if any nights* category is NA
      #     
      #     # NOTE: ties.method = "first" returns the first value in the vector, problem when all accomodation 
      #     # types are equal (most likely all zeros)
      #     
      #     maxNights <- max(PARKsegments[c(x), PARK_SegmentVars])
      #     minNights <- min(PARKsegments[c(x), PARK_SegmentVars])
      #     # maxNights/minNights: vector specifying the largest/smallest integer value of nights given.
      #     
      #     if (is.na(maxNightsType) | 
      #         as.character(maxNights) == 0 |
      #         as.character(maxNights) == as.character(minNights)){ # collect "ID"'s for problematic observations 
      #       badNights <- append(badNights, as.character(PARKsegments$ID[x])) 
      #     }
      #     
      #     else { # If the observation is clean, categorize it accordingly
      #       v <- paste(substring(maxNightsType, 7), "nonlocal", sep = "_")
      #       
      #       PARKsegments[x,v] <- 1
      #     }
      #   }
      # }
      # 
# PARKsegments <- PARKsegments[-c(match(badNights, PARKsegments$ID)),] # just drop these variables for the time being 
# row.names(PARKsegments) <- NULL   # Reset row names

###########################################################################################
#***************** Generate Segment Shares ************************************************

# Number of remaining observations 
n <- length(PARKsegments$overnight)

# Number of overnight & day observations (overall)
n_overnight <- sum((PARKsegments[,"overnight"]==1)) 
# NOTE:  if you recieve "NA" rather than a number, the data is not suffieciently cleaned
n_day <- sum((PARKsegments[,"overnight"] == 0))

# Overnight and day shares
ON_share <- n_overnight/n
DAY_share <- n_day/n



# Shares for day and overnight observations 
n_daylocal <- sum((PARKsegments[,"day_local"]==1))
n_daynonlocal <- sum(PARKsegments[, "day_local"]==0)
n_overnightlocal <- sum((PARKsegments[, "overnight_local"] == 1))

DAYlocal_share <- n_daylocal/n
DAYnonlocal_share <- n_daynonlocal/n
ONlocal_share <- n_overnightlocal/n

# Get shares by accomodation type for nonlocal observations
a <- NULL
b <- NULL
c <- NULL

for (y in PARK_SegmentVars){
  v <- paste(substring(y, 7), "nonlocal", sep = "_")
  x <- sum(PARKsegments[, v]==1)
  z <- x/n
  assign((paste(v, "share", sep = "_")), x )
  a <- append(a, v)
  b <- append(b, z)
  c <- append(c, x)
}

a <- append(c("Overall", "Overnight", "Day", "Day_Local", "Day_NonLocal", "Overnight_Local"), a)
b <- append(c(1,ON_share, DAY_share, DAYlocal_share, DAYnonlocal_share, ONlocal_share), b)
c <- append(c(n, n_overnight, n_day, n_daylocal, n_daynonlocal, n_overnightlocal), c)

PARKshares_table <- data.frame(SEGMENT = a, SHARE = b, OBSERVATIONS = c)  


###########################################################################################
#***************** Generate Spending Profiles *********************************************



# write.csv(PARKsegments, "PARKsegments.csv", row.names = FALSE)

