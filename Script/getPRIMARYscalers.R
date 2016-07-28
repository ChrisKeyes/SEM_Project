# This script will generate a matrix of scalers (0,1] to deflate length of stay where 
# applicable. The value of each scaler is a function of the party's primary purpose.
# 
#     WRITE OUT ASSUMPTIONS FOR SCALE MATRIX HERE
#     
#     tripPurpose: 1 = PARK was primary purpose
#                  2 = PARK was one destination among several of equal importance
#                  3 = PARK was incidental
#                  
#     equallyNearby: 0 = Other destination of equal importance was not in local area
#                    1 = Other destination(s) of equal importance were in local area
#     
#     primaryNearby: 0 = Primary destination was in local area
#                    1 = Primary destination was not in local area
# 
# NOTE: The current adjustment scheme cannot be applied to parks where trip purpose, 
# daysPark, or hoursPark responses are not present.

###########################################################################################
# The code below is run within a conditional statement checking for the existance of 
# neccessary variables
if(exists("tripPurpose", where = PARKsegments) == TRUE &
   exists("daysPark", where = PARKsegments) == TRUE){
  
# Depending on the PARKS data, the code below may need to be modified to create the 
# correct subsets of trip purpose types.  There are 5 subsets of trip purpose types 
# needed for full adjustment.
PARKsegments$fullPURPOSE <- NA

# Primary purpose parties:
PARKsegments$fullPURPOSE <- ifelse(PARKsegments$tripPurpose == 1, 1, NA)

# Equally important visitor parties:
PARKsegments$fullPURPOSE[(PARKsegments$tripPurpose == 2 & PARKsegments$equallyNearby == 0)] <- 2
PARKsegments$fullPURPOSE[(PARKsegments$tripPurpose == 2 & PARKsegments$equallyNearby == 1)] <- 3

# Incidental visitor parties:
PARKsegments$fullPURPOSE[(PARKsegments$tripPurpose == 3 & PARKsegments$primaryNearby == 0)] <- 4
PARKsegments$fullPURPOSE[(PARKsegments$tripPurpose == 3 & PARKsegments$primaryNearby == 1)] <- 5

# Identify and correct those observations where one or more of the three variables are incomplete
PARKsegments$fullPURPOSE[PARKsegments$tripPurpose == 2 &
                         is.na(PARKsegments$equallyNearby) &
                         is.na(PARKsegments$primaryNearby)] <- 2 # equal, assume other is in LA?

PARKsegments$fullPURPOSE[PARKsegments$tripPurpose == 2 &
                         is.na(PARKsegments$equallyNearby) &
                         PARKsegments$primaryNearby == 0] <- 3 # equal, other is not in LA?

PARKsegments$fullPURPOSE[PARKsegments$tripPurpose == 2 &
                         is.na(PARKsegments$equallyNearby) &
                         PARKsegments$primaryNearby == 1] <- 2 # equal, other is in LA?

PARKsegments$fullPURPOSE[PARKsegments$tripPurpose == 3 &
                         is.na(PARKsegments$equallyNearby) &
                         is.na(PARKsegments$primaryNearby)] <- 4

PARKsegments$fullPURPOSE[is.na(PARKsegments$tripPurpose) &
                         PARKsegments$equallyNearby == 0] <- 3 

PARKsegments$fullPURPOSE[is.na(PARKsegments$tripPurpose) &
                         PARKsegments$equallyNearby == 1] <- 2

PARKsegments$fullPURPOSE[is.na(PARKsegments$tripPurpose) &
                         is.na(PARKsegments$equallyNearby) &
                         PARKsegments$primaryNearby == 0] <- 4 

PARKsegments$fullPURPOSE[is.na(PARKsegments$tripPurpose) &
                         is.na(PARKsegments$equallyNearby) &
                         PARKsegments$primaryNearby == 1] <- 4 

PARKsegments$fullPURPOSE[is.na(PARKsegments$tripPurpose) &
                         is.na(PARKsegments$equallyNearby) &
                         is.na(PARKsegments$primaryNearby)] <- 0 # Need to sort all these out 


# Attach a column of 1's to PARKsegments named PRIMARYscalers
PARKsegments$PRIMARYscalers <- 1

PARKsegments$hoursRATIO <- PARKsegments$hoursPark/12
PARKsegments$daysRATIO <- PARKsegments$daysPark/(PARKsegments$daysPark + 1)

# Fill in PRIMARYscalers vector using segment type and fullPURPOSE assignment.
# PRIMARYscalers == 1 when day_local == 1
 
# Subset on day_nonlocal
tempDF <- subset(PARKsegments, day_nonlocal == 1, select = c("fullPURPOSE", "PRIMARYscalers", "hoursRATIO"))

    tempDF$PRIMARYscalers <- round(ifelse((tempDF$fullPURPOSE == 2 | tempDF$fullPURPOSE == 5), tempDF$hoursRATIO , 
                          ifelse(tempDF$fullPURPOSE == 4, 0, 1)), 2)

    PARKsegments$PRIMARYscalers[PARKsegments$day_nonlocal == 1] <- c(tempDF$PRIMARYscalers)

# Subset on all overnight segments 
PARKsegments$ON_Any <- rowSums(PARKsegments[,c(SEGvars_on)], na.rm = TRUE)

tempDF <- subset(PARKsegments, ON_Any == 1, select = c("fullPURPOSE", "PRIMARYscalers", "daysRATIO"))

tempDF$PRIMARYscalers <- round(ifelse((tempDF$fullPURPOSE == 2 | tempDF$fullPURPOSE == 5), tempDF$daysRATIO , 
                          ifelse(tempDF$fullPURPOSE == 4, 0, 1)), 2)

PARKsegments$PRIMARYscalers[PARKsegments$ON_Any == 1] <- c(tempDF$PRIMARYscalers)

# Need to fix PRIMARYscalers for daysRATIO == NA 

}