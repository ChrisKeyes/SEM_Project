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

# Primary purpose parties:
PARKsegments$fullPURPOSE <- ifelse(PARKsegments$tripPurpose == 1, 1, NA)

# Equally important visitor parties:
PARKsegments$fullPURPOSE[(PARKsegments$tripPurpose == 2 & PARKsegments$equallyNearby == 0)] <- 3
PARKsegments$fullPURPOSE[(PARKsegments$tripPurpose == 2 & PARKsegments$equallyNearby == 1)] <- 2

# Incidental visitor parties:
PARKsegments$fullPURPOSE[(PARKsegments$tripPurpose == 3 & PARKsegments$primaryNearby == 0)] <- 5
PARKsegments$fullPURPOSE[(PARKsegments$tripPurpose == 3 & PARKsegments$primaryNearby == 1)] <- 4

# Incomplete Responses:
PARKsegments$fullPURPOSE[is.na(PARKsegments$tripPurpose) & PARKsegments$equallyNearby == 0] <- 3
PARKsegments$fullPURPOSE[is.na(PARKsegments$tripPurpose) & PARKsegments$equallyNearby == 1] <- 2

PARKsegments$fullPURPOSE[is.na(PARKsegments$tripPurpose) & is.na(PARKsegments$equallyNearby) & PARKsegments$primaryNearby == 1] <- 4
PARKsegments$fullPURPOSE[is.na(PARKsegments$tripPurpose) & is.na(PARKsegments$equallyNearby) & is.na(PARKsegments$primaryNearby)] <- 4

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


# Attach a column of 1's to PARKsegments named PRIMARYscalers
PARKsegments$PRIMARYscalers <- 1

# Attach a column for both the hours ratio and days ratios
PARKsegments$hoursRATIO <- round(PARKsegments$hoursPark/12, 2)
PARKsegments$hoursRATIO[is.na(PARKsegments$hoursRATIO)==TRUE] <- round(mean(PARKsegments$hoursPark/12, na.rm = TRUE),2)
PARKsegments$hoursRATIO[PARKsegments$hoursRATIO > 1] <- 1

PARKsegments$daysRATIO <- round(PARKsegments$daysPark/(PARKsegments$daysPark + 1), 2)
PARKsegments$daysRATIO[is.na(PARKsegments$daysRATIO)==TRUE] <- round(mean(PARKsegments$daysPark/(PARKsegments$daysPark+1), na.rm = TRUE),2)
PARKsegments$daysRATIO[PARKsegments$daysRATIO > 1] <- 1

# Fill in PRIMARYscalers vector using segment type and fullPURPOSE assignment.
# PRIMARYscalers == 1 when day_local == 1
 
# For observations where day_nonlocal == 1
for (i in 1:length(PARKsegments$day_nonlocal)){
  if(PARKsegments$day_nonlocal[i] == 1){
    if(PARKsegments$fullPURPOSE[i] == 1){PARKsegments$PRIMARYscalers[i] <- 1}
    else if(PARKsegments$fullPURPOSE[i] == 2){PARKsegments$PRIMARYscalers[i] <- PARKsegments$hoursRATIO[i]}
    else if(PARKsegments$fullPURPOSE[i] == 3){PARKsegments$PRIMARYscalers[i] <- 1}
    else if(PARKsegments$fullPURPOSE[i] == 4){PARKsegments$PRIMARYscalers[i] <- 0}
    else if(PARKsegments$fullPURPOSE[i] == 5){PARKsegments$PRIMARYscalers[i] <- PARKsegments$hoursRATIO[i]}}}    

PARKsegments$ON_Any <- PARKsegments$overnight_local + PARKsegments$overnight_nonlocal

for (i in 1:length(PARKsegments$ON_Any)){
  if(PARKsegments$ON_Any[i] == 1){
    if(PARKsegments$fullPURPOSE[i] == 1){PARKsegments$PRIMARYscalers[i] <- 1}
    else if (PARKsegments$fullPURPOSE[i] == 2){PARKsegments$PRIMARYscalers[i] <- PARKsegments$daysRATIO[i]}
    else if (PARKsegments$fullPURPOSE[i] == 3){PARKsegments$PRIMARYscalers[i] <- 1}
    else if (PARKsegments$fullPURPOSE[i] == 4){PARKsegments$PRIMARYscalers[i] <- 0}
    else if (PARKsegments$fullPURPOSE[i] == 5){PARKsegments$PRIMARYscalers[i] <- PARKsegments$daysRATIO[i]}}}








}