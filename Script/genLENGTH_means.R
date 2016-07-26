#This script writes average length of stay estimates for each visitor segment.
#The script outputs the dataframe PARKlength_MEANS
###########################################################################################

# Set working directory
setwd("~/SEM_Project")

# Build an empty dataframe that has relevant segments as columns
# and length of stay related variables as rows
PARKlength_MEANS <- data.frame(matrix(ncol = length(SEGvars), nrow=0))
      colnames(PARKlength_MEANS) <- SEGvars

# subset the PARKsegments dataframe to only include columns needed for length of 
# stay analysis. Name the temporary data frame tempDF, and generate a vector
# of variables to subset PARKsegments on named VARS
VARS <- c("hoursPark","daysPark","nightsLocalArea", SEGvars)
tempDF <- subset(PARKsegments, select = VARS)

# Remove any observations from tempDF which do not meet criteria to be used for analysis
# The criteria required are captured in the checks explained in "CHECKvars.csv". For 
# the length of stay parameters, the check variables below must equal zero. See "genBADS.R"
# for additional information on these checks.

# In PARKbads_seg, sum across: hoursPARK_2, daysPARK_1, daysPark_2. Add a column to tempDF
# named BADlength, which is the sum of the variables above.
tempDF$BADlength <- PARKbads_seg$hoursPark_2 + PARKbads_seg$daysPark_1 + PARKbads_seg$daysPark_2
      tempDF <- subset(tempDF, tempDF$BADlength == 0)

# Create a new variable "daysParkAdj" that estimates number of days in the park. 
#       If daysPark is greater than zero, daysParkAdj = daysPark. 
#       If daysPark is greater than zero, but not an integer, round up to nearest integer
#       If daysPark is greater than zero & hoursPark is greater than 
#       zero, daysParkAdj = daysPark
tempDF$daysParkAdj <- ifelse(!is.na(tempDF$daysPark),tempDF$daysPark, 1)
tempDF$daysParkAdj <- ceiling(tempDF$daysParkAdj)

# Create a new variable "daysLocalArea" equal to nightsLocalArea + 1. 
# NOTE: nightsLocalArea = 0 when all of the accomodation types are NA. 
tempDF$daysLocalArea <- tempDF$nightsLocalArea +1

# Check to make sure that daysParkAdj <= daysLocalArea. Attach a column to tempDF 
# named "check" and is a dummy variable where:
#       check = 1 if daysLocalArea >= daysParkAdj
#       check = 0 otherwise
tempDF$check <- ifelse(tempDF$daysParkAdj <= tempDF$daysLocalArea,0,1)

# Subset tempDF by droping any observations where check = 1
tempDF <- subset(tempDF, tempDF$check == 0)

# Loop through segment types (SEGvars) to calculate average length of stay values by segment 
# and print them to the PARKlength_MEANS dataframe.
for (VAR in SEGvars){
  PARKlength_MEANS["n",VAR] <- sum(tempDF[,VAR])
  PARKlength_MEANS["daysPark",VAR] <- round(mean(tempDF[tempDF[,VAR]==1,"daysParkAdj"]),2)
  PARKlength_MEANS["daysLocalArea",VAR] <- round(mean(tempDF[tempDF[,VAR]==1,"daysLocalArea"]),2)
  PARKlength_MEANS["nightsLocalArea",VAR] <- round(mean(tempDF[
                                                    tempDF[,VAR]==1,"nightsLocalArea"]),2)
#For lenghtVSE, use daysLocalArea for day trips and nightLocalArea for overnight trips
ifelse(VAR == "day_local"|VAR == "day_nonlocal", 
  PARKlength_MEANS["lengthVSE",VAR] <- 
                round(mean(tempDF[tempDF[,VAR]==1,"daysLocalArea"]),2),
  PARKlength_MEANS["lengthVSE",VAR] <- 
                round(mean(tempDF[tempDF[,VAR]==1,"nightsLocalArea"]),2))

  PARKlength_MEANS["percentDaysInPark",VAR] <- 
                percent(mean(tempDF[tempDF[,VAR]==1,"daysParkAdj"])/
                        mean(tempDF[tempDF[,VAR]==1,"daysLocalArea"]))
}

