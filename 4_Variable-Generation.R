# This script will use the PARKbads matrix to generate 
# variables for spending analysis.

# Use the preceeding scripts to get the data
setwd("~/SEM_Project")

source("2_Cleaning.R")
    # NOTE: "2_Cleaning.R" runs and is written such that:
              # PARKbads <- TESTbads
              # PARKsem  <- TESTsem
              #     Run the code below to change to different park


# Set data frames to park of interest 
        # PARKbads <- CUVAbads
        # PARKsem <- CUVAsem

###########################################################################################
#***************** SEGMENT VARIABLE CREATION & ANALYSIS ***********************************

# The park specific data frame will be called "PARKsegments" and will be used for 
# segment analysis

# Subset data by droping observations are incomplete or unuseable. For segments, data points
# must be identifiable as "local" | "nonlocal". Observations must also provide data neccessary
# to categorize trip type. These categories are:
      # daytrip
      # lodgeIn
      # lodgeOut
      # campIn
      # campOut
      # other


# local/nonlocal: 
#     Must have answered "local" (e.g. PARKbads; local_1 == 0)
# 
# Trip type:
#     Must have answered "overnight", and for overnight visitors, must have identified their 
#     type of lodging for at least one night (e.g. PARKbads, overnight_1 == 0 & nights*_1 == 0)


# Create PARKsegments data frame from PARKsem data (copy of PARKsem data) 
PARKsegments <- PARKsem

# Create a vector of "ID"'s which cannot be identified as local | nonlocal
badLocal <- PARKbads[PARKbads$local_1 == 1 , "ID"]  

# Create a vector of "ID"'s which do not identify day | overnight trip
badOvernight <- PARKbads[PARKbads$overnight_1 == 1 , "ID"]

      # NOTE: This would be a good point for "overnight_3" check to drop invalid lodging data

# Drop the observations from PARKsegments by "ID" using badLocal & badOvernight
IDdrop <- as.factor(union(badLocal, badOvernight))

PARKsegments <- PARKsegments[ -c(match(IDdrop, PARKsegments$ID)),]

      # NOTE: the match() function ensures that we are selecting observations by their unique ID value
      #       rather than their row/column index


# PARKsegments is now our subsetted data set (from PARKsem) that can be used for segment analysis
head(PARKsegments)  

###########################################################################################
#***************** Begin Finding Segment Shares *******************************************
# Begin finding shares:
n <- length(PARKsegments$overnight)

n_overnight <- sum((PARKsegments[,"overnight"]==1)) 
# NOTE:  if you recieve "NA" rather than a number, the data is not suffieciently cleaned
n_day <- sum((PARKsegments[,"overnight"] == 0))

ON_share <- n_overnight/n

    # check for consistancy
    n_overnight == (n - n_day)   # "TRUE" is good


# Add columns to PARKsegments to classify each observation
# Each column is initially a vector of zeros
PARKsegments$day_local <- 0

SegmentVars <- c("nightsBackcountry", "nightsCampIn", "nightsCampOut",
                 "nightsLodgeIn", "nightsLodgeOut", "nightsCruise", "nightsOther")    

PARKsegmentvars <- NULL

for (y in SegmentVars){
  if (exists(y, where = PARKsegments) == TRUE){
    
        PARKsegmentvars <- append(PARKsegmentvars, y)   # vector of segments for use later
      
        v <- paste(substring(y,7),
                   "nonlocal", sep = "_")   # This code reads:
            # take the SegmentVar and remove the first six letter (e.g. "nights")
            # then combine the remaining string with "_nonlocal" and store as variable "v"
        
        PARKsegments[,v] <- 0   # for each SegmentVar that is present in the PARKsegments data, create a column
                                # named "v" and fill it with zeros
  }
}


PARKsegments$day_local[as.integer(PARKsegments$local)==1 &
                         as.numeric(PARKsegments$overnight)==0 ] <- 1

### try using "aggregate" function ####
### http://stackoverflow.com/questions/25314336/how-to-extract-the-maximum-value-within-each-group-in-a-data-frame
# for (x in PARKsegments[,"overnight"]){
#   if (as.numeric(x) == 1){
#     max(as.integer(PARKsegments[x, c(Segments_colnames)]) >=1 )
#   }
# }
# 
# apply( x , 1, function(x) which(x==max(x))) %>%
#          x <- PARKsegments[ y , PARKsegmentvars] %>%
#          y <- c(with(PARKsegments, overnight == 1)) 
            
# maxNightsType <- colnames(PARKsegments[,PARKsegmentvars])[max.col(PARKsegments[,PARKsegmentvars],ties.method="first")]  
badNights <- NULL
for (x in 1:length(PARKsegments$local)){
  if(as.integer(PARKsegments$local[x])==0 &
     as.integer(PARKsegments$overnight[x] == 1)){
          maxNightsType <- colnames(PARKsegments[c(x),PARKsegmentvars])[max.col(PARKsegments[c(x),PARKsegmentvars],
                                                                                ties.method="first")]  
          # ties.method = "first" returns the first value in the vector. Find a way to sort this out, maybe noting these 
          # ID values for later use.
          
          if(is.na(maxNightsType)){
            badNights <- append(badNights, PARKsegments$ID[x])
          }
          else {
          v <- paste(substring(maxNightsType, 7),
                     "nonlocal", sep = "_")
          
          PARKsegments[x,v] <- 1
        }
    }
}

badNights <- PARKsegments[c(badNights), "ID"]   # INCORPORATE THIS LOOP INTO BADS MATRIX


# write.csv(PARKsegments, "PARKsegments.csv", row.names = FALSE)

# SAVING THE SCRIPT BELOW FOR REFERENCE #

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


# # Create PARKsegments data frame from PARKsem data 
# PARKsegments <- PARKsem
# 
# # Rule 1: create a vector of "ID"'s which do not satisfy rule 1:
# r1Fail <- PARKbads[PARKbads$local_1 == 1 , "ID"]  #rule 1
# 
# # Rule 2: create a vector of "ID"'s which do not satisfy rule 2. 
# # First, use overnight_1 and hoursPark_1 to fill in incomplete observations:
# r2Check <- PARKbads[PARKbads$overnight_1 == 1 &
#                       PARKbads$hoursPark_1 == 1 , "ID"]
# 
# # The vector r2Check is a list of "ID"'s which are incomplete but can be assigned
# # day trip category.  Change overnight == NA to overnight == 0
# PARKsegments$overnight[c(r2Check)] <- 0
# 
# # Collect "ID"s which do not satisfy rule 2
# r2Fail <- PARKbads[PARKbads$overnight_1 == 1, "ID"]
# 
# # Remove from r2Fail the ID's which have been corrected using r2Check
# r2Fail <- as.factor(setdiff(r2Fail, r2Check))
# 
# # Drop the observations from PARKsegments which fail Rule 1 and Rule 2:
# IDdrop <- as.factor(union(r1Fail, r2Fail))
# 
# PARKsegments <- PARKsegments[ -c(match(IDdrop, PARKsegments$ID)),]
#   head(PARKsegments)  #this is our subsetted data for analysis
#   
# # Begin finding shares:
# n <- length(PARKsegments$overnight)
# 
# n_overnight <- sum((PARKsegments[,"overnight"]==1)) 
#       # NOTE:  if you recieve "NA" rather than a number, the data is not suffieciently cleaned
# n_day <- sum((PARKsegments[,"overnight"] == 0))
# 
#     ON_share <- n_overnight/n
#     
# # check for consistancy
#   n_overnight == (n - n_day)   # "TRUE" is good
#   
# 
# # Add columns to PARKsegments to classify each observation
# # Each column is initially a vector of zeros
# PARKsegments$day_local <- 0
# 
# nonlocal_Segments <- c("day", "CampIn", "CampOut" , "LodgeIn", "LodgeOut", "Other")
# 
# Segments_colnames <- paste("nights", c("CampIn", "CampOut" , "LodgeIn", "LodgeOut", "Other"), sep = "")
# 
# for (y in nonLocal_Segments){
#   v = paste(y , "nonlocal", sep = "_")
#    
#   PARKsegments[,v] <- 0
# }
# 
# 
# PARKsegments$day_local[as.integer(PARKsegments$local)==1 &
#                          as.numeric(PARKsegments$overnight)==0 ] <- 1
# 
# PARKsegments$day_nonlocal[as.integer(PARKsegments$local)==0 &
#                          as.numeric(PARKsegments$overnight)==0 ] <- 1
# 
# ### try using "aggregate" function ####
# ### http://stackoverflow.com/questions/25314336/how-to-extract-the-maximum-value-within-each-group-in-a-data-frame
# for (x in PARKsegments[,"overnight"]){
#   if (as.numeric(x) == 1){
#      max(as.integer(PARKsegments[x, c(Segments_colnames)]) >=1 )
# }
# }
# 
# 
