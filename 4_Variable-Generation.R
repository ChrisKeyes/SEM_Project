# This script will use the PARKbads matrix to generate 
# variables for spending analysis.

# Clear all data in memory
rm(list=union(ls(), ls()))

# Use the preceeding scripts to get the data
setwd("~/SEM_Project")

# library(plyr)

source("1_Load&Subset.R")

      PARKsem <- TESTdata
      
# Upload the GROUPvars and create vectors of each variable category. Keep only those variables
# that are specific to the current park.

source("~/SEM_Project/Script/getGroupVars.R")
      

source("2_Cleaning.R")

      PARKbads <- Bads
      
rm(Bads)
    rm(CUVAdata,GATEdata,YOSEdata, PARKdata)  # Remove all extra data and keep TESTdata
      
###########################################################################################
#***************** SEGMENT VARIABLE CREATION & ANALYSIS ***********************************

# Before modifying/subsetting the data further, create a copy of the original PARKsem data frame
    # NOTE: AT THIS POINT, WE COULD SAVE PARKsem TO THE OUTPUT FOLDER AND CLEAN THE DATA FRAME
    # IN MEMORY USING THE SAME NAME 
PARKsem_raw <- PARKsem

# Create PARKsegments data frame from PARKsem data (copy of PARKsem data) 
PARKsegments <- PARKsem
      # WANT TO DELETE THIS LINE AND CONTINUE WORKING WITH PARKSEM, CREATE PARK SEGMENTS LATER

# For observations where overnight == NA, but sum(nights*)>=1, change overnight <- 1 
  # (i.e. PARKbads$overnight_2 == 1)
    IDs_ON2 <- PARKbads[which(PARKbads$overnight_2 == 1), "ID"]   

        PARKsegments$overnight[IDs_ON2] <- 1
    
# For observations where overnight == 1, but one or more of the accomodation 
# types is NA, replace with zero
        
for (VAR in PARK_SegmentVars){
          
    IDs_SEG <- na.omit(PARKsegments[with(PARKsegments, overnight == 1 & is.na(PARKsegments[VAR])), "ID"])
        
        PARKsegments[match(IDs_SEG, PARKsegments$ID), VAR ] <- 0
}
        
# For observations where zip == " " (i.e. zip is blank, PARKbads$zip_2 == 1), replace with zip == NA
# (i.e. PARKbads$zip_2 == 1)
    IDs_ZIP2 <- PARKbads[which(PARKbads$zip_2 == 1), "ID"]
        
        PARKsegments$zip[c(IDs_ZIP2)] <- NA
        
# For observations where local==NA, but zip is provided, change local to 
# local == 1 if zip matches local zipcodes or
# local == 0 if zip matches non-local zipcodes

source("~/SEM_Project/Script/clean_Local-Zip.R")

# Now re-run script 2 to produce a second BADS matrix which reflects the corrected data frame

PARKsem <- PARKsegments   # This is temporary solution due to name overlap
    
    source("2_Cleaning.R")

        PARKbads_seg <- Bads
        PARKsegments <- PARKsem # This is temporary solution due to name overlap
            rm(Bads, PARKsem)
            
# Create a column "sumBADS" which is the sum of the three checks which capture
# which observations could not be cleaned/completed and will be dropped
            # local_1: local == NA
            # overnight_1: overnight == NA
            # overnight_3: overnight == 1 & nightsLocalArea == 0

PARKsegments$sumBADS <- PARKbads_seg$local_1 +
                        PARKbads_seg$overnight_1  +
                        PARKbads_seg$overnight_3
    
# Create a vector of :"ID"'s which will be dropped, then subset the data by dropping bad observations
DropIDs <- PARKsegments[PARKsegments$sumBADS >=1, "ID"]

PARKsegments <- subset(PARKsegments, sumBADS == 0 , select = c(colnames(PARKsegments)))

PARKbads_seg <- PARKbads_seg[-c(DropIDs),]

# Clean up memory
      rm(DropIDs, ErrorZip, LocalZip, m, MATCHvars, nonLocalZip, PARK_localzip,
         PARK_nonlocalzip, VAR, zips,
         IDs_local1, IDs_ZIP2, IDs_ON2, IDs_SEG)

      
###########################################################################################
#***************** Categorize Observations by Accomodation ********************************

source("~/SEM_Project/Script/IdentifySegments.R")
      
# # Add columns to PARKsegments to classify each observation
# # Each column is initially a vector of zeros
# 
# for (VAR in PARK_SegmentVars){
#   
#         v <- paste("ON", substring(VAR, 7), sep = "_")
# 
#         PARKsegments[,v] <- 0   # for each SegmentVar that is present in the PARKsegments data, create a column
#                                 # named "v" and fill it with zeros
#   }
# 
# # Identify local day trip and local overnight observations
# PARKsegments$day_local <- ifelse(PARKsegments$local == 1 & PARKsegments$overnight == 0, 1, 0)
# PARKsegments$day_nonlocal <- ifelse(PARKsegments$local == 0 & PARKsegments$overnight == 0, 1,0)
# 
# PARKsegments$overnight_local <- ifelse(PARKsegments$local == 1 & PARKsegments$overnight == 1, 1,0 )
# PARKsegments$overnight_nonlocal <- ifelse(PARKsegments$local == 0 & PARKsegments$overnight == 1, 1 ,0)
# 
# # Fill in zeros for overnights that only answered select accomodation categories (changing NA to zeros)
#         # NOTE: The assumption being made here is that if the respondent identified themselves as overnight and 
#         # identified at least one accomodation type for one or more nights, than any other accomodation types 
#         # unanswered should be zero (replace NA with zero)
# 
# 
# # Categorize all nonlocal repondants by accomodation type. Create a vector of "ID"'s which are overnight 
# # but do not identify any type of accomodation type 
# 
# IDs_badNights <- as.character(NULL) # Will capture "ID"s with incomplete data
# 
# for (x in 1:length(PARKsegments$ID)){
#   if (as.integer(PARKsegments$overnight[x]) == 1){  # Confine loop to only overnight
#     
# 
#           maxNightsType <- colnames(PARKsegments[c(x),PARK_SegmentVars])[max.col(PARKsegments[c(x),PARK_SegmentVars],
#                                                                                 ties.method="first")]
#           # maxNightsType: vector specifying the type of accomodation with the largest number of nights provided
# 
#               # NOTE: ties.method = "first" returns the first value in the vector, problem when all accomodation 
#               # types are equal (most likely all zeros)
#           
#           maxNights <- max(PARKsegments[c(x), PARK_SegmentVars])
#           minNights <- min(PARKsegments[c(x), PARK_SegmentVars])
#                 # maxNights/minNights: vector specifying the largest/smallest integer value of nights given.
#           
#           if (is.na(maxNightsType) | 
#               as.character(maxNights) == 0 |
#               as.character(maxNights) == as.character(minNights)){ 
#                   IDs_badNights <- append(IDs_badNights, as.character(PARKsegments$ID[x])) 
#                   # IDs_badNights: vector of ID's which need to be addressed due to 
#                   # equal nights in multiple segment categories or missing data that 
#                   # was not caught by checks
#           }
#           
#           else { # If the observation is clean, categorize it accordingly
#               # v <- paste(substring(maxNightsType, 7), "nonlocal", sep = "_")
#                 v <- paste("ON", substring(maxNightsType, 7), sep = "_")
#                   
#                 PARKsegments[x,v] <- 1
#         }
#     }
# }
# 
# if(length(IDs_badNights) > 0) stop(
#   "There are ID's which cannot be put into segments. Review IDs_badNights")

###########################################################################################
#***************** Generate Segment Shares ************************************************
    
# Number of remaining observations 
    n <- length(PARKsegments$overnight)
    
# Number of overnight & day observations (overall)
    n_overnight <- sum((PARKsegments[,"overnight"]==1)) 
    n_day <- sum((PARKsegments[,"overnight"] == 0))

# Overnight and day shares
    ON_share <- n_overnight/n
    DAY_share <- n_day/n
    
# Shares for day and overnight observations 
    n_daylocal <- sum(PARKsegments[,"day_local"]==1)
    n_daynonlocal <- sum(PARKsegments[, "day_nonlocal"]==1)
    n_ONlocal <- sum(PARKsegments[, "overnight_local"] == 1)
    n_ONnonlocal <- sum(PARKsegments[,"overnight_nonlocal"] == 1)
    
        DAYlocal_share <- n_daylocal/n
        DAYnonlocal_share <- n_daynonlocal/n
        ONlocal_share <- n_ONlocal/n
        ONnonlocal_share <- n_ONnonlocal/n
        
# Get shares by accomodation type for nonlocal observations
a <- NULL   # a: vector of variable names by segment
b <- NULL   # b: vector of shares by segment
c <- NULL   # c: vector of observations by segment

    for (y in PARK_SegmentVars){
      v <- paste("ON", substring(y, 7), sep = "_")  # v: name of segment dummy variable
            x <- sum(PARKsegments[, v]==1)  # x: number of observations in given segment
            z <- x/n  # share of given segments in data set
            
                # assign((paste(v, "share", sep = "_")), x )
                    a <- append(a, v)
                    b <- append(b, z)
                    c <- append(c, x)
    }

                    a <- append(c("Overall", "Overnight", "Day", "Day_Local", "Day_NonLocal",
                                  "ON_Local", "ON_nonLocal"), a)
                    
                    b <- append(c(1,ON_share, DAY_share, DAYlocal_share, DAYnonlocal_share,
                                  ONlocal_share, ONnonlocal_share), b)
                    
                    c <- append(c(n, n_overnight, n_day, n_daylocal, n_daynonlocal, n_ONlocal,
                                  n_ONnonlocal), c)

PARKshares_table <- data.frame(SEGMENT = a, SHARE = b, OBSERVATIONS = c)  


