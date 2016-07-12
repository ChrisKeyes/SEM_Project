# This script will use the PARKbads matrix to generate 
# variables for spending analysis.

# Clear all data in memory
rm(list=union(ls(), ls()))

# Use the preceeding scripts to get the data
setwd("~/SEM_Project")

library(plyr)

source("1_Load&Subset.R")

      PARKsem <- TESTdata
      
# Upload the GROUPvars and create vectors of each variable category. Keep only those variables
# that are specific to the current park.

GROUPvars <- read.table("GROUPvars.csv", header = TRUE, sep = ",", na.strings = "")
      
  SEGMENTvars <- na.omit(GROUPvars$SEGMENTvars)
  EXPvars <- na.omit(GROUPvars$EXPENDITUREvars)
  TRIPvars <- na.omit(GROUPvars$TripToArea)
  
      PARK_SegmentVars <- NULL
      PARK_ExpVars <- NULL
      PARK_TripVars <- NULL
      
      for (VAR in SEGMENTvars){
        if (exists(VAR, where = PARKsem) == TRUE){
          PARK_SegmentVars <- append(PARK_SegmentVars, VAR)
        }
      }
      
      for (VAR in EXPvars){
        if (exists(VAR, where = PARKsem) == TRUE){
          PARK_ExpVars <- append(PARK_ExpVars, VAR)
        }
      }
      
      for (VAR in TRIPvars){
        if (exists(VAR, where = PARKsem) == TRUE){
          PARK_TripVars <- append(PARK_TripVars, VAR)
        }
      }
      

source("2_Cleaning.R")

      PARKbads <- Bads
      
rm(Bads)
    rm(CUVAdata,GATEdata,YOSEdata)  # Remove all extra data and keep TESTdata
      
###########################################################################################
#***************** SEGMENT VARIABLE CREATION & ANALYSIS ***********************************

# Before modifying/subsetting the data further, create a copy of the original PARKsem data frame
PARKsem_COPY <- PARKsem

# Create PARKsegments data frame from PARKsem data (copy of PARKsem data) 
PARKsegments <- PARKsem
      # WANT TO DELETE THIS LINE AND CONTINUE WORKING WITH PARKSEM, CREATE PARK SEGMENTS LATER

# For observations where overnight == NA, but sum(nights*)>=1, change overnight <- 1 
    IDs_ON2 <- PARKbads[which(PARKbads$overnight_2 == 1), "ID"]   # save for testing/verification

PARKsegments$overnight <- ifelse(PARKbads$overnight_2 == 1, 1, PARKsegments$overnight)
      # **** CHECK TO VERIFY THIS WORKS CORRECTLY **** 

# For observations where zip == " " (i.e. zip is blank, PARKbads$zip_2 == 1), replace with zip == NA
    IDs_ZIP2 <- PARKbads[which(PARKbads$zip_2 == 1), "ID"]
    
    PARKsegments$zip[c(IDs_ZIP2)] <- NA

# For observations where overnight == 1, but one or more of the accomodation types is NA
# (e.g. PARK_SegmentVars == NA), replace with zero
for (y in PARK_SegmentVars){
  v <- paste(y, "1", sep = "_")
    IDs_badNights <- PARKbads[PARKbads[v]==1, "ID"]
      PARKsegments[match(IDs_badNights, PARKsegments$ID), y] <- 0
}

# For observations where local==NA, but zip is provided, change local to 
# local == 1 if zip matches local zipcodes or
# local == 0 if zip matches non-local zipcodes


PARK_localzip <- subset(PARKsegments, local == 1 & is.na(zip)==FALSE , select = zip)

PARK_nonlocalzip <- subset(PARKsegments, local == 0 & is.na(zip)==FALSE , select = zip)

IDs_local1 <- PARKbads[which(PARKbads$local_1 == 1), "ID"]  
          
          # save for testing/verification
          # IDs_local1 are observations where local == NA

    # NOTE: try using dplyr "count" function rather than plry

    PARK_localzip <-  count(PARK_localzip)
        colnames(PARK_localzip) <- c("LOCAL_ZIP", "FREQUENCY")
            n <- sum(PARK_localzip$FREQUENCY)
            
        LocalZip <- PARK_localzip[PARK_localzip$FREQUENCY/n > .05, "LOCAL_ZIP"]   
                # CHANGE WEIGHT AFTER TESTING 

    PARK_nonlocalzip <-  count(PARK_nonlocalzip)
        colnames(PARK_nonlocalzip) <- c("NON-LOCAL_ZIP", "FREQUENCY")
            m <- sum(PARK_nonlocalzip$FREQUENCY)
        
        nonLocalZip <- PARK_nonlocalzip[PARK_nonlocalzip$FREQUENCY/m > .0909, "NON-LOCAL_ZIP"]    
      
    ErrorZip <- intersect(LocalZip, nonLocalZip) 
          ErrorZip <- append(ErrorZip, c("DK"))  # we can add other values as needed
          
          # The any zip values that are in the intersections must be errors
    
        # PARK_localzip <- LocalZip[which(LocalZip != ErrorZip)]
        # PARK_nonlocalzip <- nonLocalZip[which(nonLocalZip != ErrorZip)]
                # NOTE: setdiff is the difference of set x net set y, not the union of 
                # x & y net the intersection
          
        PARK_localzip <- setdiff(LocalZip, ErrorZip)
        PARK_nonlocalzip <- setdiff(nonLocalZip, ErrorZip)
        
        
for (zips in PARK_localzip){
  PARKsegments[which(PARKsegments$zip == zips & is.na(PARKsegments$local)), "local"] <- 1
}
  
for (zips in PARK_nonlocalzip){
  PARKsegments[which(PARKsegments$zip == zips & is.na(PARKsegments$local)), "local"] <- 0
}
        
         # CHECK TO VERIFY THIS CODE IS WORKING

# Now re-run script 2 to produce a second BADS matrix which reflects the corrected data frame
PARKsem_COPY <- PARKsem

# Create a copy of original PARKsem for temporary use
PARKsem <- PARKsegments
    
        # NOTE: AT THIS POINT, WE COULD SAVE PARKsem TO THE OUTPUT FOLDER AND CLEAN THE DATA FRAME
        # IN MEMORY USING THE SAME NAME 

    source("2_Cleaning.R")
        PARKbads_cleaned <- Bads
        PARKsegments <- PARKsem
            rm(Bads)
            
# PARKsegments$sumBADS <- PARKbads$local_1 + PARKbads$overnight_1 + PARKbads$overnight_3
            #  local_1: local == NA
            # overnight_1: overnight == NA
            # overnight_3: 

            PARKsegments$sumBADS <- PARKbads_cleaned$local_1 +
                        PARKbads_cleaned$overnight_1 +
                        PARKbads_cleaned$overnight_3
    
# Create a vector of :"ID"'s which will be dropped, then subset the data by dropping bad observations
DropIDs <- PARKsegments[PARKsegments$sumBADS >=1, "ID"]

PARKsegments <- subset(PARKsegments, sumBADS == 0 , select = c(colnames(PARKsegments)))

PARKbads_cleaned <- PARKbads_cleaned[-c(DropIDs),]

              # # Create a vector of "ID"'s which cannot be identified as local | nonlocal
              # BADlocal_1 <- PARKbads[PARKbads$local_1 == 1 , "ID"]  
              # 
              # # Create a vector of "ID"'s which do not identify day | overnight trip
              # BADovernight_1 <- PARKbads[PARKbads$overnight_1 == 1 , "ID"]
              # 
              # # Create a vewctor of "ID"'s which have incomplete accommodation data
              #       # NOTE: maybe run this code later?  If respondent identified themselves as day/overnight, 
              #       # we could still use them for share day vs. overnight share?
              # 
              # BADovernight_3 <- PARKbads[PARKbads$overnight_3 == 1, "ID"]
              # 
              # # Compile the list of "ID"'s into a vector (union of the three vectors)
              # IDdrop <- as.factor(union(BADlocal_1, BADovernight_1))
              #     
              #     IDdrop <- as.factor(union(IDdrop, BADovernight_3))  # NOTE: union() can only be applied between 2 vectors 
              #  
              # PARKsegments <- subset(PARKsegments, sumBADS == 0 , select = colnames(PARKsegments))    
              #                                                        # at a time
              # # Drop the observations from PARKsegments 
              # PARKsegments <- PARKsegments[ -c(match(IDdrop, PARKsegments$ID)),]

              # NOTE: the match() function ensures that we are selecting observations by their unique ID value
              #       rather than their row/column index

# Reset the row numbers to a complete sequence from 1:n
          # NOTE: this may not be neccessary - factor variables maintain all associated charracteristics
          # row.names(PARKsegments) <- NULL   
          # row.names(PARKbads_cleaned) <- NULL

# PARKsegments is now our subsetted data set (from PARKsem) that can be used for segment analysis
      head(PARKsegments)  

# Clean up memory
      rm(DropIDs, ErrorZip, LocalZip, m, MATCHvars, nonLocalZip, PARK_localzip, PARK_nonlocalzip,
         zips)
###########################################################################################
#***************** Categorize Observations by Accomodation ********************************

# Add columns to PARKsegments to classify each observation
# Each column is initially a vector of zeros

      # PARKsegments$day_local <- 0

# SegmentVars <- c("nightsBackcountry", "nightsCampIn", "nightsCampOut",
#                  "nightsLodgeIn", "nightsLodgeOut", "nightsCruise", "nightsOther")    

# PARK_SegmentVars <- NULL

# for (y in SegmentVars){
#   if (exists(y, where = PARKsegments) == TRUE){
    
for (VAR in PARK_SegmentVars){
  
  # PARK_SegmentVars <- append(PARK_SegmentVars, y)   # vector of segments specific to PARK for use later
  
        v <- paste("ON", substring(VAR, 7), sep = "_")

      
        # v <- paste("ON", substring(y, 7), sep = "_")
        
        # v <- paste(substring(y,7),
        #            "nonlocal", sep = "_")   # This code reads:
            # take the SegmentVar and remove the first six letter (e.g. "nights")
            # then combine the remaining string with "_nonlocal" and store as variable "v"
        
        PARKsegments[,v] <- 0   # for each SegmentVar that is present in the PARKsegments data, create a column
                                # named "v" and fill it with zeros
  }
# }

# Identify local day trip and local overnight observations
PARKsegments$day_local <- ifelse(PARKsegments$local == 1 & PARKsegments$overnight == 0, 1, 0)
PARKsegments$day_nonlocal <- ifelse(PARKsegments$local == 0 & PARKsegments$overnight == 0, 1,0)

PARKsegments$overnight_local <- ifelse(PARKsegments$local == 1 & PARKsegments$overnight == 1, 1,0 )
PARKsegments$overnight_nonlocal <- ifelse(PARKsegments$local == 0 & PARKsegments$overnight == 1, 1 ,0)

# PARKsegments$day_local[as.integer(PARKsegments$local)==1 &
#                          as.numeric(PARKsegments$overnight)==0 ] <- 1
# 
# PARKsegments$overnight_local[as.integer(PARKsegments$local)==1 &
#                                as.numeric(PARKsegments$overnight)==1 ] <- 1


# Fill in zeros for overnights that only answered select accomodation categories (changing NA to zeros)
        # NOTE: The assumption being made here is that if the respondent identified themselves as overnight and 
        # identified at least one accomodation type for one or more nights, than any other accomodation types 
        # unanswered should be zero (replace NA with zero)

# THIS SHOULD BE MOVED TO RUN BEFORE SCRIPT 2 IS RUN AGAIN

# for (VAR in PARK_SegmentVars){
  # z <- paste(VAR, "1", sep = "_")
  # 
  #     IDfix <- PARKbads_cleaned[PARKbads_cleaned[,z]==1, "ID"]
  #       # IDfix <- PARKbads[PARKbads[,z]==1, "ID"]
  # 
  #         PARKsegments[match(IDfix, PARKsegments$ID), VAR] <- 0
# }   # FIX THIS NOT WORKING CORRECTLY

# Categorize all nonlocal repondants by accomodation type. Create a vector of "ID"'s which are overnight 
# but do not identify any type of accomodation type 

IDs_badNights <- as.character(NULL) # Will capture "ID"s with incomplete data

for (x in 1:length(PARKsegments$ID)){
  if (as.integer(PARKsegments$overnight[x]) == 1){  # Confine loop to only overnight
    

          maxNightsType <- colnames(PARKsegments[c(x),PARK_SegmentVars])[max.col(PARKsegments[c(x),PARK_SegmentVars],
                                                                                ties.method="first")]
                # maxNightsType: vector specifying the type of accomodation with the largest number of nights provided
                      # NOTE: maxNightsType = NA if any nights* category is NA
          
          # NOTE: ties.method = "first" returns the first value in the vector, problem when all accomodation 
          # types are equal (most likely all zeros)
          
          maxNights <- max(PARKsegments[c(x), PARK_SegmentVars])
          minNights <- min(PARKsegments[c(x), PARK_SegmentVars])
                # maxNights/minNights: vector specifying the largest/smallest integer value of nights given.
          
          if (is.na(maxNightsType) | # overnight == NA should be removed already (error if this is true)
              as.character(maxNights) == 0 |
              as.character(maxNights) == as.character(minNights)){ # collect "ID"'s for problematic observations 
                  IDs_badNights <- append(IDs_badNights, as.character(PARKsegments$ID[x])) 
          }
          
          else { # If the observation is clean, categorize it accordingly
              # v <- paste(substring(maxNightsType, 7), "nonlocal", sep = "_")
                v <- paste("ON", substring(maxNightsType, 7), sep = "_")
                  
                PARKsegments[x,v] <- 1
        }
    }
}


# PARKsegments <- PARKsegments[-c(match(IDs_badNights, PARKsegments$ID)),] # just drop these variables for the time being 
    # row.names(PARKsegments) <- NULL   # Reset row names

    
# THIS IS WHERE SCRIPT 2 SHOULD BE RERUN AND THE BADS MATRIX GENERATED
    
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


###########################################################################################
#***************** Generate Spending Profiles *********************************************
    


# write.csv(PARKsegments, "PARKsegments.csv", row.names = FALSE)

