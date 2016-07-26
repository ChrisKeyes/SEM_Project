# This script will use the PARKbads matrix to generate 
# variables for segment analysis.

# Clear all data in memory
rm(list=union(ls(), ls()))

# Set working directory
setwd("~/SEM_Project")

# Use the preceeding scripts to get the data
source("1_Load&Subset.R")

      # PARKsem <- TESTdata  # Use for testing purposes
      
# Upload the GROUPvars and create vectors of each variable category. Keep only those variables
# that are specific to the current park.
source("~/SEM_Project/Script/getGroupVars.R")
    source("~/SEM_Project/Script/getEXPvars.R")
      
# Using the groups of variables (SEGMENT, EXP, TRIP, PARTY, etc.), identify observations
# which are incomplete but may be repairable, incomplete and not repairable, or complete 
# but with eronious data.  Generate a matrix named "PARKbads" which has n rows (same number
# of rows as PARKsem data), and m columns (where m = number of checks performed). Each check
# produces a boolean response where:
#         0 = PASS check
#         1 = FAIL check
# See CHECKvars.csv for additional information.

source("2_Cleaning.R")

      PARKbads <- Bads  # Use for testing purposes

# Clean up memory by removing unused objects 
rm(Bads)
    rm(CUVAdata,GATEdata,YOSEdata, PARKdata)  # Remove all extra data and keep TESTdata
      
###########################################################################################
#***************** Clean, Complete/Fix, and Subset Data ***********************************

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

PARKsem = PARKsegments   # This is temporary solution due to name overlap
    
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
      rm(DropIDs, ErrorZip, LocalZip, m, nonLocalZip, PARK_localzip,
         PARK_nonlocalzip, VAR, zips,
         IDs_local1, IDs_ZIP2, IDs_ON2, IDs_SEG)

      
###########################################################################################
#***************** Categorize Observations by Accomodation ********************************

source("~/SEM_Project/Script/IdentifySegments.R")
      # NOTE:  If the script stops and produces a warning message, read the comments 
      # found within "IdentifySegments.R" script.  
      

###########################################################################################
#***************** Generate Segment Shares ************************************************
   
source("~/SEM_Project/Script/GenSegmentShares.R") 

# View the shares table produced:
      
     # View(PARKshares_table)
      
