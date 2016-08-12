# INTRODUCTION --------------------------------------------------------------------------------

# This script is the starting point for the SEM analysis using R. Read through this script
# for explanations and comments on how the script is running and what assumptions are being
# made to clean the data and produce the desired parameters.  

# The script follows a logical path from uploading raw data, renaming variables, identifying
# observations that must be cleaned or dropped, cleaning the entire data set and producing 
# the variables neccessary for the SEM analysis.

# This main script provides sufficient explanation of the steps taken and functions used but
# is intentionally concise.  For additional information, see each script being sourced.

# If you are unfamiliar with the R language, this help file may be useful before reading through
# the scripts

################################################################################################

# Before the script below is run, data sets must have been QC'd to meet minimum requirements
# found in the NEWDATAworkflow document.  It is essential that ALL DATA in the "Data" folder 
# have undergone this minimum level of "cleaning" for the following SEM analysis to work 
# correctly. See the NEWDATAworkflow document for additional information.

################################################################################################
# SECTION I - Upload data sets, select a PARK, subset and assign SEM variable names ------------

# Clear all data in memory 
rm(list=union(ls(), ls()))

# Source the functions needed and load the neccessary packages for the following analysis. 
# First, install packages if they have not been installed on local hard drive. 
source("~/SEM_Project/Script/changeAlpha.R")
source("~/SEM_Project/Script/wrapLABELS.R")
source("~/SEM_Project/Script/loadPACKAGES.R")

# Set working directory
setwd("~/SEM_Project")

# Select which park to run analysis on and remove others from memory
# #     NOTE: this option is currently restricted to selecting a single PARK

source("~/SEM_Project/Script/selectPARK.R")
      
# Upload the "SEMvars.csv" to memory and store as a data.frame named SEMvars
SEMvars <- read.csv("~/SEM_Project/SEMvars.csv")

# Subset PARKdata and assign the SEM variable names for consistancy. After the "getSEMvars.R"
# script is run, the data frame "PARKsem" is the data to be used throughout the remaining 
# analysis.  
source("~/SEM_Project/Script/getSEMvars.R")

# At this point, "PARKsem" is a subsetted version of the raw data from PARKdata.  The variables
# within "PARKsem" have been re-named to the corresponding SEMvars listed in SEMvars.csv.

# Clean up memory
rm(i, Packages, park, PKG, rowINDEX, VAR, WD, x)

###########################################################################################
# SECTION II: Identifying Bad Data Points -------------------------------------------------

# Upload the GROUPvars and create vectors of each variable category. Keep only those variables
# that are specific to the current park.
source("~/SEM_Project/Script/getGROUPvars.R")

# Using the groups of variables (SEGMENT, EXP, TRIP, PARTY, etc.), identify observations
# which are incomplete but may be fixable, incomplete and non-fixable, or complete 
# but with eronious data. First generate a temporary data.frame "tempDF" which is a copy 
# of PARKsem (no changes are initially made to PARKsem data). tempDF will be updated as
# observations are adjusted in PARKsem
tempDF <- PARKsem

# *****************************************************************************************
# entries 

# For observations where entries is greater than DaysinPark, set entries = DaysinPark. 
# The assumption here is that the respondant misinterpreted the question or gave an 
# answer which cannot be true. THese observations are identified in PARKbads_seg$entries_3
BADentryID <- na.omit(tempDF[tempDF$entries > tempDF$daysPark, "ID"])

for(i in 1:nrow(PARKsem)){
PARKsem$entries[i] <- ifelse(PARKsem$entries[i] > PARKsem$daysPark[i], PARKsem$daysPark[i], PARKsem$entries[i])
}

# *****************************************************************************************
# overnight 

# For the "overnight" variable:
#       overnight == 1 if respondant stayed in local area overnight
#       overnight == 0 if respondant did not stay overnight in local area
#       overnight == NA if respondant refused to answer the question

# For observations where overnight == NA, but respondant answered that they stayed 
# x number of nights in at least one of the overnight segment variables, we can assign him as
# an overnight visitor. (ex. overnight == NA, but nightsLodgeOut == 2, implying the respondant
# stayed 2 nights in the local area),

# Add a column to tempDF named NIGHTsum, which is the sum accross the overnight accomodation
# types.
tempDF$NIGHTsum <- rowSums(tempDF[PARK_SegmentVars], na.rm = TRUE)

# For observations where overnight == NA and NIGHTsum > 0 , change overnight to 1
PARKsem["overnight"][is.na(tempDF["overnight"]) & tempDF$NIGHTsum > 0] <- 1

# For observations where overnight == NA and NIGHTsum = 0 , change overnight to 0
PARKsem["overnight"][is.na(tempDF["overnight"]) & tempDF$NIGHTsum == 0] <- 0

# For observations where overnight == 1, but one or more of the accomodation 
# types is NA, replace with zero. Here, the assumption is that if the respondant identified 
# herself as staying overnight and stayed x nights in at least one of the accomodation types
# but refused to enter a number for the other accomodations, she did not stay in those options 
# and the number of nights is zero.

# Loop over the segment variables for the PARK, fill in any NA's with zeros for overnight parties
for (VAR in PARK_SegmentVars){
  PARKsem[VAR][is.na(PARKsem[VAR])] <- 0
}  

# *****************************************************************************************
# local

# For the "local" variable
#       local = 1 if respondant lives within local region
#       local = 0 if respondant live outside local region
#       local = NA if respondant refused to answer the question

# For observations where local = NA, but respondant provided a zip code, he can be identified as 
# local or non-local visitor by their reported zip code. The assumption here is that if the respondant
# provided a zip code that matches a zip code that a high percentatge of local/nonlocal repondants 
# answered, than that individual is local/nonlocal. 

# First, replace observations where zip == " " (i.e. zip is blank, but not NA), replace with NA.
PARKsem$zip[c(PARKsem[which(PARKsem$zip == " "), "ID"])] <- NA

# For observations where local==NA, but zip is provided, change local to 
#     local == 1 if zip matches local zipcodes or
#     local == 0 if zip matches non-local zipcodes
# See "clean_Local-Zip.R" for additional information.
source("~/SEM_Project/Script/cleanLOCAL.R")
    
# *****************************************************************************************

# Generate the variable "nightsLocalArea" which is the sum of nights across accomodation
# types. This is used in the "genBADS.R" script and the following sections.
PARKsem$nightsLocalArea <- rowSums(PARKsem[PARK_SegmentVars], na.rm = TRUE)

# *****************************************************************************************

# *****************************************************************************************

# NOTE: any additional cleaning should go here
    
# *****************************************************************************************


# The data.frame "PARKsem" is now "cleaned", based on the modest assumptions stated above.
# Next, the script will generate a matrix with the same dimensions (rows, soreted on ID) as PARKsem named
# "PARKbads". Each column of "PARKbads" represents a conditional check on one variable (column)
# in PARKsem.  For each column/check in PARKbads: 
#       0 = PASS (the observation passes the specific test)
#       1 = FAIL (the observation fails the specific test)
#             See "genBADS.R" for additional information

source("~/SEM_Project/Script/genBADS.R")
    
# Store the "bads" data.frame produced in "genBADS.R" as "PARKbads".
PARKbads <- Bads

# *****************************************************************************************
# Identify outliers in the data. See the "cleanOUTLIERS.R" script for full analysis
# and identification. 
# NOTE:  the code in "cleanOUTLIERS.R" has the majority of output supressed using 
# hashtags (comment chunks).  It is not advisable to source the script without 
# supressing output.

source("~/SEM_Project/Script/cleanOUTLIERS.R")

# Outlying and contaminant data is now identified in PARKbads$Outliers
# 1 = Outlier
# 0 o.w.

###########################################################################################
# SECTION III: Subset Data Frame - Drop Bad Data ------------------------------------------

# Create a copy of the original PARKsem data frame named "PARKsegments".  
# The data.frame "PARKsem" should remain unchanged from this point forward (it has been 
# cleaned, but no observations have been dropped). Any additional subsetting/modifying of 
# data should come from "PARKsegments".

# Create PARKsegments data frame from PARKsem data (copy of PARKsem data) 
PARKsegments <- PARKsem

# Create a column "sumBADSsegments" and attach it to "PARKsegments".  This new variable 
# refera to the checks performed in "genBADS.R" and suma across the specific columns.

# The variables in PARKbads (and their respective conditional check) needed to identify 
# party segments for each observation are:
#       local_1: 
#           local_1 = 1 if local == NA
#       overnight_1: 
#           overnight_1 = 1 if overnight == NA
#       overnight_3: 
#           overnight_3 = 1 if overnight == 1 & nightsLocalArea == 0
           
# If "sumBADSsegments" is greater than zero, the observation does not meet the minimum
# criteria to be used in the SEM analysis (see "genBADS.R" for additional information on 
# the conditional checks). 
PARKsegments$sumBADSsegments <- PARKbads$local_1 +
                                PARKbads$overnight_1  +
                                PARKbads$overnight_3 +
                                PARKbads$Outliers
    
# Subset PARKsegments by dropping observations where sumBADSsegments >= 1. 
PARKsegments <- subset(PARKsegments, sumBADSsegments == 0)

# Store a copy of PARKbads as PARKbads_seg. This new data.frame will contain only those 
# observations as those in PARKsegments (i.e. drop the same observations). 
PARKbads_seg <- subset(PARKbads, (local_1 + overnight_1 + overnight_3 + Outliers)==0)

# Notify user of number of observations dropped
nDROP <- nrow(PARKsem)-nrow(PARKsegments)
if(nDROP > 0){print(nDROP)
  message("Observations have been removed from the data set")}

###########################################################################################
# SECTION IV: Identify Party Segments, Shares, & Trip Purpose Scal ------------------------

# Using the data.frame PARKsegments, categorize each party (observation) by segment type.
# The script below will generate a column in PARKsegments for each segment type. Each
# observation will recieve a 1 for its respective segment, and 0 otherwise. For example,
# segment type day_Local  = 1 for observations where overnight == 0 & local == 1.
#       See the "getSEGMENTS.R" for additional information. 

# Generate a vector of the PARK's segment variables.  This can be used to subset or 
# reference these variables in the analysis scripts listed below.
SEGvars_day <- c("day_local", "day_nonlocal")
SEGvars_on <- paste("ON", substring(PARK_SegmentVars, 7), sep = "_")            

# Combine both vectors and store as "SEGvars"
SEGvars <- c(SEGvars_day,SEGvars_on)
       
#     NOTE:
#     If the script produces a warning message, read the comments 
#     found within "IdentifySegments.R" script.  

source("~/SEM_Project/Script/getSEGMENTS.R")

# *****************************************************************************************
#***** Generate Segment Shares Table ******************************************************
# *****************************************************************************************

source("~/SEM_Project/Script/genSEGMENT_shares.R") 

# *****************************************************************************************
#***** Get Trip Purpose Scalers ***********************************************************
# *****************************************************************************************

source("~/SEM_Project/Script/getPRIMARYscalers.R")

# At this point, if the sample sizes per segment are sufficient, then continue with the 
# script below. However, if one or more of the sample sizes per segment is not sufficiently
# large (generally n<30 observations is considered a "small" sample), skip to run the
# "getSEGMENTS_LUMPED.R" script.

###########################################################################################
# SECTION V: Generate Parameters for SEM --------------------------------------------------

# The script below will generate the neccessary parameters by sourcing additional scripts.
# For explanation on how these scripts run and how the parameters and tables are 
# generated, see the comments within the script being soucred.

# *****************************************************************************************

# To address the problem of small sample size in visitor segment categories, we will 
# consolidate (a.k.a. "lump") together small segments.  The methodology of which segments
# are lumped together is explained in the "getSEGMENTS_lumped.R" script.

# NOTE: Since the output tables have been stored as .csv files, the data.frames and 
# objects in memory will be overwritten using the lumped segments

# In the script "getSEGMENTS_lumped.R", the vector SEGvars_LUMPED will be subsetted and
# appended to reflect the new lumped segments. 

# source("~/SEM_Project/Script/getSEGMENTS_lumped.R")

# To Re-Run the scripts which generate the shares and means tables, our lumped 
# segment variables must be named "SEGvars" to match existing code. Save a copy of
# SEGvars_LUMPED as SEGvars. Then re-run scripts.
Choice.SEG <- dlgMessage(c("Do you want to 'lump' segments together?"), "yesno",
               title = "Lump Segments")$res

if (Choice.SEG == "no") {
  message("Segments have not been lumped\n")
} else {
  source("~/SEM_Project/Script/getSEGMENTS_lumped.R")
  source("~/SEM_Project/Script/genSEGMENT_shares.R") 
  message("Segments are now lumped\n")
}

# *****************************************************************************************
#***** Generate Length of Stay Means Table ************************************************
# *****************************************************************************************

source("~/SEM_Project/Script/genLENGTH_means.R") 

# *****************************************************************************************
#***** Generate Party Size Means Table ****************************************************
# *****************************************************************************************

source("~/SEM_Project/Script/genPARTY_means.R") 

# *****************************************************************************************
#***** Generate ReEntry Means Table *******************************************************
# *****************************************************************************************

source("~/SEM_Project/Script/genReENTRY_means.R") 

# *****************************************************************************************
#***** Generate Party Expenditure Means Table *********************************************
# *****************************************************************************************

source("~/SEM_Project/Script/genSPENDING_means.R") 

###########################################################################################
# SECTION VI: Generate Plots & Report Tables ----------------------------------------------

source("~/SEM_Project/Script/genREPORT_tables.R") 

Choice.PLOTS <- dlgMessage(c("Do you want to generate PARK plots?"), "yesno",
               title = "Write Tables")$res

if (Choice.PLOTS == "no") {
  message("Plots have not been generated\n")
} else {
  message("Plots have been generated and saved to Output/PARK/Plots folder\n")
  source("~/SEM_Project/Script/genPLOTS.R") 
}

###########################################################################################
# SECTION VII: Write Data in Memory to Local Drive ---------------------------------------

# The script below produces a popup prompt asking user if they would like to write the 
# data in memory to .csv files

Choice.WRITE <- dlgMessage(c("Do you want to write tables to .csv files?"), "yesno",
               title = "Write Tables")$res

if (Choice.WRITE == "no") {
  message("Tables and data.frames will remain in memory\n")
} else {
  message("Tables and data.frames have been saved to local drive\n")
  source("~/SEM_Project/Script/writeTABLES.R") 
}

###########################################################################################
# END :)













