# THis script will use a series of conditional statements to identify missing, incomplete, and outlying observations.
# 
# Use the script 1_Upload... to get the data
setwd("~/SEM_Project")

# source("1_Load&Subset-CUVA.R")
source("1_Load&Subset-FIXED.R")


# Clear the environment except for the data we want
rm(list=setdiff(ls(), c("CUVAsem","PARKsem")))  # NOTE: once script is finalized, delete this line 

# Will use "PARKsem" as general name for the dataset
PARKsem <- CUVAsem

###################################################################################################
# Upload the CHECKvars csv and store as "CHECKvars"
CHECKvars <- read.csv("~/SEM_Project/CHECKVars.csv")

# Drop SEMvars which PARKsem does not contain. First create a vector of the variable names which
# the park has (do not include "ID")
PARKvars <- colnames(PARKsem[which(colnames(PARKsem) != c("ID"))])

# Comparing the PARKvars to the SEMvars within CHECKvars, find those that match and store the 
# number of checks that correspond to those variables
Matches <- NULL    
for (v in PARKvars){
  MM <-   grep( v, CHECKvars$SEMvars, ignore.case = TRUE, value = FALSE)
  Matches <- append(Matches, MM)
}

# Drop the variables from CHECKvars that do not match those in PARKsem
CHECKvars <- data.frame(CHECKvars[Matches,])


# Create Data frame for taxonomy of bads.
# First, create a data frame named Bads using the "ID" variable from PARKsem
Bads <- data.frame(PARKsem[,c("ID")])
  colnames(Bads) <- c("ID")
  
# Using the matched variables in CHECKvar, create a column for each number of checks listed under
# "NumberChecks".  The name of the column is SEMvar_#

for (y in 1:length(CHECKvars$SEMvars)){
  z <- CHECKvars[c(y),c("NumberChecks")]
  if (is.na(z) == "TRUE"){
    #do nothing, there is no check on this variable
  }
  else { #the number of checks will be the number of columns added
    for (n in 1:z){
      ck = paste( CHECKvars$SEMvars[y] , n, sep = "_") 
      Bads[, ck] <- 0
    }
  }
}

# Each column of Bads is a check to be performed on that variable, and specific to each "ID"

####################################################################################################
# PERFORM CHECKS ON SEM VARIABLES
################## Location variable checks ########################################################

# Local_1; check for NA
check <- PARKsem[is.na(PARKsem$local),"ID"]
    Bads$local_1[c(check)] <- 1


# zip:Factor variable with 306 levels
# The "as.numeric" function forces the factors into numbers with range 1:number of levels
# For zip, as.numeric identifies each a_Zip as a number 1:306; "levels()" lists levels
levels(PARKsem$zip)   
        
# Examining levels we have:
        # 1 = " " (Null or blank entries)
        # 301:306 = All countries and non-zip codes
        
# zip_1; Identify NULL (blank zip codes)      ****NOTE: CUVA DID NOT HAVE "NA" FOR NULL, BUT ACTUAL " " (BLANKS)
check <- PARKsem[as.numeric(PARKsem$zip) == 1 ,"ID"]
      Bads$zip_1[c(check)] <- 1
        
        
# a_Zip_2; Identify Countries;    FOR CUVA 301:306 -- WILL NEED TO AUTOMATE OR SPECIFY THESE VALUES FOR EACH PARK
check <- PARKsem[as.numeric(PARKsem$zip) == 301 |
                   as.numeric(PARKsem$zip) == 302 | 
                   as.numeric(PARKsem$zip) == 303 |
                   as.numeric(PARKsem$zip) == 304 |
                   as.numeric(PARKsem$zip) == 305 |
                   as.numeric(PARKsem$zip) == 306 ,"ID"]    # "|" is the "or" syntax

      Bads$zip_2[c(check)] <- 1

# zip_3: Identify incomplete zip codes  #####still working on this
# IncompleteZip <- NULL      
# for (z in PARKsem$zip){
#   
#   if (PARKsem[is.na(as.numeric(PARKsem$zip[z])) == TRUE]){
#     #do nothing
#   }
#   else if (PARKsem[as.numeric(PARKsem$zip) > 9999 &
#                    as.numeric(PARKsem$zip) < 100000]){
#     #do nothing
#   }
#   else {
#     check <- PARKsem[as.numeric(PARKsem$zip)<=9999 |
#                        as.numeric(PARKsem$zip)>99999, "ID"]  
#     Bads$zip_3[c(check)] <- 1
#     
#   }
# }    

#***************************************************************************************************
####################### Check for NA's in expenditure categories ###################################
#     GasCleaned
#     RentalCar
#     PublicTransportation
#     Resturaunts
#     Snacks
#     Groceries
#     Hotels
#     EquipmentRental
#     SpecialtyLodge
#     Camping
#     Recreation
#     Souvenirs
#     Guide
#     Railroad
#     Tours
#     OtherExp
#     AdditionalExpen

# Create vector of expenditure variable names
ExpVars <- c("expGas", "expRentalCar","expPubTrans", "expRestaurants", "expSnacks", 
             "expGroceries", "expHotels", "expEqpRental", "expSpecLodge", "expcamp", "expRec", 
             "expSouvenirs", "expGuides", "expRail", "expTours", "expOther",
             "expAdditional")

# Loop through the variables in ExpVars
# If the given expenditure variable exists within PARKsem then check for NA's within that column
# Save the ID which corresponds to the NA and for that ID number, save a "1" in the Bads data frame
# under the column named "ExpVars_1" (where ExpVars is the current variable name in the loop)
for (y in ExpVars){
  if (exists(y, where = PARKsem) == TRUE & any(grepl(paste("^", y, sep =""), colnames(Bads))) == TRUE){
    
    check <- PARKsem[is.na(PARKsem[c(y)]),"ID"]
    
    v = paste(y , "1", sep = "_")
    Bads[c(check), c(v)] <- 1
  }
}
      
#***************************************************************************************************
####################### Check for group type and expenditures vars #################################
# adultsCovered
# childrenCovered
      
# Create vector of group type variables
GroupVars <- c("adultsCovered", "childrenCovered")

# Loop over elements of GroupVars and if they are present in PARKsem, check for blank observations     
for (y in GroupVars){
  if (exists(y, where = PARKsem) == TRUE & any(grepl(paste("^", y, sep =""), colnames(Bads))) == TRUE){
        
    check <- PARKsem[is.na(PARKsem[c(y)]),"ID"]
        check <- na.omit(check)
      
    v = paste(y , "1", sep = "_")
    Bads[c(check), c(v)] <- 1
  }
}
      
#***************************************************************************************************
####################### Check for NA's in Segment categories #######################################
# Overnight_1: Check for NA
# CampingBackcountry_1: Check for NA
# CampinginPark_1; Check for NA
# CampingOutPark_1: Check for NA
# LodgingInPark_1: Check for NA
# LodgingOutPark_1: Check for NA
# HoursinPark_1: Check for NA

SegmentVars <- c("overnight", "nightsBackcountry", "nightsCampin", "nightsCampOut",
                 "nightsLodgeIn", "nightsLodgeOut", "nightsCruise", "nightsOther")    

for (y in SegmentVars){
  if (exists(y, where = PARKsem) == TRUE & any(grepl(paste("^", y, sep =""), colnames(Bads))) == TRUE){
    
    check <- PARKsem[is.na(PARKsem[c(y)]),"ID"]

    v = paste(y , "1", sep = "_")
    Bads[c(check), c(v)] <- 1
  }
}

# *** nightsOther not working -- need to fix ***
      
#***************************************************************************************************
###################### Check the ReEnter question ##################################################
# entries_1: check for entries == NA & DKentries == NA
# These respondents refused to answer the question
check <- PARKsem[is.na(PARKsem$entries) &
                   is.na(PARKsem$DKentries) ,"ID"]    

Bads$entries_1[c(check)] <- 1


# entries_2: check for ReEnter >= 1 and DKReEnter == 99
# These respondents provided an estimate of entries, but also noted that they "did not know"
check <- PARKsem[as.numeric(PARKsem$entries)>=1 &
                   as.integer(PARKsem$DKentries)==99 ,"ID"]
    check <- na.omit(check)

Bads$entries_2[c(check)] <- 1


# DKentries_1: check if respondent only answered "Dont Know"
check <- PARKsem[is.na(PARKsem$entries) &
                   as.integer(PARKsem$DKentries)==99 ,"ID"]    
    check <- na.omit(check)

Bads$DKentries_1[c(check)] <- 1


#***************************************************************************************************
##################### Other checks #################################################################
# hoursPark_1; check for NA
check <- PARKsem[is.na(PARKsem$hoursPark) &
                   is.na(PARKsem$daysPark) ,"ID"]    
Bads$hoursPark_1[c(check)] <- 1


# daysPark_1: Check for outliers (daysPark > 14)
# NOTE: this was specific to CUVA, what sort of check should be use among all parks?
check <- PARKsem[as.numeric(PARKsem$daysPark)> 14,"ID"]    
    check <- na.omit(check)

Bads$daysPark_1[c(check)] <- 1


#***************************************************************************************************
# Append column for summation of checks for each ID
Bads$Sum <- 0
Bads$Sum <- rowSums(Bads[,c(which(colnames(Bads) != c("ID")))])    

check <- Bads[is.na(Bads$Sum) ,"ID"]    
    Bads$Sum[c(check)] <- as.numeric(0)

# Store Bads data frame as PARKbads
PARKbads <- Bads 

CUVAbads <- PARKbads

# Remove Bads dataframe
rm(Bads)
  
  

