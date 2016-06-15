# THis script will use a series of conditional statements to identify missing, incomplete, and outlying observations.
# 
# Use the script 1_Upload... to get the data
setwd("~/SEM_Project")
source("1_Load&Subset-CUVA.R")

# Clear the environment except for the data we want
rm(list=setdiff(ls(), "CUVAsem"))

# Will use "PARKsem" as general name for the dataset
PARKsem <- CUVAsem

###################################################################################################
# Upload the CHECKvars csv and store as "CHECKvars"
CHECKvars <- read.csv("~/SEM_Project/CHECKVars.csv")
   ### CHECKvars <- CHECKvars[,c("SEMvars","NumberChecks")]  #drop the notes column
   ###row.names(CHECKvars) <- CHECKvars$SEMvars

# Drop SEMvars which PARKsem does not contain
PARKvars <- colnames(PARKsem[which(colnames(PARKsem) != c("ID"))])
   ###PARKvars <- as.factor(PARKvars)

Matches <- NULL    
for (v in PARKvars){
  MM <-   grep( v, CHECKvars$SEMvars, ignore.case = TRUE, value = FALSE)
  Matches <- append(Matches, MM)
}

CHECKvars <- data.frame(CHECKvars[Matches,])
    # row.names(CHECKvars) <- NULL
    # head(CHECKvars)

# Create Data frame for taxonomy of bads
Bads <- data.frame(PARKsem[,c("ID")])
  colnames(Bads) <- c("ID")
  

 
CHECKS <- NULL  
for (y in 1:length(CHECKvars$SEMvars)){
  z <- CHECKvars[c(y),c("NumberChecks")]
  if (is.na(z) == "TRUE"){
    #do nothing, there is no check on this variable
  }
  else { #the number of checks will be the number of columns added
    CHECKS <- append(CHECKS, CHECKvars$SEMvars[y]) 
    for (n in 1:z){
      ck = paste( CHECKvars$SEMvars[y] , n, sep = "_") 
      Bads[, ck] <- 0
    }
  }
}

 
####################################################################################################

# Each column of Bads (except "ID") is a check

# LiveNearby; check for NA
check <- PARKsem[is.na(PARKsem$LiveNearby),"ID"]
    Bads$LiveNearby_1[c(check)] <- 1

# Overnight_1; check for NA
check <- PARKsem[is.na(PARKsem$Overnight),"ID"]
    Bads$Overnight_1[c(check)] <- 1
    

#***************************************************************************************************
# a_Zip:Factor variable with 306 levels
# The as.numeric function forces the factors into numbers with range 1:number of levels
    # For a_Zip, as.numeric identifies each a_Zip as a number 1:306
        levels(PARKsem$a_Zip)   
        
# Examining levels we have:
        # 1 = " " (Null or blank entries)
        # 301:306 = All countries and non-zip codes
        
# a_Zip_1; Identify NULL (blank zip codes)
check <- PARKsem[as.numeric(PARKsem$a_Zip) == 1 ,"ID"]
      Bads$a_Zip_1[c(check)] <- 1
        
        
# a_Zip_2; Identify Countries;    FOR CUVA 301:306 -- WILL NEED TO AUTOMATE OR SPECIFY THESE VALUES FOR EACH PARK
check <- PARKsem[as.numeric(PARKsem$a_Zip) == 301 |
                   as.numeric(PARKsem$a_Zip) == 302 | 
                   as.numeric(PARKsem$a_Zip) == 303 |
                   as.numeric(PARKsem$a_Zip) == 304 |
                   as.numeric(PARKsem$a_Zip) == 305 |
                   as.numeric(PARKsem$a_Zip) == 306 ,"ID"]    # "|" is the "or" syntax

      Bads$a_Zip_2[c(check)] <- 1

# a_Zip_3: Identify incomplete zip codes    ******NOT WORKING
    # check <- PARKsem[length(PARKsem$a_Zip) < 5, "ID"]
    #       Bads$a_Zip_3[c(check)] <- 1
    

#***************************************************************************************************
# Check for NA's in expenditure categories:
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
ExpVars <- c("GasCleaned", "RentalCar","PublicTransportation", "Resturaunts", "Snacks", 
             "Groceries", "Hotels", "EquipRental", "Recreation", "Souvenirs",
             "Guide", "Railroad", "Tours", "OtherExpen", "AdditionalExpen")

# Loop through the variables in ExpVars
# If the given expenditure variable exists within PARKsem then check for NA's within that column
# Save the ID which corresponds to the NA and for that ID number, save a "1" in the Bads data frame
# under the column named "ExpVars_1" (where ExpVars is the current variable name in the loop)
for (y in ExpVars){
  if (exists(y, where = PARKsem) == TRUE){
    
    check <- PARKsem[is.na(PARKsem[c(y)]),"ID"]
    
    v = paste(y , "1", sep = "_")
    Bads[c(check), c(v)] <- 1
  }
}

#***************************************************************************************************
# Check for NA's in Segment categories:
# Overnight_1: Check for NA
# CampingBackcountry_1: Check for NA
# CampinginPark_1; Check for NA
# CampingOutPark_1: Check for NA
# LodgingInPark_1: Check for NA
# LodgingOutPark_1: Check for NA
# HoursinPark_1: Check for NA

SegmentVars <- c("Overnight", "CampingBackcountry", "CampingInPark",
                 "CampingOutPark", "LodgingInPark", "LodgingOutPark", "HoursinPark")    

for (y in SegmentVars){
  if (exists(y, where = PARKsem) == TRUE){
    
    check <- PARKsem[is.na(PARKsem[c(y)]),"ID"]
    
    v = paste(y , "1", sep = "_")
    Bads[c(check), c(v)] <- 1
  }
}


#***************************************************************************************************
# a_ReEnter_1: check for a_ReEnter == NA & DKReEnter == NA
# These respondents refused to answer the question
check <- PARKsem[is.na(PARKsem$a_ReEnter) &
                   is.na(PARKsem$DKReEnter) ,"ID"]    

Bads$a_ReEnter_1[c(check)] <- 1


# a_ReEnter_2: check for ReEnter >= 1 and DKReEnter == 99
# These respondents provided an estimate of entries, but also noted that they "did not know"
check <- PARKsem[as.numeric(PARKsem$a_ReEnter)>=1 &
                   as.integer(PARKsem$DKReEnter)==99 ,"ID"]
check <- na.omit(check)
Bads$a_ReEnter_2[c(check)] <- 1


# DKReEnter_1: 
check <- PARKsem[is.na(PARKsem$a_ReEnter) &
                   as.integer(PARKsem$DKReEnter)==99 ,"ID"]    
check <- na.omit(check)

Bads$DKReEnter_1[c(check)] <- 1

# HoursinPark; check for NA
check <- PARKsem[is.na(PARKsem$HoursinPark) &
                   is.na(PARKsem$DaysinPark) ,"ID"]    
Bads$HoursinPark_1[c(check)] <- 1


# DaysinPark_1: Check for outliers (DaysinPark > 14)
check <- PARKsem[as.numeric(PARKsem$DaysinPark)> 14,"ID"]    
check <- na.omit(check)

Bads$DaysinPark_1[c(check)] <- 1


#***************************************************************************************************
# Append column for summation of checks for each ID
Bads$Sum <- rowsum(Bads[which(colnames(Bads) != c("ID"))], "ID")
      
    
    
    
  
  

