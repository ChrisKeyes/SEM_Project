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
    CHECKvars <- CHECKvars[,c("SEMvars","NumberChecks")]  #drop the notes column
      row.names(CHECKvars) <- CHECKvars$SEMvars

# Drop SEMvars which PARKsem does not contain
PARKvars <- colnames(PARKsem[which(colnames(PARKsem) != c("ID"))])
    PARKvars <- as.factor(PARKvars)
    
CHECKvars <- CHECKvars[c(PARKvars),] 
    # row.names(CHECKvars) <- NULL


# Create Data frame for taxonomy of bads
Bads <- data.frame(PARKsem[,c("ID")])
  colnames(Bads) <- c("ID")
  

# y <- "HoursinPark"
CHECKS <- NULL  
  for (y in CHECKvars$SEMvars){
    z <- CHECKvars[c(y),c("NumberChecks")]
      if (is.na(z) == "TRUE"){
        #do nothing, there is no check on this variable
      }
      else { #the number of checks will be the number of columns added
        CHECKS <- append(CHECKS, y) 
         for (n in 1:z){
            ck = paste( y , n, sep = "_") 
            Bads[, ck] <- 0
          }
      }
  }
  
####################################################################################################

# Each column of Bads (except "ID") is a check

# HoursinPark; check for NA
check <- PARKsem[is.na(PARKsem$HoursinPark),"ID"]
    Bads$HoursinPark_1[c(check)] <- 1

# LiveNearby; check for NA
check <- PARKsem[is.na(PARKsem$LiveNearby),"ID"]
    Bads$LiveNearby_1[c(check)] <- 1

# Overnight_1; check for NA
check <- PARKsem[is.na(PARKsem$Overnight),"ID"]
    Bads$Overnight_1[c(check)] <- 1
    

# DaysinPark_1; check for DaysinPark > 14   (Outlier test)
    
    
# a_Zip_1; check for NULL
    
    
# a_Zip_2; check for string length < 5 charracters (incomplete zip code)
    
    
# a_Zip_3; check for non-numeric (international) 
    
    
    
    
    
  
  

