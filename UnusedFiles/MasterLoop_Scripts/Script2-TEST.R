# THis script will use a series of conditional statements to identify missing, incomplete, and outlying observations.
# 
# Use the script 1_Upload... to get the data
setwd("~/SEM_Project")


###################################################################################################
# Upload the CHECKvars csv and store as "CHECKvars"
CHECKvars <- read.csv("~/SEM_Project/CHECKVars.csv", header = TRUE)

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
  rm(MM, v) #Remove extraneous variables

# Drop the variables from CHECKvars that do not match those in PARKsem
CHECKvars <- data.frame(CHECKvars[Matches,])

# Create Data frame for matrix of bads.
# First, create a data frame named Bads using the "ID" variable from PARKsem

Bads <- data.frame(PARKsem$ID, row.names = NULL)
  colnames(Bads) <- "ID"
  Bads$ID <- sort(Bads$ID, decreasing = FALSE)

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
    rm(n,y,z,ck) #Remove extraneous variables

# Each column of Bads is a check to be performed on that variable, and specific to each "ID"

####################################################################################################
# PERFORM CHECKS ON SEM VARIABLES
################## Location variable checks ########################################################
# Local:
# Local_1; check for NA
check <- PARKsem[is.na(PARKsem$local),"ID"]
    Bads$local_1[c(check)] <- 1

    # zip:
    # Factor variable with 306 levels
    # The "as.numeric" function forces the factors into numbers with range 1:number of levels
    # For zip, as.numeric identifies each a_Zip as a number 1:306; "levels()" lists levels
    # levels(PARKsem$zip)   
    
    # zip_1; Identify NULL (blank zip codes)     
    check <- PARKsem[is.na(PARKsem$zip),"ID"]
    Bads$zip_1[c(check)] <- 1
    
    
    # zip_2: Identify blank zip codes (e.g. zip == " ", rather than zip == NA)     
    check <- PARKsem[which(PARKsem$zip == " "), "ID"]
    Bads$zip_2[c(check)] <- 1   
    
#***************************************************************************************************
####################### Check for NA's in expenditure categories ###################################

# Loop through the variables in ExpVars
# If the given expenditure variable exists within PARKsem, and has a check to be performed
# check for NA's within that column.
# Save the ID which corresponds to the NA and for that ID number, save a "1" in the Bads data frame
# under the column named "ExpVars_1" (where ExpVars is the current variable name in the loop)
    for (VAR in PARK_ExpVars){
      check <- PARKsem[is.na(PARKsem[c(VAR)]),"ID"]
      v <- paste(VAR , "1", sep = "_")
      Bads[c(check), c(v)] <- 1
    }
    
#***************************************************************************************************
####################### Check for group type and expenditures vars #################################
#     adultsCovered
#     childrenCovered
      
# Create vector of group type variables
      # NOTE: The vector below is CASE SENSITIVE
GroupVars <- c("adultsCovered", "childrenCovered")

# Loop over elements of GroupVars and if they are present in PARKsem check for NULL observations     
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

# Loop over elements of SegmentVars and if they are present in PARKsem check for NULL observations when 
# overnight == 1. This check will identify overnight segment variables which are incomplete, but can be 
# filled in with zeros.
      

    for (VAR in PARK_SegmentVars){
      
      check <- na.omit(PARKsem[with(PARKsem, is.na(overnight) & is.na(PARKsem[VAR])), "ID"])
      
      v <- paste(VAR , "1", sep = "_")
      
      Bads[match(check, PARKsem$ID), c(v) ] <- 1
    }
    

# overnight:
    
    PARKsem$nightsLocalArea <- rowSums(PARKsem[PARK_SegmentVars], na.rm=TRUE)
    
# overnight_1: 
# Check to see if respondant refused to answer overnight (or if RSG did not fill in).  
# If overnight is NULL, see hoursPark_1 and daysPark_1 to fill in incomplete data
check <- PARKsem[is.na(PARKsem$overnight) ,"ID"]

      Bads$overnight_1[c(check)] <- 1

# overnight_2:
# Check to identify observations with overnight==NA, but either daysPark >=0 or
# hoursPark >=0 or both
check <- PARKsem[is.na(PARKsem$overnight) &
                   (is.na(PARKsem$hoursPark)==FALSE |
                      is.na(PARKsem$daysPark)==FALSE) , "ID"]

      Bads$overnight_2[c(check)] <- 1

      
# overnight_3:
# Check to verify that for overnight==1, at least one "nights*" variable is >= 1 
      Bads["overnight_3"][PARKsem["overnight"]==1 & PARKsem["nightsLocalArea"]==0] <- 1      
      
      # for (x in 1:length(PARKsem$ID)){
      #     if(is.na(PARKsem$overnight[x]) == FALSE &
      #        as.integer(PARKsem$overnight[x]) == 1){
      # 
      #     MaxNight <- max(PARKsem[x ,PARK_SegmentVars])
      #                         
      # 
      #         if(is.na(MaxNight) == FALSE &
      #            as.integer(MaxNight) == 0) {
      #              
      #           Bads$overnight_3[x] <- 1
      #         }
      #     }
      # }

# hoursPark_1:
if (exists("hoursPark", where = PARKsem) == TRUE &
    exists("daysPark", where = PARKsem) == TRUE){
  
        check <- PARKsem[is.na(PARKsem$hoursPark) &
                     is.na(PARKsem$daysPark) &
                     is.na(PARKsem$overnight),"ID"]
  
        Bads$hoursPark_1[c(check)] <- 1
}

      # hoursPark_2:
      if (exists("hoursPark", where = PARKsem) == TRUE &
          exists("daysPark", where = PARKsem) == TRUE){
        
        check <- na.omit(PARKsem[((PARKsem$hoursPark > 24)==TRUE),"ID"])     
        
        Bads[match(check, PARKsem$ID), "hoursPark_2"]  <- 1
      }
      
      # daysPark_1:
      if (exists("hoursPark", where = PARKsem) == TRUE &
          exists("daysPark", where = PARKsem) == TRUE){
        
        check <- PARKsem[is.na(PARKsem$daysPark) &
                           is.na(PARKsem$hoursPark), "ID"]
        
        Bads$daysPark_1[c(check)] <- 1
      }
      
      #daysPark_2:
      if (exists("hoursPark", where = PARKsem) == TRUE &
          exists("daysPark", where = PARKsem) == TRUE){
        
        check <- PARKsem[as.numeric(PARKsem$daysPark)> 14,"ID"]    
              check <- na.omit(check)
        
        Bads[match(check, PARKsem$ID), "daysPark_2"] <- 1
      }
      
              # # hoursPark:
              # # hoursPark_1:
              # # Check for NULL observations in hoursPark, daysPark, and overnight. If all three variables are 
              # # NULL, then must assume day trip (overnight == 0).
              # check <- PARKsem[is.na(PARKsem$hoursPark) &
              #                         is.na(PARKsem$daysPark) &
              #                         is.na(PARKsem$overnight),"ID"]
              #       
              #       Bads$hoursPark_1[c(check)] <- 1
              #       
              #       # NOTE: hoursPark_1 == 1 implies daysPark == 1 in PARKsem
              #       
              # # hoursPark_2:        
              # # Check to see if respondant answered >24 hours
              # check <- na.omit(PARKsem[((PARKsem$hoursPark > 24)==TRUE),"ID"])     
              # 
              # Bads[match(check, PARKsem$ID), "hoursPark_2"]  <- 1
              #       
              #       
              # # daysPark:
              # # daysPark_1:
              # # Check for observations with daysPark == NA and hoursPark == NA
              # # If both daysPark and hoursPark are blank, assume daysPark == 1 unless overnight suggests otherwise
              # check <- PARKsem[is.na(PARKsem$daysPark) &
              #                    is.na(PARKsem$hoursPark), "ID"]
              # 
              # Bads$daysPark_1[c(check)] <- 1
              # 
              # # daysPark_2: Check for outliers (daysPark > 14)
              # # NOTE: this was specific to CUVA, what sort of check should be use among all parks?
              # check <- PARKsem[as.numeric(PARKsem$daysPark)> 14,"ID"]    
              #       check <- na.omit(check)
              #       
              # Bads[match(check, PARKsem$ID), "daysPark_2"] <- 1
              #       

      
#***************************************************************************************************
###################### Check the ReEnter question ##################################################
# entries_1: check for entries == NA & DKentries == NA
# These respondents refused to answer the question
      if (exists("entries", where = PARKsem) == TRUE &
          exists("DKentries", where = PARKsem)== TRUE){
        
      check <- PARKsem[is.na(PARKsem$entries) &
                         is.na(PARKsem$DKentries) ,"ID"]

      Bads$entries_1[c(check)] <- 1
      }
# entries_2: check for ReEnter >= 1 and DKReEnter == 99
# These respondents provided an estimate of entries, but also noted that they "did not know"
      # check <- PARKsem[as.numeric(PARKsem$entries)>=1 &
      #                    as.integer(PARKsem$DKentries)==99 ,"ID"]
      # check <- na.omit(check)
      # 
      # Bads$entries_2[c(check)] <- 1


# DKentries_1: check if respondent only answered "Dont Know"
      if (exists("entries", where = PARKsem) == TRUE &
          exists("DKentries", where = PARKsem)== TRUE){
        
      check <- PARKsem[is.na(PARKsem$entries) &
                         as.integer(PARKsem$DKentries)==99 ,"ID"]
      check <- na.omit(check)

      Bads$DKentries_1[c(check)] <- 1
      }
#***************************************************************************************************
##################### Other checks #################################################################



#***************************************************************************************************
# Append column for summation of checks for each ID
Bads$Sum <- 0
Bads$Sum <- rowSums(Bads[,c(which(colnames(Bads) != c("ID")))])    

check <- Bads[is.na(Bads$Sum) ,"ID"]    
    Bads$Sum[c(check)] <- as.numeric(0)

  
  

