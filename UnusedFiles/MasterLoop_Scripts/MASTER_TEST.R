# MATSTER TEST SCRIPT

# install.packages("svDialogs")

rm(list=union(ls(), ls()))

source("~/SEM_Project/Script/getParkName.R")
source("~/SEM_Project/Script/getSEMvars.R")

library(plyr)
library(svDialogs)


SEMvars <- read.csv("~/SEM_Project/SEMvars.csv", header = TRUE)
GROUPvars <- read.csv("~/SEM_Project/GROUPvars.csv", header = TRUE)

setwd("~/SEM_Project/Data")

Parklist <- list.files()

    # res <- dlgList(substr(Parklist, 1, 4), 
    #                multiple = TRUE, 
    #                title = "Select Park(s)")$res
    # 
    # if (!length(res)) {
    #   cat("You cancelled the choice\n")
    # } else {
    #   cat("You selected:\n")
    #   print(res)
    # }
    
DataNameVector <- c(NULL)

# for (dataset in res){
for (dataset in Parklist){
  
  DATA <- dataset 
  
  if(DATA != "TESTdata.csv"){
    
    PARK <- substr(DATA, 1, 4)
        PARKdata <- paste(PARK, "data", sep = "")
            DataNameVector <- append(DataNameVector, PARKdata)
    
    
    filename <- paste("~/SEM_Project/Data", DATA ,sep = "/")
    
    PARKdata <- read.csv(filename, header = TRUE)
        PARKname <-  PARK  
    
source("~/SEM_Project/PracticeFiles/Script1-TEST.R")

    source("~/SEM_Project/Script/getGroupVars.R")
    source("~/SEM_Project/Script/getEXPvars.R")
        
          if( PARK == "YOSE"){
            PARKsem$local <- abs(PARKsem$local - 2)
          }

source("~/SEM_Project/PracticeFiles/Script2-TEST.R")
# Store Bads data frame as PARKbads
    PARKbads <- Bads 
    
# Remove Bads dataframe
    rm(Bads, v, y, check)
    
    
# Save the PARKsem data and PARKbads matrix to Output/PARK folder
setwd("~/SEM_Project/Output")
    PARKfile <- paste(getwd(),
                      PARK, sep = "/")
    setwd(paste(getwd(),
                PARK, sep = "/"))
    
    
    write.csv(PARKsem, paste(PARK, "sem_RAW.csv", sep = ""), row.names = FALSE)
    write.csv(PARKbads, paste(PARK, "bads_RAW.csv", sep = ""), row.names = FALSE)

setwd("~/SEM_Project") 

# Fix any observations that can be completed, fill in NULL datapoints with zeros where
# applicable

# For observations where overnight == NA, but sum(nights*)>=1, change overnight <- 1 
# (i.e. PARKbads$overnight_2 == 1)
IDs_ON2 <- PARKbads[which(PARKbads$overnight_2 == 1), "ID"]   
    
    PARKsem$overnight[IDs_ON2] <- 1
    
# For observations where overnight == 1, but one or more of the accomodation 
# types is NA, replace with zero
    
    for (VAR in PARK_SegmentVars){
      
      IDs_SEG <- na.omit(PARKsem[with(PARKsem, overnight == 1 & is.na(PARKsem[VAR])), "ID"])
      
      PARKsem[match(IDs_SEG, PARKsem$ID), VAR ] <- 0
    }
    
# For observations where zip == " " (i.e. zip is blank, PARKbads$zip_2 == 1), replace with zip == NA
# (i.e. PARKbads$zip_2 == 1)
    IDs_ZIP2 <- PARKbads[which(PARKbads$zip_2 == 1), "ID"]
    
    PARKsem$zip[c(IDs_ZIP2)] <- NA
    
# For observations where local==NA, but zip is provided, change local to 
# local == 1 if zip matches local zipcodes or
# local == 0 if zip matches non-local zipcodes

source("~/SEM_Project/PracticeFiles/Clean_Local&Zip-TEST.R")
    
# Now re-run script 2 to produce a second BADS matrix which reflects the corrected data frame

source("~/SEM_Project/PracticeFiles/Script2-TEST.R")
    
  PARKbads_seg <- Bads
  PARKsegments <- PARKsem # This is temporary solution due to name overlap
  
    rm(Bads, v, y, check)
    rm(PARKsem)
    
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
    # NOTE:  If the script stops and produces a warning message, read the comments 
    # found within "IdentifySegments.R" script.  
    
###########################################################################################
#***************** Generate Segment Shares ************************************************
    
source("~/SEM_Project/Script/GenSegmentShares.R") 

###########################################################################################
#***************** Generate Spending Profiles *********************************************
    
source("~/SEM_Project/PracticeFiles/Script5-TEST.R")
 
###########################################################################################
#***************** Generate Re-Entry Means ************************************************
 
# source("~/SEM_Project/Script/GenReEntry.R")
if (exists("entries", where = PARKsegments) == TRUE &
    exists("DKentries", where = PARKsegments)== TRUE){

tempDF <- data.frame(PARKsegments)
    tempDF$sumBADSentries <- PARKbads_seg$entries_1 + PARKbads_seg$DKentries_1
          
          tempDF <- subset(tempDF,
                           sumBADSentries == 0 ,
                           select =
                             c("entries", c(SEGvars_day, SEGvars_on)))

          colnames(tempDF) <- c("entries", SEGvars)

    b <- NULL # b: vector of mean entry by party segment
    for (VAR in SEGvars){
      b <- append(b, mean(tempDF["entries"][tempDF[VAR]==1]))
    }

    PARKentries_table <- data.frame(Mean_reEntry = b)
          row.names(PARKentries_table) <- SEGvars

          rm(tempDF, b, BADids, VAR )

          setwd("~/SEM_Project/Output")
          
          PARKfile <- paste(getwd(),
                            PARK, sep = "/")
          setwd(paste(getwd(),
                      PARK, sep = "/"))
          write.csv(PARKentries_table, paste(PARK, "_Mean_reEntry.csv", sep = ""), row.names = TRUE)
    
          setwd("~/SEM_Project/Data")


}
###########################################################################################
#***************** Store Output Tables and Data *******************************************
    
    setwd("~/SEM_Project/Output")
      PARKfile <- paste(getwd(),
                        PARK, sep = "/")
      setwd(paste(getwd(),
                  PARK, sep = "/"))

    write.csv(PARKdata, paste(PARK, "data.csv", sep = ""), row.names = FALSE)
    write.csv(PARKsegments, paste(PARK, "segments.csv", sep = ""), row.names = FALSE)
    write.csv(PARKbads_seg, paste(PARK, "bads_seg.csv", sep = ""), row.names = FALSE)
    write.csv(PARKshares_table, paste(PARK, "shares_table.csv", sep = ""), row.names = FALSE)
    write.csv(PARKspending_MEANS, paste(PARK, "_MeanExpenditures-PartyType.csv", sep = ""), row.names = FALSE)
  }
  
}

# setwd("~/SEM_Project/Output")
# 
# write.csv(SEMvars, paste(getwd(),
#                          "SEMvars.csv", sep = "/") ,
#                           row.names = FALSE)
# 
# write.csv(CHECKvars, paste(getwd(),
#                          "CHECKvars.csv", sep = "/") ,
#                           row.names = FALSE)
# 
# write.csv(GROUPvars, paste(getwd(),
#                          "GROUPvars.csv", sep = "/") ,
#                           row.names = FALSE)


#rm(list=setdiff(ls(), c("SEMvars","CHECKvars", "GROUPvars", "DataNameVector")))

