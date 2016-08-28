# This script will produce the segment shares as explained by Bowker 2007

rm(list=union(ls(), ls()))

source("~/SEM_Project/Script/loadPACKAGES.R")

setwd("~/SEM_Project")

source("~/SEM_Project/Script/selectPARK.R")
      
SEMvars <- read.csv("~/SEM_Project/SEMvars.csv")

source("~/SEM_Project/Script/getSEMvars.R")

if(PARKname == "CUVA"){
PARKsem$AnnualVisits <- PARKdata$Q35_NumParkVisits  # for CUVA
PARKsem$PrimaryActivity <- PARKdata$Q13a_PrimeActivityCoded # for CUVA
}

if(PARKname == "GATE"){

PARKsem$entries <- 1
PARKsem$hoursPark <- 3
PARKsem$daysPark <- 1

Q4 <- PARKdata[, c(grepl("Q04", colnames(PARKdata)))] # Annual Visit Question for GATE
Q4 <- Q4[,colnames(Q4) != c("Q04l_OtherPlace")]
PLACES <- colnames(Q4)

Q4$Max <-max.col(Q4, "first")
}

source("~/SEM_Project/Script/getGROUPvars.R")

tempDF <- PARKsem

BADentryID <- subset(PARKsem, entries > daysPark, ID)

for(VAR in BADentryID){
PARKsem$entries[PARKsem$ID == VAR] <- PARKsem$daysPark[PARKsem$ID == VAR]
}

tempDF$NIGHTsum <- rowSums(tempDF[PARK_SegmentVars], na.rm = TRUE)

# For observations where overnight == NA and NIGHTsum > 0 , change overnight to 1
PARKsem["overnight"][is.na(tempDF["overnight"]) & tempDF$NIGHTsum > 0] <- 1

# For observations where overnight == NA and NIGHTsum = 0 , change overnight to 0
PARKsem["overnight"][is.na(tempDF["overnight"]) & tempDF$NIGHTsum == 0] <- 0

# Loop over the segment variables for the PARK, fill in any NA's with zeros for overnight parties
for (VAR in PARK_SegmentVars){
  PARKsem[VAR][is.na(PARKsem[VAR])] <- 0
}  

PARKsem$zip[c(PARKsem[which(PARKsem$zip == " "), "ID"])] <- NA

source("~/SEM_Project/Script/cleanLOCAL.R")
    
PARKsem$nightsLocalArea <- rowSums(PARKsem[PARK_SegmentVars], na.rm = TRUE)

source("~/SEM_Project/Script/genBADS.R")
    
PARKbads <- Bads

source("~/SEM_Project/Script/cleanOUTLIERS.R")

PARKsegments <- PARKsem

PARKsegments$sumBADSsegments <- PARKbads$local_1 +
                                PARKbads$overnight_1  +
                                PARKbads$overnight_3 +
                                PARKbads$Outliers
    
PARKsegments <- subset(PARKsegments, sumBADSsegments == 0)

PARKbads_seg <- subset(PARKbads, (local_1 + overnight_1 + overnight_3 + Outliers)==0)

SEGvars_day <- c("day_local", "day_nonlocal")
SEGvars_on <- paste("ON", substring(PARK_SegmentVars, 7), sep = "_")            

SEGvars <- c(SEGvars_day,SEGvars_on)
  
     
Choice.SEG <- dlgMessage(c("Do you want to 'lump' segments together?"), "yesno",
               title = "Lump Segments")$res

if (Choice.SEG == "no") {
  message("Segments have not been lumped\n")
  source("~/SEM_Project/Script/getSEGMENTS.R")
  source("~/SEM_Project/Script/genSEGMENT_shares.R") 

} else {
  source("~/SEM_Project/Script/getSEGMENTS.R")
  source("~/SEM_Project/Script/genSEGMENT_shares.R") 
  source("~/SEM_Project/Script/getSEGMENTS_lumped.R")
  source("~/SEM_Project/Script/genSEGMENT_shares.R") 
  message("Segments are now lumped\n")
}
source("~/SEM_Project/Script/getPRIMARYscalers.R")

# Following estimates from Bowker 2007
PARKsegment_SHARES2 <- data.frame(matrix(ncol = length(SEGvars), nrow = 0))
colnames(PARKsegment_SHARES2) <- c(SEGvars)

PARKsegments$childrenCovered <- ifelse(is.na(PARKsegments$childrenCovered), 0 , 1)
PARKsegments$totalCovered <- PARKsegments$adultsCovered + PARKsegments$childrenCovered

for(VAR in SEGvars){
  PARKsegment_SHARES2["n",VAR] <- sum(PARKsegments[PARKsegments[,VAR]==1, VAR])
  PARKsegment_SHARES2["Sample.Share", VAR] <- sum(PARKsegments[PARKsegments[,VAR]==1, VAR])/nrow(PARKsegments)
  PARKsegment_SHARES2["Avg.Annual.Visits", VAR] <- mean(PARKsegments[PARKsegments[,VAR]==1, "AnnualVisits"], na.rm = T)
  PARKsegment_SHARES2["Avg.Entry.Rate", VAR] <- mean(PARKsegments[PARKsegments[,VAR]==1, "entries"], na.rm = T)
  PARKsegment_SHARES2["Avg.Party", VAR] <- mean(PARKsegments[PARKsegments[,VAR]==1, "totalCovered"], na.rm = T)
  PARKsegment_SHARES2["numerator", VAR] <- PARKsegment_SHARES2["Sample.Share", VAR]* PARKsegment_SHARES2["Avg.Annual.Visits", VAR]*
                                           PARKsegment_SHARES2["Avg.Entry.Rate", VAR]*PARKsegment_SHARES2["Avg.Party", VAR]
}

SHAREsum <- rowSums(PARKsegment_SHARES2["numerator", SEGvars])

for(VAR in SEGvars){
  PARKsegment_SHARES2["Estimated.Share", VAR] <- PARKsegment_SHARES2["numerator", VAR]/SHAREsum
}

View(PARKsegment_SHARES2)
