
################################################################################################
# SECTION I - Upload data sets, select a PARK, subset and assign SEM variable names ------------

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

# Activity 4: Hiking/Walking
# Activity 7: Bicycle

###########################################################################################
# SECTION II: Identifying Bad Data Points -------------------------------------------------

source("~/SEM_Project/Script/getGROUPvars.R")

tempDF <- PARKsem

# *****************************************************************************************
# entries 

BADentryID <- subset(PARKsem, entries > daysPark, ID)

for(VAR in BADentryID){
PARKsem$entries[PARKsem$ID == VAR] <- PARKsem$daysPark[PARKsem$ID == VAR]
}

# *****************************************************************************************
# overnight 

tempDF$NIGHTsum <- rowSums(tempDF[PARK_SegmentVars], na.rm = TRUE)

# For observations where overnight == NA and NIGHTsum > 0 , change overnight to 1
PARKsem["overnight"][is.na(tempDF["overnight"]) & tempDF$NIGHTsum > 0] <- 1

# For observations where overnight == NA and NIGHTsum = 0 , change overnight to 0
PARKsem["overnight"][is.na(tempDF["overnight"]) & tempDF$NIGHTsum == 0] <- 0

# Loop over the segment variables for the PARK, fill in any NA's with zeros for overnight parties
for (VAR in PARK_SegmentVars){
  PARKsem[VAR][is.na(PARKsem[VAR])] <- 0
}  

# *****************************************************************************************
# local
PARKsem$zip[c(PARKsem[which(PARKsem$zip == " "), "ID"])] <- NA

source("~/SEM_Project/Script/cleanLOCAL.R")
    
# *****************************************************************************************

PARKsem$nightsLocalArea <- rowSums(PARKsem[PARK_SegmentVars], na.rm = TRUE)

# *****************************************************************************************

# NOTE: any additional cleaning should go here
    
# *****************************************************************************************

source("~/SEM_Project/Script/genBADS.R")
    
PARKbads <- Bads

# *****************************************************************************************
source("~/SEM_Project/Script/cleanOUTLIERS.R")

###########################################################################################
# SECTION III: Subset Data Frame - Drop Bad Data ------------------------------------------

PARKsegments <- PARKsem

PARKsegments$sumBADSsegments <- PARKbads$local_1 +
                                PARKbads$overnight_1  +
                                PARKbads$overnight_3 +
                                PARKbads$Outliers
    
PARKsegments <- subset(PARKsegments, sumBADSsegments == 0)

PARKbads_seg <- subset(PARKbads, (local_1 + overnight_1 + overnight_3 + Outliers)==0)


###########################################################################################
# SECTION IV: Identify Party Segments, Shares, & Trip Purpose Scal ------------------------

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

# Following estimates from Bowker 2007 - Using only Day local/non-local and overnight
PARKsegment_SHARES3 <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(PARKsegment_SHARES3) <- c(c(SEGvars_day), "ON_Any")

for(VAR in colnames(PARKsegment_SHARES3)){
  PARKsegment_SHARES3["n",VAR] <- sum(PARKsegments[PARKsegments[,VAR]==1, VAR])
  PARKsegment_SHARES3["Sample.Share", VAR] <- sum(PARKsegments[PARKsegments[,VAR]==1, VAR])/nrow(PARKsegments)
  PARKsegment_SHARES3["Avg.Annual.Visits", VAR] <- mean(PARKsegments[PARKsegments[,VAR]==1, "AnnualVisits"], na.rm = T)
  PARKsegment_SHARES3["Avg.Entry.Rate", VAR] <- mean(PARKsegments[PARKsegments[,VAR]==1, "entries"], na.rm = T)
  PARKsegment_SHARES3["Avg.Party", VAR] <- mean(PARKsegments[PARKsegments[,VAR]==1, "totalCovered"], na.rm = T)
  PARKsegment_SHARES3["numerator", VAR] <- PARKsegment_SHARES3["Sample.Share", VAR]* PARKsegment_SHARES3["Avg.Annual.Visits", VAR]*
                                           PARKsegment_SHARES3["Avg.Entry.Rate", VAR]*PARKsegment_SHARES3["Avg.Party", VAR]
}

SHAREsum <- rowSums(PARKsegment_SHARES3["numerator",])

for(VAR in colnames(PARKsegment_SHARES3)){
  PARKsegment_SHARES3["Estimated.Share", VAR] <- PARKsegment_SHARES3["numerator", VAR]/SHAREsum
}

# *****************************************************************************************
SEGvarsMASTER <- SEGvars
SEGvars <- c(c(SEGvars_day), "ON_Any")

tempDF <- subset(PARKsegments, day_local == 1 | day_nonlocal == 1)

# Look at distribution of hotel spending
var.1 <- na.omit(tempDF$expHotels)
df.1 <- approxfun(density(na.omit(var.1)))
plot(density(var.1), main = "Density Distribution of expHotels for Day Trips")
var.1.new <- c(max(tempDF$expHotels))
points(var.1.new, df.1(var.1.new), col = 2)

quantile(tempDF$expHotels, c(.75, .9, .95, .98))

attach(tempDF)
boxplot(expLocalTotal ~ tripPurpose,
        ylab = "Total Local Expenditures" ,
        xlab = "Trip Purpose",
        main = "Total Local Expenditures by Trip Purpose")
numOBS <- 1:length(expLocalTotal)
x1 <- rep(1, length(expLocalTotal[tripPurpose == 1]))
x2 <- rep(2, length(expLocalTotal[tripPurpose == 2]))
x3 <- rep(3, length(expLocalTotal[tripPurpose == 3]))

y1 <- expLocalTotal[tripPurpose == 1]
y2 <- expLocalTotal[tripPurpose == 2]
y3 <- expLocalTotal[tripPurpose == 3]

identify(x1, y1, labels = tempDF$ID[numOBS[tripPurpose == 1]])
identify(x2, y2, labels = tempDF$ID[numOBS[tripPurpose == 2]])
identify(x3, y3, labels = tempDF$ID[numOBS[tripPurpose == 3]])
detach(tempDF)

# Day trip, primary purpose, activity is Hike or Bike
tempDF.prime <- subset(tempDF, tripPurpose == 1)
tempDF.prime$HBactivity <- ifelse(tempDF.prime$PrimaryActivity == 4 | tempDF.prime$PrimaryActivity == 7, 1, 0)

boxplot(expLocalTotal ~ HBactivity,
        ylab = "Total Local Expenditures" ,
        xlab = "Primary Activity",
        data = tempDF.prime)

boxplot(expHotels ~ local,
        ylab = "Total Local Expenditures" ,
        xlab = "Primary Activity",
        data = tempDF.prime)

# remove hotel exp from local total
EXPv <- PARK_ExpVars[PARK_ExpVars != c("expHotels")]

tempDF.prime$expLocalTotal <- rowSums(tempDF.prime[,EXPv[EXPv != "expLocalTotal"]])

boxplot(expLocalTotal ~ local,
        ylab = "Total Local Expenditures (no expHotels)" ,
        xlab = "Primary local",
        data = tempDF.prime)

var.1 <- na.omit(tempDF.prime$expLocalTotal)
df.1 <- approxfun(density(na.omit(var.1)))
plot(density(var.1), main = "Density Distribution of expLocalTotal for Primary Day Trips")

boxplot(expHotels ~ local,
        ylab = "Hotel Expenditure" ,
        xlab = "Primary local",
        data = tempDF.prime)


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

# source("~/SEM_Project/Script/genREPORT_tables.R") 

###########################################################################################
# SECTION VII: Write Data in Memory to Local Drive ---------------------------------------

###########################################################################################
# END :)













