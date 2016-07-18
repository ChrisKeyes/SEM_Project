# This script will take the cleaned data (PARKsem) and variables generated from script #4 
# and create spending profiles and analysize the summary statistics

setwd("~/SEM_Project")

source("4_Variable-Generation.R")


for (x in PARK_ExpVars){
  PARKsegments[,x] <- ifelse(is.na(PARKsegments[,x]),0,PARKsegments[,x])
}

PARKspending_MEANS <- data.frame(EXPENDITURES = PARK_ExpVars)
    # NOTE: when this data frame is written to CSv - name PARK_ExpMeans_by_Party

SEGvars_on <- paste("ON", substring(PARK_SegmentVars, 7), sep = "_")            
SEGvars_day <- c("day_local", "day_nonlocal")


a <- NULL # colnames by segment
b <- NULL # mean values by segment and expenditure

for (x in SEGvars_day){
  
  tempDF <-subset(PARKsegments, PARKsegments[x]==1, select = c(PARK_ExpVars))
  
  for (y in colnames(tempDF)){
    avg <- mean(tempDF[,y])
      b <- append(b, avg)
  }
  
  df <- data.frame(EXPENDITURES = PARK_ExpVars, x = b)
      colnames(df) <- c("EXPENDITURES", x)
  
  PARKspending_MEANS <- merge(PARKspending_MEANS, df , by = c("EXPENDITURES"))
      b <- NULL
}


for (x in SEGvars_on){
  
  tempDF <- subset(PARKsegments, PARKsegments[,x] == 1, select = c(PARK_ExpVars))
      expV <- colnames(tempDF)
  
  for (y in expV){
    avg <- mean(tempDF[,y])
          b <- append(b, avg)
  }
  
  
    df <- data.frame(EXPENDITURES = PARK_ExpVars, x = b)
        colnames(df) <- c("EXPENDITURES" , x)
        
        PARKspending_MEANS <- merge(PARKspending_MEANS, df, by = c("EXPENDITURES"))
            b <- NULL
}

# REMOVE "expLocalTotal" FROM TABLE AND REPLACE WITH ACTUAL SUM OF MEANS
# Move local expenditure total row to bottom of table
if(exists("expLocalTotal", where = PARKsegments) == TRUE){
  
  ltotal_row <- match("expLocalTotal", 
                      PARKspending_MEANS$EXPENDITURES)
        
        # PARKspending_MEANS <- rbind(PARKspending_MEANS, PARKspending_MEANS[c(ltotal_row),])
  
        PARKspending_MEANS <- PARKspending_MEANS[-c(ltotal_row),]

            row.names(PARKspending_MEANS) <- NULL
}


n <- length(PARKspending_MEANS$EXPENDITURES)
    m <- n + 1

EXProw <- append(as.character(PARKspending_MEANS$EXPENDITURES), "Totals_bySEG")

PARKspending_MEANS[m,] <- 0

PARKspending_MEANS$EXPENDITURES <- EXProw

    PARKspending_MEANS[m, 2:length(colnames(PARKspending_MEANS))] <- 
      colSums(PARKspending_MEANS[,2:length(colnames(PARKspending_MEANS))])    

PARKspending_MEANS[,"Mean_EXP"] <- 0

# Change row names to "EXPENDITURES" in PARKspending_MEANS
# Change row names to "SEGMENT" in PARKshares_table
# Create a vector of weights which matches the two (in order) and generate 
# weighted spending means by segment share.


  # rowSums(PARKspending_MEANS[,2:length(colnames(PARKspending_MEANS))])

View(PARKspending_MEANS)

rm(df, tempdf, a, avg, b, c, EXProw, expV, IDs_badNights, ltotal_row, m, maxNights, maxNightsType, minNights, n, v, VAR, x, y, z)

