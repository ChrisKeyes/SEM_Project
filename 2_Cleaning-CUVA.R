# THis script will use a series of conditional statements to identify missing, incomplete, and outlying observations.
# 
# Will use "PARKsem" as general name for the dataset

PARKsem <- CUVAsem

###################################################################################################

# NEED TO DO A CHECK TO VERIFY THE VARIABLES MATCH

CHECKvars <- read.csv("~/SEM_Project/SEMvars.csv")
  CHECKvars

# Create Data frame for taxonomy of bads

Bads <- data.frame(PARKsem[,c("ID")])
  colnames(Bads) <- c("ID")
  
# Use the variable names in PARKsem as list of checks and loop over each ID
  
Checks <- colnames(PARKsem[which(colnames(PARKsem) != c("ID"))])

Bads$LiveNearbyNA <- 0

# this will work for the outer loop
for (x in levels(PARKsem$ID)){
  ID1 <- x
}

# this will work for generating a data frame to fill with results
for (y in Checks){
  ck = y
    Bads[, ck] <- 0
}

ID1 <- PARKsem$ID[1]

