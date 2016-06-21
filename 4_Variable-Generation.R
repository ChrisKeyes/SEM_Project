# This script will use the PARKbads matrix to generate 
# variables for spending analysis.

# Use the preceeding scripts to get the data
setwd("~/SEM_Project")

source("1_Load&Subset.R") # Optional, can just run "2_Cleaning.R"
source("2_Cleaning.R")

# Set data frames to park of interest 
PARKbads <- CUVAbads
PARKsem <- CUVAsem

# For segment variables, drop observations which do not match rules:
# rule_1: Must have answered local (from PARKbads, local_1 == 0)
# rule_2: Must have answered overnight or daysPark (from PARKbads, overnight_1 == 0 & daysPark_1 == 0)
IDpassR1 <- PARKbads[PARKbads$local_1 == 0 , "ID"]  #rule 1
    PARKsegments <- PARKsem[c(IDpass),] 

IDpassR2 <- PARKbads[PARKbads$overnight_1 == 0 &
                     PARKbads$daysPark_1 == 0 , "ID"] #rule 2

PARKsegments <- PARKsegments[na.omit(match(IDpassR2, PARKsegments$ID)),]
