# Upload, rename, & subset script for SEM variables

# This is the first set of script for the SEM project.  Run this script in its entirety to 
# upload the datasets within the "data" folder.
# Each data set will be stored in memory and renamed to its four letter park name (i.e. "ACAD").
# The park data will then be subset into a new dataframe named "PARKsem" (i.e. "ACADsem") for cleaning.
# All data frames and objects are stored to memory, and original CSV files are unchanged.
# The variables used for subsetting are found in the SEMvars.csv file.  All variables kept are renamed to 
# respective SEM variable name for consistancy. 

################################################################################################
#Upload the survey data and name data frame by park name

#Load functions

source("~/SEM_Project/Script/getParkName.R")
source("~/SEM_Project/Script/getSEMvars.r")

# Convert .xls data to .csv

# Set working directory to the data folder
setwd("~/SEM_Project/Data")

# Save list of park datasets in Data folder
Parklist <- list.files()

# Loop over all datasets and change name to four letter name
# and create a vector of dataset names called "DataNameVector"
DataNameVector <- c(NULL)  #to be used within the loop

for (x in Parklist) {
    PARKname <- substr(x, 1, 4)
        PARKname <- paste(PARKname, "data", sep = "")
    DataNameVector <- append(DataNameVector, PARKname)
    
        WD <- getwd()
        filename <- paste(WD, x,sep = "/")
    
    park <- read.csv(filename)
    getParkName(PARKname)
        park <- NULL
}


#Create a data frame from CSV file with segment variables 
#column 1: desired variables
#column 2: search terms to find matches within survey data
SEMVars <- read.csv("~/SEM_Project/SEMvars.csv")

################################################################################################
# Use the two lines below to specify which park to subset and rename columns

PARKdata <-    CUVAdata 
PARKname <-  "CUVA"   
  
# create vector of variable names which define which variable names to keep
MATCHvars <- levels(SEMVars[,c(PARKname)])
      MATCHvars <- MATCHvars[which(MATCHvars!= c(""))]  # remove any NULL values
      
# subset data on the selected variable names      
PARKsem <- PARKdata[c(MATCHvars)]  
  
#Rename the variables to SEMvars for consistancy
colIndex <- NULL
for (r in MATCHvars){
  i <- which(SEMVars[c(PARKname)] == r)
  colIndex <- append(colIndex, i)
}
  
colnames(PARKsem) <- SEMVars$SEMvars[c(colIndex)]
  
# # Use the line below to name the data frame to the specific park  
# CUVAsem <- PARKsem
  
# End of script - Move to cleaning script
setwd("~/SEM_Project")

rm(colIndex,filename, i, MATCHvars, park, r, WD, x)



