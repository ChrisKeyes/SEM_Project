# This is the main line of code for the SEM Project
# Functions and Programs will be called from folder Script
# SEM data is within the Data folder

################################################################################################
#Upload the survey data and name data frame by park name

#Load functions
source("~/SEM_Project/Script/getParkData.R")
source("~/SEM_Project/Script/getParkName.R")
source("~/SEM_Project/Script/getSEMvars.r")
source("~/SEM_Project/Script/SearchNames.r")

# Convert .xls data to .csv
### do we want to automate this within VBA?

# Import data
# Set working directory to the data folder
setwd("~/SEM_Project/Data")

# Save list of park datasets in Data folder
Parklist <- list.files()
  Parklist

# Loop over all datasets and change name to four letter name
# and create a vector of dataset names called "DataNameVector"
DataNameVector <- c(NULL)  #to be used within the loop

for (x in Parklist) {
    ParkName <- substr(x, 1, 4)
    DataNameVector <- append(DataNameVector, ParkName)
    
        WD <- getwd()
        filename <- paste(WD, x,sep = "/")
    
    park <- read.csv(filename)
    getParkName(ParkName)
        park <- NULL
}

################################################################################################

# Choose data set to work with 
###Should we work within a large loop??
DataNameVector  #If we loop, can use this vector of park names

################################################################################################
# - Subset data to define and create visitor segments

#create vector of all varnames for current park
Varnames <- colnames(CUVA)

#Create a data frame from CSV file with segment variables 
#column 1: desired variables
#column 2: search terms to find matches within survey data
SegmentVars <- read.csv("~/SEM_Project/SegmentVars.csv")

# # create vector of variable names for spending effects
# SEMvars <- AllVars[,1]   #need to expand list 
# *****MAY DELETE THE CODE ABOVE*****

# create vector of variable names which serve as general search terms for each parks survey
SearchVars <- SegmentVars[,2]
  SearchVars

# determine which variables are matches between the survey variables and our search terms
ParkVars <- NULL
TempVars <- NULL
Matches <- NULL

for (x in SearchVars){
  TempVars <- Varnames[grepl( x ,Varnames, ignore.case = TRUE)]
  ParkVars <- append(ParkVars, TempVars)
  Matches <- append(Matches, (any(grepl(x, Varnames))))
}


# Parkvars are the matched variable names for this survey/park
# Matches is a vector of logical statements, TRUE for match, False otherwise
ParkVars
Matches

# For matches, pull the column from the dataset and save as dataframe named PARKsegments
CUVAsegments <- CUVA[c(ParkVars)]    

#Rename the variables to search variable names for consistancy
colnames(CUVAsegments) <- SearchVars[c(which(Matches == TRUE))]
  head(CUVAsegments)
 
  
################################################################################################
# - Clean out data and create variables 
  
#********* How do we want to handle "NA" for ReEnter?  (i.e. they checked "dont know") *********
  




