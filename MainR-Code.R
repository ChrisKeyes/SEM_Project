# This is the main line of code for the SEM Project
# Functions and Programs will be called from folder Script
# SEM data is within the Data folder

#Load functions
source("~/SEM_Project/Script/getParkData.R")
source("~/SEM_Project/Script/getParkName.R")
source("~/SEM_Project/Script/getSEMvars.r")

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


# Choose data set to work with 
###Should we work within a large loop??
DataNameVector  #If we loop, can use this vector of park names

#create vector of all varnames for current park
Varnames <- colnames(CUVA)

AllVars <- read.csv("~/SEM_Project/SEMvars.csv")

# create vector of variable names for spending effects
SEMvars <- AllVars[,1]   #need to expand list 

# create vector of variable names which serve as general search terms for each parks survey
SearchVars <- AllVars[,2]


# determine which variables are matches between the survey variables and our search terms
ParkVars <- NULL
TempVars <- NULL
Matches <- NULL

for (x in SearchVars){
  TempVars <- Varnames[grepl( x ,Varnames)]
  ParkVars <- append(ParkVars, TempVars)
  Matches <- append(Matches, (any(grepl(x, Varnames))))
}

# Parkvars are the matched variable names for this survey/park
# Matches is a vector of logical statements, TRUE for match, False otherwise

# For matches, pull the column from the dataset and save as dataframe named PARKSEM
CUVASEM <- CUVA[c(ParkVars)]    

# Create two vectors 
# One is a list of the ith matched names
# The other is a list of the ith non-matched variable names

ColNums <- NULL
noVars <- NULL
L <- length(Matches)
for (l in 1:L){
  if (Matches[l]=="TRUE") {
    ColNums <- append(ColNums, l)
  }
  else {
    noVars <- append(noVars, SearchVars[l])
  }
}

# Use the matched indicies from the ColNums vector to generate the neccessary variables 


# Subset data on selected varnames



