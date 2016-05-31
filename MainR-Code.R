# This is the main line of code for the SEM Project
# Functions and Programs will be called from folder Script
# SEM data is within the Data folder

#Load functions
source("~/SEM_Project/Script/getParkData.R")
source("~/SEM_Project/Script/getParkName.R")


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
DataNameVector <- c(NULL)
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

Varnames <- colnames(CUVA)
    Varnames
    
# Subset data on selected varnames

CUVA_data1 <- CUVA_dataCSV[,c(1:5,7)] #how do we want to break up varnames? 


