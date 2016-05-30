# This is the main line of code for the SEM Project
# Functions and Programs will be called from folder Script
# SEM data is within the Data folder


# Convert .xls data to .csv

# Import data
CUVA_dataCSV <- read.csv("~/SEM_Project/Parks/CUVA/Data/CUVA_dataCSV.csv")

# List variable names
Varnames <- colnames(CUVA_dataCSV)
    Varnames
    
# Subset data on selected varnames

CUVA_data1 <- CUVA_dataCSV[,c(1:5,7)] #how do we want to break up varnames? 