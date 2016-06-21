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

# source("~/SEM_Project/Script/getParkData.R")  *** delete if not used ***
# source("~/SEM_Project/Script/SearchNames.r")  *** delete if not used ***

# Convert .xls data to .csv
### do we want to automate this within VBA? #Cathy thoughts: I think we can just save our raw data as .csv

# Import data
# Set working directory to the data folder
setwd("~/SEM_Project/Data")

# Save list of park datasets in Data folder
Parklist <- list.files()

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
DataNameVector  #If we loop, can use this vector of park names

#Create a data frame from CSV file with segment variables 
#column 1: desired variables
#column 2: search terms to find matches within survey data
SEMVars <- read.csv("~/SEM_Project/SEMvars.csv")
  SEMVars
  
################################################################################################
# Use the two lines below to specify which park to subset and rename columns
PARKdata <- CUVA
PARKname <- "CUVA"
  
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
  
# Use the line below to name the data frame to the specific park  
CUVAsem <- PARKsem
  
  
# End of script - Move to cleaning script
setwd("~/SEM_Project")


################################################################################################
############# SAVE CODE BELOW AND USE PARK SPECIFIC VARIABLE SELECTION SCRIPT ABOVE ############
################################################################################################
# #create vector of all varnames for current park
# Varnames <- colnames(CUVA)
# Varnames
# 
# # # create vector of variable names for spending effects
# # SEMvars <- AllVars[,1]   #need to expand list 
# # *****MAY DELETE THE CODE ABOVE*****
# 
# # create vector of variable names which serve as general search terms for each parks survey
# SearchVars <- SEMVars[,2]
#   SearchVars
# 
# # determine which variables are matches between the survey variables and our search terms
# ParkVars <- NULL
# TempVars <- NULL
# Matches <- NULL     #vector specifying match, no match, multiple matches
# MMatches <- NULL    #multiple matches vector
# 
# # for (t in SearchVars){
# #   x <- paste(t, "$", sep = "")
# #     TempVars <- Varnames[grepl( x ,Varnames, ignore.case = TRUE)]
# #     ParkVars <- append(ParkVars, TempVars)
# #     Matches <- append(Matches, (any(grepl(x, Varnames))))
# # }
# 
# for (t in SearchVars){
#   x <- paste(t, "$", sep = "")
#   TempVars <- Varnames[grepl( x ,Varnames, ignore.case = TRUE)]
#   
#   if (length(TempVars)== 1){  #if length = 1: exact match
#     ParkVars <- append(ParkVars, TempVars)
#     Matches <- append(Matches, (any(grepl(x, Varnames))))
#   }
#   else if (length(TempVars) == 0){  #if length = 0: no match
#     Matches <- append(Matches, (any(grepl(x, Varnames))))
#   }
#   else {  #if length > 1: multiple matches
#     MMatches <- append(MMatches, TempVars)
#     Matches <- append(Matches, NA)
#   }
# }
# 
# 
# # *** Need to sort out what to do with multiple matches for one search ***
# 
# # Matches is a vector of logical statements, TRUE for one match, False for no matches, NA for multiple
# # MMatches is a vector listing multiple matches; no rules set yet on how to filter these results
# # Parkvars are the matched variable names for this park's survey 
# # List the matched variables:
#     #Matches
#       summary(Matches)
#     #ParkVars
#       summary(ParkVars)
#     
#       MMatches
# 
# # Create a data frame showing which search variables were found (TRUE = at least one match, FALSE = no match, NA = multiple matches)
# SearchResults <- cbind(SearchVars, Matches, stringsAsFactors = as.data.frame.AsIs(SearchVars))
#   SearchResults
# 
# # For multiple matches, determine which are relevent, and pull from raw data
# # From vector of multiple matches (MMatches), pull variable names which are relevent
# 
# #***************************************************************************************************************
# MMvars <- MMatches[c(1,3,6,9)]  #NOTE:  this is specific to CUVA; no rules set to filter and automate
# #NOTE:  If we change search variables, we need to check the vector above to verify it collects the correct variables
# 
# # Subset those vars from raw data frame; include "ID" for merging to matched set of variables
# MMvars <- CUVA[c(c(MMvars), "ID")]
# 
# # Rename the columns to the search terms for consistancy
# MMvarNames <- paste(as.character(SearchVars[c((which(is.na(Matches))))]))
#   l <- length(MMvars) - 1
#     colnames(MMvars)[1:c(l)] <- c(MMvarNames)
# 
# # For unique matches, pull the column from the dataset and save as dataframe named PARKsem
# CUVAsem <- CUVA[c(ParkVars)]   
# 
# #Rename the variables to search variable names for consistancy
# colIndex <- c((which(Matches == TRUE)))
# 
# colnames(CUVAsem) <- SearchVars[c((which(Matches == TRUE)))]
#   head(CUVAsem)
# 
# # Lastly, merge the data frames (filtered multiple matches from MMvars to CUVAsem, by "ID")
#   CUVAsem <- merge(CUVAsem, MMvars, by = c("ID"))
#  
#   x <- NULL
#   t <- NULL
#   TempVars <- NULL
#   Varnames <- NULL
  
################################################################################################
################################################################################################
################################################################################################



