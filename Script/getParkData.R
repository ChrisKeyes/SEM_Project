# This script uploads all the raw data sets located in the "Data" folder. The data is held 
# in memory as a data.frame and named "PARKdata".  Where "PARK" is each park's four letter 
# park code.  For example, the data set "CUVA-final.csv" is uploaded to memory as "CUVAdata"
# with no changes made to the data itself.

# NOTE: Data sets within the "Data" folder must have undergone the "pre" cleaning and 
# formatting outlined in the "NEWDATAworkflow" documentation

################################################################################################

# Set working directory to the data folder
setwd("~/SEM_Project/Data")

# Save list of park datasets (filenames of each .csv) in Data folder
Parklist <- list.files()

# Loop over all data set names and truncate the filename to the four letter park name. Upload
# each data set, assign it the corresponding "PARKdata" name and store a vector (list) of the 
# dataset names in memory called "DataNameVector"
DataNameVector <- c(NULL)  #to be used within the loop

for (x in Parklist) {
  PARKname <- substr(x, 1, 4)   # PARK name
      PARKname <- paste(PARKname, "data", sep = "") # PARKdata name
      DataNameVector <- append(DataNameVector, PARKname) # add PARKdata to list of data sets
  
  WD <- getwd() # store the working directory as variable "WD"
  DATApath <- paste(WD, x,sep = "/")  # create the full file path for the data set and store
                                      # as "filepath"
  
  park <- read.csv(DATApath) # Upload data
      assign(PARKname,park, envir = .GlobalEnv)  # Assign data the correct name "PARKdata"
            park <- NULL     # Clear "park" from variable
}

################################################################################################
