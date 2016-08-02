# This script produces a popup selection box.  The user must select a park to begin 
# analysis.

################################################################################################

# dglList function will generate a popup window with the list of parks names (four letter PARK 
# names) from the data sets present in the "Data" folder. The user response will be
# stored to memory as "PARKname"

# Set working directory to the data folder
setwd("~/SEM_Project/Data")

# Save list of park datasets (filenames of each .csv) in Data folder
Parklist <- list.files()


PARK.select <- dlgList(substr(Parklist, 1, 4),
               multiple = FALSE, # change to TRUE to select multiple parks
               title = "Select Park(s)")$res

if (!length(PARK.select)) {
  cat("You cancelled the choice\n")
} else {
  cat("You selected:\n")
  print(PARK.select)
}


# Loop over all data set names and truncate the filename to the four letter park name. Upload
# each data set, assign it the corresponding "PARKdata" name and store a vector (list) of the 
# dataset names in memory called "DataNameVector"

for (x in Parklist){
  
  PARKname <- substr(x, 1, 4)   # PARK name
  
  if(PARKname == PARK.select){
    
      PARKdata <- paste(PARKname, "data", sep = "") # PARKdata name

  WD <- getwd() # store the working directory as variable "WD"
  DATApath <- paste(WD, x,sep = "/")  # create the full file path for the data set and store
                                      # as "filepath"
  
  park <- read.csv(DATApath) # Upload data
      
  PARKdata <- assign(PARKdata,park, envir = .GlobalEnv)  # Assign data the correct name "PARKdata"
            park <- NULL     # Clear "park" from variable
}}

PARKname <- PARK.select