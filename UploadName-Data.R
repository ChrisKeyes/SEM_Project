# This is the same code as for MainR, but written to loop over all files in Data folder

# Upload, rename, & subset script for SEM variables

# This is the first set of script for the SEM project.  Run this script in its entirety to 
# upload the datasets within the "data" folder.
# Each data set will be stored in memory and renamed to its four letter park name (i.e. "ACAD").
# The park data will then be subset into a new dataframe named "PARKsem" (i.e. "ACADsem") for cleaning.
# All data frames and objects are stored to memory, and original CSV files are unchanged.
# The variables used for subsetting are found in the SEMvars.csv file.  All variables kept are renamed to 
# respective SEM variable name for consistancy. 

################################################################################################


source("~/SEM_Project/Script/getParkName.R")
source("~/SEM_Project/Script/getSEMvars.r")

setwd("~/SEM_Project/Data")

Parklist <- list.files()

SEMVars <- read.csv("~/SEM_Project/SEMvars.csv")
SEMVarsResults <- SEMVars

SearchVars <- SEMVars[,2]

#     VarID <- data.frame(c(1:length(SearchVars)))
#     colnames(VarID) <- c("VarID")
# SEMVarsResults <- data.frame(VarID, SEMVars)

# z <- Parklist[1]

for (z in Parklist) {
  
  
  ParkVars <- NULL
  TempVars <- NULL
  Matches <- NULL
  MMatches <- NULL
  
  ParkName <- substr(z, 1, 4)
  
  WD = getwd()
  filename <- paste(WD, z,sep = "/")
  
  park <- read.csv(filename)

  Varnames <- colnames(park)
  
  for (t in SearchVars){
    x <- paste(t, "$", sep = "")
    TempVars <- Varnames[grepl( x ,Varnames, ignore.case = TRUE)]
    
    if (length(TempVars)== 1){
      ParkVars <- append(ParkVars, TempVars)
      Matches <- append(Matches, (any(grepl(x, Varnames))))
    }
    else if (length(TempVars) == 0){
      Matches <- append(Matches, (any(grepl(x, Varnames))))
    }
    else {
      MMatches <- append(MMatches, TempVars)
      Matches <- append(Matches, NA)
    }
  }
  
  # SearchResults <- cbind(SearchVars, Matches, stringsAsFactors = as.data.frame.AsIs(SearchVars))
 SEMVarsResults <- data.frame(SEMVarsResults, Matches) 
     colnames(SEMVarsResults)[c(which(colnames(SEMVarsResults) == "Matches"))] <- c(paste(ParkName, "match",sep = ""))
  
  PARKsem <- park[c(ParkVars)]  
  
  # assign(ParkName, park)    # Keep this line if we want the raw data loaded to memory
  assign(paste(ParkName, "sem", sep = ""), PARKsem)
  assign(paste(ParkName, "MM", sep = ""), MMatches)
  

  }

SEMVars <- NULL
PARKsem <- NULL
park <- NULL
ParkName <- NULL
ParkVars <- NULL
Matches <- NULL
MMatches <- NULL
t <- NULL
x <- NULL
z <- NULL


# Remaining:  
#### set rules for reviewing PARKMM vectors and merging those variables back into PARKsem
#### wait until we have more data to set rules


