# This script will upload the GROUPvars.csv, then generate vectors of each group
# of variables for analysis.  Each group reflects a specific set of parameters to generate.
# For example, "SEGMENTvars" are the segment variable names relevant for the selected PARK.

###########################################################################################
# Set the working directory
setwd("~/SEM_Project")

# Upload "GROUPvars.csv" and save it as a data.frame named "GROUPvars"
GROUPvars <- read.table("GROUPvars.csv", 
                        header = TRUE, sep = ",", 
                        na.strings = "")

# Generate vectors of the GROUP variables for each column in GROUPvars. Do not select
# NULL values.
EXPvars <- na.omit(GROUPvars$EXPENDITUREvars)
SEGMENTvars <- na.omit(GROUPvars$SEGMENTvars)
TRIPvars <- na.omit(GROUPvars$TripToArea)

# For each set of GROUP variables, keep only those relevant to the park. For example,
# some parks have the segment "Cruise", while others do not. For each set of variables,
# those that are kept are stored in the vectors below (ex. PARK_ExpVars contains all
# relevant expenditure variables for the PARK)
PARK_ExpVars <- NULL
PARK_SegmentVars <- NULL
PARK_TripVars <- NULL

for (VAR in EXPvars){
  if (exists(VAR, where = PARKsem) == TRUE){
    PARK_ExpVars <- append(PARK_ExpVars, VAR)
  }
}


for (VAR in SEGMENTvars){
  if (exists(VAR, where = PARKsem) == TRUE){
    PARK_SegmentVars <- append(PARK_SegmentVars, VAR)
  }
}

for (VAR in TRIPvars){
  if (exists(VAR, where = PARKsem) == TRUE){
    PARK_TripVars <- append(PARK_TripVars, VAR)
  }
}

# Below is a temporary solution to deal with non-numeric expenditures
PARK_ExpVars <- PARK_ExpVars[PARK_ExpVars != "expOtherSpec" &
                             PARK_ExpVars != "DKexpLocal" &
                             PARK_ExpVars != "DKexpAdditional"]


###########################################################################################
