# Remove from PARK_ExpVars variables which are non-numeric or boolean

# This function will create a vector of the current 
# PARK's group vars

GROUPvars <- read.table("GROUPvars.csv", header = TRUE, sep = ",", na.strings = "")

EXPvars <- na.omit(GROUPvars$EXPENDITUREvars)

PARK_ExpVars <- NULL

for (VAR in EXPvars){
  if (exists(VAR, where = PARKsem) == TRUE){ 
    PARK_ExpVars <- append(PARK_ExpVars, VAR)
  }
}

# Below is a temporary solution to deal with non-numeric expenditures
PARK_ExpVars <- PARK_ExpVars[PARK_ExpVars != "expOtherSpec" &
                               PARK_ExpVars != "DKexpLocal"]

rm(GROUPvars, EXPvars, VAR)