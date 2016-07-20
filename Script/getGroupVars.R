# This function will create a vector of the current 
# PARK's group vars

GROUPvars <- read.table("GROUPvars.csv", header = TRUE, sep = ",", na.strings = "")

SEGMENTvars <- na.omit(GROUPvars$SEGMENTvars)
TRIPvars <- na.omit(GROUPvars$TripToArea)

PARK_SegmentVars <- NULL
PARK_TripVars <- NULL

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

# EXPvars <- na.omit(GROUPvars$EXPENDITUREvars)
# PARK_ExpVars <- NULL
# for (VAR in EXPvars){
#   if (exists(VAR, where = PARKsem) == TRUE){
#     PARK_ExpVars <- append(PARK_ExpVars, VAR)
#   }
# }

rm(GROUPvars, SEGMENTvars, TRIPvars, VAR)
