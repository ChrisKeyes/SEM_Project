# This script will "lump" segments together when sample size is less than 30 observations.
# The lumping (consolidation) of segments is based on segment charracteristics.  The 
# current scheme is:
#
#     ON_Camp = ON_CampIn + ON_CampOut
#     ON_CampIn = ON_CampIn + ON_Backcountry
#     ON_Lodge = ON_LodgeIn + ON_LodgeOut
#     ON_Other = ON_Other + *any other segments


###########################################################################################

# Set working directory
setwd("~/SEM_Project")


# Identify which segments have <30 observations and store in vector named small_SEGMENTS
#     NOTE: Only checking "overnight" segments for simplicity and under assumption that 
#     day segments will always be sufficiently large samples.
small_SEGMENTS <- NULL

for (VAR in SEGvars_on){
  if (as.numeric(PARKsegments_SHARES["n", VAR]) < 30){ small_SEGMENTS <- append(small_SEGMENTS, VAR)}}

# Use "small_SEGMENTS" to lump together segments and generate vector of lumped segment 
# variable names

for (VAR in small_SEGMENTS){
  if(VAR == "ON_Backcountry"){
    PARKsegments$ON_CampIn <- PARKsegments$ON_CampIn + PARKsegments$ON_Backcountry
    SEGvars_LUMPED <- SEGvars_LUMPED[SEGvars_LUMPED != VAR]}
  else if(VAR == "ON_Cruise"){
    PARKsegments$ON_Other <- PARKsegments$ON_Other + PARKsegments
    SEGvars_LUMPED <- SEGvars_LUMPED[SEGvars_LUMPED != VAR]
  }}

for (VAR in small_SEGMENTS){ 
  
  if (VAR == "ON_CampIn" | VAR == "ON_CampOut"){
    PARKsegments$ON_Camp <- 
          ifelse((PARKsegments$ON_CampIn == 1 | PARKsegments$ON_CampOut == 1), 1, 0)
    
    SEGvars_LUMPED <- SEGvars_LUMPED[SEGvars_LUMPED != "ON_CampIn" & SEGvars_LUMPED != "ON_CampOut"]
    SEGvars_LUMPED <- append(SEGvars_LUMPED, c("ON_Camp"))}
  
  else if (VAR == "ON_LodgeIn" | VAR == "ON_LodgeOut"){
    PARKsegments$ON_Lodge <- 
          ifelse((PARKsegments$ON_LodgeIn == 1 | PARKsegments$ON_LodgeOut == 1), 1, 0)
    
    SEGvars_LUMPED <- SEGvars_LUMPED[SEGvars_LUMPED != "ON_LodgeIn" & SEGvars_LUMPED != "ON_LodgeOut"]
    SEGvars_LUMPED <- append(SEGvars_LUMPED, c("ON_Lodge"))}}    

SEGvars_LUMPED <- unique(SEGvars_LUMPED)


    
 