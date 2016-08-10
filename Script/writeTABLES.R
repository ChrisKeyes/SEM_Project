# This script will write the tables generated through the
# SEM analysis to .csv files.

###########################################################################################

# Store tables to PARK's "Output" folder
# Check for Full or Lumped segments and write data to appropriate folder destination
if(Choice.SEG == "no"){
  
# set the working directory as the folder to write csv to
setwd(paste(paste("~/SEM_Project/Output", PARKname, sep = "/"), "Segments_Full", sep = "/"))

# Write the PARKsem and PARKbads data frames to .csv
write.csv(PARKsegments_SHARES, paste(PARKname, "segment_SHARES.csv", sep = ""), row.names = TRUE) 
write.csv(PARKlength_MEANS, paste(PARKname, "length_MEANS.csv", sep = ""), row.names = TRUE) 
write.csv(PARKreEntry_MEANS, paste(PARKname, "reEntry_MEANS.csv", sep = ""), row.names = TRUE) 
write.csv(PARKparty_MEANS, paste(PARKname, "party_MEANS.csv", sep = ""), row.names = TRUE) 
write.csv(PARKspending_MEANS, paste(PARKname, "spending_MEANS.csv", sep = ""), row.names = TRUE) 
write.csv(PARKexpBYparty_MEANS, paste(PARKname, "expenditure_party-daynight.csv", sep = ""), row.names = TRUE) 

# Set the working directory 
setwd("~/SEM_Project")
}

if(Choice.SEG == "yes"){
  
# set the working directory as the folder to write csv to
setwd(paste(paste("~/SEM_Project/Output", PARKname, sep = "/"), "Segments_Lumped", sep = "/"))

# Write the PARKsem and PARKbads data frames to .csv
write.csv(PARKsegments_SHARES, paste(PARKname, "segment_SHARES_Lumped.csv", sep = ""), row.names = TRUE) 
write.csv(PARKlength_MEANS, paste(PARKname, "length_MEANS_Lumped.csv", sep = ""), row.names = TRUE) 
write.csv(PARKreEntry_MEANS, paste(PARKname, "reEntry_MEANS_Lumped.csv", sep = ""), row.names = TRUE) 
write.csv(PARKparty_MEANS, paste(PARKname, "party_MEANS_Lumped.csv", sep = ""), row.names = TRUE) 
write.csv(PARKspending_MEANS, paste(PARKname, "spending_MEANS_Lumped.csv", sep = ""), row.names = TRUE) 
write.csv(PARKexpBYparty_MEANS, paste(PARKname, "expenditure_party-daynight_Lumped.csv", sep = ""), row.names = TRUE) 

# Set the working directory 
setwd("~/SEM_Project")
  
}