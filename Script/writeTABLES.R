# This script will write the tables generated through the
# SEM analysis to .csv files.

###########################################################################################

# Store tables to PARK's "Output" folder

# set the working directory as the folder to write csv to
setwd(paste(paste("~/SEM_Project/Output", PARKname, sep = "/"), "Segments_Full", sep = "/"))

# Write the PARKsem and PARKbads data frames to .csv
write.csv(PARKsegments_SHARES, paste(PARKname, "segment_SHARES_full.csv", sep = ""), row.names = TRUE) 
write.csv(PARKlength_MEANS, paste(PARKname, "length_MEANS_full.csv", sep = ""), row.names = TRUE) 
write.csv(PARKreEntry_MEANS, paste(PARKname, "reEntry_MEANS_full.csv", sep = ""), row.names = TRUE) 
write.csv(PARKparty_MEANS, paste(PARKname, "party_MEANS_full.csv", sep = ""), row.names = TRUE) 
write.csv(PARKspending_MEANS, paste(PARKname, "spending_MEANS_full.csv", sep = ""), row.names = TRUE) 

# Set the working directory 
setwd("~/SEM_Project")
