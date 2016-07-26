# This script produces a popup selection box.  The user must select a park to begin 
# analysis.

################################################################################################

# dglList function will generate a popup window with the list of parks names (four letter PARK 
# names) from the data sets present in the "Data" folder. The user response will be
# stored to memory as "PARKname"

PARKname <- dlgList(substr(Parklist, 1, 4),
               multiple = FALSE, # change to TRUE to select multiple parks
               title = "Select Park(s)")$res

if (!length(PARKname)) {
  cat("You cancelled the choice\n")
} else {
  cat("You selected:\n")
  print(PARKname)
}

# need to finish assigning PARKname to the data set