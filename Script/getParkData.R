
# this gets the dataframe from a file
getParkData <- function(filename) {
  if (file.exists(filename)) {
    parkdf <- read.csv(file=filename)
  }
  else {
    stop("File does not exist")
  }
  return(parkdf)
}

# Have not found a use for this code yet