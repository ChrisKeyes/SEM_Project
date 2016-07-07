# MATSTER TEST SCRIPT

source("~/SEM_Project/Script/getParkName.R")
source("~/SEM_Project/Script/getSEMvars.R")

SEMvars <- read.csv("~/SEM_Project/SEMvars.csv")
GROUPvars <- read.csv("~/SEM_Project/GROUPvars.csv")

setwd("~/SEM_Project/Data")

Parklist <- list.files()
DataNameVector <- c(NULL)


for (dataset in Parklist){
  x <- dataset 
  
  if(x != "TESTdata.csv"){
    
    PARK <- substr(x, 1, 4)
        PARKdata <- paste(PARK, "data", sep = "")
            DataNameVector <- append(DataNameVector, PARKdata)
    
    
    filename <- paste("~/SEM_Project/Data", x,sep = "/")
    
    PARKdata <- read.csv(filename)
    PARKname <-  PARK  
    
source("~/SEM_Project/PracticeFiles/Script1-TEST.R")
source("~/SEM_Project/PracticeFiles/Script2-TEST.R")
    
    setwd("~/SEM_Project/Output")
      PARKfile <- paste(getwd(),
                        PARK, sep = "/")
      setwd(paste(getwd(),
                  PARK, sep = "/"))

      
    write.csv(PARKsem, paste(PARK, "sem.csv", sep = ""), row.names = FALSE)
    write.csv(PARKbads, paste(PARK, "bads.csv", sep = ""), row.names = FALSE)
    write.csv(PARKdata, paste(PARK, "data.csv", sep = ""), row.names = FALSE)
    
    
    # assign(paste(PARK, "sem", sep = ""), PARKsem)
    # assign(paste(PARK, "bads", sep = ""), PARKbads)
    # assign(paste(PARK, "data", sep = ""), PARKdata)
    
  }
  
}

setwd("~/SEM_Project/Output")

write.csv(SEMvars, paste(getwd(),
                         "SEMvars.csv", sep = "/") , 
                          row.names = FALSE)

write.csv(CHECKvars, paste(getwd(),
                         "CHECKvars.csv", sep = "/") , 
                          row.names = FALSE)

write.csv(GROUPvars, paste(getwd(),
                         "GROUPvars.csv", sep = "/") , 
                          row.names = FALSE)


rm(list=setdiff(ls(), c("SEMvars","CHECKvars", "GROUPvars", "DataNameVector"))) 

