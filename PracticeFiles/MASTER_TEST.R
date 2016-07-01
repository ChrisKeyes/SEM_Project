# MATSTER TEST SCRIPT

source("~/SEM_Project/Script/getParkName.R")
source("~/SEM_Project/Script/getSEMvars.R")

SEMVars <- read.csv("~/SEM_Project/SEMvars.csv")
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
    assign(paste(PARK, "sem", sep = ""), PARKsem)
    assign(paste(PARK, "bads", sep = ""), PARKbads)
    assign(paste(PARK, "data", sep = ""), PARKdata)
    
  }
}