# MATSTER TEST SCRIPT

source("~/SEM_Project/Script/getParkName.R")
source("~/SEM_Project/Script/getSEMvars.R")

SEMvars <- read.csv("~/SEM_Project/SEMvars.csv", header = TRUE)
GROUPvars <- read.csv("~/SEM_Project/GROUPvars.csv", header = TRUE)

setwd("~/SEM_Project/Data")

Parklist <- list.files()
DataNameVector <- c(NULL)


for (dataset in Parklist){
  DATA <- dataset 
  
  if(DATA != "TESTdata.csv"){
    
    PARK <- substr(DATA, 1, 4)
        PARKdata <- paste(PARK, "data", sep = "")
            DataNameVector <- append(DataNameVector, PARKdata)
    
    
    filename <- paste("~/SEM_Project/Data", DATA,sep = "/")
    
    PARKdata <- read.csv(filename, header = TRUE)
    PARKname <-  PARK  
    
source("~/SEM_Project/PracticeFiles/Script1-TEST.R")

          if( PARK == "YOSE"){
            PARKsem$local <- abs(PARKsem$local - 2)
          }

source("~/SEM_Project/PracticeFiles/Script2-TEST.R")

source("~/SEM_Project/PracticeFiles/Script4-TEST.R")
    
source("~/SEM_Project/PracticeFiles/Script5-TEST.R")
    
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

    
   
    write.csv(PARKsegments, paste(PARK, "segments.csv", sep = ""), row.names = FALSE)
    write.csv(PARKshares_table, paste(PARK, "shares_table.csv", sep = ""), row.names = FALSE)
    write.csv(PARKspending_MEANS, paste(PARK, "mean_expenditures.csv", sep = ""), row.names = FALSE)
    
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


#rm(list=setdiff(ls(), c("SEMvars","CHECKvars", "GROUPvars", "DataNameVector")))

