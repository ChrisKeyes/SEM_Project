# This script will take the cleaned data (PARKsem) and variables generated from script #4 
# and create spending profiles and analysize the summary statistics

setwd("~/SEM_Project")

expVARS <- colnames(PARKbads[grepl(paste("^", "exp*", sep = ""),colnames(PARKbads))])
nightsVARS <- colnames(PARKbads[grepl(paste("^", "nights*", sep = ""), colnames(PARKbads))])

PARKexp <- as.character(strsplit(expVARS, "_1"))
PARKnights <- as.character(strsplit(nightsVARS, "_1"))

for (x in PARKnights){
  PARKsegments[,x] <- ifelse(is.na(PARKsegments[,x]),0,PARKsegments[,x])
}

for (x in PARKexp){
  PARKsegments[,x] <- ifelse(is.na(PARKsegments[,x]),0,PARKsegments[,x])
}

PARKspending_MEANS <- data.frame(EXPENDITURES = PARKexp)


a <- NULL # colnames by segment
b <- NULL # mean values
segmentVARS <- NULL # segment names

for (x in PARKnights){
  v <- paste(substring(x, 7), "nonlocal", sep = "_")
  segmentVARS <- append(segmentVARS, v)
}

for (x in segmentVARS){
  
  z <- subset(PARKsegments, PARKsegments[,x] == 1, select = c(PARKexp))
  expV <- colnames(z)
  
  # for (y in 1:length(expV)){
  #     avg <- mean(z[,y])
  #         b <- append(b, avg)
  # }
  
  for (y in expV){
    avg <- mean(z[,y])
    b <- append(b, avg)
  }
  
  
  df <- data.frame(EXPENDITURES = PARKexp, x = b)
  colnames(df) <- c("EXPENDITURES" , x)
  
  PARKspending_MEANS <- merge(PARKspending_MEANS, df, by = c("EXPENDITURES"))
  b <- NULL
}

for (x in c("day_local", "overnight_local", "day_nonlocal")){
  tempDF <-subset(PARKsegments, PARKsegments[x]==1, select = c(PARKexp))
  
  for (y in colnames(tempDF)){
    avg <- mean(tempDF[,y])
    b <- append(b, avg)
  }
  df <- data.frame(EXPENDITURES = PARKexp, x = b)
  colnames(df) <- c("EXPENDITURES", x)
  
  PARKspending_MEANS <- merge(PARKspending_MEANS, df , by = c("EXPENDITURES"))
  b <- NULL
}






