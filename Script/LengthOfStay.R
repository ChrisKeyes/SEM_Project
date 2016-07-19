#This script writes average length of stay estimates for each visitor segment
#The script outputs the dataframe PARKlength_MEANS

setwd("~/SEM_Project")
library(scales)
source("4_Variable-Generation.R")

#build an empty dataframe that has relevant segments as columns
#and length of stay related variables as rows
SEGvars_on <- paste("ON", substring(PARK_SegmentVars, 7), sep = "_")            
SEGvars_day <- c("day_local", "day_nonlocal")
SEGS <- c(SEGvars_day,SEGvars_on)
PARKlength_MEANS <- data.frame(matrix(ncol = length(SEGS), nrow=0))
colnames(PARKlength_MEANS)<-SEGS

#subset the PARKsegments dataframe to only include columns needed for length of stay analysis
VARS <- c("hoursPark","daysPark","nightsLocalArea", SEGS)
df <- subset(PARKsegments, select = VARS)

#remove bad observations for length of stay analysis
df$bads <- PARKbads_seg$hoursPark_2 + PARKbads_seg$daysPark_1 + PARKbads_seg$daysPark_2
df <- subset(df, df$bads == 0)

#create a new variable daysParkAdj that estimates number of days in the park
#Note: If a respondent answered both hours park and days park, then use days park (ignore hours park)
#Round partial days up to nearest whole number
df$hoursParkAdj <- ifelse(df$hoursPark >0,1,0)
df$daysParkAdj <- ifelse(!is.na(df$daysPark),df$daysPark, df$hoursParkAdj)
df$daysParkAdj <- ceiling(df$daysParkAdj)

#create a new variable daysLocalArea equal to nightsLocalArea + 1
df$daysLocalArea <- df$nightsLocalArea +1

#Check to make sure that daysParkAdj <= daysLocalArea and drop bad observation
df$check <- ifelse(df$daysParkAdj <= df$daysLocalArea,0,1)
df <- subset(df, df$check == 0)

#Loop through segment types to calculate average length of stay values by segment 
#and print them to the PARKlength_MEANS dataframe
for (s in SEGS){
  PARKlength_MEANS["n",s] <- sum(df[,s])
  PARKlength_MEANS["daysPark",s] <- round(mean(df[df[,s]==1,"daysParkAdj"]),2)
  PARKlength_MEANS["daysLocalArea",s] <- round(mean(df[df[,s]==1,"daysLocalArea"]),2)
  PARKlength_MEANS["nightsLocalArea",s] <- round(mean(df[df[,s]==1,"nightsLocalArea"]),2)
  #For lenghtVSE, use daysLocalArea for day trips and nightLocalArea for overnight trips
   ifelse(s == "day_local"|s == "day_nonlocal", 
          PARKlength_MEANS["lengthVSE",s] <- round(mean(df[df[,s]==1,"daysLocalArea"]),2),
          PARKlength_MEANS["lengthVSE",s] <- round(mean(df[df[,s]==1,"nightsLocalArea"]),2))
  PARKlength_MEANS["percentDaysInPark",s] <- percent(mean(df[df[,s]==1,"daysParkAdj"])/mean(df[df[,s]==1,"daysLocalArea"]))
}
View(PARKlength_MEANS)

#remove unnecessary variables
#rm(df,s,SEGS,SEGvars_day,SEGvars_on,VARS)
