#This script writes average party size estimates for each visitor segment
#The script outputs the dataframe PARKparty_MEANS

setwd("~/SEM_Project")
source("4_Variable-Generation.R")

#build an empty dataframe that has relevant segments as columns
#and length of stay related variables as rows
SEGvars_on <- paste("ON", substring(PARK_SegmentVars, 7), sep = "_")            
SEGvars_day <- c("day_local", "day_nonlocal")
SEGS <- c(SEGvars_day,SEGvars_on)
PARKparty_MEANS <- data.frame(matrix(ncol = length(SEGS), nrow=0))
colnames(PARKparty_MEANS)<-SEGS

#subset the PARKsegments dataframe to only include columns needed for party size analysis
VARS <- c("grpSize","adultsCovered","childrenCovered","numSplit", SEGS)
df <- subset(PARKsegments, select = VARS)

#remove bad observations for party size analysis
df <- subset(df, !is.na(df$adultsCovered))

#create a new variable totalCovered
df$totalCovered <- rowSums(df[,c("adultsCovered","childrenCovered")], na.rm=T)

#Loop through segment types to calculate average party size values by segment 
#and print them to the PARKparty_MEANS dataframe
for (s in SEGS){
  PARKparty_MEANS["n",s] <- sum(df[,s])
  PARKparty_MEANS["GroupSize",s] <- round(mean(df[df[,s]==1,"grpSize"]),2)
  #PARKparty_MEANS["AdultsCovered",s] <- round(mean(df[df[,s]==1,"adultsCovered"]),2)
  #PARKparty_MEANS["ChildrenCovered",s] <- round(mean(df[df[,s]==1,"childrenCovered"]),2)
  PARKparty_MEANS["TotalCovered",s] <- round(mean(df[df[,s]==1,"totalCovered"]),2)
  PARKparty_MEANS["NumberSplit",s] <- round(mean(df[df[,s]==1,"numSplit"],na.rm=T),2)
}
View(PARKparty_MEANS)