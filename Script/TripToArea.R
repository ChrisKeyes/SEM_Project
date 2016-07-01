#This script develops variables and statistics for Qs related to visitors trip to the area



setwd("~/SEM_Project")
source("2_Cleaning.R")

PARKsem <- TESTdata

#build a subset dataframe from PARKsem
df <- data.frame(PARKsem$ID, row.names = NULL)
colnames(df) <- "ID"
df$ID <- sort(df$ID, decreasing = FALSE)

GROUPvars <- read.csv("GROUPvars.csv")
TripToAreaVars <- GROUPvars$TripToArea 
TripToAreaVars <- na.omit(TripToAreaVars)

#Cathy, start here. You are trying to build a data frame that has all of the columns indicated in the
#GROUPvars csv.

# for (y in 1:length(TripToAreaVars)){
#   var <- TripToAreaVars[y]
#   df$var <- PARKsem[,var]
# }

# colnames(df,c("ID",TripToAreaVars))

CHRISdf <- data.frame(PARKsem[,match(TripToAreaVars, colnames(PARKsem))]) # This should do it


#next remove bads
#...

#Evaluate the time spent in the park
#associated SEMvars are hoursPark and daysPark. 
#create a new variable daysParkAdj that estimates number of days in the park

summary(PARKsem$hoursPark)
# convert hoursPark to factor to see levels of answers
hoursParkF <- factor(CUVAsem$hoursPark)
hoursParkF
levels(hoursParkF)
summary(hoursParkF)
plot(hoursParkF, main="Hours spent in park")
#who are the people who spent 25 hours in the park?
check <- PARKbads[with(PARKbads, hoursPark_2==1), c("ID")] 

#I'm remaking a bads matrix here (temporary) to play with 
Bads <- data.frame(PARKsem$ID, row.names = NULL)
colnames(Bads) <- "ID"
Bads$ID <- sort(Bads$ID, decreasing = FALSE)

#Mark hoursPark na in Bads
Bads$hoursPark_NA <- ifelse(is.na(PARKsem$hoursPark),1,0)

#Mark daysPark na in Bads
Bads$daysPark_NA <- ifelse(is.na(PARKsem$daysPark),1,0)

sum(Bads$daysPark_NA)
sum(Bads$hoursPark_NA)
#this equals 866, but the length of the df is 865, so 1 person must have had na in both responses

PARKsem$daysParkAdj <- PARKsem[Bads$hoursPark_NA == 1 & Bads$daysPark_NA == 0,"daysPark"]

