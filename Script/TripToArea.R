#This script develops variables and statistics for Qs related to visitors trip to the area

PARKsem <- TESTdata

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

