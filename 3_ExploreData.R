###This script develops basic statistics and graphs to look at individual survey Qs

library(plyr)

###Do you live in the local area?###################################################

#Number of respondents who answered this Q
cnt <- length(na.omit(PARKsem$local))
cntsize <- paste("N =",cnt)

#Really what I want is a bar chart
#So the first step is to turn the 0's and 1's into factors with local, non-local as names

local <- factor(PARKsem$local, levels=c(0,1), labels=c("Non-Local", "Local"))
frqLocal <- count(na.omit(local))
frqLocal$per <- frqLocal$freq/cnt*100

#Note: the barplot command requires a vector. The xtabs creates a vector of counts of a categorical variable
#barplot(xtabs(~local),
#        #width = c(.1,.1),
#        space=.25,
#        horiz=T, 
#        col="black", 
#        las=1, 
#        main="Percent of local and non-local visitors")

barplot(frqLocal$per,
        #width = c(.1,.1),
        space=.25,
        horiz=T, 
        col="black", 
        las=1, 
        main="Percent of local and non-local visitors",
        names.arg = c(levels(local)))


