# This script will generate the re-entry rates for PARK

# These are the checks from script 2:

        # entries_1: check for entries == NA & DKentries == NA
        # These respondents refused to answer the question
                
        # entries_2: check for ReEnter >= 1 and DKReEnter == 99
        # These respondents provided an estimate of entries, but also noted that they "did not know"

        # DKentries_1: check if respondent only answered "Dont Know"

# Drop observations where 
        # PARKbads$entries_1 = 1
        # PARKbads$DKentries_1 = 1
# Also drop observations where entries == 0  

# Clear all data in memory
rm(list=union(ls(), ls()))

# Set working directory
setwd("~/SEM_Project")

source("4_Variable-Generation.R")
source("5_Spending&Analysis.R")

tempDF <- data.frame(PARKsegments)

tempDF$sumBADSentries <- PARKbads_seg$entries_1 + PARKbads_seg$DKentries_1

tempDF <- subset(tempDF,
                 sumBADSentries == 0 &
                   entries > 0,
                 select = 
                   c("entries", c(SEGvars_day, SEGvars_on)))
      
      colnames(tempDF) <- c("entries", SEGvars)
      
b <- NULL # b: vector of mean entry by party segment

for (VAR in SEGvars){
b <- append(b, mean(tempDF["entries"][tempDF[VAR]==1]))
}

PARKentries_table <- data.frame(Mean_reEntry = b)
                    row.names(PARKentries_table) = SEGvars  

View(PARKentries_table)

rm(tempDF, b, BADids, VAR )
