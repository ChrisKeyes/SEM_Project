###########################################################################################
# This script generates the tables in the PARK report.

###########################################################################################
# Set working directory
setwd("~/SEM_Project")

# *****************************************************************************************
# Generate table 1 (spending profiles for expenditures per day by segments)
table1 <- PARKspending_MEANS[c(rownames(PARKspending_MEANS) != "Total_MeanExp"), SEGvars ]


for(EXP in rownames(table1)){
for(VAR in SEGvars){
  
  if(VAR == "day_local" | VAR == "day_nonlocal"){
    table1[EXP, VAR] <- round(PARKspending_MEANS[EXP, VAR]/
                              PARKlength_MEANS["daysLocalArea", VAR], 2)}

  else{
    table1[EXP, VAR] <- round(PARKspending_MEANS[EXP, VAR]/
                              PARKlength_MEANS["nightsLocalArea", VAR],2)
}}}

for(VAR in SEGvars){
    
  if(VAR == "day_local" | VAR == "day_nonlocal"){
    table1["Avg.Spending/Party_Day", VAR] <- sum(table1[,VAR], na.rm = TRUE)}

  else{
    table1["Avg.Spending/Party_Night", VAR] <- sum(table1[,VAR], na.rm = TRUE)
}}

# Add a rowsum of total expenditures per day accross segments
table1$All_Visitors <- rowSums(table1, na.rm = TRUE)

# CHRIS - use cathy's script to re-order PARK_exp variable names (row names)

# *****************************************************************************************
# Generate table 2 (trip charracteristics table)
table2 <- data.frame(matrix(ncol = length(SEGvars), nrow = 0))
    colnames(table2) <- SEGvars

table2["Segment_Shares",] <- PARKsegments_SHARES["SEGMENT_share",]
table2["Re-entry_Rate",] <- PARKreEntry_MEANS["Party_reEntry",]
table2["Party_Size",] <- PARKparty_MEANS["TotalCovered",]

for(VAR in SEGvars){
if(VAR == "day_local" | VAR == "day_nonlocal"){
    table2["Days_LocalArea", VAR] <- PARKlength_MEANS["daysLocalArea", VAR]
    table2["Nights_LocalArea", VAR] <- NA
    table2["Hours_Park", VAR] <- round(mean(
            PARKsegments[PARKsegments[,VAR]==1, "hoursPark"], na.rm = T), 2)
    table2["Days_Park", VAR] <- NA}

else { 
     table2["Days_LocalArea", VAR] <- NA
     table2["Nights_LocalArea", VAR] <- PARKlength_MEANS["nightsLocalArea", VAR]
     table2["Hours_Park", VAR] <- NA
     table2["Days_Park", VAR] <- PARKlength_MEANS["daysPark", VAR]}}

for(VAR in SEGvars){
    table2["Non-LocalVisitors", VAR] <- percent(sum(
        PARKsegments[PARKsegments$local == 0 & PARKsegments[,VAR]==1, VAR], na.rm = T)/
              length(PARKsegments[PARKsegments[,VAR]==1,VAR]))}


# Add a rowsum of total expenditures per day accross segments
# table2$All_Visitors <- rowSums(table2)

# *****************************************************************************************
# Generate table 3 (Party Day(Night) Attributable)

table3 <- data.frame(matrix(ncol = length(SEGvars), nrow = 0))
    colnames(table3) <- SEGvars
    
# Write line to pull in total visitation values from PAM's .csv file
RECvisits <- 2284612 # CUVA visitors

table3["Recreation_Visits",] <- RECvisits*SEGwghts
table3["Party_Trips",] <- round(table3["Recreation_Visits",]/
                                        PARKreEntry_MEANS["Party_reEntry",]/
                                        PARKparty_MEANS["TotalCovered",] , 2)
table3["Total.Days_ATTR",] <- table3["Party_Trips",]*PARKlength_MEANS["DaysAttributable",]
table3["Total.Nights_ATTR",] <- table3["Party_Trips",]*PARKlength_MEANS["NightsAttributable",]

# Add column "All_Visitors" which is row sum across segments
table3$All_Visitors <- round(rowSums(table3[,SEGvars], na.rm = TRUE), 2)
    
# # Replace Avg.Days & Nigts ATTR for all visitors with weighted average
# table3["Total.Days_ATTR", "All_Visitors"] <- round(AVG.daysATTR, 2)
# table3["Total.Nights_ATTR", "All_Visitors"] <- round(AVG.nightsATTR, 2)
    
    
# *****************************************************************************************
# Generate table 4 (Visitor Spending Estimates)
PARKexp <- PARK_ExpVars[PARK_ExpVars !="expLocalTotal"]

table4 <- data.frame(matrix(ncol = 2, nrow = length(PARKexp)))
    colnames(table4) <- c("Spending_All.Visitors", "Spending_nonLocal.Visitors")
    rownames(table4) <- PARKexp
    
    
tempDF <- table1[PARKexp, SEGvars]

for(EXP in rownames(tempDF)){for(VAR in SEGvars){
    if(VAR == "day_local" | VAR == "day_nonlocal"){
         tempDF[EXP, VAR] <- table1[EXP, VAR]*table3["Total.Days_ATTR",VAR]}
    
    else(tempDF[EXP, VAR] <- table1[EXP, VAR]*table3["Total.Nights_ATTR",VAR])}}
      
tempDF$TOTAL <- rowSums(tempDF[SEGvars]) 
 
table4$Spending_All.Visitors <- tempDF$TOTAL    

# To find total spending for non_local visitors, generate spending profiles for non-local
# visitor segments.

tempDF <- subset(PARKsegments, local == 0)
    
PARKspending_MEANS.NL <- data.frame(matrix(ncol = length(SEGvars), nrow = length(PARKexp)))
  colnames(PARKspending_MEANS.NL) <- SEGvars
  rownames(PARKspending_MEANS.NL) <- PARKexp
  
for(EXP in PARKexp){for(VAR in SEGvars){
    PARKspending_MEANS.NL[EXP, VAR] <- mean(tempDF[tempDF[VAR] == 1, EXP])}} 
  
for(EXP in PARKexp){for(VAR in SEGvars){
  
  if(VAR == "day_local"){PARKspending_MEANS.NL[EXP, VAR] <- 0}
    
  else if(VAR == "day_nonlocal"){  
     PARKspending_MEANS.NL[EXP, VAR] <- PARKspending_MEANS.NL[EXP, VAR]/
                                        PARKlength_MEANS.NL["daysLocalArea", VAR]
     PARKspending_MEANS.NL[EXP, VAR] <- PARKspending_MEANS.NL[EXP, VAR]*
                                        table3["Total.Days_ATTR", VAR]}
  else{
     PARKspending_MEANS.NL[EXP, VAR] <- PARKspending_MEANS.NL[EXP, VAR]/
                                        PARKlength_MEANS.NL["nightsLocalArea", VAR]
     PARKspending_MEANS.NL[EXP, VAR] <- PARKspending_MEANS.NL[EXP, VAR]*
                                        table3["Total.Nights_ATTR", VAR]}
}}

PARKspending_MEANS.NL$TOTAL <- rowSums(PARKspending_MEANS.NL[SEGvars])

table4$Spending_nonLocal.Visitors <- PARKspending_MEANS.NL$TOTAL

table4["Total_Visitor_Spending",] <- colSums(table4[c(colnames(table4))])

# *****************************************************************************************
View(table1)
View(table2)
View(table3)
View(table4)

# Write tables to .csv files in the output folder: Output/PARK/ReportTables
file.path <- paste(getwd(), paste("Output", paste(PARKname, "Report_Tables", sep = "/"), sep = "/"), sep = "/")

write.csv(table1, paste(file.path, "table1.csv", sep = "/"), row.names = TRUE) 
write.csv(table2, paste(file.path, "table2.csv", sep = "/"), row.names = TRUE) 
write.csv(table3, paste(file.path, "table3.csv", sep = "/"), row.names = TRUE) 
write.csv(table4, paste(file.path, "table4.csv", sep = "/"), row.names = TRUE) 

