###########################################################################################
# This script generates the tables in the PARK report.

###########################################################################################
# Set working directory
setwd("~/SEM_Project")

# *****************************************************************************************
# Generate table 1 (spending profiles for expenditures per day by segments)
table1 <- PARKspending_MEANS[c(rownames(PARKspending_MEANS) != "Total_MeanExp"), SEGvars ]

for(EXP in rownames(table1)){for(VAR in SEGvars){
  if(VAR == "day_local" | VAR == "day_nonlocal"){
  table1[EXP, VAR] <- round(PARKspending_MEANS[EXP, VAR]/PARKlength_MEANS["daysLocalArea", VAR], 2)}
  else(table1[EXP, VAR] <-
      round(PARKspending_MEANS[EXP, VAR]/PARKlength_MEANS["nightsLocalArea", VAR], 2))}}
    
# Add a rowsum of total expenditures per day accross segments
table1$All_Visitors <- rowSums(table1)


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
    table2["Nights_LocalArea", VAR] <- 0
    table2["Days_Park", VAR] <- NA
    table2["Hours_Park", VAR] <- round(mean(PARKsegments[PARKsegments[,VAR]==1, "hoursPark"], na.rm = T), 2)}

else{ 
     table2["Days_LocalArea", VAR] <- NA
     table2["Nights_LocalArea", VAR] <- PARKlength_MEANS["nightsLocalArea", VAR]
     table2["Days_Park", VAR] <- PARKlength_MEANS["daysPark", VAR]
     table2["Hours_Park", VAR] <- NA
     }}

for(VAR in SEGvars){
if(VAR == "day_local"){table2["Non-LocalVisitors", VAR] <- NA}
  
else{
    table2["Non-LocalVisitors", VAR] <- 
      percent(sum(
        PARKsegments[PARKsegments$local == 0 & PARKsegments[,VAR]==1, VAR], na.rm = T)/
            length(PARKsegments[PARKsegments[,VAR]==1,VAR]))
}}

# Add a rowsum of total expenditures per day accross segments
# table2$All_Visitors <- rowSums(table2)

# *****************************************************************************************
# Generate table 3 (Party Day(Night) Attributable)




