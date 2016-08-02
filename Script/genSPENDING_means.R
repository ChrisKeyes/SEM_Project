# This script completes two tasks. 
# 
# First, generate the spending profiles by segment type. The spending profiles 
# are the mean expenditures per party, by segment type. The mean values will be stored to
# a data.frame named "PARKspending_MEANS".
# 
# Second, generate average expenditure per party per day and per party per night. For day
# visitors, average expenditure is kept in party/day units.  For overnight visitors, average
# expenditure is in party/nights units. In both cases, the average expenditure values are
# adjusted to account for party/days or party/nights attributable to the park.  This 
# adjustment is made in the "genLENGTH_means.R" script.

###########################################################################################
#***** PART I: Generate Spending Profiles by Segment **************************************

# Set working directory
setwd("~/SEM_Project")

# Loop over the expenditure variables for the park (contained in PARK_ExpVars), and for 
# each expenditure variable, if the observation is NA, replace with a zero.  The assumption
# here being that if the respondant refused to answer (provide a number) for the given 
# expenditure category, then the respondant did not spend any money in that category.
for (VAR in PARK_ExpVars){
  PARKsegments[,VAR] <- ifelse(is.na(PARKsegments[,VAR]),0,PARKsegments[,VAR])
}

# Generate a data.frame named "PARKspending_MEANS". This will store the expenditure means. 
# The columns will be the segment variables, and the rows will be the expenditures.
PARKspending_MEANS <- data.frame(matrix(
                                      ncol = length(SEGvars),
                                      nrow = length(PARK_ExpVars)))
colnames(PARKspending_MEANS) <- SEGvars
row.names(PARKspending_MEANS) <- PARK_ExpVars

# Loop over the segment variables, and generate the mean expenditures.
# NOTE: The mean values are the mean expenditure per party, by segment type. NOT per
# person spending averages. 

for (VAR in SEGvars){
  for (EXP in PARK_ExpVars){
    PARKspending_MEANS[EXP, VAR] <- round(mean(
          PARKsegments[PARKsegments[,VAR]==1, EXP]), 2)
}}
  

# If the expenditure variable named: "expLocalTotal" is in PARKsegments,
# remove it from PARKspending_MEANS. 
if(exists("expLocalTotal", where = PARKsegments) == TRUE){
    PARKspending_MEANS <- 
      subset(PARKspending_MEANS, row.names(PARKspending_MEANS) != "expLocalTotal")
}

# Attach a row to PARKspending_MEANS which is the sum of the average expenditures 
# by party (column sum)
for (VAR in SEGvars){
PARKspending_MEANS["Total_MeanExp", VAR] <- 
                      sum(PARKspending_MEANS[, VAR], na.rm = TRUE)}


# Attach a column to PARKspending_MEANS which is the weighted average of each 
# expenditure category. 

# A vector of weights from the "PARKshares_table" (the row SEGMENT_share)
# was generated in "genSEGMENT_shares.R" script and is named "SEGwghts". 

# Loop over the expenditure variables and generate the weighted average for 
# each.  Store these values in a column named "MEAN_byEXP".
for (EXP in row.names(PARKspending_MEANS)){
        PARKspending_MEANS[EXP, "MEAN_byEXP"] <- 
            round(weighted.mean(PARKspending_MEANS[EXP, c(SEGvars)], SEGwghts), 2)
}

###########################################################################################
#***** PART II: Generate Party/Day & Party/Nights Expenditure *****************************

# Verify that length of stay means have been generated (e.g. "PARKlength_MEANS" data.frame)

if(exists("PARKlength_MEANS", where = globalenv())==FALSE) stop(
  "PARKlength_MEANS does not exist.  Run 'genLENGTH_means.R' before running this script")


# Generate a data.frame named "PARKexpBYparty_MEANS". This will store the expenditure means. 
# The columns will be the segment variables, and the rows will be the expenditures.
PARKexpBYparty_MEANS <- data.frame(matrix(ncol = length(SEGvars), nrow = 0))
                                
colnames(PARKexpBYparty_MEANS) <- SEGvars

# If PARKsegments has the variable "expLocalTotal", remove the values and generate them
# manually. If the variable does not exist, create it.  "expLocalTotal" is the sum of 
# all the expenditure variables relevant to the park (e.g. those in PARKexp_vars)
PARKexp <- PARK_ExpVars[PARK_ExpVars != "expLocalTotal"]
PARKsegments$expLocalTotal <- rowSums(PARKsegments[,c(PARKexp)])

for(VAR in SEGvars){
  PARKexpBYparty_MEANS["Mean_Total_Expenditure", VAR] <- round(mean(
        PARKsegments[PARKsegments[,VAR]==1, "expLocalTotal"]), 2)
  PARKexpBYparty_MEANS["Days_Attributable", VAR] <- round(PARKlength_MEANS["DaysAttributable", VAR], 2)
  PARKexpBYparty_MEANS["Nights_Attributable", VAR] <- round(PARKlength_MEANS["NightsAttributable", VAR], 2)
  
  if (VAR == "day_local" | VAR == "day_nonlocal"){
  PARKexpBYparty_MEANS["Mean_Exp_Party/Day.Night", VAR] <- 
      round(PARKexpBYparty_MEANS["Mean_Total_Expenditure", VAR]*PARKexpBYparty_MEANS["Days_Attributable", VAR],2)}

  else (PARKexpBYparty_MEANS["Mean_Exp_Party/Day.Night", VAR] <- 
    round(PARKexpBYparty_MEANS["Mean_Total_Expenditure", VAR]/PARKexpBYparty_MEANS["Nights_Attributable", VAR],2))}








