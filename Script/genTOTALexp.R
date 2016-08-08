###########################################################################################
# This script generates the total spending tables
# 
###########################################################################################
# Set working directory
setwd("~/SEM_Project")

# Generate a data.frame named "PARKexpBYparty_MEANS". This will store the expenditure means. 
# The columns will be the segment variables, and the rows will be the expenditures.
PARKexp_TOTAL <- data.frame(matrix(ncol = length(SEGvars), nrow = 0))
    colnames(PARKexp_TOTAL) <- SEGvars
    
PARKvisits <- 2284612 # CUVA visitors
# Add script to pull total visitation from .csv file

# Fill in total expenditure table.  The first row is the total annual visitors per segment.
PARKexp_TOTAL["Annual_Visitors",] <- PARKvisits*SEGwghts
PARKexp_TOTAL["Avg.Party",] <- PARKparty_MEANS["GroupSize",]
PARKexp_TOTAL["Estimated_Parties",] <- round(PARKexp_TOTAL["Annual_Visitors",]/PARKexp_TOTAL["Avg.Part",], 0)
PARKexp_TOTAL["Avg.Days_ATTR",] <- PARKlength_MEANS["DaysAttributable",]
PARKexp_TOTAL["Avg.Nights_ATTR",] <- PARKlength_MEANS["NightsAttributable",]



for(VAR in SEGvars){
  if(VAR == "day_local" | VAR == "day_nonlocal"){
      PARKexp_TOTAL["Estimated_Party_Days(Nights)", VAR] <- 
          round(PARKexp_TOTAL["Estimated_Parties", VAR]*PARKexp_TOTAL["Avg.Days_ATTR", VAR], 2)}
  else(PARKexp_TOTAL["Estimated_Party_Days(Nights)", VAR] <- 
      round(PARKexp_TOTAL["Estimated_Parties", VAR]*PARKexp_TOTAL["Avg.Nights_ATTR", VAR], 2))}

for(VAR in SEGvars){for(EXP in rownames(PARKspending_MEANS)){
  PARKexp_TOTAL[EXP, VAR] <- PARKspending_MEANS[EXP, VAR]
}}


PARKexp_TOTAL["Avg.Total_Exp.", ] <- PARKexpBYparty_MEANS["Mean_Total_Expenditure",]
PARKexp_TOTAL["Estimated_Exp./Day(Night)",] <- 
        PARKexp_TOTAL["Avg.Total_Exp.",]*PARKexp_TOTAL["Estimated_Party_Days(Nights)",]



