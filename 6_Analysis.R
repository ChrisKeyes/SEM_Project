# Analysis of spending means by segment segments
# for CUVA dataset

# Clear all data in memory
rm(list=union(ls(), ls()))

# Set working directory
setwd("~/SEM_Project")

source("~/SEM_Project/4_Variable-Generation.R")

source("~/SEM_Project/5_Spending&Analysis.R")

# Test the difference in mean local total expenditure ("expLocalTotal")
# across segment types

SEGvars <- c(SEGvars_day, SEGvars_on)


MUtotal <-subset(PARKspending_MEANS, 
                 EXPENDITURES == "expLocalTotal", 
                 select = c(SEGvars_day, SEGvars_on))

row.names(MUtotal) <- "avgLocalTotal"                
View(MUtotal)

# NOTE: CREATE A TOTAL EXPENDITURE VARIABLE AS SUM OF EXPS AND FIND THAT MEAN
      # TEST SIGNIFICANCE OF SUMMED MEAN AGAINST RESPONDANT MEANT

t.test(MUtotal$day_local, MUtotal$day_nonlocal)

results <-  t.test(
              subset(PARKsegments, day_local == 1, select = "expLocalTotal"),
              subset(PARKsegments, day_nonlocal == 1 , select = "expLocalTotal")
                  )

for (x in SEGvars){
  xNOT <- setdiff(SEGvars, x)
  for(y in xNOT){
    OUTPUT <- t.test(
      subset(PARKsegments, PARKsegments[,x] == 1, select = "expLocalTotal"),
      subset(PARKsegments, PARKsegments[,y] == 1 , select = "expLocalTotal")
    )
    assign(paste(x, y , sep = "vs"), OUTPUT)
  }}

# FIND A BETTER WAY TO SAVE THESE RESULTS IN EASY TO READ OUTPUT

