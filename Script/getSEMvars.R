# function SEMvars creates a vector of variable names which each dataset has called Vars
# and creates a vector of variable names which the park data does not have called noVars
# and lastly creates a vector of variable names which are essential to spending effect
# analysis but are specific to this park and creates a vector of these variable names 
# called NewVars

getSEMvars <- function(SearchVars){

  
for (x in SearchVars){
  TempVars <- Varnames[grepl( x ,Varnames)]
  Vars <- append(Vars, TempVars)
    }
}