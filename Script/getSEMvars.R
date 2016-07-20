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

################################################################################################
############# SAVE CODE BELOW AND USE PARK SPECIFIC VARIABLE SELECTION SCRIPT ABOVE ############
################################################################################################
# #create vector of all varnames for current park
# Varnames <- colnames(CUVA)
# Varnames
# 
# # # create vector of variable names for spending effects
# # SEMvars <- AllVars[,1]   #need to expand list 
# # *****MAY DELETE THE CODE ABOVE*****
# 
# # create vector of variable names which serve as general search terms for each parks survey
# SearchVars <- SEMVars[,2]
#   SearchVars
# 
# # determine which variables are matches between the survey variables and our search terms
# ParkVars <- NULL
# TempVars <- NULL
# Matches <- NULL     #vector specifying match, no match, multiple matches
# MMatches <- NULL    #multiple matches vector
# 
# # for (t in SearchVars){
# #   x <- paste(t, "$", sep = "")
# #     TempVars <- Varnames[grepl( x ,Varnames, ignore.case = TRUE)]
# #     ParkVars <- append(ParkVars, TempVars)
# #     Matches <- append(Matches, (any(grepl(x, Varnames))))
# # }
# 
# for (t in SearchVars){
#   x <- paste(t, "$", sep = "")
#   TempVars <- Varnames[grepl( x ,Varnames, ignore.case = TRUE)]
#   
#   if (length(TempVars)== 1){  #if length = 1: exact match
#     ParkVars <- append(ParkVars, TempVars)
#     Matches <- append(Matches, (any(grepl(x, Varnames))))
#   }
#   else if (length(TempVars) == 0){  #if length = 0: no match
#     Matches <- append(Matches, (any(grepl(x, Varnames))))
#   }
#   else {  #if length > 1: multiple matches
#     MMatches <- append(MMatches, TempVars)
#     Matches <- append(Matches, NA)
#   }
# }
# 
# 
# # *** Need to sort out what to do with multiple matches for one search ***
# 
# # Matches is a vector of logical statements, TRUE for one match, False for no matches, NA for multiple
# # MMatches is a vector listing multiple matches; no rules set yet on how to filter these results
# # Parkvars are the matched variable names for this park's survey 
# # List the matched variables:
#     #Matches
#       summary(Matches)
#     #ParkVars
#       summary(ParkVars)
#     
#       MMatches
# 
# # Create a data frame showing which search variables were found (TRUE = at least one match, FALSE = no match, NA = multiple matches)
# SearchResults <- cbind(SearchVars, Matches, stringsAsFactors = as.data.frame.AsIs(SearchVars))
#   SearchResults
# 
# # For multiple matches, determine which are relevent, and pull from raw data
# # From vector of multiple matches (MMatches), pull variable names which are relevent
# 
# #***************************************************************************************************************
# MMvars <- MMatches[c(1,3,6,9)]  #NOTE:  this is specific to CUVA; no rules set to filter and automate
# #NOTE:  If we change search variables, we need to check the vector above to verify it collects the correct variables
# 
# # Subset those vars from raw data frame; include "ID" for merging to matched set of variables
# MMvars <- CUVA[c(c(MMvars), "ID")]
# 
# # Rename the columns to the search terms for consistancy
# MMvarNames <- paste(as.character(SearchVars[c((which(is.na(Matches))))]))
#   l <- length(MMvars) - 1
#     colnames(MMvars)[1:c(l)] <- c(MMvarNames)
# 
# # For unique matches, pull the column from the dataset and save as dataframe named PARKsem
# CUVAsem <- CUVA[c(ParkVars)]   
# 
# #Rename the variables to search variable names for consistancy
# colIndex <- c((which(Matches == TRUE)))
# 
# colnames(CUVAsem) <- SearchVars[c((which(Matches == TRUE)))]
#   head(CUVAsem)
# 
# # Lastly, merge the data frames (filtered multiple matches from MMvars to CUVAsem, by "ID")
#   CUVAsem <- merge(CUVAsem, MMvars, by = c("ID"))
#  
#   x <- NULL
#   t <- NULL
#   TempVars <- NULL
#   Varnames <- NULL

################################################################################################
################################################################################################
################################################################################################

