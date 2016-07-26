###########################################################################################

# This script will take the identified party segments and generate respective shares.
# The results will be stored in a table named "PARKsegments_SHARES".

###########################################################################################
# Generate the table (data.frame) named "PARKsegments_SHARES" with column names of each 
# segment type
PARKsegments_SHARES <- data.frame(matrix(ncol = length(SEGvars), nrow = 0))
    colnames(PARKsegments_SHARES) <- c(SEGvars)
    
# Loop over the segment shares (SEGvars), and store the number of observations (n),
# and the percent share for that segment. Also generate a vector of the segment 
# shares named "SEGwghts" to use for weighted means in future analysis.
SEGwghts <- NULL

for (VAR in SEGvars){
  PARKsegments_SHARES["n", VAR] <- sum((PARKsegments[,VAR] == 1))
  
  wght <- sum((PARKsegments[,VAR] == 1)) / length(PARKsegments[,VAR])
  SEGwghts <- append(SEGwghts, wght)
              
  PARKsegments_SHARES["SEGMENT_share", VAR] <- percent(wght)
}

###########################################################################################
  
    
