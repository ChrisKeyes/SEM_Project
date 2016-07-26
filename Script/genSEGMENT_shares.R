###########################################################################################

# This script will take the identified party segments and generate respective shares.
# The results will be stored in a table named "PARKshares_table".

###########################################################################################
# Generate the table (data.frame) named "PARKshares_table" with column names of each 
# segment type
PARKshares_table <- data.frame(matrix(ncol = length(SEGvars), nrow = 0))
    colnames(PARKshares_table) <- c(SEGvars)
    
# Loop over the segment shares (SEGvars), and store the number of observations (n),
# and the percent share for that segment. 
SEGwghts <- NULL

for (VAR in SEGvars){
  PARKshares_table["n", VAR] <- sum((PARKsegments[,VAR] == 1))
  
        wght <- sum((PARKsegments[,VAR] == 1)) / length(PARKsegments[,VAR])
              SEGwghts <- append(SEGwghts, wght)
              
  PARKshares_table["SEGMENT_share", VAR] <- percent(wght)
}

###########################################################################################
  
    
