# This script is for REFERENCE ONLY!  
# See "genSEGMENTshares.R" for the current script in use. 

###########################################################################################

# Identify the number of remaining observations 
n <- length(PARKsegments$overnight)

# Identify the number of overnight & day observations (overall) and their shares
n_overnight <- sum((PARKsegments[,"overnight"]==1)) 
n_day <- sum((PARKsegments[,"overnight"] == 0))

    # Overnight and day shares
    ON_share <- n_overnight/n
    DAY_share <- n_day/n

# Identify the number of day and overnight observations (by "local") and their shares 
n_daylocal <- sum(PARKsegments[,"day_local"]==1)
n_daynonlocal <- sum(PARKsegments[, "day_nonlocal"]==1)
n_ONlocal <- sum(PARKsegments[, "overnight_local"] == 1)
n_ONnonlocal <- sum(PARKsegments[,"overnight_nonlocal"] == 1)

    # Day and Overnight Local shares
    DAYlocal_share <- n_daylocal/n
    DAYnonlocal_share <- n_daynonlocal/n
    ONlocal_share <- n_ONlocal/n
    ONnonlocal_share <- n_ONnonlocal/n

# Identify the number and shares for overnight segment types
    # NOTE: These segment types include both Local and nonLocal parties
    
# Generate vectors for output
a <- NULL   # a: vector of variable names by segment
b <- NULL   # b: vector of shares by segment
c <- NULL   # c: vector of observations by segment

# Loop over the PARK specific segments (e.g. PARK_SegmentVars) and store 
# the count and share of each
for (y in PARK_SegmentVars){
  
  v <- paste("ON", substring(y, 7), sep = "_")  
        # v: name of segment dummy variable
  x <- sum(PARKsegments[, v]==1)  
        # x: number of observations in given segment
  z <- x/n  
        # share of given segments in data set
  
          a <- append(a, v)
          b <- append(b, z)
          c <- append(c, x)
}

a <- append(c("Overall", "Overnight", "Day", "Day_Local", "Day_NonLocal",
              "ON_Local", "ON_nonLocal"), a)

b <- append(c(1,ON_share, DAY_share, DAYlocal_share, DAYnonlocal_share,
              ONlocal_share, ONnonlocal_share), b)

c <- append(c(n, n_overnight, n_day, n_daylocal, n_daynonlocal, n_ONlocal,
              n_ONnonlocal), c)

PARKshares_table <- data.frame(SEGMENT = a, SHARE = b, OBSERVATIONS = c)  



