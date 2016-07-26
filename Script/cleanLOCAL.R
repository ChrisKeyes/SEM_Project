# This script will use PARK zipcodes to 
# categorize an observation as local | nonlocal when
# local == NA (repondant refused to answer). 

# For observations where local==NA, but zip is provided, change local to 
      # local == 1 if zip matches local zipcodes or
      # local == 0 if zip matches non-local zipcodes
      # local == NA if zip does not match local or non-local zipcodes

###########################################################################################
# Subset PARKsem into two temporary data frame, one for local respondants and one for
# non-local respondents. 
PARK_localzip <- subset(PARKsem, local == 1 & is.na(zip)==FALSE , select = zip)

PARK_nonlocalzip <- subset(PARKsem, local == 0 & is.na(zip)==FALSE , select = zip)

# For both subsets, gather the zipcodes and their frequencies.
# Starting with local zipcodes:
PARK_localzip <-  count(PARK_localzip)
    colnames(PARK_localzip) <- c("LOCAL_ZIP", "FREQUENCY")
        n <- sum(PARK_localzip$FREQUENCY)

# Using a cutoff percentile value (currently set to 10%), keep those local zipcodes
# which have frequencies greater than 10%. Save that vector of zipcodes as "LocalZip"
LocalZip <- PARK_localzip[PARK_localzip$FREQUENCY/n > .10, "LOCAL_ZIP"]   

# Repeat for non-local zip codes.
PARK_nonlocalzip <-  count(PARK_nonlocalzip)
    colnames(PARK_nonlocalzip) <- c("NON-LOCAL_ZIP", "FREQUENCY")
        m <- sum(PARK_nonlocalzip$FREQUENCY)

nonLocalZip <- PARK_nonlocalzip[PARK_nonlocalzip$FREQUENCY/m > .10, "NON-LOCAL_ZIP"]    

# Using the two vectors of zipcodes, generate a vector of any zipcodes which appear in 
# both local and non-local vectors. The assumption being that any zipcode which fall into both
# subsets is erronous. Name the vector "ErrorZip" 
ErrorZip <- intersect(LocalZip, nonLocalZip) 

# Since the "zipcode" variable is a factor variable with varying respondant answers 
# across parks, add to "ErrorZip" those responses which are common but neither local nor 
# non-local (type "?factor" into R-console for additional information).
# For example, a common response which we do not want to use to identify local or non-local
# zip codes is "DK".  The repondant answered "dont know" for zipcode. 
ErrorZip <- append(ErrorZip, c("DK")) 

# Using "ErrorZip" vector, net out local and non-local zipcodes by subtracting "ErrorZip"
        PARK_localzip <- setdiff(LocalZip, ErrorZip)
        PARK_nonlocalzip <- setdiff(nonLocalZip, ErrorZip)

# Lastly, loop through PARKsem data.frame and for any observation where local == NA
# if zipcode matches a zipcode in the list of local zipcodes (PARK_localzip), change 
# local = 1
for (ZIPS in PARK_localzip){
  PARKsem[which(PARKsem$zip == ZIPS & is.na(PARKsem$local)), "local"] <- 1
}

# If zipcode matches a zipcode in the list of non-local zipcodes (PARK_nonlocalzip), 
# change local = 0
for (ZIPS in PARK_nonlocalzip){
  PARKsem[which(PARKsem$zip == ZIPS & is.na(PARKsem$local)), "local"] <- 0
}



