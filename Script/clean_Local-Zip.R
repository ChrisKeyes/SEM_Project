# This script will use the current PARK zipcodes to 
# categorize an observation as local | nonlocal when
# local == NA (repondant refused to answer)

library(plyr) # Load neccessary package

# For observations where local==NA, but zip is provided, change local to 
# local == 1 if zip matches local zipcodes or
# local == 0 if zip matches non-local zipcodes


PARK_localzip <- subset(PARKsegments, local == 1 & is.na(zip)==FALSE , select = zip)

PARK_nonlocalzip <- subset(PARKsegments, local == 0 & is.na(zip)==FALSE , select = zip)

IDs_local1 <- PARKbads[which(PARKbads$local_1 == 1), "ID"]
# IDs_local1 are observations where local == NA

PARK_localzip <-  count(PARK_localzip)
    colnames(PARK_localzip) <- c("LOCAL_ZIP", "FREQUENCY")
        n <- sum(PARK_localzip$FREQUENCY)

LocalZip <- PARK_localzip[PARK_localzip$FREQUENCY/n > .25, "LOCAL_ZIP"]   
# CHANGE WEIGHT AFTER TESTING 

PARK_nonlocalzip <-  count(PARK_nonlocalzip)
    colnames(PARK_nonlocalzip) <- c("NON-LOCAL_ZIP", "FREQUENCY")
        m <- sum(PARK_nonlocalzip$FREQUENCY)

nonLocalZip <- PARK_nonlocalzip[PARK_nonlocalzip$FREQUENCY/m > .25, "NON-LOCAL_ZIP"]    
# CHANGE WEIGHT AFTER TESTING 

ErrorZip <- intersect(LocalZip, nonLocalZip) 
      ErrorZip <- append(ErrorZip, c("DK"))  # we can add other values as needed

        # NOTE: setdiff is the difference of set x net set y, not the union of 
        # x & y net the intersection

        PARK_localzip <- setdiff(LocalZip, ErrorZip)
        PARK_nonlocalzip <- setdiff(nonLocalZip, ErrorZip)

for (zips in PARK_localzip){
  PARKsegments[which(PARKsegments$zip == zips & is.na(PARKsegments$local)), "local"] <- 1
}

for (zips in PARK_nonlocalzip){
  PARKsegments[which(PARKsegments$zip == zips & is.na(PARKsegments$local)), "local"] <- 0
}



