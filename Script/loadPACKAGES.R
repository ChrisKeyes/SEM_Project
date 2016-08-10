# This script will load the neccessary packages for the SEM analysis.  If a package is not in
# base R, and has not been previously installed, it is downloaded and then will load.

################################################################################################

# Non base R packages to check:
#       NOTE: If a new package is needed, enter the name in the vector "Packages" below
Packages <- c("svDialogs", "scales")

for (PKG in Packages ){
  if(PKG %in% rownames(installed.packages()) == FALSE) {install.packages(PKG)}
}

# After the check above is complete, the code below loads the neccessary scripts
#       NOTE: If a new package is needed, enter the name in the same script style as 
#       those below
library(plyr)
library(svDialogs)
library(scales)
library(RColorBrewer)
 

################################################################################################
