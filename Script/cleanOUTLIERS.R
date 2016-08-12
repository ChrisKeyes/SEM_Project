# Exploratory analysis to identify outliers.  
# NOTE: This script intentionally does not produce plots or graphs.  
# For plots and regression output results, remove the hashtags from the commented chunks.

###########################################################################################
# Set working directory
setwd("~/SEM_Project")

# Clean Variables ---------------------------------------------------------
# All the exploratory analysis will be done using a temporary data.frame. No
# changes are made to PARKsem data.

tempDF <- PARKsem
rownames(tempDF) <- PARKsem$ID

# Clean up  a few variables to help with regression. 
tempDF$hoursPark <- ifelse(is.na(tempDF$hoursPark)==TRUE, 0, tempDF$hoursPark)
tempDF$daysPark <- ifelse(is.na(tempDF$daysPark) == TRUE, 0 , tempDF$daysPark)
tempDF$childrenCovered <- ifelse(is.na(tempDF$childrenCovered) == T, 0 , tempDF$childrenCovered)
    # Looking at count(numChild), we have frequency of 0 = 643 which is roughly the 
    # sum of NA's and zeros in childrenCovered

tempDF$expLocalTotal <- rowSums(tempDF[,PARK_ExpVars[PARK_ExpVars != "expLocalTotal"]], na.rm = T)
tempDF$nightsLocalArea <- rowSums(tempDF[PARK_SegmentVars], na.rm = TRUE)

tempDF$totalCovered <- tempDF$adultsCovered + tempDF$childrenCovered
# If adults covered is zero, change to 1 for ADJadultsCovered 
tempDF$ADJadultsCovered <- ifelse(tempDF$adultsCovered == 0, 1 , tempDF$adultsCovered)
tempDF$ADJtotalCovered <- tempDF$ADJadultsCovered + tempDF$childrenCovered

# Stynes & White Outlier/Contaminant Identification -----------------------

if(PARKname=="CUVA"){
ID.Excess.Spending.DAY <- as.character(na.omit(tempDF[tempDF$nightsLocalArea == 0 & tempDF$expLocalTotal > 1000, "ID"]))
ID.Excess.Spending.ON <- rownames(which(tempDF[tempDF$nightsLocalArea >=1, PARK_ExpVars]>1000, arr.ind = T))
# CUVA places 10 day limit on Camp In
ID.Excess.Camping <- tempDF[which(tempDF$nightsCampIn > 10, arr.ind = T), "ID"]
}

if(PARKname == "YOSE"){
ID.Excess.Spending.DAY <- as.character(na.omit(tempDF[tempDF$nightsLocalArea == 0 & tempDF$expLocalTotal > 5000, "ID"]))
ID.Excess.Spending.ON <- rownames(which(tempDF[tempDF$nightsLocalArea >=1, PARK_ExpVars] > 20000, arr.ind = T))
# YOSE places 14 day limit on camping during summer months (when survey is taken)
# and 30 day limit Sept - April
ID.Excess.Camping <- tempDF[which(tempDF$nightsCampIn > 14, arr.ind = T), "ID"]
}

ID.Stynes <- union(ID.Excess.Spending.DAY, union(ID.Excess.Spending.ON, ID.Excess.Camping))


# tempDF[ID.inspect,]

# Outlier Check - Regression ----------------------------------------------

# NOTE: the rules outlined above and explained in Stynes & White 2005 capture all 
# but one observation which are likely outliers or contaminants.  
# The regressions below confirm this, but are relatively tedious and cumbersome
# to work through. 

###########################################################################################
# lm.1 <- lm(expLocalTotal ~ totalCovered, data = tempDF)
# lm.2 <- lm(expLocalTotal ~ nightsLocalArea, data = tempDF)
# lm.3 <- lm(expLocalTotal ~ daysPark, data = tempDF)
# lm.4 <- lm(expLocalTotal ~ hoursPark, data = tempDF)
# lm.5 <- lm(expLocalTotal ~ ADJadultsCovered, data = tempDF)
# 
# lm.6 <- lm(expLocalTotal ~ overnight + daysPark, data = tempDF)
# lm.7 <- lm(expLocalTotal ~ adultsCovered + childrenCovered + nightsLocalArea, data = tempDF)
# lm.8 <- lm(expLocalTotal ~ adultsCovered + daysPark + hoursPark + overnight + local, data = tempDF)
# lm.9 <- lm(expLocalTotal ~ adultsCovered + childrenCovered + overnight + local + nightsLocalArea, data = tempDF)
# lm.10 <- lm(expLocalTotal ~ adultsCovered + childrenCovered + expHotels + nightsLocalArea, data = tempDF)
# 
# 
# tempDF$lntotalCOV <- log(tempDF$ADJtotalCovered)
# 
# lm.7 <- glm(expLocalTotal ~ lntotalCOV, family = gaussian(link = "identity"), data = tempDF)
# 
# glm.1 <- glm(expLocalTotal ~ totalCovered, family = quasipoisson(link = "log"), data = tempDF)
# glm.2 <- glm(expLocalTotal ~ adultsCovered + childrenCovered, family = gaussian(link = "identity") , data = tempDF)
# glm.3 <- glm(expLocalTotal ~ adultsCovered + daysPark + hoursPark + overnight + local, family = gaussian(link = "identity"), data = tempDF)
# glm.4 <- glm(expLocalTotal ~ totalCovered + overnight + local, family =
#                                             quasipoisson(link = "log"), data = tempDF)
# 
# fit <- lm.1 # Change this assignment depending on model choice
# 
# summary(fit)
# 
# par(mfrow = c(2,2))
# plot(fit, 1) # Residual vs. Fitted values
# plot(fit, 2) # QQ - Normal <- Always useful
# plot(fit, 4) # Cook's Distance <- Use this
# plot(fit, 5) # Residuals vs. Leverage Plot  <-- Very useful
# outlierTest(fit)
###########################################################################################


# Definitions -------------------------------------------------------------

# Pearson Residuals: 
# (Yi - Mu)/sigma  i.e. for each Yi value, subtract mean and divide by standard
# deviation. Analogous to standardizing Y values

# Cook's Distance: 
# Measure of influcence of individual observation against sample data

# Leverage: 
# Measure of distance for independent variable against the other observations

# Distributions -----------------------------------------------------------

# The link below is a good resource
# https://cran.r-project.org/web/views/Distributions.html

###########################################################################################
# var.1 <- na.omit(tempDF$hoursPark)
# df.1 <- approxfun(density(na.omit(var.1)))
# plot(density(var.1), main = "Density Distribution of hoursPark")
# var.1.new <- c(max(tempDF$hoursPark))
# points(var.1.new, df.1(var.1.new), col = 2)
# 
# var.2 <- na.omit(tempDF$daysPark)
# df.1 <- approxfun(density(na.omit(var.2)))
# plot(density(var.2), main = "Density Distribution of daysPark")
# var.2.new <- c(max(tempDF$daysPark))
# points(var.2.new, df.1(var.2.new), col = 2)
# 
# var.3 <- na.omit(tempDF$expLocalTotal)
# df.1 <- approxfun(density(na.omit(var.3)))
# plot(density(var.3), main = "Density Distribution of expLocalTotal" ,
#                      sub = "Observations >3 standard deviations above mean are highlighted",
#                      xlab = "expLocalTotal")
# var.3.new <- c(tempDF[tempDF$expLocalTotal > mean(tempDF$expLocalTotal, na.rm = T)+(3*sd(tempDF$expLocalTotal, na.rm = T)), "expLocalTotal"])
# points(var.3.new, df.1(var.3.new), col = 2)
# 
# 
# var.4 <- na.omit(tempDF$expLocalTotal[tempDF$tripPurpose==1])
# df.4 <- approxfun(density(na.omit(var.4)))
# plot(density(var.4), main = "Density Distribution of expLocalTotal for Primary Trips" ,
#                      sub = "Observations >3 standard deviations above mean are highlighted",
#                      xlab = "expLocalTotal")
# var.4.new <- c(tempDF[tempDF$expLocalTotal > mean(tempDF$expLocalTotal, na.rm = T)+(3*sd(tempDF$expLocalTotal, na.rm = T)), "expLocalTotal"])
# points(var.4.new, df.1(var.4.new), col = 2)
# 
# var.5 <- na.omit(tempDF$expLocalTotal[tempDF$tripPurpose==2])
# df.5 <- approxfun(density(na.omit(var.5)))
# plot(density(var.5), main = "Density Distribution of expLocalTotal for Primary Trips" ,
#                      sub = "Observations >3 standard deviations above mean are highlighted",
#                      xlab = "expLocalTotal")
# var.5.new <- c(tempDF[tempDF$expLocalTotal > mean(tempDF$expLocalTotal, na.rm = T)+(3*sd(tempDF$expLocalTotal, na.rm = T)), "expLocalTotal"])
# points(var.5.new, df.1(var.5.new), col = 2)
# 
# var.6 <- na.omit(tempDF$expLocalTotal[tempDF$tripPurpose==3])
# df.6 <- approxfun(density(na.omit(var.6)))
# plot(density(var.6), main = "Density Distribution of expLocalTotal for Primary Trips" ,
#                      sub = "Observations >3 standard deviations above mean are highlighted",
#                      xlab = "expLocalTotal")
# var.6.new <- c(tempDF[tempDF$expLocalTotal > mean(tempDF$expLocalTotal, na.rm = T)+(3*sd(tempDF$expLocalTotal, na.rm = T)), "expLocalTotal"])
# points(var.6.new, df.1(var.6.new), col = 2)
# 
# # plotdist(as.numeric(var.1), histo = TRUE, demp = F)
# var.1 <- as.numeric(ceiling(var.1))
# fit.1 <- fitdist(var.1, "norm")
# summary(fit.1)
###########################################################################################



# ****************************************************************************************
# ***** Historgrams **********************************************************************

###########################################################################################
# dens <- density(tempDF$totalCovered, na.rm = TRUE)
# HIST <- hist(tempDF$totalCovered, breaks = pretty_breaks(n = c(length(unique(tempDF$totalCovered)))),  plot = FALSE)
# hist(tempDF$totalCovered, breaks = pretty_breaks(n = c(length(unique(tempDF$totalCovered)))),
#                xlim = range(HIST$breaks, dens$x),
#                ylim = range(HIST$density, dens$y),
#                xlab = "Total Covered", ylab = "Density",
#                freq = FALSE, main = "Total Covered Historgram w. Density Function")
# lines(dens, lty = 2, lwd = 2)
# box()
###########################################################################################


# ****************************************************************************************
# ***** Boxplots *************************************************************************


###########################################################################################
# # Adults Covered
# attach(tempDF)
# 
# boxplot(ADJadultsCovered ~ local, 
#         ylab = "Adjusted Adults Covered" , 
#         xlab = "local", 
#         main = "Adjusted Adults Covered by Local")
# numOBS <- 1:length(ADJadultsCovered)
# x1 <- rep(1, length(ADJadultsCovered[local == 0]))
# x2 <- rep(2, length(ADJadultsCovered[local == 1]))
# y1 <- ADJadultsCovered[local == 0]
# y2 <- ADJadultsCovered[local == 1]
# 
# identify(x1, y1, labels = tempDF$ID[numOBS[local == 0]])
# identify(x2, y2, labels = tempDF$ID[numOBS[local == 1]])
# 
# # Total Covered
# boxplot(ADJtotalCovered ~ local, 
#         ylab = "Adjusted Total Covered" , 
#         xlab = "local", 
#         main = "Adjusted Total Covered by Local")
# 
# numOBS <- 1:length(ADJtotalCovered)
# x1 <- rep(1, length(ADJtotalCovered[local == 0]))
# x2 <- rep(2, length(ADJtotalCovered[local == 1]))
# y1 <- ADJtotalCovered[local == 0]
# y2 <- ADJtotalCovered[local == 1]
# 
# identify(x1, y1, labels = tempDF$ID[numOBS[local == 0]])
# identify(x2, y2, labels = tempDF$ID[numOBS[local == 1]])
#
# detach(tempDF)
# 
# tempDF2 <- subset(tempDF, is.na(expLocalTotal)==F)
# 
# attach(tempDF2)
# boxplot(expLocalTotal ~ tripPurpose,
#         ylab = "Total Local Expenditures" ,
#         xlab = "Trip Purpose",
#         main = "Total Local Expenditures by Trip Purpose")
# numOBS <- 1:length(expLocalTotal)
# x1 <- rep(1, length(expLocalTotal[tripPurpose == 1]))
# x2 <- rep(2, length(expLocalTotal[tripPurpose == 2]))
# x3 <- rep(3, length(expLocalTotal[tripPurpose == 3]))
# 
# y1 <- expLocalTotal[tripPurpose == 1]
# y2 <- expLocalTotal[tripPurpose == 2]
# y3 <- expLocalTotal[tripPurpose == 3]
# 
# identify(x1, y1, labels = tempDF$ID[numOBS[tripPurpose == 1]])
# identify(x2, y2, labels = tempDF$ID[numOBS[tripPurpose == 2]])
# identify(x3, y3, labels = tempDF$ID[numOBS[tripPurpose == 3]])
# detach(tempDF2)
# 
# tempDF3 <- subset(tempDF2, overnight == 1 )
# 
# attach(tempDF3)
# boxplot(expLocalTotal ~ tripPurpose,
#         ylab = "Total Local Expenditures" ,
#         xlab = "Trip Purpose",
#         main = "Total Local Expenditures by Trip Purpose for Overnight Visitors")
# numOBS <- 1:length(expLocalTotal)
# x1 <- rep(1, length(expLocalTotal[tripPurpose == 1]))
# x2 <- rep(2, length(expLocalTotal[tripPurpose == 2]))
# x3 <- rep(3, length(expLocalTotal[tripPurpose == 3]))
# 
# y1 <- expLocalTotal[tripPurpose == 1]
# y2 <- expLocalTotal[tripPurpose == 2]
# y3 <- expLocalTotal[tripPurpose == 3]
# 
# identify(x1, y1, labels = ID[numOBS[tripPurpose == 1]])
# identify(x2, y2, labels = ID[numOBS[tripPurpose == 2]])
# identify(x3, y3, labels = ID[numOBS[tripPurpose == 3]])
# 
# detach(tempDF3)

# # Look into C0480, C0779, C1002, C1173
# #  C0011 appears valid.
# 
# detach(tempDF)
###########################################################################################


# Examine totalCovered variable for excessive group size
# summary(tempDF$ADJtotalCovered)
sd.totCov <- sd(tempDF$ADJadultsCovered, na.rm = T) 

ID.Excess.Adults1 <- tempDF[which(tempDF$ADJadultsCovered > mean(tempDF$ADJadultsCovered, na.rm = T)+
                                      (3*ceiling(sd.totCov)), arr.ind = T), "ID"]

ID.Excess.Adults2 <- tempDF[which(tempDF$ADJadultsCovered > mean(tempDF$ADJadultsCovered, na.rm = T)+(3*ceiling(sd.totCov)) &
                                  tempDF$expLocalTotal == 0, arr.ind = T), "ID"]

# ID.Excess.Adults2 is a conservative approach. expLocalTotal==0 if all exp vars are NA

# summary(tempDF[union(ID.Excess.Adults1, ID.Excess.Adults2), ])
# tempDF[union(ID.Excess.Adults1, ID.Excess.Adults2), ]

# C0779 & C1002 are outliers

# Generate vector of ID's which correspond to the identified outliers
ID.Outliers <- union(ID.Stynes, ID.Excess.Adults2)

# Generate a column named "Outliers" in PARKbads which is 1 for the outliers, 0 o.w.
tempDF$Outliers <- 0
  tempDF[ID.Outliers, "Outliers"] <- 1

PARKbads$Outliers <- tempDF$Outliers



