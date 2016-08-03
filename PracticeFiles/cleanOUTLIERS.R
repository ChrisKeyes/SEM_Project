# Outliers test script
###########################################################################################
# Clear all data in memory 
rm(list=union(ls(), ls()))

# Load the neccessary packages for the following analysis. First, install packages if
# they have not been installed on local hard drive. 
source("~/SEM_Project/Script/loadPACKAGES.R")
library(prettyR)

# Set working directory
setwd("~/SEM_Project")

# Select which park to run analysis on and remove others from memory
# #     NOTE: this option is currently restricted to selecting a single PARK

source("~/SEM_Project/Script/selectPARK.R")

# Upload the "SEMvars.csv" to memory and store as a data.frame named SEMvars
SEMvars <- read.csv("~/SEM_Project/SEMvars.csv")

# Subset PARKdata and assign the SEM variable names for consistancy. After the "getSEMvars.R"
# script is run, the data frame "PARKsem" is the data to be used throughout the remaining 
# analysis.  
source("~/SEM_Project/Script/getSEMvars.R")
source("~/SEM_Project/Script/getGROUPvars.R")

###########################################################################################
# Variables to check for outliers:
# daysPark
# hoursPark
# nightsLocalArea
# totalcovered (totalcovered = adultscovered + childrencovered)
# all exp vars

# Generate neccessary variables
PARKsem$nightsLocalArea <- rowSums(PARKsem[PARK_SegmentVars], na.rm = TRUE)
PARKsem$totalCovered <- PARKsem$adultsCovered + PARKsem$childrenCovered

PARK_ExpVars <- PARK_ExpVars[PARK_ExpVars != "expLocalTotal"]

PARKsem$expLocalTotal <- rowSums(PARKsem[,PARK_ExpVars], na.rm = TRUE)

attach(PARKsem)

VARS <- c("daysPark", "hoursPark", "nightsLocalArea", "totalCovered")

# Summary statistics table
describe(PARKsem[VARS], num.desc = c("mean", "median","sd","max", "valid.n"))

B.MAX <- which(daysPark==60)
B.MAX <- append(B.MAX, which(hoursPark == 25))
B.MAX <- append(B.MAX, which(nightsLocalArea == 69))
B.MAX <- append(B.MAX, which(totalCovered == 20))

# ****************************************************************************************
# Histograms:

# dens <- density(daysPark, na.rm = TRUE)
# H.days <- hist(daysPark, plot = FALSE)
# hist(daysPark, xlim = range(H.days$breaks, dens$x),
#                ylim = range(H.days$density, dens$y),
#                xlab = "daysPark", ylab = "Density",
#                freq = FALSE, main = "")
# lines(dens, lty = 2, lwd = 2)
# box()

detach(PARKsem)

for(VAR in VARS){
dens <- density(PARKsem[,VAR], na.rm = TRUE)
HIST <- hist(PARKsem[,VAR], plot = FALSE)
hist(PARKsem[,VAR], xlim = range(HIST$breaks, dens$x),
               ylim = range(HIST$density, dens$y),
               xlab = as.character(VAR), ylab = "Density",
               freq = FALSE, main = "")
lines(dens, lty = 2, lwd = 2)
box()
}

attach(PARKsem)

# ****************************************************************************************
# Boxplots
# daysPark
boxplot(daysPark ~ overnight, ylab = "daysPark" , xlab = "overnight")
numOBS <- 1:length(daysPark)
x1 <- rep(1, length(daysPark[overnight == 0]))
x2 <- rep(2, length(daysPark[overnight == 1]))
y1 <- daysPark[overnight == 0]
y2 <- daysPark[overnight == 1]

identify(x1, y1, labels = numOBS[overnight == 0])
identify(x2, y2, labels = numOBS[overnight == 1])

# Potential outliers from daysPark by overnight
B.DAYS <- 316

# hoursPark
boxplot(hoursPark ~ overnight, ylab = "hoursPark" , xlab = "overnight")
numOBS <- 1:length(hoursPark)
x1 <- rep(1, length(hoursPark[overnight == 0]))
x2 <- rep(2, length(hoursPark[overnight == 1]))
y1 <- hoursPark[overnight == 0]
y2 <- hoursPark[overnight == 1]

identify(x1, y1, labels = numOBS[overnight == 0])
identify(x2, y2, labels = numOBS[overnight == 1])

# potential outliers from hoursPark by overnight
B.HOURS <- c(159, 523, 430)

# totalCovered
boxplot(totalCovered)
numOBS <- 1:length(totalCovered)
identify(rep(1, length(na.omit(totalCovered))), na.omit(totalCovered), labels = numOBS[is.na(totalCovered)==FALSE])

# potential outliers based on totalCovered
B.TC <- c(228,8,516)

# ****************************************************************************************
lm.1 <- lm(expLocalTotal ~ adultsCovered + childrenCovered + nightsLocalArea + overnight + as.factor(tripPurpose))
summary(lm.1)
plot(lm.1, 2)

T.resids <- (fitted(lm.1)-mean(resid(lm.1), na.rm = T))/sd(resid(lm.1), na.rm = T)
plot(T.resids)

m <-mean(resid(lm.1), na.rm = TRUE)
st.dv <- sd(resid(lm.1), na.rm = TRUE)

range.norm <- c(m - 3.72*st.dv, m + 3.72*st.dv)

dens1 <- density(resid(lm.1), na.rm = TRUE)
dens2 <- dnorm(pretty(range.norm, 200), mean = m, sd = st.dv)

plot(dens1, xlim = range(c(dens1$x, range.norm)),
            ylim = range(c(dens1$y, dens2)), 
            xlab = "Residuals", main = "")
lines(pretty(range.norm, 200), dens2, lty = 2)
box()

# combine potential outliers from previous exploration, remove them, rerun model
rm.OUT <- unique(c(B.MAX, B.DAYS, B.HOURS, B.TC))

detach(PARKsem)
tempDF <- PARKsem

tempDF <- tempDF[setdiff(seq(1:length(tempDF$ID)), rm.OUT),]

attach(tempDF)
