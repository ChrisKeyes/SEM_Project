# Outliers test script
###########################################################################################
# Clear all data in memory 
rm(list=union(ls(), ls()))

# Load the neccessary packages for the following analysis. First, install packages if
# they have not been installed on local hard drive. 
source("~/SEM_Project/Script/loadPACKAGES.R")


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

#     There are 572 NA's for childrenCovered

PARK_ExpVars <- PARK_ExpVars[PARK_ExpVars != "expLocalTotal"]

PARKsem$expLocalTotal <- rowSums(PARKsem[,PARK_ExpVars], na.rm = TRUE)

attach(PARKsem)

VARS <- c("daysPark", "hoursPark", "nightsLocalArea", "totalCovered", "adultsCovered", "childrenCovered")

# Summary statistics table
describe(PARKsem[VARS], num.desc = c("mean", "median","sd","max", "valid.n"))

B.MAX <-  c(c(which(daysPark==60)),
          c(which(hoursPark == 25)),
          c(which(nightsLocalArea == 69)),
          c(which(totalCovered == 20)),
          c(which(adultsCovered == 33)),
          c(which(childrenCovered == 18)))

B.MAX <- unique(B.MAX)

# ****************************************************************************************
# ***** Historgrams *************************************************************************

# dens <- density(daysPark, na.rm = TRUE)
# H.days <- hist(daysPark, plot = FALSE)
# hist(daysPark, xlim = range(H.days$breaks, dens$x),
#                ylim = range(H.days$density, dens$y),
#                xlab = "daysPark", ylab = "Density",
#                freq = FALSE, main = "")
# lines(dens, lty = 2, lwd = 2)
# box()

library(prettyR)

detach(PARKsem)

for(VAR in VARS){
dens <- density(PARKsem[,VAR], na.rm = TRUE)
HIST <- hist(PARKsem[,VAR], breaks = pretty_breaks(n = c(nrow(unique(PARKsem[VAR])))),  plot = FALSE)
hist(PARKsem[,VAR], breaks = pretty_breaks(n = c(nrow(unique(PARKsem[VAR])))),
               xlim = range(HIST$breaks, dens$x),
               ylim = range(HIST$density, dens$y),
               xlab = as.character(VAR), ylab = "Density",
               freq = FALSE, main = "")
lines(dens, lty = 2, lwd = 2)
box()
}

attach(PARKsem)

# ****************************************************************************************
# ***** Boxplots *************************************************************************

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
# ***** Linear Regression ****************************************************************
detach(PARKsem)

tempDF <- PARKsem

# Clean up  a few variables to help with regression
tempDF$hoursPark <- ifelse(is.na(tempDF$hoursPark)==TRUE, 0, tempDF$hoursPark)
tempDF$daysPark <- ifelse(is.na(tempDF$daysPark) == TRUE, 0 , tempDF$daysPark)
tempDF$childrenCovered <- ifelse(is.na(tempDF$childrenCovered) == T, 0 , tempDF$childrenCovered)
    # Looking at count(numChild), we have frequency of 0 = 643 which is roughly the 
    # sum of NA's and zeros in childrenCovered

tempDF$totalCovered <- tempDF$adultsCovered + tempDF$childrenCovered

# Simple linear model
lm.1 <- lm(expLocalTotal ~ totalCovered, data = tempDF)
lm.2 <- lm(expLocalTotal ~ totalCovered + nightsLocalArea, data = tempDF)
lm.3 <- lm(expLocalTotal ~ adultsCovered + overnight, data = tempDF)
lm.4 <- lm(expLocalTotal ~ adultsCovered + daysPark + hoursPark + overnight + local, data = tempDF)
lm.5 <- lm(expLocalTotal ~ adultsCovered + childrenCovered + overnight + local + nightsLocalArea, data = tempDF)
lm.6 <- lm(expLocalTotal ~ adultsCovered + childrenCovered + expHotels + nightsLocalArea, data = tempDF)



fit <- lm.6

summary(fit)
plot(fit, 2)  

fit.str <- rstudent(fit) # studentized residuals
plot(fit.str)
identify(fit.str)

# Below is the exploratory methods outline in 
#       http://www.statmethods.net/stats/rdiagnostics.html

library(car)

# Assesing outliers using QQplot for normal residuals and Bonferonni p-value for 
# extreme observations 
outlierTest(fit)

detach(tempDF)
tempDF <- tempDF[-c(491, 549, 790),]
attach(tempDF)

qqPlot(fit, main = "QQ Plot")
leveragePlots(fit)


# Test for influential observations
# added variable plot
avPlots(fit)  # similar to leveragePlots()

# Cooks's D Plot, identify obs with D values > 4/(n-k-1)
cutoff <- 4/((nrow(expLocalTotal) - length(fit$coefficients) - 1))
plot(fit, which = 4, cook.levels = cutoff)
# Influence Plots
influencePlot(fit, id.method = "identify", main = "Influence Plot",
              sub = "Circle size is proportional to Cook's Distance")

# Check for Normality of Residuals
qqPlot(fit, main = "QQ Plot") # same plot as 

library(MASS)

fit.str <- studres(fit)
hist(fit.str, freq = F, main = "Distribution of Studentized Residuals")
xfit <- seq(min(fit.str), max(fit.str), length = 40)
yfit <- dnorm(xfit)
lines(xfit, yfit)

# Check for Heteroskedasticity
ncvTest(fit)
spreadLevelPlot(fit)





m <-mean(resid(fit), na.rm = TRUE)
st.dv <- sd(resid(fit), na.rm = TRUE)

range.norm <- c(m - 3.72*st.dv, m + 3.72*st.dv)

dens1 <- density(resid(fit), na.rm = TRUE)
dens2 <- dnorm(pretty(range.norm, 200), mean = m, sd = st.dv)

plot(dens1, xlim = range(c(dens1$x, range.norm)),
            ylim = range(c(dens1$y, dens2)), 
            xlab = "Residuals", main = "")
lines(pretty(range.norm, 200), dens2, lty = 2)
box()


fit <- lm(expLocalTotal ~ adultsCovered + childrenCovered + nightsLocalArea + overnight + as.factor(tripPurpose))
summary(fit)
plot(fit, 2)

T.resids <- (fitted(fit)-mean(resid(fit), na.rm = T))/sd(resid(fit), na.rm = T)
plot(T.resids)

m <-mean(resid(fit), na.rm = TRUE)
st.dv <- sd(resid(fit), na.rm = TRUE)

range.norm <- c(m - 3.72*st.dv, m + 3.72*st.dv)

dens1 <- density(resid(fit), na.rm = TRUE)
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
