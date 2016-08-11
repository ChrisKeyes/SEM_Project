# Outliers test script
###########################################################################################
# Clear all data in memory 
rm(list=union(ls(), ls()))

# Load the neccessary packages for the following analysis. First, install packages if
# they have not been installed on local hard drive. 
source("~/SEM_Project/Script/loadPACKAGES.R")
library(car)
library(prettyR)
library(MASS)
library(outliers)
library(stats4)
library(fitdistrplus)
library(binom)
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


# Clean Variables ---------------------------------------------------------

tempDF <- PARKsem
rownames(tempDF) <- PARKsem$ID

# Clean up  a few variables to help with regression
tempDF$hoursPark <- ifelse(is.na(tempDF$hoursPark)==TRUE, 0, tempDF$hoursPark)
tempDF$daysPark <- ifelse(is.na(tempDF$daysPark) == TRUE, 0 , tempDF$daysPark)
tempDF$childrenCovered <- ifelse(is.na(tempDF$childrenCovered) == T, 0 , tempDF$childrenCovered)
    # Looking at count(numChild), we have frequency of 0 = 643 which is roughly the 
    # sum of NA's and zeros in childrenCovered

tempDF$expLocalTotal <- rowSums(tempDF[,PARK_ExpVars[PARK_ExpVars != "expLocalTotal"]], na.rm = T)
tempDF$nightsLocalArea <- rowSums(tempDF[PARK_SegmentVars], na.rm = TRUE)

tempDF$totalCovered <- tempDF$adultsCovered + tempDF$childrenCovered
tempDF$ADJadultsCovered <- ifelse(tempDF$adultsCovered == 0, 1 , tempDF$adultsCovered)
tempDF$ADJtotalCovered <- tempDF$ADJadultsCovered + tempDF$childrenCovered

# Stynes & White Outlier/Contaminant Identification -----------------------
ID.Excess.Spending.DAY <- as.character(na.omit(tempDF[tempDF$overnight == 0 & tempDF$expLocalTotal > 1000, "ID"]))
ID.Excess.Spending.ON <- rownames(which(tempDF[tempDF$overnight==1, PARK_ExpVars]>1000, arr.ind = T))
ID.Excess.Camping <- tempDF[which(tempDF$nightsCampIn > 10, arr.ind = T), "ID"]
#       CUVA places a 10 day limit on camping within the park 

ID.inspect <- union(ID.Excess.Camping, union(ID.Excess.Spending.DAY, ID.Excess.Spending.ON))
tempDF[ID.inspect,]

# Examine totalCovered variable for excessive group size
summary(tempDF$ADJtotalCovered)
sd.totCov <- sd(tempDF$ADJadultsCovered, na.rm = T) 

ID.Excess.Adults1 <- tempDF[which(tempDF$ADJadultsCovered > mean(tempDF$ADJadultsCovered, na.rm = T)+
                                      (3*ceiling(sd.totCov)), arr.ind = T), "ID"]

ID.Excess.Adults2 <- tempDF[which(tempDF$ADJadultsCovered > mean(tempDF$ADJadultsCovered, na.rm = T)+(3*ceiling(sd.totCov)) &
                                  tempDF$expLocalTotal == 0, arr.ind = T), "ID"]
# ID.Excess.Adults2 is a conservative approach.

summary(tempDF[c(ID.Excess.Adults1, ID.Excess.Adults2), ])
tempDF[c(ID.Excess.Adults1, ID.Excess.Adults2), ]

# From summary stats, drop: C0779, C1002, C1085, C1179


# Drop observations from ID.inspect

# tempDF <- tempDF[setdiff(tempDF$ID,ID.inspect),]

# Outlier Check - Regression ----------------------------------------------

# NOTE: the rules outlined by Stynes capture all but one observation which are likely
# outliers or contaminants.  The regressions below confirm this and are far more complicated
# to work through. 

# Simple linear model. NOTE: add new models using new object name. Then assign that name
# to "fit" below
lm.1 <- lm(expLocalTotal ~ totalCovered, data = tempDF)
lm.2 <- lm(expLocalTotal ~ nightsLocalArea, data = tempDF)
lm.3 <- lm(expLocalTotal ~ daysPark, data = tempDF)
lm.4 <- lm(expLocalTotal ~ hoursPark, data = tempDF)
lm.5 <- lm(expLocalTotal ~ ADJadultsCovered, data = tempDF)

lm.6 <- lm(expLocalTotal ~ overnight + daysPark, data = tempDF)
lm.7 <- lm(expLocalTotal ~ adultsCovered + childrenCovered + nightsLocalArea, data = tempDF)
lm.8 <- lm(expLocalTotal ~ adultsCovered + daysPark + hoursPark + overnight + local, data = tempDF)
lm.9 <- lm(expLocalTotal ~ adultsCovered + childrenCovered + overnight + local + nightsLocalArea, data = tempDF)
lm.10 <- lm(expLocalTotal ~ adultsCovered + childrenCovered + expHotels + nightsLocalArea, data = tempDF)


tempDF$lntotalCOV <- log(tempDF$ADJtotalCovered)

lm.7 <- glm(expLocalTotal ~ lntotalCOV, family = gaussian(link = "identity"), data = tempDF)

glm.1 <- glm(expLocalTotal ~ totalCovered, family = quasipoisson(link = "log"), data = tempDF)
glm.2 <- glm(expLocalTotal ~ adultsCovered + childrenCovered, family = gaussian(link = "identity") , data = tempDF)
glm.3 <- glm(expLocalTotal ~ adultsCovered + daysPark + hoursPark + overnight + local, family = gaussian(link = "identity"), data = tempDF)
glm.4 <- glm(expLocalTotal ~ totalCovered + overnight + local, family = 
                                            quasipoisson(link = "log"), data = tempDF)

fit <- lm.5

summary(fit)

par(mfrow = c(2,2))
plot(fit, 1) # Residual vs. Fitted values 
plot(fit, 2) # QQ - Normal <- Always useful
plot(fit, 4) # Cook's Distance <- Use this
plot(fit, 5) # Residuals vs. Leverage Plot  <-- Very useful
outlierTest(fit)

# Pearson Residuals: (Yi - Mu)/sigma  i.e. for each Yi value, subtract mean and divide by standard
# deviation. Analogous to standardizing Y values

# Cook's Distance: Measure of influcence of individual observation against sample data

# Leverage: Measure of distance for independent variable against the other observations

# Below is the exploratory methods outline in 
#       http://www.statmethods.net/stats/rdiagnostics.html

# Assesing outliers using QQplot for normal residuals and Bonferonni p-value for 
# extreme observations 

# Test for influential observations
# added variable plot
avPlots(fit)  # similar to leveragePlots()

# Influence Plots
influencePlot(fit, id.method = "identify", main = "Influence Plot",
              sub = "Circle size is proportional to Cook's Distance")

fit.str <- studres(fit)
hist(fit.str, freq = F, main = "Distribution of Studentized Residuals")
xfit <- seq(min(fit.str), max(fit.str), length = 40)
yfit <- dnorm(xfit)
lines(xfit, yfit)

# Check for Heteroskedasticity
ncvTest(fit)

# m <-mean(resid(fit), na.rm = TRUE)
# st.dv <- sd(resid(fit), na.rm = TRUE)
# 
# range.norm <- c(m - 3.72*st.dv, m + 3.72*st.dv)
# 
# dens1 <- density(resid(fit), na.rm = TRUE)
# dens2 <- dnorm(pretty(range.norm, 200), mean = m, sd = st.dv)
# 
# plot(dens1, xlim = range(c(dens1$x, range.norm)),
#             ylim = range(c(dens1$y, dens2)), 
#             xlab = "Residuals", main = "")
# lines(pretty(range.norm, 200), dens2, lty = 2)
# box()
# 
# 
# fit <- lm(expLocalTotal ~ adultsCovered + childrenCovered + nightsLocalArea + overnight + as.factor(tripPurpose))
# summary(fit)
# plot(fit, 2)
# 
# T.resids <- (fitted(fit)-mean(resid(fit), na.rm = T))/sd(resid(fit), na.rm = T)
# plot(T.resids)
# 
# m <-mean(resid(fit), na.rm = TRUE)
# st.dv <- sd(resid(fit), na.rm = TRUE)
# 
# range.norm <- c(m - 3.72*st.dv, m + 3.72*st.dv)
# 
# dens1 <- density(resid(fit), na.rm = TRUE)
# dens2 <- dnorm(pretty(range.norm, 200), mean = m, sd = st.dv)
# 
# plot(dens1, xlim = range(c(dens1$x, range.norm)),
#             ylim = range(c(dens1$y, dens2)), 
#             xlab = "Residuals", main = "")
# lines(pretty(range.norm, 200), dens2, lty = 2)
# box()


# Outlier Check - Plots ---------------------------------------------------

# Variables to check for outliers:
# daysPark
# hoursPark
# nightsLocalArea
# totalcovered (totalcovered = adultscovered + childrencovered)
# all exp vars

VARS <- c("daysPark", "hoursPark", "nightsLocalArea", "totalCovered", "adultsCovered", "childrenCovered", "expLocalTotal")

# Summary statistics table
describe(tempDF[VARS], num.desc = c("mean", "median","sd","max", "valid.n"))


B.MAX <-  c(tempDF[which(tempDF$daysPark==60), "ID"],
            tempDF[which(tempDF$hoursPark==25), "ID"],
            tempDF[which(tempDF$nightsLocalArea==69), "ID"],
            tempDF[which(tempDF$totalCovered==20), "ID"],
            tempDF[which(tempDF$adultsCovered==33), "ID"],
            tempDF[which(tempDF$childrenCovered==18), "ID"])

B.MAX <- unique(B.MAX)
ID.inspect.MAX <- tempDF[B.MAX, "ID"]


# Distributions -----------------------------------------------------------

# The link below is a good resource
# https://cran.r-project.org/web/views/Distributions.html
var.1 <- na.omit(PARKsem$hoursPark)
df.1 <- approxfun(density(na.omit(var.1)))
plot(density(var.1))
var.1.new <- c(1, 0, 4)
points(var.1.new, df.1(var.1.new), col = 2)

# plotdist(as.numeric(var.1), histo = TRUE, demp = F)
var.1 <- as.numeric(ceiling(var.1))
fit.1 <- fitdist(var.1, "norm")
summary(fit.1)

par(mfrow = c(2,2))
plot.legend <- c("Normal")
denscomp(fit.1, legendtext = plot.legend)
qqcomp(fit.1, legendtext = plot.legend)
cdfcomp(fit.1, legendtext = plot.legend)
ppcomp(fit.1, legendtext = plot.legend)

# var.2 <- as.numeric(ceiling(na.omit(tempDF$daysPark)))
# fit.2 <- fitdist(var.2, "gamma", method = "mme")
# 
# par(mfrow = c(2,2))
# plot.legend <- c("Normal", "gamma")
# denscomp(list = c(fit.1, fit.2), legendtext = plot.legend)
# qqcomp(c(fit.1, fit.2), legendtext = plot.legend)
# cdfcomp(c(fit.1, fit.2), legendtext = plot.legend)
# ppcomp(c(fit.1, fit.2), legendtext = plot.legend)

# ****************************************************************************************

# fg.1 <- fitdist(var.1, "gamma")
# fln.1 <- fitdist(var.1, "lnorm")
# 
# par(mfrow = c(2,2))
# plot.legend <- c("lognormal", "gamma")
# denscomp(list(fln.1, fg.1), legendtext = plot.legend)
# qqcomp(list(fln.1, fg.1), legendtext = plot.legend)
# cdfcomp(list(fln.1, fg.1), legendtext = plot.legend)
# ppcomp(list(fln.1, fg.1), legendtext = plot.legend)



# ****************************************************************************************
# ***** Historgrams *************************************************************************


# for(VAR in VARS){
# dens <- density(tempDF[,VAR], na.rm = TRUE)
# HIST <- hist(tempDF[,VAR], breaks = pretty_breaks(n = c(nrow(unique(tempDF[VAR])))),  plot = FALSE)
# hist(tempDF[,VAR], breaks = pretty_breaks(n = c(nrow(unique(tempDF[VAR])))),
#                xlim = range(HIST$breaks, dens$x),
#                ylim = range(HIST$density, dens$y),
#                xlab = as.character(VAR), ylab = "Density",
#                freq = FALSE, main = "")
# lines(dens, lty = 2, lwd = 2)
# box()
# }
dens <- density(tempDF$totalCovered, na.rm = TRUE)
HIST <- hist(tempDF$totalCovered, breaks = pretty_breaks(n = c(length(unique(tempDF$totalCovered)))),  plot = FALSE)
hist(tempDF$totalCovered, breaks = pretty_breaks(n = c(length(unique(tempDF$totalCovered)))),
               xlim = range(HIST$breaks, dens$x),
               ylim = range(HIST$density, dens$y),
               xlab = "Total Covered", ylab = "Density",
               freq = FALSE, main = "Total Covered Historgram w. Density Function")
lines(dens, lty = 2, lwd = 2)
box()




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

# Adults Covered
attach(tempDF)

boxplot(ADJadultsCovered ~ local, 
        ylab = "Adjusted Adults Covered" , 
        xlab = "local", 
        main = "Adjusted Adults Covered by Local")
numOBS <- 1:length(ADJadultsCovered)
x1 <- rep(1, length(ADJadultsCovered[local == 0]))
x2 <- rep(2, length(ADJadultsCovered[local == 1]))
y1 <- ADJadultsCovered[local == 0]
y2 <- ADJadultsCovered[local == 1]

identify(x1, y1, labels = tempDF$ID[numOBS[local == 0]])
identify(x2, y2, labels = tempDF$ID[numOBS[local == 1]])

# Should drop C0480, C0779, C1002, C1173

# Total Covered

boxplot(ADJtotalCovered ~ local, 
        ylab = "Adjusted Total Covered" , 
        xlab = "local", 
        main = "Adjusted Total Covered by Local")

numOBS <- 1:length(ADJtotalCovered)
x1 <- rep(1, length(ADJtotalCovered[local == 0]))
x2 <- rep(2, length(ADJtotalCovered[local == 1]))
y1 <- ADJtotalCovered[local == 0]
y2 <- ADJtotalCovered[local == 1]

identify(x1, y1, labels = tempDF$ID[numOBS[local == 0]])
identify(x2, y2, labels = tempDF$ID[numOBS[local == 1]])

# Should drop C0480, C0779, C1002, C1173
#  C0011 appears valid.
ID.Excess.Adults <- union(as.character(ID.Excess.Adults2), c("C0480", "C0779", "C1002", "C1173"))

detach(tempDF)
# ****************************************************************************************
# ***** Linear Regression ****************************************************************

# combine potential outliers from previous exploration, remove them, rerun model
rm.OUT <- unique(c(B.MAX, B.DAYS, B.HOURS, B.TC))


tempDF <- tempDF[setdiff(seq(1:length(tempDF$ID)), rm.OUT),]

