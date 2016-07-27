# This is a test script to examine methods of finding outliers in our data.
# Variables are both discrete and continuous, and may have normal,
# poisson, or binomial distributions.
################################################################################################
# UNIVARIATE DATA
# 
# NORMAL DISTRIBUTIONS:
# We can use the "grubbs test" 
# Under the assumption of normality, gurbbs is hypothesis 
# testing Ho: no outliers, Ha: at least one.
# It is a comparison test that itterates over every observation
# and the test statistic is G = max(|Yi - Ybar|)/sd
################################################################################################


# Set working directory
setwd("~/SEM_Project")

# Load data/variables
source("~/SEM_Project/SEM_StartHere.R")
library(stats4)
library(MASS)


# Create vectors to experiment with.  Use several different variables from PARKsem
V1 <- PARKsem[is.na(PARKsem$grpSize[])==FALSE, "grpSize"]
V2 <- PARKsem[is.na(PARKsem$entries[])==FALSE, "entries"]
V3 <- PARKsem[is.na(PARKsem$expRail[])==FALSE,"expRail"]
V4 <- PARKsem[is.na(PARKsem$numAdults[])==FALSE ,"numAdults"]

V5 <- PARKsem[is.na(PARKsem$adultsCovered[])==FALSE , "adultsCovered"]
V6 <- PARKsem[is.na(PARKsem$childrenCovered[])==FALSE , "childrenCovered"]

# Plot these guys to see their shape
hist(V1, freq = TRUE, density = 10)

plot(density(V1), main = "Density Estimate 'grpSize'")
plot(ecdf(V1), main = "Emperical CDF of 'grpSize'")

zV1 <- (V1 - mean(V1))/sd(V1)
qqnorm(zV1)
abline(0,1)

################################################################################################

H <- hist(V1, breaks = 15, density = 10)
xhist <- c(min(H$breaks), H$breaks)
yhist <- c(0, H$density,0)

xfit <- seq(min(V1), max(V1), length = 40)
yfit <- dnorm(xfit, mean = mean(V1), sd = sd(V1)) # normal density
# yfit <- dlnorm(xfit, meanlog = mean(V1), sdlog = sd(V1))  # log normal density
# yfit <- dpois(xfit, lambda = var(V1) )

plot(xhist, yhist, type = "s", ylim = c(0, max(yhist, yfit)))
lines(xfit, yfit, col = "red")

################################################################################################

# Chi-Square Goodness of Fit Test
# install.packages("vcd")
library(vcd)

table(V2)
V2 <- ceiling(V2)
table(V2)

gf1.V2 <- goodfit(V2, type = "poisson", method = "ML" )
summary(gf1.V2)
plot(gf1.V2)

gf2.V2 <- goodfit(V2, type = "binomial", method = "ML")
summary(gf2.V2)
plot(gf2.V2)

################################################################################################

# install.packages("outliers")
library(outliers)
outlier(V2)
table(V2)
median(V2)

outlier(V2, logical = TRUE)
################################################################################################

# Conservative approach to outlier identification using MLE and poisson
Y <- 2*(V2)^(1/2)
OUTs <- Y[Y>(median(Y)+3)]
# THIS WORKS FOR COUNT DATA WITH POISSION DIST!!!


library(fitdistrplus)
fitdist(V2, "poisson", method = "mle")

fitdistr(V2, "normal")

# Another Method:
# Generate random vectors of suspected distributions 
# with the same mean and variance as data, then test fit
V2.poisson <- rpois(n = length(V2), lambda = mean(V2))
qqplot(V2, V2.poisson)

ks.test(V2, "ppois", lambda = mean(V2))
