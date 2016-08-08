# Confidence intervals
# 
# CI for proportion
library(binom)
library(survey)
s1 <- na.omit(PARKsegments$day_local)

binom.confint(x = sum(s1), n = length(s1), methods = "all")
binom.confint(x = sum(s1), n = length(s1), methods = "asymptotic")
binom.confint(x = sum(s1), n = length(s1), methods = "wilson")

# CI for mean
tempDF <- PARKsegments[PARKsegments$day_local == 1, ]
tempDF$expLocalTotal <- ifelse(is.na(tempDF$expLocalTotal)==TRUE, 0, tempDF$expLocalTotal)

m1 <- tempDF$expLocalTotal
mu <- mean(m1) 

n <- length(m1)
se <- sd(m1)/sqrt(m1)

lowci <- mu-qt(0.975, n-1)*se
upci <- mu-qt(0.025, n-1)*se

cat("mean: ", mu, "95% CI:", lowci, upci, "\n")

res1 <- svymean(~)