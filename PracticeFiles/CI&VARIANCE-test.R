# Confidence intervals

# Packages
library(binom)
library(survey)
# CONFIDENCE INTERVALS  ---------------------------------------------------
# #########################################################################

# CI FOR PROPORTIONS ------------------------------------------------------
# 
# Confidence interval around proportion for binomial probrability
# Falisard pp. 51

# Get a segment share
s1 <- na.omit(PARKsegments$day_local)

# Use binom.confint
binom.confint(x = sum(s1), n = length(s1), methods = "all")

# The "asymtotic" option is robust for sufficiently large samples and
# non-small proportions
binom.confint(x = sum(s1), n = length(s1), methods = "asymptotic")

# The "wilson" method is recomended for small sample size or small
# proportion size
binom.confint(x = sum(s1), n = length(s1), methods = "wilson")

# Repeat the exercise for all segments using a loop
SEGshare.CI <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(SEGshare.CI) <- c("Segment", "Sample Size", "Share", "95% CI Lower", "95% CI Upper", "Method")

for(VAR in SEGvars){

SEG <- subset(PARKsegments, select = VAR)

if(sum(SEG) >= 30 ){CI <- binom.confint(x = sum(SEG), n = nrow(SEG), methods = "asymptotic")}
if(sum(SEG) < 30){CI <- binom.confint(x = sum(SEG), n = nrow(SEG), methods = "wilson")}

SEGshare.CI[nrow(SEGshare.CI)+1, ] <- c(VAR, CI$x, round(CI$mean, 3), round(CI$lower, 3), round(CI$upper, 3), levels(CI$method))
}

write.csv(SEGshare.CI, "~/SEM_Project/Output/Statistics/SegmentShares_CI.csv", row.names = F)

# #########################################################################
# CI FOR MEANS ------------------------------------------------------------

# Falisard pp 54

# Get a segment
tempDF <- PARKsegments[PARKsegments$day_local == 1, ]
tempDF$expLocalTotal <- ifelse(is.na(tempDF$expLocalTotal)==TRUE, 0, tempDF$expLocalTotal)

# Manually generate confidence interval using mean, standard dev. and quantile values
m1 <- tempDF$expLocalTotal
mu <- mean(m1) 

n <- length(m1)
se <- sd(m1)/sqrt(n)

lowci <- mu-qt(0.975, n-1)*se
upci <- mu-qt(0.025, n-1)*se

cat("mean: ", mu, "95% CI:", lowci, upci, "\n")

# Loop through each segment and fill table with results
EXP.CI_table <- data.frame(matrix(ncol = 7 , nrow = 0))
colnames(EXP.CI_table) <- c("Segment", "Expenditure", "Observations", "Mean", "Standard Error", "95% CI Lower Bound", "95% CI Upper Bound")

for(VAR in SEGvars){
  tempDF <- subset(PARKsegments, PARKsegments[VAR]==1)

for(EXP in PARK_ExpVars){
m1 <- tempDF[,EXP]
mu <- round(mean(m1),2)

n <- length(m1)
se <- round(sd(m1)/sqrt(n), 2)

lowci <- round(mu-qt(0.975, n-1)*se, 2)
upci <-  round(mu-qt(0.025, n-1)*se, 2)

EXP.CI_table[nrow(EXP.CI_table)+1, ] <- c(VAR, EXP, n, mu, se, lowci, upci)
}}

write.csv(EXP.CI_table, "~/SEM_Project/Output/Statistics/Expenditure_CIs.csv", row.names = F)

# #########################################################################









