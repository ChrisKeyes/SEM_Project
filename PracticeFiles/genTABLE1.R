# Recreate Table 1 from Stynes & White 2006
# Create each sub-table individually then bind them together

# As of 8/12 - This script is incomplete
localday <- data.frame(matrix(ncol = 5), nrow = 0)
col.names <- c("All_Cases", "Excluding Outliers-1", "Excluding Outliers-1 Weighted",
                      "Excluding Outliers-2", "Excluding Outliers-2 Weighted")
      colnames(localday) <- col.names

PARKsegments$expLocalTotal <- ifelse(is.na(PARKsegments$expLocalTotal), 0 , 
                                           PARKsegments$expLocalTotal)

summary(PARKsegments$expLocalTotal)
describe(PARKsegments["expLocalTotal"], num.desc = c("mean", "median","sd","max", "valid.n"))

ldayDF <- subset(PARKsegments, day_local == 1)

localday["Local_Day",] <- NA
localday <- localday[-1,]

N <- nrow(ldayDF)
MEAN <- mean(ldayDF$expLocalTotal)
MEDIAN <- median(ldayDF$expLocalTotal)
MEAN.trim <- mean(ldayDF$expLocalTotal, trim = .025)
SD <- sd(ldayDF$expLocalTotal)
CV <- SD/MEAN
P <- N/nrow(PARKsegments)
ERR <- 1.95*(sqrt((P*(1-P))/N))

# localday["Local_Day",] <- NA
localday["N_cases", "All_Cases"] <- N
localday["Mean", "All_Cases"] <- round(MEAN, 2)
localday["Median", "All_Cases"] <- round(MEDIAN,2)
localday["Trimmed_Mean", "All_Cases"] <- round(MEAN.trim,2)
localday["Standard_Deviation", "All_Cases"] <- round(SD, 2)
localday["CV", "All_Cases"] <- round(CV,2)
localday["Percent_Error-(95%)", "All_Cases"] <- percent(ERR)




tempDF <- PARKsegments[PARKsegments$day_local == 1, ]
tempDF <- subset(tempDF, expLocalTotal < 1000)
summary(tempDF$expLocalTotal)

# unweighted
N <- nrow(tempDF)
MEAN <- mean(tempDF$expLocalTotal)
MEDIAN <- median(tempDF$expLocalTotal)
MEAN.trim <- mean(tempDF$expLocalTotal, trim = .025)
SD <- sd(tempDF$expLocalTotal)
CV <- SD/MEAN
P <- N/nrow(PARKsegments)
ERR <- 1.95*(sqrt((P*(1-P))/N))

# weighted
N <- nrow(tempDF)
MEAN <- sum(((tempDF$weight)/N)*tempDF$expLocalTotal)
MEDIAN <- median(tempDF$expLocalTotal)
MEAN.trim <- weighted.mean(tempDF$expLocalTotal,(tempDF$weight)/N, trim = .025)
SD <- sd(tempDF$expLocalTotal*(tempDF$weight)/N)
CV <- SD/MEAN
P <- N/nrow(PARKsegments)
ERR <- 1.95*(sqrt((P*(1-P))/N))

# Choose column name 
col.name <- "All_Cases"
col.name <- "Excluding Outliers-1"
col.name <- "Excluding Outliers-1 Weighted"
col.name <- "Excluding Outliers-2"
col.name <-  "Excluding Outliers-2 Weighted"

# Fill in table
localday["N_cases", col.name] <- N
localday["Mean", col.name] <- round(MEAN, 2)
localday["Median", col.name] <- round(MEDIAN,2)
localday["Trimmed_Mean", col.name] <- round(MEAN.trim,2)
localday["Standard_Deviation", col.name] <- round(SD, 2)
localday["CV", col.name] <- round(CV,4)
localday["Percent_Error-(95%)", col.name] <- percent(ERR)










table2 <- data.frame(matrix(ncol = 5), nrow = 0)
colnames(table2) <- c("All Cases", "Excluding Outliers-1", "Excluding Outliers-1 Weighted",
                      "Excluding Outliers-2", "Excluding Outliers-2 Weighted")

table1["Local Day",] <-1
table2["Nonlocal Day",]  <- 3

table3 <- merge(table1, table2, by = column)
table3 <- rbind(table1, table2)

