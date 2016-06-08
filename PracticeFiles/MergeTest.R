# This is using the merge command to merge two data frames on one column variable
# Even if the observations are out of order, merge will match them accordingly

test <- read.csv("~/SEM_Project/mergetest.csv", header = FALSE)  # No column names in csv

test1 <- test[,c(1,2)]
test2 <- test[,c(3,4)]

colnames(test1) <- c("idd","col2")

colnames(test2) <- c("idd","col4")


TT <- merge(test1,test2, by = c("idd"))
            