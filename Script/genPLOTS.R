###graphics for report#######################################

#stacked bar chart of spending profiles. Stack using subcategores of spending (lodging, food, local transportation, recreation)


df <- PARKspending_MEANS
df <- head(df,-1) #this removes the last row of the PARKspending_MEANS dataframe which contains total spending
df$EXP <- row.names(df)
expMatrix <- subset(GROUPvars, EXPENDITUREvars %in% df$EXP, c("EXPENDITUREvars","EXPENDITUREnames","EXPgroup"))
#add a column EXPgroup column to df by matching  EXPgroup to df
df$EXPgroup <- expMatrix[match(df$EXP,expMatrix$EXPENDITUREvars),"EXPgroup"]

#the next thing to do is to sum values on df$EXPgroup
df2 <- ddply(df,"EXPgroup",numcolwise(sum))
rownames(df2) <- df2$EXPgroup
df2 <- df2[, !(colnames(df2) %in% c("EXPgroup"))]

#name vector for segments (need to automate this based on final lumped segments - needs to match order of column names in df)
#SEGnames <- c("Local Day", "NL Day", "Other","Lodge","Camp","All Visitors")

#SEGS <- colnames(df2)
segMatrix <- GROUPvars[,c("allSEGS","SEGnames")]
segMatrix <- segMatrix[complete.cases(segMatrix),]
segMatrix <- segMatrix[segMatrix$allSEGS %in% SEGvars,]

m <- match(names(df2), segMatrix$allSEGS)
df3 <- df2
names(df3)[!is.na(m)] <- as.character(segMatrix$SEGnames[na.omit(m)])
colnames(df3)[colnames(df3) == "MEAN_byEXP"] <- "Average - All Visitors"

barplot(as.matrix(df3),
        beside = F,
        #names.arg = SEGnames,
        legend.text = T,
        horiz = F,
        axes = F,
        )
par(mar=c(5,2,4,2)+.1,
    cex.lab = 3)