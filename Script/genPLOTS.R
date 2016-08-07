###graphics for report#######################################
library("RColorBrewer")
source("Script/wrapLABELS.R")

#stacked bar chart of spending profiles. Stack using subcategores of spending (lodging, food, local transportation, recreation, retail, other expenditures)

df <- PARKspending_MEANS[c(1:nrow(PARKspending_MEANS)-1),] #need to replace with per party per day/night expenditures
df$EXP <- row.names(df)

#identify the expenditure group for each row in df by matching df$EXP to the corresponding EXPgroup in GROUPvars.csv 
  #first subset GROUPvars to contain only the expenditure variables relevant to PARK
  expMatrix <- subset(GROUPvars, EXPENDITUREvars %in% df$EXP, c("EXPENDITUREvars","EXPENDITUREnames","EXPgroup"))
  #second add EXPgroup column to df by matching EXPgroup to EXP
  df$EXPgroup <- expMatrix[match(df$EXP,expMatrix$EXPENDITUREvars),"EXPgroup"]

#sum expenditure values on df$EXPgroup to get total expenditures by expenditure group
df2 <- ddply(df,"EXPgroup",numcolwise(sum))
#set row names equal to expenditure group
rownames(df2) <- df2$EXPgroup 
#reorder rows
rorder <- c("Lodging and Camping", "Food", "Local Transportation", "Recreation", "Retail", "Other Expenditures")
df2 <- df2[match(rorder, df2$EXPgroup),]
#remove the expenditure group column
df2 <- df2[, !(colnames(df2) %in% c("EXPgroup"))]

#rename columns using SEGnames in GROUPvars
  #first subset GROUPvars to contain only the segment variables relevant to PARK
  segMatrix <- GROUPvars[,c("allSEGS","SEGnames")]
  segMatrix <- segMatrix[complete.cases(segMatrix),]
  segMatrix <- segMatrix[segMatrix$allSEGS %in% SEGvars,]
  #next rename columns to match SEGnames in GROUPvars 
  df3 <- df2
  m <- match(names(df2), segMatrix$allSEGS)
  names(df3)[!is.na(m)] <- as.character(segMatrix$SEGnames[na.omit(m)])
  colnames(df3)[colnames(df3) == "MEAN_byEXP"] <- "All Visitors"
  
#determine upper y-axis limit
EXPtotals <- PARKspending_MEANS["Total_MeanExp",]
ylimit <- ceiling(max(EXPtotals)/10)*10

#wrap labels for x-axis
labs <- wrap.labels(names(df3),5)

png(file="Plots/spendingProfiles.png",width=1400, height=1400*.6, res=200)
par(mar=c(3,5,3,2),
    bty = "n"
    )
barplot(as.matrix(df3),
        beside = F,
        horiz = F,
        axes = F,
        xaxt = "n", #this turns off the text for the x-axis
        ylim = c(0,ylimit), 
        col = brewer.pal(nrow(df3),"Dark2"),
        border = "white"
        )
abline(h=c(seq(10, ylimit, 10)), lty = 1, col = "grey93")
par(xpd = T, #this allows the legend to be outside the plot area
    add = T
    )
barplot(as.matrix(df3),
        beside = F,
        horiz = F,
        axes = F,
        ylim = c(0,ylimit), 
        legend.text = T,
        args.legend = list(x = ncol(df3)+2, y= ylimit +17, bg = "white", box.col = "white", border = NA),
        names.arg = labs,
        col = brewer.pal(nrow(df3),"Dark2"),
        border = "white",
        add =T
        )
axis(side = 2, at=axTicks(2), tick = F, las = 1, labels=sprintf("$%s", axTicks(2)))
title(main="Spending Profiles", adj=0, cex.main=1.5)
title(ylab="Spending per party per day / night",line=3.5,font.lab=2)
dev.off()

