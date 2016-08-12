###graphics for report#######################################
# library("RColorBrewer")
# source("Script/wrapLABELS.R")
# source("Script/changeAlpha.R")

############################################################################################
#stacked bar chart of spending profiles. Stack using subcategores of spending 
# (lodging, food, local transportation, recreation, retail, other expenditures)

# df <- PARKspending_MEANS[c(1:nrow(PARKspending_MEANS)-1),] #need to replace with per party per day/night expenditures
# df$EXP <- row.names(df)

tempDF <- PARKspending_MEANS[PARKexp,]

for(EXP in row.names(tempDF)){for(VAR in SEGvars){
  if(VAR == "day_local" | VAR == "day_nonlocal"){
    tempDF[EXP, VAR] <- round(tempDF[EXP, VAR]/PARKlength_MEANS["daysLocalArea", VAR], 2)}
  else(tempDF[EXP, VAR] <- round(tempDF[EXP, VAR]/PARKlength_MEANS["nightsLocalArea", VAR],2))}}

tempDF[,c("EXPENDITUREnames","EXPgroup")] <- 
        GROUPvars[match(row.names(tempDF), GROUPvars$EXPENDITUREvars),
                                           c("EXPENDITUREnames","EXPgroup")]

tempDF <- ddply(tempDF, "EXPgroup", numcolwise(sum))
row.names(tempDF) <- tempDF$EXPgroup

#identify the expenditure group for each row in df by matching df$EXP to the corresponding EXPgroup in GROUPvars.csv 
  #first subset GROUPvars to contain only the expenditure variables relevant to PARK
  # expMatrix <- subset(GROUPvars, EXPENDITUREvars %in% df$EXP, c("EXPENDITUREvars","EXPENDITUREnames","EXPgroup"))
  #second add EXPgroup column to df by matching EXPgroup to EXP
  # df$EXPgroup <- expMatrix[match(df$EXP,expMatrix$EXPENDITUREvars),"EXPgroup"]

#sum expenditure values on df$EXPgroup to get total expenditures by expenditure group
# df2 <- ddply(df,"EXPgroup",numcolwise(sum))
#set row names equal to expenditure group
# rownames(df2) <- df2$EXPgroup 
#reorder rows and drop expenditure group column
# df2 <- tempDF

rorder <- c("Lodging and Camping", "Food", "Local Transportation", "Recreation", "Retail", "Other Expenditures")

tempDF <- tempDF[match(rorder, tempDF$EXPgroup), colnames(tempDF) != "EXPgroup"]

# #remove the expenditure group column
# df2 <- df2[, !(colnames(df2) %in% c("EXPgroup"))]
colnames(tempDF) <- c(as.character(GROUPvars[match(SEGvars, GROUPvars[,"allSEGS"]), "SEGnames"]), "All Visitors")

#rename columns using SEGnames in GROUPvars
  #first subset GROUPvars to contain only the segment variables relevant to PARK
  # segMatrix <- GROUPvars[,c("allSEGS","SEGnames")]
  #     segMatrix <- segMatrix[complete.cases(segMatrix),]
  #         segMatrix <- segMatrix[segMatrix$allSEGS %in% SEGvars,]
  
#next rename columns to match SEGnames in GROUPvars 
  # df3 <- tempDF
  # m <- match(names(tempDF), segMatrix$allSEGS)
  # names(df3)[!is.na(m)] <- as.character(segMatrix$SEGnames[na.omit(m)])
  # colnames(df3)[colnames(df3) == "MEAN_byEXP"] <- "All Visitors"

# Generate a variable name for the file path to save plots 
file.path <- paste(getwd(), paste("Output", PARKname, sep = "/"), sep = "/")
file.path <- paste(file.path, "plots", sep = "/")


#set colors
  # colors1 <- brewer.pal(nrow(df3),"Dark2")  
  colors1 <- brewer.pal(nrow(tempDF),"Dark2")  

#determine upper y-axis limit
EXPtotals <- PARKspending_MEANS["Total_MeanExp",]
ylimit <- ceiling(max(EXPtotals)/10)*10

#wrap labels for x-axis
labs <- wrap.labels(names(tempDF),5)

# png(file="Plots/spendingProfiles.png",width=1400, height=1400*.6, res=200)
png(file=paste(file.path, "spendingProfiles.png", sep = "/"),width=1400, height=1400*.6, res=200)

par(mar=c(3,5,3,2),
    bty = "n"
    )
barplot(as.matrix(tempDF),
        beside = F,
        horiz = F,
        axes = F,
        xaxt = "n", #this turns off the text for the x-axis
        ylim = c(0,ylimit),
        col = colors1,
        border = "white"
        )
abline(h=c(seq(10, ylimit, 10)), lty = 1, col = "grey93")
par(xpd = T, #this allows the legend to be outside the plot area
    add = T
    )
barplot(as.matrix(tempDF),
        beside = F,
        horiz = F,
        axes = F,
        ylim = c(0,ylimit), 
        legend.text = T,
        args.legend = list(x = ncol(tempDF)+2, y= ylimit +17, bg = "white", box.col = "white", border = NA),
        names.arg = labs,
        #col = brewer.pal(nrow(df3),"Dark2"),
        col = colors1,
        border = "white",
        add =T
        )
axis(side = 2, at=axTicks(2), tick = F, las = 1, labels=sprintf("$%s", axTicks(2)))
title(main="Spending Profiles", adj=0, cex.main=1.5)
title(ylab="Spending per party per day / night",line=3.5,font.lab=2)
dev.off()



##########################################################################################
#stacked barplot showing spending by non-local visitors and local visitors, by expenditure groups

#generate a matrix that has the format of the data for this plot. 
#Note: Need to write code to actually make this matrix, the values here are made up
# df <- data.frame(c(80000,20000),c(50000,10000),c(6000,1000),c(9000,2000),c(15000,500),c(20000,1000))
# names(df) <- c("Lodging & Camping", "Food", "Local Transport", "Recreation", "Retail", "Other Expenditures")
# rownames(df) <- c("Non-local visitor spending", "Local visitor spending")

tempDF <- table4[row.names(table4) != "Total_Visitor_Spending",]

tempDF[,"EXPgroup"] <- 
        GROUPvars[match(row.names(tempDF), GROUPvars$EXPENDITUREvars), "EXPgroup"]

tempDF <- ddply(tempDF, "EXPgroup", numcolwise(sum))
      row.names(tempDF) <- tempDF$EXPgroup

tempDF <- tempDF[,c("Spending_nonLocal.Visitors", "Spending_All.Visitors")]/1000
tempDF <- t(tempDF)


I <- matrix(0, ncol = ncol(tempDF), nrow = (2*ncol(tempDF)))

for(j in 1:ncol(tempDF)){
  I[(2*j-1), j] <- tempDF[1,j]
  I[(2*j), j] <- tempDF[2, j]
}
colnames(I) <- colnames(tempDF)
tempDF <- I

# table 4 is estimated expenditure by spending category for all visitors and non-local visitors
          

# zeros <- rep(0, 12)
# df2 <- matrix(c(df[,1],zeros,df[,2],zeros,df[,3],zeros,df[,4],zeros,df[,5],zeros,df[,6]), ncol=6)
# names(df2) <- rorder 
# df3 <- df2/1000 #to get spending in terms of $1000 of dollars

#set colors
colors2 <- changeAlpha(colors1,0.3) #creates a transparant version of the colors in colors1 
colors3 <- c(rbind(colors1,colors2)) #interweaves the two color vectors together

#determine upper y-axis limit
# EXPtotals <- colSums(df2)
# EXPtotals <- colSums(tempDF)

# ylimit <- ceiling(max(EXPtotals)/1000)+10
ylimit <- ceiling(max(colSums(tempDF))+3000)

# wrap labels for x-axis
# labs <- wrap.labels(names(df),11)
labs <- wrap.labels(colnames(tempDF),11)

# png(file="Plots/TotalSpending.png",width=1400, height=1400*.6, res=200)
png(file= paste(file.path, "TotalSpending.png", sep = "/") ,width=1400, height=1400*.6, res=200)
par(mar=c(3,5,4,0),
    bty = "n"
)
barplot(as.matrix(tempDF), #as.matrix(df3),
        beside = F,
        horiz = F,
        axes = F,
        xaxt = "n", #this turns off the text for the x-axis
        ylim = c(0,ylimit), 
        col = colors3,
        border = "white"
)
# abline(h=c(seq(20, ylimit, 20)), lty = 1, col = "grey93")
abline(h=c(seq(5000, ylimit, 5000)), lty = 1, col = "grey93")

par(xpd = T, #this allows the legend to be outside the plot area
    add = T
)
barplot(as.matrix(tempDF), # as.matrix(df3),
        beside = F,
        horiz = F,
        axes = F,
        ylim = c(0,ylimit), 
        #legend.text = T,
        #args.legend = list(x = ncol(df3)+2, y= ylimit +17, bg = "white", box.col = "white", border = NA),
        names.arg = labs,
        cex.names = 0.9,
        #col = brewer.pal(nrow(df3),"Dark2"),
        col = colors3,
        border = "white",
        add =T
)
axis(side = 2, at=axTicks(2), tick = F, las = 1, labels=sprintf("$%s", axTicks(2)))
#axis(side = 1, at= axTicks(2),labels = labs)
title(main="Total Visitor Spending", adj=0, cex.main=1.5)
mtext("Dark colors show spending by non-local visitors", font=3, adj=0, cex=.9)
title(ylab="Spending, $1000 of dollars",line=3.5,font.lab=2)
dev.off()

