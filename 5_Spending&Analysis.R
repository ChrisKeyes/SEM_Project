# This script will take the cleaned data (PARKsem) and variables generated from script #4 
# and create spending profiles and analysize the summary statistics

setwd("~/SEM_Project")

source("4_Variable-Generation.R")

# Clear the environment except for the data we want
rm(list=setdiff(ls(), c("CHECKvars", "PARKbads", "PARKsegments", "PARKsem", "PARKshares_table")))  # NOTE: once script is finalized, delete this line 

expVARS <- colnames(PARKbads[grepl(paste("^", "exp*", sep = ""),colnames(PARKbads))])
nightsVARS <- colnames(PARKbads[grepl(paste("^", "nights*", sep = ""), colnames(PARKbads))])

PARKexp <- as.character(strsplit(expVARS, "_1"))
PARKnights <- as.character(strsplit(nightsVARS, "_1"))

# Use the bads matrix to fix any incomplete observations where accomodation type is NA
# ex. Respondent said campIN=2, but left all other accomodation types bland (e.g. nights*_1 == 1)

# NOTE: this is a quick solution below

# For all PARKnights and PARKexp variables, convert NA's to zero (assumes respondents refused to answer 
# accomodation and  expenditure types which did not apply to them)
for (x in PARKnights){
  PARKsegments[,x] <- ifelse(is.na(PARKsegments[,x]),0,PARKsegments[,x])
}

for (x in PARKexp){
  PARKsegments[,x] <- ifelse(is.na(PARKsegments[,x]),0,PARKsegments[,x])
}




             a <- NULL # colnames by segment
             b <- NULL # mean values
             segmentVARS <- NULL # segment names
             
            for (x in PARKnights){
              v <- paste(substring(x, 7), "nonlocal", sep = "_")
              segmentVARS <- append(segmentVARS, v)
            }
PARKspending <- data.frame(EXPENDITURE = PARKexp)

for (x in segmentVARS){
  
  z <- subset(PARKsegments, PARKsegments[,x] == 1, select = c(PARKexp))
  expV <- colnames(z)
  
  for (y in 1:length(expV)){
      avg <- mean(z[,y])
          b <- append(b, avg)
  }
    df <- data.frame(EXPENDITURES = PARKexp, x = b)
        colnames(df) <- c("EXPENDITURES" , x)
            assign(x, df)
        b <- NULL
}

             

    
for (x in segmentVARS) {
  avg <- mean(data.frame(x)[])
}


for (y in PARKexp)
      
        avg <- mean(z[,y])
            a <- append(a, y)
            b <- append(b, avg)
    
    # else {
    #   avg <- c(z[,y])
    #       a <- append(a, y)
    #       b <- append(b, avg)
    #   
    #  
    
  df <- data.frame(EXP = c(a) , SEG = c(b))
  assign(x, df)
}




            # 
            # 
            # PARKspending <- data.frame(EXPENDITURE = PARKexp, row.names = PARKexp)
            # 
            # for (x in c){
            #   z <- subset(PARKsegments, PARKsegments[,x]==1, select = c(PARKexp))w
            #   
            #   if (length(z[,1]) == 1) {
            #     
            #     df <- data.frame(matrix(unlist(z)), row.names = PARKexp) #  c(subset(PARKsegments,PARKsegments[,x] == 1, select = c(PARKexp)))))
            #       colnames(df) <- c(x)  
            #           assign(x,df)
            #           
            #   }
            #   else {
            #     df <- data.frame(matrix(unlist(c(colMeans(z)))), row.names = PARKexp)
            #         colnames(df) <- c(x) #subset(PARKsegments, PARKsegments[,x] == 1, select = c(PARKexp))))))
            #           assign(x, df)
            #     
            #   }
            #   
            # }





# almost there with this loop, should convert "NA"'s to 0 using BADS before running this and 
# need to verify values being produced.
