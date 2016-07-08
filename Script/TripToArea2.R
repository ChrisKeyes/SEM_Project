#This script develops stats and plots for Qs related to visitor trips to the area

setwd("~/SEM_Project")
source("2_Cleaning.R")

PARKsem <- TESTdata
PARK <- "TEST"

###build a subset dataframe from PARKsem ##########################################
          # SegmentVars   <- c("nightsCampIn","nightsCampOut","nightsLodgeIn","nightsLodgeOut",
          #                    "nightsCruise","nightsOther")
          # 
          # Trip2AreaVars <- c("ID","hoursPark","daysPark",
          #                    "overnight","tripPurpose","equallyNearby",
          #                    "primaryNearby")
          # 
          #         FullTripToAreaVars <- c(Trip2AreaVars, SegmentVars)
          
                  
                  FullTripToAreaVars <- c("ID","hoursPark", "daysPark","overnight","nightsCampIn",
                        "nightsCampOut","nightsLodgeIn","nightsLodgeOut","nightsCruise",
                        "nightsOther","tripPurpose","equallyNearby","primaryNearby")
TripToAreaVars <- NULL
for (y in FullTripToAreaVars){
  if (exists(y, where = PARKsem) == TRUE){
    TripToAreaVars <- append(TripToAreaVars, y)
  }
}
df <- PARKsem[TripToAreaVars]

          # TripToAreaVars <- c("ID","hoursPark", "daysPark","overnight","nightsCampIn","nightsCampOut","nightsLodgeIn","nightsLodgeOut","nightsCruise","nightsOther","tripPurpose","equallyNearby","primaryNearby")
          # PARK_TripToAreaVars <- NULL
          # 
          # for (y in TripToAreaVars){
          #   if (exists(y, where = PARKsem) == TRUE){
          #     PARK_TripToAreaVars <- append(PARK_TripToAreaVars, y)
          #   }
          # }
          #     df <- PARKsem[PARK_TripToAreaVars]


#remove bads from df 
#hoursPark_2 == 1 if hoursPark >24
#daysPark_1 == 1 if hoursPark and daysPark were unaswered
#daysPark_2 == 1 if daysPark > 14
#overnight_3 == 1 if respondents answered overnight = 1 but don't list any lodging in nights*
df$bads <- PARKbads$hoursPark_2 + PARKbads$daysPark_1 + PARKbads$daysPark_2 + PARKbads$overnight_3
    df <- subset(df, df$bads == 0)


###Create variables for time spent in the park######################################

#create a new variable daysParkAdj that estimates number of days in the park
#Note: If a respondent answered both hours park and days park, then use days park (ignore hours park)
#Round partial days up to nearest whole number

df$hoursParkAdj <- ifelse(df$hoursPark >0,1,0)
    df$daysParkAdj <- ifelse(!is.na(df$daysPark),df$daysPark, df$hoursParkAdj)
        df$daysParkAdj <- ceiling(df$daysParkAdj)
#dfverify <- df[c("hoursPark","hoursParkAdj", "daysPark","daysParkAdj")]


###Create variables for time spent in local area ######################################

FullLodgingTypes <- c("nightsCampIn","nightsCampOut","nightsLodgeIn","nightsLodgeOut",
                      "nightsCruise","nightsOther")
LodgingTypes <- NULL
for (y in FullLodgingTypes){
  if (exists(y, where = PARKsem) == TRUE){
    LodgingTypes <- append(LodgingTypes, y)
  }
}
        
        # PARK_SegmentVars <- intersect(colnames(df), SegmentVars)

        # SegmentVars   <- c("nightsCampIn","nightsCampOut","nightsLodgeIn","nightsLodgeOut",
        #                    "nightsCruise","nightsOther")
        # PARK_SegmentVars <- NULL
        # 
        #   for (y in FullLodgingTypes){
        #     if (exists(y, where = PARKsem) == TRUE){
        #       PARK_SegmentVars <- append(PARK_SegmentVars, y)
        #     }
        #   }


#create new variables nightsLocalArea and daysLocalArea that estimate time spent in the local area

df$nightsLocalArea <- rowSums(df[LodgingTypes], na.rm = TRUE)
#if nightsLocalArea > 0 and overnight = na, fill in missing overnight value with 1
df["overnight"][is.na(df["overnight"]) & df["nightsLocalArea"]>0] <- 1
#if overnight == 0 and nightsLocalArea == 0, then daysLocalArea = 1, 
#else if overnight == 1 and nightsLocalArea > 0, then daysLocalArea = nightsLocalArea + 1
df$daysLocalArea <- ifelse(df$overnight == 0 & df$nightsLocalArea == 0, 1, 
                           ifelse(df$overnight == 1 & df$nightsLocalArea > 0, df$nightsLocalArea + 1, 
                                  0))

df$daysLocalAreaCHECK <- ifelse((df$overnight == 0 | is.na(df$overnight)) & df$nightsLocalArea == 0, 1, 
                           ifelse(df$overnight == 1 & df$nightsLocalArea > 0, df$nightsLocalArea + 1, 
                                  0))
      # If overnight == NA & nightsLocalArea == 0, should we just assume daysLocalArea = 1 since they took the survey?

#drop observations for which daysLocalArea == 0 or na
df <- subset(df, df$daysLocalArea > 0 | !is.na(df$daysLocalArea))
#remove outliers (for now... remove if daysLocalArea >14)
df <- subset(df, df$daysLocalArea <= 14)
#dfverify <- df[c("ID",LodgingTypes,"overnight","nightsLocalArea","daysLocalArea")]

###Plot distributions of respondent values ################################

#plot daysLocalArea in a histogram
hist(df$daysLocalArea,
     main = "Days spent in local area",
     xlab = "Days in Local Area")

#plot daysLocalArea in a box plot
boxplot(df$daysLocalArea,
        main = "Days spent in local area",
        ylab = "Days in Local Area")

#plot daysParkAdj in a histogram
hist(df$daysParkAdj,
     main = paste("Days Spent within", PARK, sep=" "),
     xlab = "Days in Park")

#plot daysParkAdj in a box plot
boxplot(df$daysParkAdj,
        main = paste("Days Spent within", PARK, sep=" "),
        ylab = "Days in Park")

###Calculate summary stats for time in park and time in local area variables


# ###Evaluate trip purpose########################################
# 
#basic summary of trip purpose
df <- PARKsem[TripToAreaVars]
    df <- subset(df,!is.na(df$tripPurpose))
      
tripPurpose <- as.factor(df$tripPurpose)
    cnt <- nrow(df)

numPrimary <- sum(df$tripPurpose == 1)
    perPrimary <- numPrimary / cnt

numEqually <- sum(df$tripPurpose == 2)
    perEqually <- numEqually / cnt

numInci <- sum(df$tripPurpose == 3)
    perInci <- numInci / cnt

#evaluate equal purpose trips
#if tripPurpose = 2 and equallyNearby !is.na, want summary of 0s (not nearby) and 1s (nearby)
df_equally <- subset(df, df$tripPurpose == 2 & !is.na(df$equallyNearby))
    cnt <- nrow(df_equally)

numEqually_Nearby <- sum(df_equally$equallyNearby == 1)
    pEqually_Nearby <- numEqually_Nearby / cnt

    perEqually_Nearby <- perEqually * pEqually_Nearby

numEqually_NotNearby <- sum(df_equally$equallyNearby == 0)
    pEqually_NotNearby <- numEqually_NotNearby / cnt
    perEqually_NotNearby <- perEqually * pEqually_NotNearby

#evaluate incidental trips
#if tripPurpose = 3 and primaryNearby !is.na, want summary of 0s (not nearby) and 1s (nearby)
df_incidental <- subset(df, df$tripPurpose ==3 & !is.na(df$primaryNearby))
    cnt <- nrow(df_incidental)
    
numInci_Nearby <- sum(df_incidental$primaryNearby == 1)
    pInci_Nearby <- numInci_Nearby / cnt
        perInci_Nearby <- perInci * pInci_Nearby

        
              
# numInci_NotNearby <- sum(df_incidental$primaryNearby == 0)
# pInci_NotNearby <- numPrimary_NotNearby / cnt
# perInci_NotNearby <- perInci * pInci_NotNearby
# 
# #Note: perPrimary + perEqually_Nearby +perEqually_NotNearby +perInci_Nearby+perInci_NotNearby = 1
# #matrix of percentages by trip purpose
# perTripPurpose = matrix(c(perPrimary,perEqually_Nearby,perEqually_NotNearby,perInci_Nearby,perInci_NotNearby),ncol=5)
# colnames(perTripPurpose)=c("perPrimary","perEqually_Nearby","perEqually_NotNearby","perInci_Nearby","perInci_NotNearby")
# 
# #plot stacked bar chart of equal and incidental trips
# #(OR, maybe since I'm not sure if we can split this by segment, just plot a pie chart?)
