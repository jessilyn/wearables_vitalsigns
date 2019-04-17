
#### Purpose: to Create "Dose Curve" Subsets ####

#Dependencies: 
#   1) clin_20170923.csv 
#   2) 7 sets of subset files, ea. totals NROW(ClinWithWear)

#Output: 
#   7 different csv files (1 for each time interval)

#Note to programmer: some warnings related to min/max 
#returning Inf are expected. These can be ignored.

#### REQUIRED PACKAGES ####

require(data.table)
#require(ggplot2)
#require(lubridate)
#require(microbenchmark)
require(plyr)
require(psych)
require(zoo)

#### Read in clinical ####

ClinWithWear <- read.csv("F:\\Clinical_Data\\clin_20170923.csv",
                         header=TRUE,sep=",",
                         stringsAsFactors=FALSE)

#### Optional: Note MD5s which have data that should be removed from study.
rem <- c("5d1a706641a0b458da136a16b6c65d6b","c7c2d5fe4b4b981165a14f0684b31dae",
         "8ce925a402a0b3e31883d3c600ede9cd","807a78004456272761454351a6a759ff")
#To run without any MD5s removed, make rem an empty vector: rem <- c().

#### DESCRIBE DAY-PRIOR DATA ####
files <- list.files("F:\\BasisSubsets\\FromBasisData_20161111_Normal_20170802\\DayPrior_Files",
                    pattern="*.csv",full.names=TRUE)

gsr_cache <- list()
hr_cache <- list()
sk_cache <- list()
st_cache <- list()

rhr_cache <- list()

highAct_gsr_cache <- list()
highAct_hr_cache <- list()
highAct_sk_cache <- list()
highAct_st_cache <- list()

lowAct_gsr_cache <- list()
lowAct_hr_cache <- list()
lowAct_sk_cache <- list()
lowAct_st_cache <- list()

day_gsr_cache <- list()
day_hr_cache <- list()
day_sk_cache <- list()
day_st_cache <- list()

day_rhr_cache <- list()

day_highAct_gsr_cache <- list()
day_highAct_hr_cache <- list()
day_highAct_sk_cache <- list()
day_highAct_st_cache <- list()

day_lowAct_gsr_cache <- list()
day_lowAct_hr_cache <- list()
day_lowAct_sk_cache <- list()
day_lowAct_st_cache <- list()

night_gsr_cache <- list()
night_hr_cache <- list()
night_sk_cache <- list()
night_st_cache <- list()

night_rhr_cache <- list()

night_highAct_gsr_cache <- list()
night_highAct_hr_cache <- list()
night_highAct_sk_cache <- list()
night_highAct_st_cache <- list()

night_lowAct_gsr_cache <- list()
night_lowAct_hr_cache <- list()
night_lowAct_sk_cache <- list()
night_lowAct_st_cache <- list()

#the next two lines are for testing; remove or comment out for analysis.
# for(i in 1:length(files[1:200])){
#   print(paste(i, file.info(files[i])$size))
# }
# i=140

for(i in 1:length(files)){                  
  #check file size
  check <- file.info(files[i])$size
  #if file size is zero, fill with NAs; else continue.
  if(check!=0){
    #read data
    df <- read.csv(files[i],header=TRUE,sep=",")
    #if column name Wearable_Account_MD5 is present...
    #check whether column contains MD5s to be removed.
    #if so, remove rows where MD5 is in list of removable MD5s ("rem")
    if("Wearable_Account_MD5" %in% names(df)){
      if(length(intersect(unique(df$Wearable_Account_MD5),rem))!=0){
        df <- df[!(df$Wearable_Account_MD5 %in% rem),]
      }
      #note when file doesn't contain standard MD5 column and skip for review later
    } else {
      if(!("Wearable_Account_MD5" %in% names(df))){
        print(paste("MD5 not found in",i))
        print(names(df))
        next
      }
    }
    #convert timestamp to POSIXct
    df$Date <- as.POSIXct(paste(df$Timestamp_Local),format="%Y-%m-%d %H:%M:%S")
    #if column in file is completely empty, TRUE, else FALSE.
    emptyCols <- apply(df,2,function(x) all(is.na(x)))
    #if at least one column was completely empty
    if(length(intersect(names(which(emptyCols)),c("GSR","Heart_Rate","Skin_Temperature_F","Steps")))>2){
      gsr_cache[[i]] <- data.frame(NA)
      hr_cache[[i]] <- data.frame(NA)
      sk_cache[[i]] <- data.frame(NA)
      st_cache[[i]] <- data.frame(NA)
      
      rhr_cache[[i]] <- data.frame(NA)
      
      highAct_gsr_cache[[i]] <- data.frame(NA)
      highAct_hr_cache[[i]] <- data.frame(NA)
      highAct_sk_cache[[i]] <- data.frame(NA)
      highAct_st_cache[[i]] <- data.frame(NA)
      
      lowAct_gsr_cache[[i]] <- data.frame(NA)
      lowAct_hr_cache[[i]] <- data.frame(NA)
      lowAct_sk_cache[[i]] <- data.frame(NA)
      lowAct_st_cache[[i]] <- data.frame(NA)
      
      day_gsr_cache[[i]] <- data.frame(NA)
      day_hr_cache[[i]] <- data.frame(NA)
      day_sk_cache[[i]] <- data.frame(NA)
      day_st_cache[[i]] <- data.frame(NA)
      
      day_rhr_cache[[i]] <- data.frame(NA)
      
      day_highAct_gsr_cache[[i]] <- data.frame(NA)
      day_highAct_hr_cache[[i]] <- data.frame(NA)
      day_highAct_sk_cache[[i]] <- data.frame(NA)
      day_highAct_st_cache[[i]] <- data.frame(NA)
      
      day_lowAct_gsr_cache[[i]] <- data.frame(NA)
      day_lowAct_hr_cache[[i]] <- data.frame(NA)
      day_lowAct_sk_cache[[i]] <- data.frame(NA)
      day_lowAct_st_cache[[i]] <- data.frame(NA)
      
      night_gsr_cache[[i]] <- data.frame(NA)
      night_hr_cache[[i]] <- data.frame(NA)
      night_sk_cache[[i]] <- data.frame(NA)
      night_st_cache[[i]] <- data.frame(NA)
      
      night_rhr_cache[[i]] <- data.frame(NA)
      
      night_highAct_gsr_cache[[i]] <- data.frame(NA)
      night_highAct_hr_cache[[i]] <- data.frame(NA)
      night_highAct_sk_cache[[i]] <- data.frame(NA)
      night_highAct_st_cache[[i]] <- data.frame(NA)
      
      night_lowAct_gsr_cache[[i]] <- data.frame(NA)
      night_lowAct_hr_cache[[i]] <- data.frame(NA)
      night_lowAct_sk_cache[[i]] <- data.frame(NA)
      night_lowAct_st_cache[[i]] <- data.frame(NA)
      print(paste0("#",i," had at least 3 completely empty columns; subject omitted."))
      next
    }
    # if(TRUE %in% emptyCols){
    #   gsr_cache[[i]] <- data.frame(NA)
    #   hr_cache[[i]] <- data.frame(NA)
    #   sk_cache[[i]] <- data.frame(NA)
    #   st_cache[[i]] <- data.frame(NA)
    #   
    #   rhr_cache[[i]] <- data.frame(NA)
    #   
    #   highAct_gsr_cache[[i]] <- data.frame(NA)
    #   highAct_hr_cache[[i]] <- data.frame(NA)
    #   highAct_sk_cache[[i]] <- data.frame(NA)
    #   highAct_st_cache[[i]] <- data.frame(NA)
    #   
    #   lowAct_gsr_cache[[i]] <- data.frame(NA)
    #   lowAct_hr_cache[[i]] <- data.frame(NA)
    #   lowAct_sk_cache[[i]] <- data.frame(NA)
    #   lowAct_st_cache[[i]] <- data.frame(NA)
    #   
    #   day_gsr_cache[[i]] <- data.frame(NA)
    #   day_hr_cache[[i]] <- data.frame(NA)
    #   day_sk_cache[[i]] <- data.frame(NA)
    #   day_st_cache[[i]] <- data.frame(NA)
    #   
    #   day_rhr_cache[[i]] <- data.frame(NA)
    #   
    #   day_highAct_gsr_cache[[i]] <- data.frame(NA)
    #   day_highAct_hr_cache[[i]] <- data.frame(NA)
    #   day_highAct_sk_cache[[i]] <- data.frame(NA)
    #   day_highAct_st_cache[[i]] <- data.frame(NA)
    #   
    #   day_lowAct_gsr_cache[[i]] <- data.frame(NA)
    #   day_lowAct_hr_cache[[i]] <- data.frame(NA)
    #   day_lowAct_sk_cache[[i]] <- data.frame(NA)
    #   day_lowAct_st_cache[[i]] <- data.frame(NA)
    #   
    #   night_gsr_cache[[i]] <- data.frame(NA)
    #   night_hr_cache[[i]] <- data.frame(NA)
    #   night_sk_cache[[i]] <- data.frame(NA)
    #   night_st_cache[[i]] <- data.frame(NA)
    #   
    #   night_rhr_cache[[i]] <- data.frame(NA)
    #   
    #   night_highAct_gsr_cache[[i]] <- data.frame(NA)
    #   night_highAct_hr_cache[[i]] <- data.frame(NA)
    #   night_highAct_sk_cache[[i]] <- data.frame(NA)
    #   night_highAct_st_cache[[i]] <- data.frame(NA)
    #   
    #   night_lowAct_gsr_cache[[i]] <- data.frame(NA)
    #   night_lowAct_hr_cache[[i]] <- data.frame(NA)
    #   night_lowAct_sk_cache[[i]] <- data.frame(NA)
    #   night_lowAct_st_cache[[i]] <- data.frame(NA)
    #   print(paste(i,"Empty"))
    #   next
    # }
    #if file has less than a day's worth of data; omit subject.
    if(NROW(df) < 1440){
      gsr_cache[[i]] <- data.frame(NA)
      hr_cache[[i]] <- data.frame(NA)
      sk_cache[[i]] <- data.frame(NA)
      st_cache[[i]] <- data.frame(NA)
      
      rhr_cache[[i]] <- data.frame(NA)
      
      highAct_gsr_cache[[i]] <- data.frame(NA)
      highAct_hr_cache[[i]] <- data.frame(NA)
      highAct_sk_cache[[i]] <- data.frame(NA)
      highAct_st_cache[[i]] <- data.frame(NA)
      
      lowAct_gsr_cache[[i]] <- data.frame(NA)
      lowAct_hr_cache[[i]] <- data.frame(NA)
      lowAct_sk_cache[[i]] <- data.frame(NA)
      lowAct_st_cache[[i]] <- data.frame(NA)
      
      day_gsr_cache[[i]] <- data.frame(NA)
      day_hr_cache[[i]] <- data.frame(NA)
      day_sk_cache[[i]] <- data.frame(NA)
      day_st_cache[[i]] <- data.frame(NA)
      
      day_rhr_cache[[i]] <- data.frame(NA)
      
      day_highAct_gsr_cache[[i]] <- data.frame(NA)
      day_highAct_hr_cache[[i]] <- data.frame(NA)
      day_highAct_sk_cache[[i]] <- data.frame(NA)
      day_highAct_st_cache[[i]] <- data.frame(NA)
      
      day_lowAct_gsr_cache[[i]] <- data.frame(NA)
      day_lowAct_hr_cache[[i]] <- data.frame(NA)
      day_lowAct_sk_cache[[i]] <- data.frame(NA)
      day_lowAct_st_cache[[i]] <- data.frame(NA)
      
      night_gsr_cache[[i]] <- data.frame(NA)
      night_hr_cache[[i]] <- data.frame(NA)
      night_sk_cache[[i]] <- data.frame(NA)
      night_st_cache[[i]] <- data.frame(NA)
      
      night_rhr_cache[[i]] <- data.frame(NA)
      
      night_highAct_gsr_cache[[i]] <- data.frame(NA)
      night_highAct_hr_cache[[i]] <- data.frame(NA)
      night_highAct_sk_cache[[i]] <- data.frame(NA)
      night_highAct_st_cache[[i]] <- data.frame(NA)
      
      night_lowAct_gsr_cache[[i]] <- data.frame(NA)
      night_lowAct_hr_cache[[i]] <- data.frame(NA)
      night_lowAct_sk_cache[[i]] <- data.frame(NA)
      night_lowAct_st_cache[[i]] <- data.frame(NA)
      next
    }
    #get descriptive stats for main variables
    gsr_cache[[i]] <- data.frame(describe(df$GSR))
    hr_cache[[i]] <- data.frame(describe(df$Heart_Rate))
    sk_cache[[i]] <- data.frame(describe(df$Skin_Temperature_F))
    st_cache[[i]] <- data.frame(describe(df$Steps))
    
    #label hours (makes subsetting day/night easier)
    source("F:\\R_Commands\\LabelHours.R")
    label.hours(df, "Timestamp_Local")
    
    #describe day/night subsets
    day_gsr_cache[[i]] <- data.frame(describe(
      df$GSR[which(df$Hour>=8 & df$Hour<20)]))
    day_hr_cache[[i]] <- data.frame(describe(
      df$Heart_Rate[which(df$Hour>=8 & df$Hour<20)]))
    day_sk_cache[[i]] <- data.frame(describe(
      df$Skin_Temperature_F[which(df$Hour>=8 & df$Hour<20)]))
    day_st_cache[[i]] <- data.frame(describe(
      df$Steps[which(df$Hour>=8 & df$Hour<20)]))
    
    night_gsr_cache[[i]] <- data.frame(describe(
      df$GSR[which(df$Hour>=20 | df$Hour<8)]))
    night_hr_cache[[i]] <- data.frame(describe(
      df$Heart_Rate[which(df$Hour>=20 | df$Hour<8)]))
    night_sk_cache[[i]] <- data.frame(describe(
      df$Skin_Temperature_F[which(df$Hour>=20 | df$Hour<8)]))
    night_st_cache[[i]] <- data.frame(describe(
      df$Steps[which(df$Hour>=20 | df$Hour<8)]))
    
    #optional if statement for testing purposes; default is always true
    if(1==1){
      df$lowSteps <- df$Steps
      #set zero values in steps to NA; if changed, modify step decile calculations.
      df$Steps[df$Steps==0] <- NA
      #if step column is empty, fill with na then skip;
      if(all(is.na(df$Steps))){
        print(paste0("Error: No Steps for Candidate #",i))
        
        rhr_cache[[i]] <- data.frame(NA)
        
        highAct_gsr_cache[[i]] <- data.frame(NA)
        highAct_hr_cache[[i]] <- data.frame(NA)
        highAct_sk_cache[[i]] <- data.frame(NA)
        highAct_st_cache[[i]] <- data.frame(NA)
        
        lowAct_gsr_cache[[i]] <- data.frame(NA)
        lowAct_hr_cache[[i]] <- data.frame(NA)
        lowAct_sk_cache[[i]] <- data.frame(NA)
        lowAct_st_cache[[i]] <- data.frame(NA)
        
        day_gsr_cache[[i]] <- data.frame(NA)
        day_hr_cache[[i]] <- data.frame(NA)
        day_sk_cache[[i]] <- data.frame(NA)
        day_st_cache[[i]] <- data.frame(NA)
        
        day_rhr_cache[[i]] <- data.frame(NA)
        
        day_highAct_gsr_cache[[i]] <- data.frame(NA)
        day_highAct_hr_cache[[i]] <- data.frame(NA)
        day_highAct_sk_cache[[i]] <- data.frame(NA)
        day_highAct_st_cache[[i]] <- data.frame(NA)
        
        day_lowAct_gsr_cache[[i]] <- data.frame(NA)
        day_lowAct_hr_cache[[i]] <- data.frame(NA)
        day_lowAct_sk_cache[[i]] <- data.frame(NA)
        day_lowAct_st_cache[[i]] <- data.frame(NA)
        
        night_gsr_cache[[i]] <- data.frame(NA)
        night_hr_cache[[i]] <- data.frame(NA)
        night_sk_cache[[i]] <- data.frame(NA)
        night_st_cache[[i]] <- data.frame(NA)
        
        night_rhr_cache[[i]] <- data.frame(NA)
        
        night_highAct_gsr_cache[[i]] <- data.frame(NA)
        night_highAct_hr_cache[[i]] <- data.frame(NA)
        night_highAct_sk_cache[[i]] <- data.frame(NA)
        night_highAct_st_cache[[i]] <- data.frame(NA)
        
        night_lowAct_gsr_cache[[i]] <- data.frame(NA)
        night_lowAct_hr_cache[[i]] <- data.frame(NA)
        night_lowAct_sk_cache[[i]] <- data.frame(NA)
        night_lowAct_st_cache[[i]] <- data.frame(NA)
        next
      } else {
        if(1==1){
          df$Steps[df$Steps==0] <- NA
          #ID steps decile locations
          StDecLocations <- quantile(
            df$Steps, na.rm=TRUE, 
            probs = seq(0.1,0.9,by=0.1))
          if(length(unique(StDecLocations)) != 9){
            check <- which(table(StDecLocations) > 1)
            for(u in 1:length(check)){
              check2 <- which(StDecLocations==names(check)[u])
              theMin <- as.numeric(min(check2))
              for(v in 1:length(check2)){
                if(check2[v] == theMin){
                  next
                } else {
                  StDecLocations[check2[v]] <- sum(
                    StDecLocations[check2[v]],0.0000000000001)
                }
              }
            }
          }
          df$StDecID <- findInterval(df$Steps,
                                     c(-Inf,StDecLocations, Inf))
        }
        #ID heart rate decile locations
        HrDecLocations <- quantile(
          df$Heart_Rate, na.rm=TRUE, 
          probs = seq(0.1,0.9, by=0.1))
        if(length(unique(HrDecLocations)) != 9){
          check <- which(table(HrDecLocations) > 1)
          for(u in 1:length(check)){
            check2 <- which(HrDecLocations==names(check)[u])
            theMin <- as.numeric(min(check2))
            for(v in 1:length(check2)){
              if(check2[v] == theMin){
                next
              } else {
                HrDecLocations[check2[v]] <- sum(
                  HrDecLocations[check2[v]],0.0000000000001)
              }
            }
          }
        }
        df$HrDecID <- findInterval(df$Heart_Rate,
                                   c(-Inf,HrDecLocations,Inf))
        #ID skin temperature decile locations
        SkDecLocations <- quantile(
          df$Skin_Temperature_F, na.rm=TRUE, 
          probs = seq(0.1,0.9,by=0.1))
        if(length(unique(SkDecLocations)) != 9){
          check <- which(table(SkDecLocations) > 1)
          for(u in 1:length(check)){
            check2 <- which(SkDecLocations==names(check)[u])
            theMin <- as.numeric(min(check2))
            for(v in 1:length(check2)){
              if(check2[v] == theMin){
                next
              } else {
                SkDecLocations[check2[v]] <- sum(
                  SkDecLocations[check2[v]],0.0000000000001)
              }
            }
          }
        }
        df$SkDecID <- findInterval(df$Skin_Temperature_F,
                                   c(-Inf,SkDecLocations, Inf))
        #ID gsr decile locations
        GsrDecLocations <- quantile(
          df$GSR, na.rm=TRUE, 
          probs = seq(0.1,0.9,by=0.1))
        if(length(unique(GsrDecLocations)) != 9){
          check <- which(table(GsrDecLocations) > 1)
          for(u in 1:length(check)){
            check2 <- which(GsrDecLocations==names(check)[u])
            theMin <- as.numeric(min(check2))
            for(v in 1:length(check2)){
              if(check2[v] == theMin){
                next
              } else {
                GsrDecLocations[check2[v]] <- sum(
                  GsrDecLocations[check2[v]],0.0000000000001)
              }
            }
          }
        }
        df$GsrDecID <- findInterval(df$GSR,
                                    c(-Inf,GsrDecLocations, Inf))
        
        #SUBSET RESTING HEART RATE PARAMETERS TO "rdf" table
        restingST <- ave(df$lowSteps, df$Wearable_Account_MD5,
                         FUN=function(x) rollapplyr(x, 10, sum, by=1, partial=TRUE))
        restingST[1:10-1] <- c(rep(NA,10-1)) #window-1

        #To record resting heart rate as rolling 10-minute mean when sum of steps in last 10 min < 10:
        # restingHR <- ave(df$Heart_Rate, df$Wearable_Account_MD5,
        #                  FUN=function(x) rollapplyr(x, 10, mean, na.rm=TRUE,
        #                                             by=1, partial=TRUE, fill=NA))
        # 
        # restingHR <- restingHR[which(restingST<10 & !is.na(restingST))]
        
        #To record resting HR as all values when sum of steps in last 10 min < 10:
        restingHR <- df$Heart_Rate[which(restingST<10 & !is.na(restingST))]
        
        rhr_cache[[i]] <- data.frame(describe(restingHR))
        
        df_day <- df[which(df$Hour>=8 & df$Hour<20),]
        
        restingST <- ave(df_day$lowSteps, df_day$Wearable_Account_MD5,
                         FUN=function(x) rollapplyr(x, 10, sum, by=1, partial=TRUE)) 
        restingST[1:10-1] <- c(rep(NA,10-1)) #window-1
        
        #To record resting heart rate as rolling 10-minute mean when sum of steps in last 10 min < 10:
        # restingHR <- ave(df$Heart_Rate, df$Wearable_Account_MD5,
        #                  FUN=function(x) rollapplyr(x, 10, mean, na.rm=TRUE,
        #                                             by=1, partial=TRUE, fill=NA))
        # 
        # restingHR <- restingHR[which(restingST<10 & !is.na(restingST))]
        
        #To record resting HR as all values when sum of steps in last 10 min < 10:
        restingHR <- df_day$Heart_Rate[which(restingST<10 & !is.na(restingST))]
        
        # restingHR <- restingHR[which(restingST<10 & !is.na(restingST))] 
        
        day_rhr_cache[[i]] <- data.frame(describe(restingHR))
        rm(df_day)
        
        df_night <- df[which(df$Hour>=20 | df$Hour<8),]
        
        restingST <- ave(df_night$lowSteps, df_night$Wearable_Account_MD5,
                         FUN=function(x) rollapplyr(x, 10, sum, by=1, partial=TRUE))
        restingST[1:10-1] <- c(rep(NA,10-1)) #window-1
        
        # restingHR <- ave(df_night$Heart_Rate, df_night$Wearable_Account_MD5,
                         # FUN=function(x) rollapplyr(x, 10, mean, by=1, partial=TRUE)) 
        
        restingHR <- df_night$Heart_Rate[which(restingST<10 & !is.na(restingST))] 
        
        night_rhr_cache[[i]] <- data.frame(describe(restingHR))
        rm(df_night)
        
        #label high activity (defined as top 10% of Steps)
        if(1==1){
          df$highActivity <- ifelse(
            df$StDecID==10,
            TRUE,
            FALSE)
          #df$highActivity_eventNum <- rleid(df$highActivity)
        }
        
        durationDf <- df[df$highActivity == "TRUE",]
        durationDf$Date <- as.POSIXct(paste(durationDf$Timestamp_Local),
                                      format="%Y-%m-%d %H:%M:%S")
        
        
        #describe variables during high activity
        highAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        highAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        highAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        highAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        
        df_day <- df[which(df$Hour>=8 & df$Hour<20),]
        durationDf_day <- df_day[df_day$highActivity == "TRUE",]
        durationDf_day$Date <- as.POSIXct(paste(durationDf_day$Timestamp_Local),
                                          format="%Y-%m-%d %H:%M:%S")
        
        day_highAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        day_highAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        day_highAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        day_highAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        rm(df_day)
        
        df_night <- df[which(df$Hour>=20 | df$Hour<8),]
        durationDf_day <- df_night[df_night$highActivity == "TRUE",]
        durationDf_day$Date <- as.POSIXct(paste(durationDf_day$Timestamp_Local),
                                          format="%Y-%m-%d %H:%M:%S")
        
        night_highAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        night_highAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        night_highAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        night_highAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        rm(df_night)
        
        #label low activity (defined as lowest 10% of Steps)
        if(1==1){
          df$lowActivity <- ifelse(
            df$StDecID==1,
            TRUE,
            FALSE)
          #df$lowActivity_eventNum <- rleid(df$lowActivity)
        }
        
        durationDf <- df[df$lowActivity == "TRUE",]
        durationDf$Date <- as.POSIXct(paste(durationDf$Timestamp_Local),
                                      format="%Y-%m-%d %H:%M:%S")
        
        #describe variables during low activity
        lowAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        lowAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        lowAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        lowAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        
        df_day <- df[which(df$Hour>=8 & df$Hour<20),]
        durationDf <- df_day[df_day$lowActivity == "TRUE",]
        durationDf$Date <- as.POSIXct(paste(durationDf$Timestamp_Local),
                                      format="%Y-%m-%d %H:%M:%S")
        
        day_lowAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        day_lowAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        day_lowAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        day_lowAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        rm(df_day)
        
        df_night <- df[which(df$Hour>=20 | df$Hour<8),]
        durationDf <- df_night[df_night$lowActivity == "TRUE",]
        durationDf$Date <- as.POSIXct(paste(durationDf$Timestamp_Local),
                                      format="%Y-%m-%d %H:%M:%S")
        
        night_lowAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        night_lowAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        night_lowAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        night_lowAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        rm(df_night)
      }
    }
  } else {
    gsr_cache[[i]] <- data.frame(NA)
    hr_cache[[i]] <- data.frame(NA)
    sk_cache[[i]] <- data.frame(NA)
    st_cache[[i]] <- data.frame(NA)
    
    rhr_cache[[i]] <- data.frame(NA)
    
    highAct_gsr_cache[[i]] <- data.frame(NA)
    highAct_hr_cache[[i]] <- data.frame(NA)
    highAct_sk_cache[[i]] <- data.frame(NA)
    highAct_st_cache[[i]] <- data.frame(NA)
    
    lowAct_gsr_cache[[i]] <- data.frame(NA)
    lowAct_hr_cache[[i]] <- data.frame(NA)
    lowAct_sk_cache[[i]] <- data.frame(NA)
    lowAct_st_cache[[i]] <- data.frame(NA)
    
    day_gsr_cache[[i]] <- data.frame(NA)
    day_hr_cache[[i]] <- data.frame(NA)
    day_sk_cache[[i]] <- data.frame(NA)
    day_st_cache[[i]] <- data.frame(NA)
    
    day_rhr_cache[[i]] <- data.frame(NA)
    
    day_highAct_gsr_cache[[i]] <- data.frame(NA)
    day_highAct_hr_cache[[i]] <- data.frame(NA)
    day_highAct_sk_cache[[i]] <- data.frame(NA)
    day_highAct_st_cache[[i]] <- data.frame(NA)
    
    day_lowAct_gsr_cache[[i]] <- data.frame(NA)
    day_lowAct_hr_cache[[i]] <- data.frame(NA)
    day_lowAct_sk_cache[[i]] <- data.frame(NA)
    day_lowAct_st_cache[[i]] <- data.frame(NA)
    
    night_gsr_cache[[i]] <- data.frame(NA)
    night_hr_cache[[i]] <- data.frame(NA)
    night_sk_cache[[i]] <- data.frame(NA)
    night_st_cache[[i]] <- data.frame(NA)
    
    night_rhr_cache[[i]] <- data.frame(NA)
    
    night_highAct_gsr_cache[[i]] <- data.frame(NA)
    night_highAct_hr_cache[[i]] <- data.frame(NA)
    night_highAct_sk_cache[[i]] <- data.frame(NA)
    night_highAct_st_cache[[i]] <- data.frame(NA)
    
    night_lowAct_gsr_cache[[i]] <- data.frame(NA)
    night_lowAct_hr_cache[[i]] <- data.frame(NA)
    night_lowAct_sk_cache[[i]] <- data.frame(NA)
    night_lowAct_st_cache[[i]] <- data.frame(NA)
    
    print(i)
    next
  }
  print(i)
}

gsr_cache <- rbind.fill(gsr_cache)
hr_cache <- rbind.fill(hr_cache)
sk_cache <- rbind.fill(sk_cache)
st_cache <- rbind.fill(st_cache)

rhr_cache <- rbind.fill(rhr_cache)

highAct_gsr_cache <- rbind.fill(highAct_gsr_cache)
highAct_hr_cache <- rbind.fill(highAct_hr_cache)
highAct_sk_cache <- rbind.fill(highAct_sk_cache)
highAct_st_cache <- rbind.fill(highAct_st_cache)

lowAct_gsr_cache <- rbind.fill(lowAct_gsr_cache)
lowAct_hr_cache <- rbind.fill(lowAct_hr_cache)
lowAct_sk_cache <- rbind.fill(lowAct_sk_cache)
lowAct_st_cache <- rbind.fill(lowAct_st_cache)

day_gsr_cache <- rbind.fill(day_gsr_cache)
day_hr_cache <- rbind.fill(day_hr_cache)
day_sk_cache <- rbind.fill(day_sk_cache)
day_st_cache <- rbind.fill(day_st_cache)

day_rhr_cache <- rbind.fill(day_rhr_cache)

day_highAct_gsr_cache <- rbind.fill(day_highAct_gsr_cache)
day_highAct_hr_cache <- rbind.fill(day_highAct_hr_cache)
day_highAct_sk_cache <- rbind.fill(day_highAct_sk_cache)
day_highAct_st_cache <- rbind.fill(day_highAct_st_cache)

day_lowAct_gsr_cache <- rbind.fill(day_lowAct_gsr_cache)
day_lowAct_hr_cache <- rbind.fill(day_lowAct_hr_cache)
day_lowAct_sk_cache <- rbind.fill(day_lowAct_sk_cache)
day_lowAct_st_cache <- rbind.fill(day_lowAct_st_cache)

night_gsr_cache <- rbind.fill(night_gsr_cache)
night_hr_cache <- rbind.fill(night_hr_cache)
night_sk_cache <- rbind.fill(night_sk_cache)
night_st_cache <- rbind.fill(night_st_cache)

night_rhr_cache <- rbind.fill(night_rhr_cache)

night_highAct_gsr_cache <- rbind.fill(night_highAct_gsr_cache)
night_highAct_hr_cache <- rbind.fill(night_highAct_hr_cache)
night_highAct_sk_cache <- rbind.fill(night_highAct_sk_cache)
night_highAct_st_cache <- rbind.fill(night_highAct_st_cache)

night_lowAct_gsr_cache <- rbind.fill(night_lowAct_gsr_cache)
night_lowAct_hr_cache <- rbind.fill(night_lowAct_hr_cache)
night_lowAct_sk_cache <- rbind.fill(night_lowAct_sk_cache)
night_lowAct_st_cache <- rbind.fill(night_lowAct_st_cache)

gsr_cache <- gsr_cache[,-1] #first column was for NAs
hr_cache <- hr_cache[,-1] #first column was for NAs
sk_cache <- sk_cache[,-1] #first column was for NAs
st_cache <- st_cache[,-1] #first column was for NAs

rhr_cache <- rhr_cache[,-1] #first column was for NAs

highAct_gsr_cache <- highAct_gsr_cache[,-1] #first column was for NAs
highAct_hr_cache <- highAct_hr_cache[,-1] #first column was for NAs
highAct_sk_cache <- highAct_sk_cache[,-1] #first column was for NAs
highAct_st_cache <- highAct_st_cache[,-1] #first column was for NAs

lowAct_gsr_cache <- lowAct_gsr_cache[,-1] #first column was for NAs
lowAct_hr_cache <- lowAct_hr_cache[,-1] #first column was for NAs
lowAct_sk_cache <- lowAct_sk_cache[,-1] #first column was for NAs
lowAct_st_cache <- lowAct_st_cache[,-1] #first column was for NAs

day_gsr_cache <- day_gsr_cache[,-1] #first column was for NAs
day_hr_cache <- day_hr_cache[,-1] #first column was for NAs
day_sk_cache <- day_sk_cache[,-1] #first column was for NAs
day_st_cache <- day_st_cache[,-1] #first column was for NAs

day_rhr_cache <- day_rhr_cache[,-1] #first column was for NAs

day_highAct_gsr_cache <- day_highAct_gsr_cache[,-1] #first column was for NAs
day_highAct_hr_cache <- day_highAct_hr_cache[,-1] #first column was for NAs
day_highAct_sk_cache <- day_highAct_sk_cache[,-1] #first column was for NAs
day_highAct_st_cache <- day_highAct_st_cache[,-1] #first column was for NAs

day_lowAct_gsr_cache <- day_lowAct_gsr_cache[,-1] #first column was for NAs
day_lowAct_hr_cache <- day_lowAct_hr_cache[,-1] #first column was for NAs
day_lowAct_sk_cache <- day_lowAct_sk_cache[,-1] #first column was for NAs
day_lowAct_st_cache <- day_lowAct_st_cache[,-1] #first column was for NAs

night_gsr_cache <- night_gsr_cache[,-1] #first column was for NAs
night_hr_cache <- night_hr_cache[,-1] #first column was for NAs
night_sk_cache <- night_sk_cache[,-1] #first column was for NAs
night_st_cache <- night_st_cache[,-1] #first column was for NAs

night_rhr_cache <- night_rhr_cache[,-1] #first column was for NAs

night_highAct_gsr_cache <- night_highAct_gsr_cache[,-1] #first column was for NAs
night_highAct_hr_cache <- night_highAct_hr_cache[,-1] #first column was for NAs
night_highAct_sk_cache <- night_highAct_sk_cache[,-1] #first column was for NAs
night_highAct_st_cache <- night_highAct_st_cache[,-1] #first column was for NAs

night_lowAct_gsr_cache <- night_lowAct_gsr_cache[,-1] #first column was for NAs
night_lowAct_hr_cache <- night_lowAct_hr_cache[,-1] #first column was for NAs
night_lowAct_sk_cache <- night_lowAct_sk_cache[,-1] #first column was for NAs
night_lowAct_st_cache <- night_lowAct_st_cache[,-1] #first column was for NAs

colnames(gsr_cache) <- paste0("gsr_",colnames(gsr_cache))
colnames(hr_cache) <- paste0("hr_",colnames(hr_cache))
colnames(sk_cache) <- paste0("sk_",colnames(sk_cache))
colnames(st_cache) <- paste0("st_",colnames(st_cache))

colnames(rhr_cache) <- paste0("rhr_",colnames(rhr_cache))

colnames(highAct_gsr_cache) <- paste0("highAct_gsr_",colnames(highAct_gsr_cache))
colnames(highAct_hr_cache) <- paste0("highAct_hr_",colnames(highAct_hr_cache))
colnames(highAct_sk_cache) <- paste0("highAct_sk_",colnames(highAct_sk_cache))
colnames(highAct_st_cache) <- paste0("highAct_st_",colnames(highAct_st_cache))

colnames(lowAct_gsr_cache) <- paste0("lowAct_gsr_",colnames(lowAct_gsr_cache))
colnames(lowAct_hr_cache) <- paste0("lowAct_hr_",colnames(lowAct_hr_cache))
colnames(lowAct_sk_cache) <- paste0("lowAct_sk_",colnames(lowAct_sk_cache))
colnames(lowAct_st_cache) <- paste0("lowAct_st_",colnames(lowAct_st_cache))

colnames(day_gsr_cache) <- paste0("day_gsr_",colnames(day_gsr_cache))
colnames(day_hr_cache) <- paste0("day_hr_",colnames(day_hr_cache))
colnames(day_sk_cache) <- paste0("day_sk_",colnames(day_sk_cache))
colnames(day_st_cache) <- paste0("day_st_",colnames(day_st_cache))

colnames(day_rhr_cache) <- paste0("day_rhr_",colnames(day_rhr_cache))

colnames(day_highAct_gsr_cache) <- paste0("day_highAct_gsr_",
                                          colnames(day_highAct_gsr_cache))
colnames(day_highAct_hr_cache) <- paste0("day_highAct_hr_",
                                         colnames(day_highAct_hr_cache))
colnames(day_highAct_sk_cache) <- paste0("day_highAct_sk_",
                                         colnames(day_highAct_sk_cache))
colnames(day_highAct_st_cache) <- paste0("day_highAct_st_",
                                         colnames(day_highAct_st_cache))

colnames(day_lowAct_gsr_cache) <- paste0("day_lowAct_gsr_",
                                         colnames(day_lowAct_gsr_cache))
colnames(day_lowAct_hr_cache) <- paste0("day_lowAct_hr_",
                                        colnames(day_lowAct_hr_cache))
colnames(day_lowAct_sk_cache) <- paste0("day_lowAct_sk_",
                                        colnames(day_lowAct_sk_cache))
colnames(day_lowAct_st_cache) <- paste0("day_lowAct_st_",
                                        colnames(day_lowAct_st_cache))

colnames(night_gsr_cache) <- paste0("night_gsr_",colnames(night_gsr_cache))
colnames(night_hr_cache) <- paste0("night_hr_",colnames(night_hr_cache))
colnames(night_sk_cache) <- paste0("night_sk_",colnames(night_sk_cache))
colnames(night_st_cache) <- paste0("night_st_",colnames(night_st_cache))

colnames(night_rhr_cache) <- paste0("night_rhr_",colnames(night_rhr_cache))

colnames(night_highAct_gsr_cache) <- paste0("night_highAct_gsr_",
                                            colnames(night_highAct_gsr_cache))
colnames(night_highAct_hr_cache) <- paste0("night_highAct_hr_",
                                           colnames(night_highAct_hr_cache))
colnames(night_highAct_sk_cache) <- paste0("night_highAct_sk_",
                                           colnames(night_highAct_sk_cache))
colnames(night_highAct_st_cache) <- paste0("night_highAct_st_",
                                           colnames(night_highAct_st_cache))

colnames(night_lowAct_gsr_cache) <- paste0("night_lowAct_gsr_",
                                           colnames(night_lowAct_gsr_cache))
colnames(night_lowAct_hr_cache) <- paste0("night_lowAct_hr_",
                                          colnames(night_lowAct_hr_cache))
colnames(night_lowAct_sk_cache) <- paste0("night_lowAct_sk_",
                                          colnames(night_lowAct_sk_cache))
colnames(night_lowAct_st_cache) <- paste0("night_lowAct_st_",
                                          colnames(night_lowAct_st_cache))

stats <- cbind(
  ClinWithWear,gsr_cache,hr_cache,sk_cache,st_cache,
  rhr_cache,highAct_gsr_cache,highAct_hr_cache,
  highAct_sk_cache,highAct_st_cache,
  lowAct_gsr_cache,lowAct_hr_cache,
  lowAct_sk_cache,lowAct_st_cache,
  day_gsr_cache,day_hr_cache,day_sk_cache,day_st_cache,
  day_rhr_cache,day_highAct_gsr_cache,day_highAct_hr_cache,
  day_highAct_sk_cache,day_highAct_st_cache,
  day_lowAct_gsr_cache,day_lowAct_hr_cache,
  day_lowAct_sk_cache,day_lowAct_st_cache,
  night_gsr_cache,night_hr_cache,night_sk_cache,night_st_cache,
  night_rhr_cache,night_highAct_gsr_cache,night_highAct_hr_cache,
  night_highAct_sk_cache,night_highAct_st_cache,
  night_lowAct_gsr_cache,night_lowAct_hr_cache,
  night_lowAct_sk_cache,night_lowAct_st_cache)

write.csv(stats,
          paste0("F:\\PhysSubTables_20161111_Normal_20170802\\Basis2016_Clean_Norm_DayPrior_20180504.csv"),
          row.names=FALSE)


#### DESCRIBE 3-DAY-PRIOR DATA ####
files <- list.files("F:\\BasisSubsets\\FromBasisData_20161111_Normal_20170802\\3DayPrior_Files",
                    pattern="*.csv",full.names=TRUE)

gsr_cache <- list()
hr_cache <- list()
sk_cache <- list()
st_cache <- list()

rhr_cache <- list()

highAct_gsr_cache <- list()
highAct_hr_cache <- list()
highAct_sk_cache <- list()
highAct_st_cache <- list()

lowAct_gsr_cache <- list()
lowAct_hr_cache <- list()
lowAct_sk_cache <- list()
lowAct_st_cache <- list()


day_gsr_cache <- list()
day_hr_cache <- list()
day_sk_cache <- list()
day_st_cache <- list()

day_rhr_cache <- list()

day_highAct_gsr_cache <- list()
day_highAct_hr_cache <- list()
day_highAct_sk_cache <- list()
day_highAct_st_cache <- list()

day_lowAct_gsr_cache <- list()
day_lowAct_hr_cache <- list()
day_lowAct_sk_cache <- list()
day_lowAct_st_cache <- list()

night_gsr_cache <- list()
night_hr_cache <- list()
night_sk_cache <- list()
night_st_cache <- list()

night_rhr_cache <- list()

night_highAct_gsr_cache <- list()
night_highAct_hr_cache <- list()
night_highAct_sk_cache <- list()
night_highAct_st_cache <- list()

night_lowAct_gsr_cache <- list()
night_lowAct_hr_cache <- list()
night_lowAct_sk_cache <- list()
night_lowAct_st_cache <- list()
###

# for(i in 1:length(files[1:200])){
#   print(paste(i, file.info(files[i])$size))
# }
# i=140

for(i in 1:length(files)){                       
  check <- file.info(files[i])$size
  if(check!=0){
    df <- read.csv(files[i],header=TRUE,sep=",")
    if("Wearable_Account_MD5" %in% names(df)){
      if(length(intersect(unique(df$Wearable_Account_MD5),rem))!=0){
        df <- df[!(df$Wearable_Account_MD5 %in% rem),]
      }
    } else {
      if(!("Wearable_Account_MD5" %in% names(df))){
        print(paste("MD5 not found in",i))
        print(names(df))
        next
      }
    }
    df$Date <- as.POSIXct(paste(df$Timestamp_Local),format="%Y-%m-%d %H:%M:%S")
    emptyCols <- apply(df,2,function(x) all(is.na(x)))
    if(TRUE %in% emptyCols){
      gsr_cache[[i]] <- data.frame(NA)
      hr_cache[[i]] <- data.frame(NA)
      sk_cache[[i]] <- data.frame(NA)
      st_cache[[i]] <- data.frame(NA)
      
      rhr_cache[[i]] <- data.frame(NA)
      
      highAct_gsr_cache[[i]] <- data.frame(NA)
      highAct_hr_cache[[i]] <- data.frame(NA)
      highAct_sk_cache[[i]] <- data.frame(NA)
      highAct_st_cache[[i]] <- data.frame(NA)
      
      lowAct_gsr_cache[[i]] <- data.frame(NA)
      lowAct_hr_cache[[i]] <- data.frame(NA)
      lowAct_sk_cache[[i]] <- data.frame(NA)
      lowAct_st_cache[[i]] <- data.frame(NA)
      
      day_gsr_cache[[i]] <- data.frame(NA)
      day_hr_cache[[i]] <- data.frame(NA)
      day_sk_cache[[i]] <- data.frame(NA)
      day_st_cache[[i]] <- data.frame(NA)
      
      day_rhr_cache[[i]] <- data.frame(NA)
      
      day_highAct_gsr_cache[[i]] <- data.frame(NA)
      day_highAct_hr_cache[[i]] <- data.frame(NA)
      day_highAct_sk_cache[[i]] <- data.frame(NA)
      day_highAct_st_cache[[i]] <- data.frame(NA)
      
      day_lowAct_gsr_cache[[i]] <- data.frame(NA)
      day_lowAct_hr_cache[[i]] <- data.frame(NA)
      day_lowAct_sk_cache[[i]] <- data.frame(NA)
      day_lowAct_st_cache[[i]] <- data.frame(NA)
      
      night_gsr_cache[[i]] <- data.frame(NA)
      night_hr_cache[[i]] <- data.frame(NA)
      night_sk_cache[[i]] <- data.frame(NA)
      night_st_cache[[i]] <- data.frame(NA)
      
      night_rhr_cache[[i]] <- data.frame(NA)
      
      night_highAct_gsr_cache[[i]] <- data.frame(NA)
      night_highAct_hr_cache[[i]] <- data.frame(NA)
      night_highAct_sk_cache[[i]] <- data.frame(NA)
      night_highAct_st_cache[[i]] <- data.frame(NA)
      
      night_lowAct_gsr_cache[[i]] <- data.frame(NA)
      night_lowAct_hr_cache[[i]] <- data.frame(NA)
      night_lowAct_sk_cache[[i]] <- data.frame(NA)
      night_lowAct_st_cache[[i]] <- data.frame(NA)
      next
    }
    if(NROW(df) < 1440){
      gsr_cache[[i]] <- data.frame(NA)
      hr_cache[[i]] <- data.frame(NA)
      sk_cache[[i]] <- data.frame(NA)
      st_cache[[i]] <- data.frame(NA)
      
      rhr_cache[[i]] <- data.frame(NA)
      
      highAct_gsr_cache[[i]] <- data.frame(NA)
      highAct_hr_cache[[i]] <- data.frame(NA)
      highAct_sk_cache[[i]] <- data.frame(NA)
      highAct_st_cache[[i]] <- data.frame(NA)
      
      lowAct_gsr_cache[[i]] <- data.frame(NA)
      lowAct_hr_cache[[i]] <- data.frame(NA)
      lowAct_sk_cache[[i]] <- data.frame(NA)
      lowAct_st_cache[[i]] <- data.frame(NA)
      
      day_gsr_cache[[i]] <- data.frame(NA)
      day_hr_cache[[i]] <- data.frame(NA)
      day_sk_cache[[i]] <- data.frame(NA)
      day_st_cache[[i]] <- data.frame(NA)
      
      day_rhr_cache[[i]] <- data.frame(NA)
      
      day_highAct_gsr_cache[[i]] <- data.frame(NA)
      day_highAct_hr_cache[[i]] <- data.frame(NA)
      day_highAct_sk_cache[[i]] <- data.frame(NA)
      day_highAct_st_cache[[i]] <- data.frame(NA)
      
      day_lowAct_gsr_cache[[i]] <- data.frame(NA)
      day_lowAct_hr_cache[[i]] <- data.frame(NA)
      day_lowAct_sk_cache[[i]] <- data.frame(NA)
      day_lowAct_st_cache[[i]] <- data.frame(NA)
      
      night_gsr_cache[[i]] <- data.frame(NA)
      night_hr_cache[[i]] <- data.frame(NA)
      night_sk_cache[[i]] <- data.frame(NA)
      night_st_cache[[i]] <- data.frame(NA)
      
      night_rhr_cache[[i]] <- data.frame(NA)
      
      night_highAct_gsr_cache[[i]] <- data.frame(NA)
      night_highAct_hr_cache[[i]] <- data.frame(NA)
      night_highAct_sk_cache[[i]] <- data.frame(NA)
      night_highAct_st_cache[[i]] <- data.frame(NA)
      
      night_lowAct_gsr_cache[[i]] <- data.frame(NA)
      night_lowAct_hr_cache[[i]] <- data.frame(NA)
      night_lowAct_sk_cache[[i]] <- data.frame(NA)
      night_lowAct_st_cache[[i]] <- data.frame(NA)
      next
    }
    gsr_cache[[i]] <- data.frame(describe(df$GSR))
    hr_cache[[i]] <- data.frame(describe(df$Heart_Rate))
    sk_cache[[i]] <- data.frame(describe(df$Skin_Temperature_F))
    st_cache[[i]] <- data.frame(describe(df$Steps))
    
    
    source("F:\\R_Commands\\LabelHours.R")
    label.hours(df, "Timestamp_Local")
    
    day_gsr_cache[[i]] <- data.frame(describe(
      df$GSR[which(df$Hour>=8 & df$Hour<20)]))
    day_hr_cache[[i]] <- data.frame(describe(
      df$Heart_Rate[which(df$Hour>=8 & df$Hour<20)]))
    day_sk_cache[[i]] <- data.frame(describe(
      df$Skin_Temperature_F[which(df$Hour>=8 & df$Hour<20)]))
    day_st_cache[[i]] <- data.frame(describe(
      df$Steps[which(df$Hour>=8 & df$Hour<20)]))
    
    night_gsr_cache[[i]] <- data.frame(describe(
      df$GSR[which(df$Hour>=20 | df$Hour<8)]))
    night_hr_cache[[i]] <- data.frame(describe(
      df$Heart_Rate[which(df$Hour>=20 | df$Hour<8)]))
    night_sk_cache[[i]] <- data.frame(describe(
      df$Skin_Temperature_F[which(df$Hour>=20 | df$Hour<8)]))
    night_st_cache[[i]] <- data.frame(describe(
      df$Steps[which(df$Hour>=20 | df$Hour<8)]))
    ###
    
    ## NEW
    
    if(1==1){
      df$lowSteps <- df$Steps
      df$Steps[df$Steps==0] <- NA
      if(all(is.na(df$Steps))){
        print(paste0("Error: No Steps for Candidate #",i))
        
        rhr_cache[[i]] <- data.frame(NA)
        
        highAct_gsr_cache[[i]] <- data.frame(NA)
        highAct_hr_cache[[i]] <- data.frame(NA)
        highAct_sk_cache[[i]] <- data.frame(NA)
        highAct_st_cache[[i]] <- data.frame(NA)
        
        lowAct_gsr_cache[[i]] <- data.frame(NA)
        lowAct_hr_cache[[i]] <- data.frame(NA)
        lowAct_sk_cache[[i]] <- data.frame(NA)
        lowAct_st_cache[[i]] <- data.frame(NA)
        
        day_gsr_cache[[i]] <- data.frame(NA)
        day_hr_cache[[i]] <- data.frame(NA)
        day_sk_cache[[i]] <- data.frame(NA)
        day_st_cache[[i]] <- data.frame(NA)
        
        day_rhr_cache[[i]] <- data.frame(NA)
        
        day_highAct_gsr_cache[[i]] <- data.frame(NA)
        day_highAct_hr_cache[[i]] <- data.frame(NA)
        day_highAct_sk_cache[[i]] <- data.frame(NA)
        day_highAct_st_cache[[i]] <- data.frame(NA)
        
        day_lowAct_gsr_cache[[i]] <- data.frame(NA)
        day_lowAct_hr_cache[[i]] <- data.frame(NA)
        day_lowAct_sk_cache[[i]] <- data.frame(NA)
        day_lowAct_st_cache[[i]] <- data.frame(NA)
        
        night_gsr_cache[[i]] <- data.frame(NA)
        night_hr_cache[[i]] <- data.frame(NA)
        night_sk_cache[[i]] <- data.frame(NA)
        night_st_cache[[i]] <- data.frame(NA)
        
        night_rhr_cache[[i]] <- data.frame(NA)
        
        night_highAct_gsr_cache[[i]] <- data.frame(NA)
        night_highAct_hr_cache[[i]] <- data.frame(NA)
        night_highAct_sk_cache[[i]] <- data.frame(NA)
        night_highAct_st_cache[[i]] <- data.frame(NA)
        
        night_lowAct_gsr_cache[[i]] <- data.frame(NA)
        night_lowAct_hr_cache[[i]] <- data.frame(NA)
        night_lowAct_sk_cache[[i]] <- data.frame(NA)
        night_lowAct_st_cache[[i]] <- data.frame(NA)
        next
        
      } else {
        if(1==1){
          df$Steps[df$Steps==0] <- NA
          #ID steps decile locations
          StDecLocations <- quantile(
            df$Steps, na.rm=TRUE, 
            probs = seq(0.1,0.9,by=0.1))
          if(length(unique(StDecLocations)) != 9){
            check <- which(table(StDecLocations) > 1)
            for(u in 1:length(check)){
              check2 <- which(StDecLocations==names(check)[u])
              theMin <- as.numeric(min(check2))
              for(v in 1:length(check2)){
                if(check2[v] == theMin){
                  next
                } else {
                  StDecLocations[check2[v]] <- sum(
                    StDecLocations[check2[v]],0.0000000000001)
                }
              }
            }
          }
          df$StDecID <- findInterval(df$Steps,
                                     c(-Inf,StDecLocations, Inf))
        }
        #ID heart rate decile locations
        HrDecLocations <- quantile(
          df$Heart_Rate, na.rm=TRUE, 
          probs = seq(0.1,0.9, by=0.1))
        if(length(unique(HrDecLocations)) != 9){
          check <- which(table(HrDecLocations) > 1)
          for(u in 1:length(check)){
            check2 <- which(HrDecLocations==names(check)[u])
            theMin <- as.numeric(min(check2))
            for(v in 1:length(check2)){
              if(check2[v] == theMin){
                next
              } else {
                HrDecLocations[check2[v]] <- sum(
                  HrDecLocations[check2[v]],0.0000000000001)
              }
            }
          }
        }
        df$HrDecID <- findInterval(df$Heart_Rate,
                                   c(-Inf,HrDecLocations,Inf))
        #ID skin temperature decile locations
        SkDecLocations <- quantile(
          df$Skin_Temperature_F, na.rm=TRUE, 
          probs = seq(0.1,0.9,by=0.1))
        if(length(unique(SkDecLocations)) != 9){
          check <- which(table(SkDecLocations) > 1)
          for(u in 1:length(check)){
            check2 <- which(SkDecLocations==names(check)[u])
            theMin <- as.numeric(min(check2))
            for(v in 1:length(check2)){
              if(check2[v] == theMin){
                next
              } else {
                SkDecLocations[check2[v]] <- sum(
                  SkDecLocations[check2[v]],0.0000000000001)
              }
            }
          }
        }
        df$SkDecID <- findInterval(df$Skin_Temperature_F,
                                   c(-Inf,SkDecLocations, Inf))
        #ID gsr decile locations
        GsrDecLocations <- quantile(
          df$GSR, na.rm=TRUE, 
          probs = seq(0.1,0.9,by=0.1))
        if(length(unique(GsrDecLocations)) != 9){
          check <- which(table(GsrDecLocations) > 1)
          for(u in 1:length(check)){
            check2 <- which(GsrDecLocations==names(check)[u])
            theMin <- as.numeric(min(check2))
            for(v in 1:length(check2)){
              if(check2[v] == theMin){
                next
              } else {
                GsrDecLocations[check2[v]] <- sum(
                  GsrDecLocations[check2[v]],0.0000000000001)
              }
            }
          }
        }
        df$GsrDecID <- findInterval(df$GSR,
                                    c(-Inf,GsrDecLocations, Inf))
        
        #SUBSET RESTING HEART RATE PARAMETERS TO "rdf" table
        restingST <- ave(df$lowSteps, df$Wearable_Account_MD5,
                         FUN=function(x) rollapplyr(x, 10, sum, by=1, partial=TRUE))
        restingST[1:10-1] <- c(rep(NA,10-1)) #window-1
        
        #To record resting heart rate as rolling 10-minute mean when sum of steps in last 10 min < 10:
        # restingHR <- ave(df$Heart_Rate, df$Wearable_Account_MD5,
        #                  FUN=function(x) rollapplyr(x, 10, mean, na.rm=TRUE,
        #                                             by=1, partial=TRUE, fill=NA))
        # 
        # restingHR <- restingHR[which(restingST<10 & !is.na(restingST))]
        
        #To record resting HR as all values when sum of steps in last 10 min < 10:
        restingHR <- df$Heart_Rate[which(restingST<10 & !is.na(restingST))]
        
        rhr_cache[[i]] <- data.frame(describe(restingHR))
        
        df_day <- df[which(df$Hour>=8 & df$Hour<20),]
        
        restingST <- ave(df_day$lowSteps, df_day$Wearable_Account_MD5,
                         FUN=function(x) rollapplyr(x, 10, sum, by=1, partial=TRUE)) 
        restingST[1:10-1] <- c(rep(NA,10-1)) #window-1
        
        #To record resting heart rate as rolling 10-minute mean when sum of steps in last 10 min < 10:
        # restingHR <- ave(df$Heart_Rate, df$Wearable_Account_MD5,
        #                  FUN=function(x) rollapplyr(x, 10, mean, na.rm=TRUE,
        #                                             by=1, partial=TRUE, fill=NA))
        # 
        # restingHR <- restingHR[which(restingST<10 & !is.na(restingST))]
        
        #To record resting HR as all values when sum of steps in last 10 min < 10:
        restingHR <- df_day$Heart_Rate[which(restingST<10 & !is.na(restingST))]
        
        # restingHR <- restingHR[which(restingST<10 & !is.na(restingST))] 
        
        day_rhr_cache[[i]] <- data.frame(describe(restingHR))
        rm(df_day)
        
        df_night <- df[which(df$Hour>=20 | df$Hour<8),]
        
        restingST <- ave(df_night$lowSteps, df_night$Wearable_Account_MD5,
                         FUN=function(x) rollapplyr(x, 10, sum, by=1, partial=TRUE))
        restingST[1:10-1] <- c(rep(NA,10-1)) #window-1
        
        # restingHR <- ave(df_night$Heart_Rate, df_night$Wearable_Account_MD5,
        # FUN=function(x) rollapplyr(x, 10, mean, by=1, partial=TRUE)) 
        
        restingHR <- df_night$Heart_Rate[which(restingST<10 & !is.na(restingST))] 
        
        night_rhr_cache[[i]] <- data.frame(describe(restingHR))
        rm(df_night)
        ###
        
        #label high activity (defined as top 10% of Steps)
        if(1==1){
          df$highActivity <- ifelse(
            df$StDecID==10,
            TRUE,
            FALSE)
          #df$highActivity_eventNum <- rleid(df$highActivity)
        }
        
        durationDf <- df[df$highActivity == "TRUE",]
        durationDf$Date <- as.POSIXct(paste(durationDf$Timestamp_Local),
                                      format="%Y-%m-%d %H:%M:%S")
        
        #describe variables during high activity
        highAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        highAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        highAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        highAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        
        
        df_day <- df[which(df$Hour>=8 & df$Hour<20),]
        durationDf_day <- df_day[df_day$highActivity == "TRUE",]
        durationDf_day$Date <- as.POSIXct(paste(durationDf_day$Timestamp_Local),
                                          format="%Y-%m-%d %H:%M:%S")
        
        day_highAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        day_highAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        day_highAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        day_highAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        rm(df_day)
        
        df_night <- df[which(df$Hour>=20 | df$Hour<8),]
        durationDf_day <- df_night[df_night$highActivity == "TRUE",]
        durationDf_day$Date <- as.POSIXct(paste(durationDf_day$Timestamp_Local),
                                          format="%Y-%m-%d %H:%M:%S")
        
        night_highAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        night_highAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        night_highAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        night_highAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        rm(df_night)
        ###
        
        #label low activity (defined as lowest 10% of Steps)
        if(1==1){
          df$lowActivity <- ifelse(
            df$StDecID==1,
            TRUE,
            FALSE)
          #df$lowActivity_eventNum <- rleid(df$lowActivity)
        }
        
        durationDf <- df[df$lowActivity == "TRUE",]
        durationDf$Date <- as.POSIXct(paste(durationDf$Timestamp_Local),
                                      format="%Y-%m-%d %H:%M:%S")
        
        #describe variables during low activity
        lowAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        lowAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        lowAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        lowAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        
        
        df_day <- df[which(df$Hour>=8 & df$Hour<20),]
        durationDf <- df_day[df_day$lowActivity == "TRUE",]
        durationDf$Date <- as.POSIXct(paste(durationDf$Timestamp_Local),
                                      format="%Y-%m-%d %H:%M:%S")
        
        day_lowAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        day_lowAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        day_lowAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        day_lowAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        rm(df_day)
        
        df_night <- df[which(df$Hour>=20 | df$Hour<8),]
        durationDf <- df_night[df_night$lowActivity == "TRUE",]
        durationDf$Date <- as.POSIXct(paste(durationDf$Timestamp_Local),
                                      format="%Y-%m-%d %H:%M:%S")
        
        night_lowAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        night_lowAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        night_lowAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        night_lowAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        rm(df_night)
        ###
      }
    }
  } else {
    gsr_cache[[i]] <- data.frame(NA)
    hr_cache[[i]] <- data.frame(NA)
    sk_cache[[i]] <- data.frame(NA)
    st_cache[[i]] <- data.frame(NA)
    
    rhr_cache[[i]] <- data.frame(NA)
    
    highAct_gsr_cache[[i]] <- data.frame(NA)
    highAct_hr_cache[[i]] <- data.frame(NA)
    highAct_sk_cache[[i]] <- data.frame(NA)
    highAct_st_cache[[i]] <- data.frame(NA)
    
    lowAct_gsr_cache[[i]] <- data.frame(NA)
    lowAct_hr_cache[[i]] <- data.frame(NA)
    lowAct_sk_cache[[i]] <- data.frame(NA)
    lowAct_st_cache[[i]] <- data.frame(NA)
    
    day_gsr_cache[[i]] <- data.frame(NA)
    day_hr_cache[[i]] <- data.frame(NA)
    day_sk_cache[[i]] <- data.frame(NA)
    day_st_cache[[i]] <- data.frame(NA)
    
    day_rhr_cache[[i]] <- data.frame(NA)
    
    day_highAct_gsr_cache[[i]] <- data.frame(NA)
    day_highAct_hr_cache[[i]] <- data.frame(NA)
    day_highAct_sk_cache[[i]] <- data.frame(NA)
    day_highAct_st_cache[[i]] <- data.frame(NA)
    
    day_lowAct_gsr_cache[[i]] <- data.frame(NA)
    day_lowAct_hr_cache[[i]] <- data.frame(NA)
    day_lowAct_sk_cache[[i]] <- data.frame(NA)
    day_lowAct_st_cache[[i]] <- data.frame(NA)
    
    night_gsr_cache[[i]] <- data.frame(NA)
    night_hr_cache[[i]] <- data.frame(NA)
    night_sk_cache[[i]] <- data.frame(NA)
    night_st_cache[[i]] <- data.frame(NA)
    
    night_rhr_cache[[i]] <- data.frame(NA)
    
    night_highAct_gsr_cache[[i]] <- data.frame(NA)
    night_highAct_hr_cache[[i]] <- data.frame(NA)
    night_highAct_sk_cache[[i]] <- data.frame(NA)
    night_highAct_st_cache[[i]] <- data.frame(NA)
    
    night_lowAct_gsr_cache[[i]] <- data.frame(NA)
    night_lowAct_hr_cache[[i]] <- data.frame(NA)
    night_lowAct_sk_cache[[i]] <- data.frame(NA)
    night_lowAct_st_cache[[i]] <- data.frame(NA)
    
    print(i)
    next
  }
  print(i)
}

gsr_cache <- rbind.fill(gsr_cache)
hr_cache <- rbind.fill(hr_cache)
sk_cache <- rbind.fill(sk_cache)
st_cache <- rbind.fill(st_cache)

rhr_cache <- rbind.fill(rhr_cache)

highAct_gsr_cache <- rbind.fill(highAct_gsr_cache)
highAct_hr_cache <- rbind.fill(highAct_hr_cache)
highAct_sk_cache <- rbind.fill(highAct_sk_cache)
highAct_st_cache <- rbind.fill(highAct_st_cache)

lowAct_gsr_cache <- rbind.fill(lowAct_gsr_cache)
lowAct_hr_cache <- rbind.fill(lowAct_hr_cache)
lowAct_sk_cache <- rbind.fill(lowAct_sk_cache)
lowAct_st_cache <- rbind.fill(lowAct_st_cache)

day_gsr_cache <- rbind.fill(day_gsr_cache)
day_hr_cache <- rbind.fill(day_hr_cache)
day_sk_cache <- rbind.fill(day_sk_cache)
day_st_cache <- rbind.fill(day_st_cache)

day_rhr_cache <- rbind.fill(day_rhr_cache)

day_highAct_gsr_cache <- rbind.fill(day_highAct_gsr_cache)
day_highAct_hr_cache <- rbind.fill(day_highAct_hr_cache)
day_highAct_sk_cache <- rbind.fill(day_highAct_sk_cache)
day_highAct_st_cache <- rbind.fill(day_highAct_st_cache)

day_lowAct_gsr_cache <- rbind.fill(day_lowAct_gsr_cache)
day_lowAct_hr_cache <- rbind.fill(day_lowAct_hr_cache)
day_lowAct_sk_cache <- rbind.fill(day_lowAct_sk_cache)
day_lowAct_st_cache <- rbind.fill(day_lowAct_st_cache)

night_gsr_cache <- rbind.fill(night_gsr_cache)
night_hr_cache <- rbind.fill(night_hr_cache)
night_sk_cache <- rbind.fill(night_sk_cache)
night_st_cache <- rbind.fill(night_st_cache)

night_rhr_cache <- rbind.fill(night_rhr_cache)

night_highAct_gsr_cache <- rbind.fill(night_highAct_gsr_cache)
night_highAct_hr_cache <- rbind.fill(night_highAct_hr_cache)
night_highAct_sk_cache <- rbind.fill(night_highAct_sk_cache)
night_highAct_st_cache <- rbind.fill(night_highAct_st_cache)

night_lowAct_gsr_cache <- rbind.fill(night_lowAct_gsr_cache)
night_lowAct_hr_cache <- rbind.fill(night_lowAct_hr_cache)
night_lowAct_sk_cache <- rbind.fill(night_lowAct_sk_cache)
night_lowAct_st_cache <- rbind.fill(night_lowAct_st_cache)

gsr_cache <- gsr_cache[,-1] #first column was for NAs
hr_cache <- hr_cache[,-1] #first column was for NAs
sk_cache <- sk_cache[,-1] #first column was for NAs
st_cache <- st_cache[,-1] #first column was for NAs

rhr_cache <- rhr_cache[,-1] #first column was for NAs

highAct_gsr_cache <- highAct_gsr_cache[,-1] #first column was for NAs
highAct_hr_cache <- highAct_hr_cache[,-1] #first column was for NAs
highAct_sk_cache <- highAct_sk_cache[,-1] #first column was for NAs
highAct_st_cache <- highAct_st_cache[,-1] #first column was for NAs

lowAct_gsr_cache <- lowAct_gsr_cache[,-1] #first column was for NAs
lowAct_hr_cache <- lowAct_hr_cache[,-1] #first column was for NAs
lowAct_sk_cache <- lowAct_sk_cache[,-1] #first column was for NAs
lowAct_st_cache <- lowAct_st_cache[,-1] #first column was for NAs

day_gsr_cache <- day_gsr_cache[,-1] #first column was for NAs
day_hr_cache <- day_hr_cache[,-1] #first column was for NAs
day_sk_cache <- day_sk_cache[,-1] #first column was for NAs
day_st_cache <- day_st_cache[,-1] #first column was for NAs

day_rhr_cache <- day_rhr_cache[,-1] #first column was for NAs

day_highAct_gsr_cache <- day_highAct_gsr_cache[,-1] #first column was for NAs
day_highAct_hr_cache <- day_highAct_hr_cache[,-1] #first column was for NAs
day_highAct_sk_cache <- day_highAct_sk_cache[,-1] #first column was for NAs
day_highAct_st_cache <- day_highAct_st_cache[,-1] #first column was for NAs

day_lowAct_gsr_cache <- day_lowAct_gsr_cache[,-1] #first column was for NAs
day_lowAct_hr_cache <- day_lowAct_hr_cache[,-1] #first column was for NAs
day_lowAct_sk_cache <- day_lowAct_sk_cache[,-1] #first column was for NAs
day_lowAct_st_cache <- day_lowAct_st_cache[,-1] #first column was for NAs

night_gsr_cache <- night_gsr_cache[,-1] #first column was for NAs
night_hr_cache <- night_hr_cache[,-1] #first column was for NAs
night_sk_cache <- night_sk_cache[,-1] #first column was for NAs
night_st_cache <- night_st_cache[,-1] #first column was for NAs

night_rhr_cache <- night_rhr_cache[,-1] #first column was for NAs

night_highAct_gsr_cache <- night_highAct_gsr_cache[,-1] #first column was for NAs
night_highAct_hr_cache <- night_highAct_hr_cache[,-1] #first column was for NAs
night_highAct_sk_cache <- night_highAct_sk_cache[,-1] #first column was for NAs
night_highAct_st_cache <- night_highAct_st_cache[,-1] #first column was for NAs

night_lowAct_gsr_cache <- night_lowAct_gsr_cache[,-1] #first column was for NAs
night_lowAct_hr_cache <- night_lowAct_hr_cache[,-1] #first column was for NAs
night_lowAct_sk_cache <- night_lowAct_sk_cache[,-1] #first column was for NAs
night_lowAct_st_cache <- night_lowAct_st_cache[,-1] #first column was for NAs

colnames(gsr_cache) <- paste0("gsr_",colnames(gsr_cache))
colnames(hr_cache) <- paste0("hr_",colnames(hr_cache))
colnames(sk_cache) <- paste0("sk_",colnames(sk_cache))
colnames(st_cache) <- paste0("st_",colnames(st_cache))

colnames(rhr_cache) <- paste0("rhr_",colnames(rhr_cache))

colnames(highAct_gsr_cache) <- paste0("highAct_gsr_",colnames(highAct_gsr_cache))
colnames(highAct_hr_cache) <- paste0("highAct_hr_",colnames(highAct_hr_cache))
colnames(highAct_sk_cache) <- paste0("highAct_sk_",colnames(highAct_sk_cache))
colnames(highAct_st_cache) <- paste0("highAct_st_",colnames(highAct_st_cache))

colnames(lowAct_gsr_cache) <- paste0("lowAct_gsr_",colnames(lowAct_gsr_cache))
colnames(lowAct_hr_cache) <- paste0("lowAct_hr_",colnames(lowAct_hr_cache))
colnames(lowAct_sk_cache) <- paste0("lowAct_sk_",colnames(lowAct_sk_cache))
colnames(lowAct_st_cache) <- paste0("lowAct_st_",colnames(lowAct_st_cache))

colnames(day_gsr_cache) <- paste0("day_gsr_",colnames(day_gsr_cache))
colnames(day_hr_cache) <- paste0("day_hr_",colnames(day_hr_cache))
colnames(day_sk_cache) <- paste0("day_sk_",colnames(day_sk_cache))
colnames(day_st_cache) <- paste0("day_st_",colnames(day_st_cache))

colnames(day_rhr_cache) <- paste0("day_rhr_",colnames(day_rhr_cache))

colnames(day_highAct_gsr_cache) <- paste0("day_highAct_gsr_",
                                          colnames(day_highAct_gsr_cache))
colnames(day_highAct_hr_cache) <- paste0("day_highAct_hr_",
                                         colnames(day_highAct_hr_cache))
colnames(day_highAct_sk_cache) <- paste0("day_highAct_sk_",
                                         colnames(day_highAct_sk_cache))
colnames(day_highAct_st_cache) <- paste0("day_highAct_st_",
                                         colnames(day_highAct_st_cache))

colnames(day_lowAct_gsr_cache) <- paste0("day_lowAct_gsr_",
                                         colnames(day_lowAct_gsr_cache))
colnames(day_lowAct_hr_cache) <- paste0("day_lowAct_hr_",
                                        colnames(day_lowAct_hr_cache))
colnames(day_lowAct_sk_cache) <- paste0("day_lowAct_sk_",
                                        colnames(day_lowAct_sk_cache))
colnames(day_lowAct_st_cache) <- paste0("day_lowAct_st_",
                                        colnames(day_lowAct_st_cache))

colnames(night_gsr_cache) <- paste0("night_gsr_",colnames(night_gsr_cache))
colnames(night_hr_cache) <- paste0("night_hr_",colnames(night_hr_cache))
colnames(night_sk_cache) <- paste0("night_sk_",colnames(night_sk_cache))
colnames(night_st_cache) <- paste0("night_st_",colnames(night_st_cache))

colnames(night_rhr_cache) <- paste0("night_rhr_",colnames(night_rhr_cache))

colnames(night_highAct_gsr_cache) <- paste0("night_highAct_gsr_",
                                            colnames(night_highAct_gsr_cache))
colnames(night_highAct_hr_cache) <- paste0("night_highAct_hr_",
                                           colnames(night_highAct_hr_cache))
colnames(night_highAct_sk_cache) <- paste0("night_highAct_sk_",
                                           colnames(night_highAct_sk_cache))
colnames(night_highAct_st_cache) <- paste0("night_highAct_st_",
                                           colnames(night_highAct_st_cache))

colnames(night_lowAct_gsr_cache) <- paste0("night_lowAct_gsr_",
                                           colnames(night_lowAct_gsr_cache))
colnames(night_lowAct_hr_cache) <- paste0("night_lowAct_hr_",
                                          colnames(night_lowAct_hr_cache))
colnames(night_lowAct_sk_cache) <- paste0("night_lowAct_sk_",
                                          colnames(night_lowAct_sk_cache))
colnames(night_lowAct_st_cache) <- paste0("night_lowAct_st_",
                                          colnames(night_lowAct_st_cache))

stats <- cbind(
  ClinWithWear,gsr_cache,hr_cache,sk_cache,st_cache,
  rhr_cache,highAct_gsr_cache,highAct_hr_cache,
  highAct_sk_cache,highAct_st_cache,
  lowAct_gsr_cache,lowAct_hr_cache,
  lowAct_sk_cache,lowAct_st_cache,
  day_gsr_cache,day_hr_cache,day_sk_cache,day_st_cache,
  day_rhr_cache,day_highAct_gsr_cache,day_highAct_hr_cache,
  day_highAct_sk_cache,day_highAct_st_cache,
  day_lowAct_gsr_cache,day_lowAct_hr_cache,
  day_lowAct_sk_cache,day_lowAct_st_cache,
  night_gsr_cache,night_hr_cache,night_sk_cache,night_st_cache,
  night_rhr_cache,night_highAct_gsr_cache,night_highAct_hr_cache,
  night_highAct_sk_cache,night_highAct_st_cache,
  night_lowAct_gsr_cache,night_lowAct_hr_cache,
  night_lowAct_sk_cache,night_lowAct_st_cache)

write.csv(stats,
          paste0("F:\\PhysSubTables_20161111_Normal_20170802\\Basis2016_Clean_Norm_3DayPrior_20180504.csv"),
          row.names=FALSE)


#### DESCRIBE 5-DAY-PRIOR DATA ####
files <- list.files("F:\\BasisSubsets\\FromBasisData_20161111_Normal_20170802\\5DayPrior_Files",
                    pattern="*.csv",full.names=TRUE)

gsr_cache <- list()
hr_cache <- list()
sk_cache <- list()
st_cache <- list()

rhr_cache <- list()

highAct_gsr_cache <- list()
highAct_hr_cache <- list()
highAct_sk_cache <- list()
highAct_st_cache <- list()

lowAct_gsr_cache <- list()
lowAct_hr_cache <- list()
lowAct_sk_cache <- list()
lowAct_st_cache <- list()


day_gsr_cache <- list()
day_hr_cache <- list()
day_sk_cache <- list()
day_st_cache <- list()

day_rhr_cache <- list()

day_highAct_gsr_cache <- list()
day_highAct_hr_cache <- list()
day_highAct_sk_cache <- list()
day_highAct_st_cache <- list()

day_lowAct_gsr_cache <- list()
day_lowAct_hr_cache <- list()
day_lowAct_sk_cache <- list()
day_lowAct_st_cache <- list()

night_gsr_cache <- list()
night_hr_cache <- list()
night_sk_cache <- list()
night_st_cache <- list()

night_rhr_cache <- list()

night_highAct_gsr_cache <- list()
night_highAct_hr_cache <- list()
night_highAct_sk_cache <- list()
night_highAct_st_cache <- list()

night_lowAct_gsr_cache <- list()
night_lowAct_hr_cache <- list()
night_lowAct_sk_cache <- list()
night_lowAct_st_cache <- list()
###

for(i in 1:length(files)){                       
  check <- file.info(files[i])$size
  if(check!=0){
    df <- read.csv(files[i],header=TRUE,sep=",")
    if("Wearable_Account_MD5" %in% names(df)){
      if(length(intersect(unique(df$Wearable_Account_MD5),rem))!=0){
        df <- df[!(df$Wearable_Account_MD5 %in% rem),]
      }
    } else {
      if(!("Wearable_Account_MD5" %in% names(df))){
        print(paste("MD5 not found in",i))
        print(names(df))
        next
      }
    }
    df$Date <- as.POSIXct(paste(df$Timestamp_Local),format="%Y-%m-%d %H:%M:%S")
    emptyCols <- apply(df,2,function(x) all(is.na(x)))
    if(TRUE %in% emptyCols){
      gsr_cache[[i]] <- data.frame(NA)
      hr_cache[[i]] <- data.frame(NA)
      sk_cache[[i]] <- data.frame(NA)
      st_cache[[i]] <- data.frame(NA)
      
      rhr_cache[[i]] <- data.frame(NA)
      
      highAct_gsr_cache[[i]] <- data.frame(NA)
      highAct_hr_cache[[i]] <- data.frame(NA)
      highAct_sk_cache[[i]] <- data.frame(NA)
      highAct_st_cache[[i]] <- data.frame(NA)
      
      lowAct_gsr_cache[[i]] <- data.frame(NA)
      lowAct_hr_cache[[i]] <- data.frame(NA)
      lowAct_sk_cache[[i]] <- data.frame(NA)
      lowAct_st_cache[[i]] <- data.frame(NA)
      
      day_gsr_cache[[i]] <- data.frame(NA)
      day_hr_cache[[i]] <- data.frame(NA)
      day_sk_cache[[i]] <- data.frame(NA)
      day_st_cache[[i]] <- data.frame(NA)
      
      day_rhr_cache[[i]] <- data.frame(NA)
      
      day_highAct_gsr_cache[[i]] <- data.frame(NA)
      day_highAct_hr_cache[[i]] <- data.frame(NA)
      day_highAct_sk_cache[[i]] <- data.frame(NA)
      day_highAct_st_cache[[i]] <- data.frame(NA)
      
      day_lowAct_gsr_cache[[i]] <- data.frame(NA)
      day_lowAct_hr_cache[[i]] <- data.frame(NA)
      day_lowAct_sk_cache[[i]] <- data.frame(NA)
      day_lowAct_st_cache[[i]] <- data.frame(NA)
      
      night_gsr_cache[[i]] <- data.frame(NA)
      night_hr_cache[[i]] <- data.frame(NA)
      night_sk_cache[[i]] <- data.frame(NA)
      night_st_cache[[i]] <- data.frame(NA)
      
      night_rhr_cache[[i]] <- data.frame(NA)
      
      night_highAct_gsr_cache[[i]] <- data.frame(NA)
      night_highAct_hr_cache[[i]] <- data.frame(NA)
      night_highAct_sk_cache[[i]] <- data.frame(NA)
      night_highAct_st_cache[[i]] <- data.frame(NA)
      
      night_lowAct_gsr_cache[[i]] <- data.frame(NA)
      night_lowAct_hr_cache[[i]] <- data.frame(NA)
      night_lowAct_sk_cache[[i]] <- data.frame(NA)
      night_lowAct_st_cache[[i]] <- data.frame(NA)
      next
    }
    if(NROW(df) < 1440){
      gsr_cache[[i]] <- data.frame(NA)
      hr_cache[[i]] <- data.frame(NA)
      sk_cache[[i]] <- data.frame(NA)
      st_cache[[i]] <- data.frame(NA)
      
      rhr_cache[[i]] <- data.frame(NA)
      
      highAct_gsr_cache[[i]] <- data.frame(NA)
      highAct_hr_cache[[i]] <- data.frame(NA)
      highAct_sk_cache[[i]] <- data.frame(NA)
      highAct_st_cache[[i]] <- data.frame(NA)
      
      lowAct_gsr_cache[[i]] <- data.frame(NA)
      lowAct_hr_cache[[i]] <- data.frame(NA)
      lowAct_sk_cache[[i]] <- data.frame(NA)
      lowAct_st_cache[[i]] <- data.frame(NA)
      
      day_gsr_cache[[i]] <- data.frame(NA)
      day_hr_cache[[i]] <- data.frame(NA)
      day_sk_cache[[i]] <- data.frame(NA)
      day_st_cache[[i]] <- data.frame(NA)
      
      day_rhr_cache[[i]] <- data.frame(NA)
      
      day_highAct_gsr_cache[[i]] <- data.frame(NA)
      day_highAct_hr_cache[[i]] <- data.frame(NA)
      day_highAct_sk_cache[[i]] <- data.frame(NA)
      day_highAct_st_cache[[i]] <- data.frame(NA)
      
      day_lowAct_gsr_cache[[i]] <- data.frame(NA)
      day_lowAct_hr_cache[[i]] <- data.frame(NA)
      day_lowAct_sk_cache[[i]] <- data.frame(NA)
      day_lowAct_st_cache[[i]] <- data.frame(NA)
      
      night_gsr_cache[[i]] <- data.frame(NA)
      night_hr_cache[[i]] <- data.frame(NA)
      night_sk_cache[[i]] <- data.frame(NA)
      night_st_cache[[i]] <- data.frame(NA)
      
      night_rhr_cache[[i]] <- data.frame(NA)
      
      night_highAct_gsr_cache[[i]] <- data.frame(NA)
      night_highAct_hr_cache[[i]] <- data.frame(NA)
      night_highAct_sk_cache[[i]] <- data.frame(NA)
      night_highAct_st_cache[[i]] <- data.frame(NA)
      
      night_lowAct_gsr_cache[[i]] <- data.frame(NA)
      night_lowAct_hr_cache[[i]] <- data.frame(NA)
      night_lowAct_sk_cache[[i]] <- data.frame(NA)
      night_lowAct_st_cache[[i]] <- data.frame(NA)
      next
    }
    gsr_cache[[i]] <- data.frame(describe(df$GSR))
    hr_cache[[i]] <- data.frame(describe(df$Heart_Rate))
    sk_cache[[i]] <- data.frame(describe(df$Skin_Temperature_F))
    st_cache[[i]] <- data.frame(describe(df$Steps))
    
    
    source("F:\\R_Commands\\LabelHours.R")
    label.hours(df, "Timestamp_Local")
    
    day_gsr_cache[[i]] <- data.frame(describe(
      df$GSR[which(df$Hour>=8 & df$Hour<20)]))
    day_hr_cache[[i]] <- data.frame(describe(
      df$Heart_Rate[which(df$Hour>=8 & df$Hour<20)]))
    day_sk_cache[[i]] <- data.frame(describe(
      df$Skin_Temperature_F[which(df$Hour>=8 & df$Hour<20)]))
    day_st_cache[[i]] <- data.frame(describe(
      df$Steps[which(df$Hour>=8 & df$Hour<20)]))
    
    night_gsr_cache[[i]] <- data.frame(describe(
      df$GSR[which(df$Hour>=20 | df$Hour<8)]))
    night_hr_cache[[i]] <- data.frame(describe(
      df$Heart_Rate[which(df$Hour>=20 | df$Hour<8)]))
    night_sk_cache[[i]] <- data.frame(describe(
      df$Skin_Temperature_F[which(df$Hour>=20 | df$Hour<8)]))
    night_st_cache[[i]] <- data.frame(describe(
      df$Steps[which(df$Hour>=20 | df$Hour<8)]))
    ###
    
    ## NEW
    
    if(1==1){
      df$lowSteps <- df$Steps
      df$Steps[df$Steps==0] <- NA
      if(all(is.na(df$Steps))){
        print(paste0("Error: No Steps for Candidate #",i))
        
        rhr_cache[[i]] <- data.frame(NA)
        
        highAct_gsr_cache[[i]] <- data.frame(NA)
        highAct_hr_cache[[i]] <- data.frame(NA)
        highAct_sk_cache[[i]] <- data.frame(NA)
        highAct_st_cache[[i]] <- data.frame(NA)
        
        lowAct_gsr_cache[[i]] <- data.frame(NA)
        lowAct_hr_cache[[i]] <- data.frame(NA)
        lowAct_sk_cache[[i]] <- data.frame(NA)
        lowAct_st_cache[[i]] <- data.frame(NA)
        
        day_gsr_cache[[i]] <- data.frame(NA)
        day_hr_cache[[i]] <- data.frame(NA)
        day_sk_cache[[i]] <- data.frame(NA)
        day_st_cache[[i]] <- data.frame(NA)
        
        day_rhr_cache[[i]] <- data.frame(NA)
        
        day_highAct_gsr_cache[[i]] <- data.frame(NA)
        day_highAct_hr_cache[[i]] <- data.frame(NA)
        day_highAct_sk_cache[[i]] <- data.frame(NA)
        day_highAct_st_cache[[i]] <- data.frame(NA)
        
        day_lowAct_gsr_cache[[i]] <- data.frame(NA)
        day_lowAct_hr_cache[[i]] <- data.frame(NA)
        day_lowAct_sk_cache[[i]] <- data.frame(NA)
        day_lowAct_st_cache[[i]] <- data.frame(NA)
        
        night_gsr_cache[[i]] <- data.frame(NA)
        night_hr_cache[[i]] <- data.frame(NA)
        night_sk_cache[[i]] <- data.frame(NA)
        night_st_cache[[i]] <- data.frame(NA)
        
        night_rhr_cache[[i]] <- data.frame(NA)
        
        night_highAct_gsr_cache[[i]] <- data.frame(NA)
        night_highAct_hr_cache[[i]] <- data.frame(NA)
        night_highAct_sk_cache[[i]] <- data.frame(NA)
        night_highAct_st_cache[[i]] <- data.frame(NA)
        
        night_lowAct_gsr_cache[[i]] <- data.frame(NA)
        night_lowAct_hr_cache[[i]] <- data.frame(NA)
        night_lowAct_sk_cache[[i]] <- data.frame(NA)
        night_lowAct_st_cache[[i]] <- data.frame(NA)
        next
        
      } else {
        if(1==1){
          df$Steps[df$Steps==0] <- NA
          #ID steps decile locations
          StDecLocations <- quantile(
            df$Steps, na.rm=TRUE, 
            probs = seq(0.1,0.9,by=0.1))
          if(length(unique(StDecLocations)) != 9){
            check <- which(table(StDecLocations) > 1)
            for(u in 1:length(check)){
              check2 <- which(StDecLocations==names(check)[u])
              theMin <- as.numeric(min(check2))
              for(v in 1:length(check2)){
                if(check2[v] == theMin){
                  next
                } else {
                  StDecLocations[check2[v]] <- sum(
                    StDecLocations[check2[v]],0.0000000000001)
                }
              }
            }
          }
          df$StDecID <- findInterval(df$Steps,
                                     c(-Inf,StDecLocations, Inf))
        }
        #ID heart rate decile locations
        HrDecLocations <- quantile(
          df$Heart_Rate, na.rm=TRUE, 
          probs = seq(0.1,0.9, by=0.1))
        if(length(unique(HrDecLocations)) != 9){
          check <- which(table(HrDecLocations) > 1)
          for(u in 1:length(check)){
            check2 <- which(HrDecLocations==names(check)[u])
            theMin <- as.numeric(min(check2))
            for(v in 1:length(check2)){
              if(check2[v] == theMin){
                next
              } else {
                HrDecLocations[check2[v]] <- sum(
                  HrDecLocations[check2[v]],0.0000000000001)
              }
            }
          }
        }
        df$HrDecID <- findInterval(df$Heart_Rate,
                                   c(-Inf,HrDecLocations,Inf))
        #ID skin temperature decile locations
        SkDecLocations <- quantile(
          df$Skin_Temperature_F, na.rm=TRUE, 
          probs = seq(0.1,0.9,by=0.1))
        if(length(unique(SkDecLocations)) != 9){
          check <- which(table(SkDecLocations) > 1)
          for(u in 1:length(check)){
            check2 <- which(SkDecLocations==names(check)[u])
            theMin <- as.numeric(min(check2))
            for(v in 1:length(check2)){
              if(check2[v] == theMin){
                next
              } else {
                SkDecLocations[check2[v]] <- sum(
                  SkDecLocations[check2[v]],0.0000000000001)
              }
            }
          }
        }
        df$SkDecID <- findInterval(df$Skin_Temperature_F,
                                   c(-Inf,SkDecLocations, Inf))
        #ID gsr decile locations
        GsrDecLocations <- quantile(
          df$GSR, na.rm=TRUE, 
          probs = seq(0.1,0.9,by=0.1))
        if(length(unique(GsrDecLocations)) != 9){
          check <- which(table(GsrDecLocations) > 1)
          for(u in 1:length(check)){
            check2 <- which(GsrDecLocations==names(check)[u])
            theMin <- as.numeric(min(check2))
            for(v in 1:length(check2)){
              if(check2[v] == theMin){
                next
              } else {
                GsrDecLocations[check2[v]] <- sum(
                  GsrDecLocations[check2[v]],0.0000000000001)
              }
            }
          }
        }
        df$GsrDecID <- findInterval(df$GSR,
                                    c(-Inf,GsrDecLocations, Inf))
        
        #SUBSET RESTING HEART RATE PARAMETERS TO "rdf" table
        restingST <- ave(df$lowSteps, df$Wearable_Account_MD5,
                         FUN=function(x) rollapplyr(x, 10, sum, by=1, partial=TRUE))
        restingST[1:10-1] <- c(rep(NA,10-1)) #window-1
        
        #To record resting heart rate as rolling 10-minute mean when sum of steps in last 10 min < 10:
        # restingHR <- ave(df$Heart_Rate, df$Wearable_Account_MD5,
        #                  FUN=function(x) rollapplyr(x, 10, mean, na.rm=TRUE,
        #                                             by=1, partial=TRUE, fill=NA))
        # 
        # restingHR <- restingHR[which(restingST<10 & !is.na(restingST))]
        
        #To record resting HR as all values when sum of steps in last 10 min < 10:
        restingHR <- df$Heart_Rate[which(restingST<10 & !is.na(restingST))]
        
        rhr_cache[[i]] <- data.frame(describe(restingHR))
        
        df_day <- df[which(df$Hour>=8 & df$Hour<20),]
        
        restingST <- ave(df_day$lowSteps, df_day$Wearable_Account_MD5,
                         FUN=function(x) rollapplyr(x, 10, sum, by=1, partial=TRUE)) 
        restingST[1:10-1] <- c(rep(NA,10-1)) #window-1
        
        #To record resting heart rate as rolling 10-minute mean when sum of steps in last 10 min < 10:
        # restingHR <- ave(df$Heart_Rate, df$Wearable_Account_MD5,
        #                  FUN=function(x) rollapplyr(x, 10, mean, na.rm=TRUE,
        #                                             by=1, partial=TRUE, fill=NA))
        # 
        # restingHR <- restingHR[which(restingST<10 & !is.na(restingST))]
        
        #To record resting HR as all values when sum of steps in last 10 min < 10:
        restingHR <- df_day$Heart_Rate[which(restingST<10 & !is.na(restingST))]
        
        # restingHR <- restingHR[which(restingST<10 & !is.na(restingST))] 
        
        day_rhr_cache[[i]] <- data.frame(describe(restingHR))
        rm(df_day)
        
        df_night <- df[which(df$Hour>=20 | df$Hour<8),]
        
        restingST <- ave(df_night$lowSteps, df_night$Wearable_Account_MD5,
                         FUN=function(x) rollapplyr(x, 10, sum, by=1, partial=TRUE))
        restingST[1:10-1] <- c(rep(NA,10-1)) #window-1
        
        # restingHR <- ave(df_night$Heart_Rate, df_night$Wearable_Account_MD5,
        # FUN=function(x) rollapplyr(x, 10, mean, by=1, partial=TRUE)) 
        
        restingHR <- df_night$Heart_Rate[which(restingST<10 & !is.na(restingST))] 
        
        night_rhr_cache[[i]] <- data.frame(describe(restingHR))
        rm(df_night)
        ###
        
        #label high activity (defined as top 10% of Steps)
        if(1==1){
          df$highActivity <- ifelse(
            df$StDecID==10,
            TRUE,
            FALSE)
          #df$highActivity_eventNum <- rleid(df$highActivity)
        }
        
        durationDf <- df[df$highActivity == "TRUE",]
        durationDf$Date <- as.POSIXct(paste(durationDf$Timestamp_Local),
                                      format="%Y-%m-%d %H:%M:%S")
        
        #describe variables during high activity
        highAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        highAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        highAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        highAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        
        
        df_day <- df[which(df$Hour>=8 & df$Hour<20),]
        durationDf_day <- df_day[df_day$highActivity == "TRUE",]
        durationDf_day$Date <- as.POSIXct(paste(durationDf_day$Timestamp_Local),
                                          format="%Y-%m-%d %H:%M:%S")
        
        day_highAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        day_highAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        day_highAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        day_highAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        rm(df_day)
        
        df_night <- df[which(df$Hour>=20 | df$Hour<8),]
        durationDf_day <- df_night[df_night$highActivity == "TRUE",]
        durationDf_day$Date <- as.POSIXct(paste(durationDf_day$Timestamp_Local),
                                          format="%Y-%m-%d %H:%M:%S")
        
        night_highAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        night_highAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        night_highAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        night_highAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        rm(df_night)
        ###
        
        #label low activity (defined as lowest 10% of Steps)
        if(1==1){
          df$lowActivity <- ifelse(
            df$StDecID==1,
            TRUE,
            FALSE)
          #df$lowActivity_eventNum <- rleid(df$lowActivity)
        }
        
        durationDf <- df[df$lowActivity == "TRUE",]
        durationDf$Date <- as.POSIXct(paste(durationDf$Timestamp_Local),
                                      format="%Y-%m-%d %H:%M:%S")
        
        #describe variables during low activity
        lowAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        lowAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        lowAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        lowAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        
        
        df_day <- df[which(df$Hour>=8 & df$Hour<20),]
        durationDf <- df_day[df_day$lowActivity == "TRUE",]
        durationDf$Date <- as.POSIXct(paste(durationDf$Timestamp_Local),
                                      format="%Y-%m-%d %H:%M:%S")
        
        day_lowAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        day_lowAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        day_lowAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        day_lowAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        rm(df_day)
        
        df_night <- df[which(df$Hour>=20 | df$Hour<8),]
        durationDf <- df_night[df_night$lowActivity == "TRUE",]
        durationDf$Date <- as.POSIXct(paste(durationDf$Timestamp_Local),
                                      format="%Y-%m-%d %H:%M:%S")
        
        night_lowAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        night_lowAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        night_lowAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        night_lowAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        rm(df_night)
        ###
      }
    }
  } else {
    gsr_cache[[i]] <- data.frame(NA)
    hr_cache[[i]] <- data.frame(NA)
    sk_cache[[i]] <- data.frame(NA)
    st_cache[[i]] <- data.frame(NA)
    
    rhr_cache[[i]] <- data.frame(NA)
    
    highAct_gsr_cache[[i]] <- data.frame(NA)
    highAct_hr_cache[[i]] <- data.frame(NA)
    highAct_sk_cache[[i]] <- data.frame(NA)
    highAct_st_cache[[i]] <- data.frame(NA)
    
    lowAct_gsr_cache[[i]] <- data.frame(NA)
    lowAct_hr_cache[[i]] <- data.frame(NA)
    lowAct_sk_cache[[i]] <- data.frame(NA)
    lowAct_st_cache[[i]] <- data.frame(NA)
    
    day_gsr_cache[[i]] <- data.frame(NA)
    day_hr_cache[[i]] <- data.frame(NA)
    day_sk_cache[[i]] <- data.frame(NA)
    day_st_cache[[i]] <- data.frame(NA)
    
    day_rhr_cache[[i]] <- data.frame(NA)
    
    day_highAct_gsr_cache[[i]] <- data.frame(NA)
    day_highAct_hr_cache[[i]] <- data.frame(NA)
    day_highAct_sk_cache[[i]] <- data.frame(NA)
    day_highAct_st_cache[[i]] <- data.frame(NA)
    
    day_lowAct_gsr_cache[[i]] <- data.frame(NA)
    day_lowAct_hr_cache[[i]] <- data.frame(NA)
    day_lowAct_sk_cache[[i]] <- data.frame(NA)
    day_lowAct_st_cache[[i]] <- data.frame(NA)
    
    night_gsr_cache[[i]] <- data.frame(NA)
    night_hr_cache[[i]] <- data.frame(NA)
    night_sk_cache[[i]] <- data.frame(NA)
    night_st_cache[[i]] <- data.frame(NA)
    
    night_rhr_cache[[i]] <- data.frame(NA)
    
    night_highAct_gsr_cache[[i]] <- data.frame(NA)
    night_highAct_hr_cache[[i]] <- data.frame(NA)
    night_highAct_sk_cache[[i]] <- data.frame(NA)
    night_highAct_st_cache[[i]] <- data.frame(NA)
    
    night_lowAct_gsr_cache[[i]] <- data.frame(NA)
    night_lowAct_hr_cache[[i]] <- data.frame(NA)
    night_lowAct_sk_cache[[i]] <- data.frame(NA)
    night_lowAct_st_cache[[i]] <- data.frame(NA)
    
    print(i)
    next
  }
  print(i)
}

gsr_cache <- rbind.fill(gsr_cache)
hr_cache <- rbind.fill(hr_cache)
sk_cache <- rbind.fill(sk_cache)
st_cache <- rbind.fill(st_cache)

rhr_cache <- rbind.fill(rhr_cache)

highAct_gsr_cache <- rbind.fill(highAct_gsr_cache)
highAct_hr_cache <- rbind.fill(highAct_hr_cache)
highAct_sk_cache <- rbind.fill(highAct_sk_cache)
highAct_st_cache <- rbind.fill(highAct_st_cache)

lowAct_gsr_cache <- rbind.fill(lowAct_gsr_cache)
lowAct_hr_cache <- rbind.fill(lowAct_hr_cache)
lowAct_sk_cache <- rbind.fill(lowAct_sk_cache)
lowAct_st_cache <- rbind.fill(lowAct_st_cache)

day_gsr_cache <- rbind.fill(day_gsr_cache)
day_hr_cache <- rbind.fill(day_hr_cache)
day_sk_cache <- rbind.fill(day_sk_cache)
day_st_cache <- rbind.fill(day_st_cache)

day_rhr_cache <- rbind.fill(day_rhr_cache)

day_highAct_gsr_cache <- rbind.fill(day_highAct_gsr_cache)
day_highAct_hr_cache <- rbind.fill(day_highAct_hr_cache)
day_highAct_sk_cache <- rbind.fill(day_highAct_sk_cache)
day_highAct_st_cache <- rbind.fill(day_highAct_st_cache)

day_lowAct_gsr_cache <- rbind.fill(day_lowAct_gsr_cache)
day_lowAct_hr_cache <- rbind.fill(day_lowAct_hr_cache)
day_lowAct_sk_cache <- rbind.fill(day_lowAct_sk_cache)
day_lowAct_st_cache <- rbind.fill(day_lowAct_st_cache)

night_gsr_cache <- rbind.fill(night_gsr_cache)
night_hr_cache <- rbind.fill(night_hr_cache)
night_sk_cache <- rbind.fill(night_sk_cache)
night_st_cache <- rbind.fill(night_st_cache)

night_rhr_cache <- rbind.fill(night_rhr_cache)

night_highAct_gsr_cache <- rbind.fill(night_highAct_gsr_cache)
night_highAct_hr_cache <- rbind.fill(night_highAct_hr_cache)
night_highAct_sk_cache <- rbind.fill(night_highAct_sk_cache)
night_highAct_st_cache <- rbind.fill(night_highAct_st_cache)

night_lowAct_gsr_cache <- rbind.fill(night_lowAct_gsr_cache)
night_lowAct_hr_cache <- rbind.fill(night_lowAct_hr_cache)
night_lowAct_sk_cache <- rbind.fill(night_lowAct_sk_cache)
night_lowAct_st_cache <- rbind.fill(night_lowAct_st_cache)

gsr_cache <- gsr_cache[,-1] #first column was for NAs
hr_cache <- hr_cache[,-1] #first column was for NAs
sk_cache <- sk_cache[,-1] #first column was for NAs
st_cache <- st_cache[,-1] #first column was for NAs

rhr_cache <- rhr_cache[,-1] #first column was for NAs

highAct_gsr_cache <- highAct_gsr_cache[,-1] #first column was for NAs
highAct_hr_cache <- highAct_hr_cache[,-1] #first column was for NAs
highAct_sk_cache <- highAct_sk_cache[,-1] #first column was for NAs
highAct_st_cache <- highAct_st_cache[,-1] #first column was for NAs

lowAct_gsr_cache <- lowAct_gsr_cache[,-1] #first column was for NAs
lowAct_hr_cache <- lowAct_hr_cache[,-1] #first column was for NAs
lowAct_sk_cache <- lowAct_sk_cache[,-1] #first column was for NAs
lowAct_st_cache <- lowAct_st_cache[,-1] #first column was for NAs

day_gsr_cache <- day_gsr_cache[,-1] #first column was for NAs
day_hr_cache <- day_hr_cache[,-1] #first column was for NAs
day_sk_cache <- day_sk_cache[,-1] #first column was for NAs
day_st_cache <- day_st_cache[,-1] #first column was for NAs

day_rhr_cache <- day_rhr_cache[,-1] #first column was for NAs

day_highAct_gsr_cache <- day_highAct_gsr_cache[,-1] #first column was for NAs
day_highAct_hr_cache <- day_highAct_hr_cache[,-1] #first column was for NAs
day_highAct_sk_cache <- day_highAct_sk_cache[,-1] #first column was for NAs
day_highAct_st_cache <- day_highAct_st_cache[,-1] #first column was for NAs

day_lowAct_gsr_cache <- day_lowAct_gsr_cache[,-1] #first column was for NAs
day_lowAct_hr_cache <- day_lowAct_hr_cache[,-1] #first column was for NAs
day_lowAct_sk_cache <- day_lowAct_sk_cache[,-1] #first column was for NAs
day_lowAct_st_cache <- day_lowAct_st_cache[,-1] #first column was for NAs

night_gsr_cache <- night_gsr_cache[,-1] #first column was for NAs
night_hr_cache <- night_hr_cache[,-1] #first column was for NAs
night_sk_cache <- night_sk_cache[,-1] #first column was for NAs
night_st_cache <- night_st_cache[,-1] #first column was for NAs

night_rhr_cache <- night_rhr_cache[,-1] #first column was for NAs

night_highAct_gsr_cache <- night_highAct_gsr_cache[,-1] #first column was for NAs
night_highAct_hr_cache <- night_highAct_hr_cache[,-1] #first column was for NAs
night_highAct_sk_cache <- night_highAct_sk_cache[,-1] #first column was for NAs
night_highAct_st_cache <- night_highAct_st_cache[,-1] #first column was for NAs

night_lowAct_gsr_cache <- night_lowAct_gsr_cache[,-1] #first column was for NAs
night_lowAct_hr_cache <- night_lowAct_hr_cache[,-1] #first column was for NAs
night_lowAct_sk_cache <- night_lowAct_sk_cache[,-1] #first column was for NAs
night_lowAct_st_cache <- night_lowAct_st_cache[,-1] #first column was for NAs

colnames(gsr_cache) <- paste0("gsr_",colnames(gsr_cache))
colnames(hr_cache) <- paste0("hr_",colnames(hr_cache))
colnames(sk_cache) <- paste0("sk_",colnames(sk_cache))
colnames(st_cache) <- paste0("st_",colnames(st_cache))

colnames(rhr_cache) <- paste0("rhr_",colnames(rhr_cache))

colnames(highAct_gsr_cache) <- paste0("highAct_gsr_",colnames(highAct_gsr_cache))
colnames(highAct_hr_cache) <- paste0("highAct_hr_",colnames(highAct_hr_cache))
colnames(highAct_sk_cache) <- paste0("highAct_sk_",colnames(highAct_sk_cache))
colnames(highAct_st_cache) <- paste0("highAct_st_",colnames(highAct_st_cache))

colnames(lowAct_gsr_cache) <- paste0("lowAct_gsr_",colnames(lowAct_gsr_cache))
colnames(lowAct_hr_cache) <- paste0("lowAct_hr_",colnames(lowAct_hr_cache))
colnames(lowAct_sk_cache) <- paste0("lowAct_sk_",colnames(lowAct_sk_cache))
colnames(lowAct_st_cache) <- paste0("lowAct_st_",colnames(lowAct_st_cache))

colnames(day_gsr_cache) <- paste0("day_gsr_",colnames(day_gsr_cache))
colnames(day_hr_cache) <- paste0("day_hr_",colnames(day_hr_cache))
colnames(day_sk_cache) <- paste0("day_sk_",colnames(day_sk_cache))
colnames(day_st_cache) <- paste0("day_st_",colnames(day_st_cache))

colnames(day_rhr_cache) <- paste0("day_rhr_",colnames(day_rhr_cache))

colnames(day_highAct_gsr_cache) <- paste0("day_highAct_gsr_",
                                          colnames(day_highAct_gsr_cache))
colnames(day_highAct_hr_cache) <- paste0("day_highAct_hr_",
                                         colnames(day_highAct_hr_cache))
colnames(day_highAct_sk_cache) <- paste0("day_highAct_sk_",
                                         colnames(day_highAct_sk_cache))
colnames(day_highAct_st_cache) <- paste0("day_highAct_st_",
                                         colnames(day_highAct_st_cache))

colnames(day_lowAct_gsr_cache) <- paste0("day_lowAct_gsr_",
                                         colnames(day_lowAct_gsr_cache))
colnames(day_lowAct_hr_cache) <- paste0("day_lowAct_hr_",
                                        colnames(day_lowAct_hr_cache))
colnames(day_lowAct_sk_cache) <- paste0("day_lowAct_sk_",
                                        colnames(day_lowAct_sk_cache))
colnames(day_lowAct_st_cache) <- paste0("day_lowAct_st_",
                                        colnames(day_lowAct_st_cache))

colnames(night_gsr_cache) <- paste0("night_gsr_",colnames(night_gsr_cache))
colnames(night_hr_cache) <- paste0("night_hr_",colnames(night_hr_cache))
colnames(night_sk_cache) <- paste0("night_sk_",colnames(night_sk_cache))
colnames(night_st_cache) <- paste0("night_st_",colnames(night_st_cache))

colnames(night_rhr_cache) <- paste0("night_rhr_",colnames(night_rhr_cache))

colnames(night_highAct_gsr_cache) <- paste0("night_highAct_gsr_",
                                            colnames(night_highAct_gsr_cache))
colnames(night_highAct_hr_cache) <- paste0("night_highAct_hr_",
                                           colnames(night_highAct_hr_cache))
colnames(night_highAct_sk_cache) <- paste0("night_highAct_sk_",
                                           colnames(night_highAct_sk_cache))
colnames(night_highAct_st_cache) <- paste0("night_highAct_st_",
                                           colnames(night_highAct_st_cache))

colnames(night_lowAct_gsr_cache) <- paste0("night_lowAct_gsr_",
                                           colnames(night_lowAct_gsr_cache))
colnames(night_lowAct_hr_cache) <- paste0("night_lowAct_hr_",
                                          colnames(night_lowAct_hr_cache))
colnames(night_lowAct_sk_cache) <- paste0("night_lowAct_sk_",
                                          colnames(night_lowAct_sk_cache))
colnames(night_lowAct_st_cache) <- paste0("night_lowAct_st_",
                                          colnames(night_lowAct_st_cache))

stats <- cbind(
  ClinWithWear,gsr_cache,hr_cache,sk_cache,st_cache,
  rhr_cache,highAct_gsr_cache,highAct_hr_cache,
  highAct_sk_cache,highAct_st_cache,
  lowAct_gsr_cache,lowAct_hr_cache,
  lowAct_sk_cache,lowAct_st_cache,
  day_gsr_cache,day_hr_cache,day_sk_cache,day_st_cache,
  day_rhr_cache,day_highAct_gsr_cache,day_highAct_hr_cache,
  day_highAct_sk_cache,day_highAct_st_cache,
  day_lowAct_gsr_cache,day_lowAct_hr_cache,
  day_lowAct_sk_cache,day_lowAct_st_cache,
  night_gsr_cache,night_hr_cache,night_sk_cache,night_st_cache,
  night_rhr_cache,night_highAct_gsr_cache,night_highAct_hr_cache,
  night_highAct_sk_cache,night_highAct_st_cache,
  night_lowAct_gsr_cache,night_lowAct_hr_cache,
  night_lowAct_sk_cache,night_lowAct_st_cache)

write.csv(stats,
          paste0("F:\\PhysSubTables_20161111_Normal_20170802\\Basis2016_Clean_Norm_5DayPrior_20180504.csv"),
          row.names=FALSE)


#### DESCRIBE WEEK-PRIOR DATA ####
files <- list.files("F:\\BasisSubsets\\FromBasisData_20161111_Normal_20170802\\WeekPrior_Files",
                    pattern="*.csv",full.names=TRUE)

gsr_cache <- list()
hr_cache <- list()
sk_cache <- list()
st_cache <- list()

rhr_cache <- list()

highAct_gsr_cache <- list()
highAct_hr_cache <- list()
highAct_sk_cache <- list()
highAct_st_cache <- list()

lowAct_gsr_cache <- list()
lowAct_hr_cache <- list()
lowAct_sk_cache <- list()
lowAct_st_cache <- list()


day_gsr_cache <- list()
day_hr_cache <- list()
day_sk_cache <- list()
day_st_cache <- list()

day_rhr_cache <- list()

day_highAct_gsr_cache <- list()
day_highAct_hr_cache <- list()
day_highAct_sk_cache <- list()
day_highAct_st_cache <- list()

day_lowAct_gsr_cache <- list()
day_lowAct_hr_cache <- list()
day_lowAct_sk_cache <- list()
day_lowAct_st_cache <- list()

night_gsr_cache <- list()
night_hr_cache <- list()
night_sk_cache <- list()
night_st_cache <- list()

night_rhr_cache <- list()

night_highAct_gsr_cache <- list()
night_highAct_hr_cache <- list()
night_highAct_sk_cache <- list()
night_highAct_st_cache <- list()

night_lowAct_gsr_cache <- list()
night_lowAct_hr_cache <- list()
night_lowAct_sk_cache <- list()
night_lowAct_st_cache <- list()
###

for(i in 1:length(files)){                       
  check <- file.info(files[i])$size
  if(check!=0){
    df <- read.csv(files[i],header=TRUE,sep=",")
    if("Wearable_Account_MD5" %in% names(df)){
      if(length(intersect(unique(df$Wearable_Account_MD5),rem))!=0){
        df <- df[!(df$Wearable_Account_MD5 %in% rem),]
      }
    } else {
      if(!("Wearable_Account_MD5" %in% names(df))){
        print(paste("MD5 not found in",i))
        print(names(df))
        next
      }
    }
    df$Date <- as.POSIXct(paste(df$Timestamp_Local),format="%Y-%m-%d %H:%M:%S")
    emptyCols <- apply(df,2,function(x) all(is.na(x)))
    if(TRUE %in% emptyCols){
      gsr_cache[[i]] <- data.frame(NA)
      hr_cache[[i]] <- data.frame(NA)
      sk_cache[[i]] <- data.frame(NA)
      st_cache[[i]] <- data.frame(NA)
      
      rhr_cache[[i]] <- data.frame(NA)
      
      highAct_gsr_cache[[i]] <- data.frame(NA)
      highAct_hr_cache[[i]] <- data.frame(NA)
      highAct_sk_cache[[i]] <- data.frame(NA)
      highAct_st_cache[[i]] <- data.frame(NA)
      
      lowAct_gsr_cache[[i]] <- data.frame(NA)
      lowAct_hr_cache[[i]] <- data.frame(NA)
      lowAct_sk_cache[[i]] <- data.frame(NA)
      lowAct_st_cache[[i]] <- data.frame(NA)
      
      day_gsr_cache[[i]] <- data.frame(NA)
      day_hr_cache[[i]] <- data.frame(NA)
      day_sk_cache[[i]] <- data.frame(NA)
      day_st_cache[[i]] <- data.frame(NA)
      
      day_rhr_cache[[i]] <- data.frame(NA)
      
      day_highAct_gsr_cache[[i]] <- data.frame(NA)
      day_highAct_hr_cache[[i]] <- data.frame(NA)
      day_highAct_sk_cache[[i]] <- data.frame(NA)
      day_highAct_st_cache[[i]] <- data.frame(NA)
      
      day_lowAct_gsr_cache[[i]] <- data.frame(NA)
      day_lowAct_hr_cache[[i]] <- data.frame(NA)
      day_lowAct_sk_cache[[i]] <- data.frame(NA)
      day_lowAct_st_cache[[i]] <- data.frame(NA)
      
      night_gsr_cache[[i]] <- data.frame(NA)
      night_hr_cache[[i]] <- data.frame(NA)
      night_sk_cache[[i]] <- data.frame(NA)
      night_st_cache[[i]] <- data.frame(NA)
      
      night_rhr_cache[[i]] <- data.frame(NA)
      
      night_highAct_gsr_cache[[i]] <- data.frame(NA)
      night_highAct_hr_cache[[i]] <- data.frame(NA)
      night_highAct_sk_cache[[i]] <- data.frame(NA)
      night_highAct_st_cache[[i]] <- data.frame(NA)
      
      night_lowAct_gsr_cache[[i]] <- data.frame(NA)
      night_lowAct_hr_cache[[i]] <- data.frame(NA)
      night_lowAct_sk_cache[[i]] <- data.frame(NA)
      night_lowAct_st_cache[[i]] <- data.frame(NA)
      next
    }
    if(NROW(df) < 1440){
      gsr_cache[[i]] <- data.frame(NA)
      hr_cache[[i]] <- data.frame(NA)
      sk_cache[[i]] <- data.frame(NA)
      st_cache[[i]] <- data.frame(NA)
      
      rhr_cache[[i]] <- data.frame(NA)
      
      highAct_gsr_cache[[i]] <- data.frame(NA)
      highAct_hr_cache[[i]] <- data.frame(NA)
      highAct_sk_cache[[i]] <- data.frame(NA)
      highAct_st_cache[[i]] <- data.frame(NA)
      
      lowAct_gsr_cache[[i]] <- data.frame(NA)
      lowAct_hr_cache[[i]] <- data.frame(NA)
      lowAct_sk_cache[[i]] <- data.frame(NA)
      lowAct_st_cache[[i]] <- data.frame(NA)
      
      day_gsr_cache[[i]] <- data.frame(NA)
      day_hr_cache[[i]] <- data.frame(NA)
      day_sk_cache[[i]] <- data.frame(NA)
      day_st_cache[[i]] <- data.frame(NA)
      
      day_rhr_cache[[i]] <- data.frame(NA)
      
      day_highAct_gsr_cache[[i]] <- data.frame(NA)
      day_highAct_hr_cache[[i]] <- data.frame(NA)
      day_highAct_sk_cache[[i]] <- data.frame(NA)
      day_highAct_st_cache[[i]] <- data.frame(NA)
      
      day_lowAct_gsr_cache[[i]] <- data.frame(NA)
      day_lowAct_hr_cache[[i]] <- data.frame(NA)
      day_lowAct_sk_cache[[i]] <- data.frame(NA)
      day_lowAct_st_cache[[i]] <- data.frame(NA)
      
      night_gsr_cache[[i]] <- data.frame(NA)
      night_hr_cache[[i]] <- data.frame(NA)
      night_sk_cache[[i]] <- data.frame(NA)
      night_st_cache[[i]] <- data.frame(NA)
      
      night_rhr_cache[[i]] <- data.frame(NA)
      
      night_highAct_gsr_cache[[i]] <- data.frame(NA)
      night_highAct_hr_cache[[i]] <- data.frame(NA)
      night_highAct_sk_cache[[i]] <- data.frame(NA)
      night_highAct_st_cache[[i]] <- data.frame(NA)
      
      night_lowAct_gsr_cache[[i]] <- data.frame(NA)
      night_lowAct_hr_cache[[i]] <- data.frame(NA)
      night_lowAct_sk_cache[[i]] <- data.frame(NA)
      night_lowAct_st_cache[[i]] <- data.frame(NA)
      next
    }
    gsr_cache[[i]] <- data.frame(describe(df$GSR))
    hr_cache[[i]] <- data.frame(describe(df$Heart_Rate))
    sk_cache[[i]] <- data.frame(describe(df$Skin_Temperature_F))
    st_cache[[i]] <- data.frame(describe(df$Steps))
    
    
    source("F:\\R_Commands\\LabelHours.R")
    label.hours(df, "Timestamp_Local")
    
    day_gsr_cache[[i]] <- data.frame(describe(
      df$GSR[which(df$Hour>=8 & df$Hour<20)]))
    day_hr_cache[[i]] <- data.frame(describe(
      df$Heart_Rate[which(df$Hour>=8 & df$Hour<20)]))
    day_sk_cache[[i]] <- data.frame(describe(
      df$Skin_Temperature_F[which(df$Hour>=8 & df$Hour<20)]))
    day_st_cache[[i]] <- data.frame(describe(
      df$Steps[which(df$Hour>=8 & df$Hour<20)]))
    
    night_gsr_cache[[i]] <- data.frame(describe(
      df$GSR[which(df$Hour>=20 | df$Hour<8)]))
    night_hr_cache[[i]] <- data.frame(describe(
      df$Heart_Rate[which(df$Hour>=20 | df$Hour<8)]))
    night_sk_cache[[i]] <- data.frame(describe(
      df$Skin_Temperature_F[which(df$Hour>=20 | df$Hour<8)]))
    night_st_cache[[i]] <- data.frame(describe(
      df$Steps[which(df$Hour>=20 | df$Hour<8)]))
    ###
    
    ## NEW
    
    if(1==1){
      df$lowSteps <- df$Steps
      df$Steps[df$Steps==0] <- NA
      if(all(is.na(df$Steps))){
        print(paste0("Error: No Steps for Candidate #",i))
        
        rhr_cache[[i]] <- data.frame(NA)
        
        highAct_gsr_cache[[i]] <- data.frame(NA)
        highAct_hr_cache[[i]] <- data.frame(NA)
        highAct_sk_cache[[i]] <- data.frame(NA)
        highAct_st_cache[[i]] <- data.frame(NA)
        
        lowAct_gsr_cache[[i]] <- data.frame(NA)
        lowAct_hr_cache[[i]] <- data.frame(NA)
        lowAct_sk_cache[[i]] <- data.frame(NA)
        lowAct_st_cache[[i]] <- data.frame(NA)
        
        day_gsr_cache[[i]] <- data.frame(NA)
        day_hr_cache[[i]] <- data.frame(NA)
        day_sk_cache[[i]] <- data.frame(NA)
        day_st_cache[[i]] <- data.frame(NA)
        
        day_rhr_cache[[i]] <- data.frame(NA)
        
        day_highAct_gsr_cache[[i]] <- data.frame(NA)
        day_highAct_hr_cache[[i]] <- data.frame(NA)
        day_highAct_sk_cache[[i]] <- data.frame(NA)
        day_highAct_st_cache[[i]] <- data.frame(NA)
        
        day_lowAct_gsr_cache[[i]] <- data.frame(NA)
        day_lowAct_hr_cache[[i]] <- data.frame(NA)
        day_lowAct_sk_cache[[i]] <- data.frame(NA)
        day_lowAct_st_cache[[i]] <- data.frame(NA)
        
        night_gsr_cache[[i]] <- data.frame(NA)
        night_hr_cache[[i]] <- data.frame(NA)
        night_sk_cache[[i]] <- data.frame(NA)
        night_st_cache[[i]] <- data.frame(NA)
        
        night_rhr_cache[[i]] <- data.frame(NA)
        
        night_highAct_gsr_cache[[i]] <- data.frame(NA)
        night_highAct_hr_cache[[i]] <- data.frame(NA)
        night_highAct_sk_cache[[i]] <- data.frame(NA)
        night_highAct_st_cache[[i]] <- data.frame(NA)
        
        night_lowAct_gsr_cache[[i]] <- data.frame(NA)
        night_lowAct_hr_cache[[i]] <- data.frame(NA)
        night_lowAct_sk_cache[[i]] <- data.frame(NA)
        night_lowAct_st_cache[[i]] <- data.frame(NA)
        next
        
      } else {
        if(1==1){
          df$Steps[df$Steps==0] <- NA
          #ID steps decile locations
          StDecLocations <- quantile(
            df$Steps, na.rm=TRUE, 
            probs = seq(0.1,0.9,by=0.1))
          if(length(unique(StDecLocations)) != 9){
            check <- which(table(StDecLocations) > 1)
            for(u in 1:length(check)){
              check2 <- which(StDecLocations==names(check)[u])
              theMin <- as.numeric(min(check2))
              for(v in 1:length(check2)){
                if(check2[v] == theMin){
                  next
                } else {
                  StDecLocations[check2[v]] <- sum(
                    StDecLocations[check2[v]],0.0000000000001)
                }
              }
            }
          }
          df$StDecID <- findInterval(df$Steps,
                                     c(-Inf,StDecLocations, Inf))
        }
        #ID heart rate decile locations
        HrDecLocations <- quantile(
          df$Heart_Rate, na.rm=TRUE, 
          probs = seq(0.1,0.9, by=0.1))
        if(length(unique(HrDecLocations)) != 9){
          check <- which(table(HrDecLocations) > 1)
          for(u in 1:length(check)){
            check2 <- which(HrDecLocations==names(check)[u])
            theMin <- as.numeric(min(check2))
            for(v in 1:length(check2)){
              if(check2[v] == theMin){
                next
              } else {
                HrDecLocations[check2[v]] <- sum(
                  HrDecLocations[check2[v]],0.0000000000001)
              }
            }
          }
        }
        df$HrDecID <- findInterval(df$Heart_Rate,
                                   c(-Inf,HrDecLocations,Inf))
        #ID skin temperature decile locations
        SkDecLocations <- quantile(
          df$Skin_Temperature_F, na.rm=TRUE, 
          probs = seq(0.1,0.9,by=0.1))
        if(length(unique(SkDecLocations)) != 9){
          check <- which(table(SkDecLocations) > 1)
          for(u in 1:length(check)){
            check2 <- which(SkDecLocations==names(check)[u])
            theMin <- as.numeric(min(check2))
            for(v in 1:length(check2)){
              if(check2[v] == theMin){
                next
              } else {
                SkDecLocations[check2[v]] <- sum(
                  SkDecLocations[check2[v]],0.0000000000001)
              }
            }
          }
        }
        df$SkDecID <- findInterval(df$Skin_Temperature_F,
                                   c(-Inf,SkDecLocations, Inf))
        #ID gsr decile locations
        GsrDecLocations <- quantile(
          df$GSR, na.rm=TRUE, 
          probs = seq(0.1,0.9,by=0.1))
        if(length(unique(GsrDecLocations)) != 9){
          check <- which(table(GsrDecLocations) > 1)
          for(u in 1:length(check)){
            check2 <- which(GsrDecLocations==names(check)[u])
            theMin <- as.numeric(min(check2))
            for(v in 1:length(check2)){
              if(check2[v] == theMin){
                next
              } else {
                GsrDecLocations[check2[v]] <- sum(
                  GsrDecLocations[check2[v]],0.0000000000001)
              }
            }
          }
        }
        df$GsrDecID <- findInterval(df$GSR,
                                    c(-Inf,GsrDecLocations, Inf))
        
        #SUBSET RESTING HEART RATE PARAMETERS TO "rdf" table
        restingST <- ave(df$lowSteps, df$Wearable_Account_MD5,
                         FUN=function(x) rollapplyr(x, 10, sum, by=1, partial=TRUE))
        restingST[1:10-1] <- c(rep(NA,10-1)) #window-1
        
        #To record resting heart rate as rolling 10-minute mean when sum of steps in last 10 min < 10:
        # restingHR <- ave(df$Heart_Rate, df$Wearable_Account_MD5,
        #                  FUN=function(x) rollapplyr(x, 10, mean, na.rm=TRUE,
        #                                             by=1, partial=TRUE, fill=NA))
        # 
        # restingHR <- restingHR[which(restingST<10 & !is.na(restingST))]
        
        #To record resting HR as all values when sum of steps in last 10 min < 10:
        restingHR <- df$Heart_Rate[which(restingST<10 & !is.na(restingST))]
        
        rhr_cache[[i]] <- data.frame(describe(restingHR))
        
        df_day <- df[which(df$Hour>=8 & df$Hour<20),]
        
        restingST <- ave(df_day$lowSteps, df_day$Wearable_Account_MD5,
                         FUN=function(x) rollapplyr(x, 10, sum, by=1, partial=TRUE)) 
        restingST[1:10-1] <- c(rep(NA,10-1)) #window-1
        
        #To record resting heart rate as rolling 10-minute mean when sum of steps in last 10 min < 10:
        # restingHR <- ave(df$Heart_Rate, df$Wearable_Account_MD5,
        #                  FUN=function(x) rollapplyr(x, 10, mean, na.rm=TRUE,
        #                                             by=1, partial=TRUE, fill=NA))
        # 
        # restingHR <- restingHR[which(restingST<10 & !is.na(restingST))]
        
        #To record resting HR as all values when sum of steps in last 10 min < 10:
        restingHR <- df_day$Heart_Rate[which(restingST<10 & !is.na(restingST))]
        
        # restingHR <- restingHR[which(restingST<10 & !is.na(restingST))] 
        
        day_rhr_cache[[i]] <- data.frame(describe(restingHR))
        rm(df_day)
        
        df_night <- df[which(df$Hour>=20 | df$Hour<8),]
        
        restingST <- ave(df_night$lowSteps, df_night$Wearable_Account_MD5,
                         FUN=function(x) rollapplyr(x, 10, sum, by=1, partial=TRUE))
        restingST[1:10-1] <- c(rep(NA,10-1)) #window-1
        
        # restingHR <- ave(df_night$Heart_Rate, df_night$Wearable_Account_MD5,
        # FUN=function(x) rollapplyr(x, 10, mean, by=1, partial=TRUE)) 
        
        restingHR <- df_night$Heart_Rate[which(restingST<10 & !is.na(restingST))] 
        
        night_rhr_cache[[i]] <- data.frame(describe(restingHR))
        rm(df_night)
        ###
        
        #label high activity (defined as top 10% of Steps)
        if(1==1){
          df$highActivity <- ifelse(
            df$StDecID==10,
            TRUE,
            FALSE)
          #df$highActivity_eventNum <- rleid(df$highActivity)
        }
        
        durationDf <- df[df$highActivity == "TRUE",]
        durationDf$Date <- as.POSIXct(paste(durationDf$Timestamp_Local),
                                      format="%Y-%m-%d %H:%M:%S")
        
        #describe variables during high activity
        highAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        highAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        highAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        highAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        
        
        df_day <- df[which(df$Hour>=8 & df$Hour<20),]
        durationDf_day <- df_day[df_day$highActivity == "TRUE",]
        durationDf_day$Date <- as.POSIXct(paste(durationDf_day$Timestamp_Local),
                                          format="%Y-%m-%d %H:%M:%S")
        
        day_highAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        day_highAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        day_highAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        day_highAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        rm(df_day)
        
        df_night <- df[which(df$Hour>=20 | df$Hour<8),]
        durationDf_day <- df_night[df_night$highActivity == "TRUE",]
        durationDf_day$Date <- as.POSIXct(paste(durationDf_day$Timestamp_Local),
                                          format="%Y-%m-%d %H:%M:%S")
        
        night_highAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        night_highAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        night_highAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        night_highAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        rm(df_night)
        ###
        
        #label low activity (defined as lowest 10% of Steps)
        if(1==1){
          df$lowActivity <- ifelse(
            df$StDecID==1,
            TRUE,
            FALSE)
          #df$lowActivity_eventNum <- rleid(df$lowActivity)
        }
        
        durationDf <- df[df$lowActivity == "TRUE",]
        durationDf$Date <- as.POSIXct(paste(durationDf$Timestamp_Local),
                                      format="%Y-%m-%d %H:%M:%S")
        
        #describe variables during low activity
        lowAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        lowAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        lowAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        lowAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        
        
        df_day <- df[which(df$Hour>=8 & df$Hour<20),]
        durationDf <- df_day[df_day$lowActivity == "TRUE",]
        durationDf$Date <- as.POSIXct(paste(durationDf$Timestamp_Local),
                                      format="%Y-%m-%d %H:%M:%S")
        
        day_lowAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        day_lowAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        day_lowAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        day_lowAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        rm(df_day)
        
        df_night <- df[which(df$Hour>=20 | df$Hour<8),]
        durationDf <- df_night[df_night$lowActivity == "TRUE",]
        durationDf$Date <- as.POSIXct(paste(durationDf$Timestamp_Local),
                                      format="%Y-%m-%d %H:%M:%S")
        
        night_lowAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        night_lowAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        night_lowAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        night_lowAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        rm(df_night)
        ###
      }
    }
  } else {
    gsr_cache[[i]] <- data.frame(NA)
    hr_cache[[i]] <- data.frame(NA)
    sk_cache[[i]] <- data.frame(NA)
    st_cache[[i]] <- data.frame(NA)
    
    rhr_cache[[i]] <- data.frame(NA)
    
    highAct_gsr_cache[[i]] <- data.frame(NA)
    highAct_hr_cache[[i]] <- data.frame(NA)
    highAct_sk_cache[[i]] <- data.frame(NA)
    highAct_st_cache[[i]] <- data.frame(NA)
    
    lowAct_gsr_cache[[i]] <- data.frame(NA)
    lowAct_hr_cache[[i]] <- data.frame(NA)
    lowAct_sk_cache[[i]] <- data.frame(NA)
    lowAct_st_cache[[i]] <- data.frame(NA)
    
    day_gsr_cache[[i]] <- data.frame(NA)
    day_hr_cache[[i]] <- data.frame(NA)
    day_sk_cache[[i]] <- data.frame(NA)
    day_st_cache[[i]] <- data.frame(NA)
    
    day_rhr_cache[[i]] <- data.frame(NA)
    
    day_highAct_gsr_cache[[i]] <- data.frame(NA)
    day_highAct_hr_cache[[i]] <- data.frame(NA)
    day_highAct_sk_cache[[i]] <- data.frame(NA)
    day_highAct_st_cache[[i]] <- data.frame(NA)
    
    day_lowAct_gsr_cache[[i]] <- data.frame(NA)
    day_lowAct_hr_cache[[i]] <- data.frame(NA)
    day_lowAct_sk_cache[[i]] <- data.frame(NA)
    day_lowAct_st_cache[[i]] <- data.frame(NA)
    
    night_gsr_cache[[i]] <- data.frame(NA)
    night_hr_cache[[i]] <- data.frame(NA)
    night_sk_cache[[i]] <- data.frame(NA)
    night_st_cache[[i]] <- data.frame(NA)
    
    night_rhr_cache[[i]] <- data.frame(NA)
    
    night_highAct_gsr_cache[[i]] <- data.frame(NA)
    night_highAct_hr_cache[[i]] <- data.frame(NA)
    night_highAct_sk_cache[[i]] <- data.frame(NA)
    night_highAct_st_cache[[i]] <- data.frame(NA)
    
    night_lowAct_gsr_cache[[i]] <- data.frame(NA)
    night_lowAct_hr_cache[[i]] <- data.frame(NA)
    night_lowAct_sk_cache[[i]] <- data.frame(NA)
    night_lowAct_st_cache[[i]] <- data.frame(NA)
    
    print(i)
    next
  }
  print(i)
}

gsr_cache <- rbind.fill(gsr_cache)
hr_cache <- rbind.fill(hr_cache)
sk_cache <- rbind.fill(sk_cache)
st_cache <- rbind.fill(st_cache)

rhr_cache <- rbind.fill(rhr_cache)

highAct_gsr_cache <- rbind.fill(highAct_gsr_cache)
highAct_hr_cache <- rbind.fill(highAct_hr_cache)
highAct_sk_cache <- rbind.fill(highAct_sk_cache)
highAct_st_cache <- rbind.fill(highAct_st_cache)

lowAct_gsr_cache <- rbind.fill(lowAct_gsr_cache)
lowAct_hr_cache <- rbind.fill(lowAct_hr_cache)
lowAct_sk_cache <- rbind.fill(lowAct_sk_cache)
lowAct_st_cache <- rbind.fill(lowAct_st_cache)

day_gsr_cache <- rbind.fill(day_gsr_cache)
day_hr_cache <- rbind.fill(day_hr_cache)
day_sk_cache <- rbind.fill(day_sk_cache)
day_st_cache <- rbind.fill(day_st_cache)

day_rhr_cache <- rbind.fill(day_rhr_cache)

day_highAct_gsr_cache <- rbind.fill(day_highAct_gsr_cache)
day_highAct_hr_cache <- rbind.fill(day_highAct_hr_cache)
day_highAct_sk_cache <- rbind.fill(day_highAct_sk_cache)
day_highAct_st_cache <- rbind.fill(day_highAct_st_cache)

day_lowAct_gsr_cache <- rbind.fill(day_lowAct_gsr_cache)
day_lowAct_hr_cache <- rbind.fill(day_lowAct_hr_cache)
day_lowAct_sk_cache <- rbind.fill(day_lowAct_sk_cache)
day_lowAct_st_cache <- rbind.fill(day_lowAct_st_cache)

night_gsr_cache <- rbind.fill(night_gsr_cache)
night_hr_cache <- rbind.fill(night_hr_cache)
night_sk_cache <- rbind.fill(night_sk_cache)
night_st_cache <- rbind.fill(night_st_cache)

night_rhr_cache <- rbind.fill(night_rhr_cache)

night_highAct_gsr_cache <- rbind.fill(night_highAct_gsr_cache)
night_highAct_hr_cache <- rbind.fill(night_highAct_hr_cache)
night_highAct_sk_cache <- rbind.fill(night_highAct_sk_cache)
night_highAct_st_cache <- rbind.fill(night_highAct_st_cache)

night_lowAct_gsr_cache <- rbind.fill(night_lowAct_gsr_cache)
night_lowAct_hr_cache <- rbind.fill(night_lowAct_hr_cache)
night_lowAct_sk_cache <- rbind.fill(night_lowAct_sk_cache)
night_lowAct_st_cache <- rbind.fill(night_lowAct_st_cache)

gsr_cache <- gsr_cache[,-1] #first column was for NAs
hr_cache <- hr_cache[,-1] #first column was for NAs
sk_cache <- sk_cache[,-1] #first column was for NAs
st_cache <- st_cache[,-1] #first column was for NAs

rhr_cache <- rhr_cache[,-1] #first column was for NAs

highAct_gsr_cache <- highAct_gsr_cache[,-1] #first column was for NAs
highAct_hr_cache <- highAct_hr_cache[,-1] #first column was for NAs
highAct_sk_cache <- highAct_sk_cache[,-1] #first column was for NAs
highAct_st_cache <- highAct_st_cache[,-1] #first column was for NAs

lowAct_gsr_cache <- lowAct_gsr_cache[,-1] #first column was for NAs
lowAct_hr_cache <- lowAct_hr_cache[,-1] #first column was for NAs
lowAct_sk_cache <- lowAct_sk_cache[,-1] #first column was for NAs
lowAct_st_cache <- lowAct_st_cache[,-1] #first column was for NAs

day_gsr_cache <- day_gsr_cache[,-1] #first column was for NAs
day_hr_cache <- day_hr_cache[,-1] #first column was for NAs
day_sk_cache <- day_sk_cache[,-1] #first column was for NAs
day_st_cache <- day_st_cache[,-1] #first column was for NAs

day_rhr_cache <- day_rhr_cache[,-1] #first column was for NAs

day_highAct_gsr_cache <- day_highAct_gsr_cache[,-1] #first column was for NAs
day_highAct_hr_cache <- day_highAct_hr_cache[,-1] #first column was for NAs
day_highAct_sk_cache <- day_highAct_sk_cache[,-1] #first column was for NAs
day_highAct_st_cache <- day_highAct_st_cache[,-1] #first column was for NAs

day_lowAct_gsr_cache <- day_lowAct_gsr_cache[,-1] #first column was for NAs
day_lowAct_hr_cache <- day_lowAct_hr_cache[,-1] #first column was for NAs
day_lowAct_sk_cache <- day_lowAct_sk_cache[,-1] #first column was for NAs
day_lowAct_st_cache <- day_lowAct_st_cache[,-1] #first column was for NAs

night_gsr_cache <- night_gsr_cache[,-1] #first column was for NAs
night_hr_cache <- night_hr_cache[,-1] #first column was for NAs
night_sk_cache <- night_sk_cache[,-1] #first column was for NAs
night_st_cache <- night_st_cache[,-1] #first column was for NAs

night_rhr_cache <- night_rhr_cache[,-1] #first column was for NAs

night_highAct_gsr_cache <- night_highAct_gsr_cache[,-1] #first column was for NAs
night_highAct_hr_cache <- night_highAct_hr_cache[,-1] #first column was for NAs
night_highAct_sk_cache <- night_highAct_sk_cache[,-1] #first column was for NAs
night_highAct_st_cache <- night_highAct_st_cache[,-1] #first column was for NAs

night_lowAct_gsr_cache <- night_lowAct_gsr_cache[,-1] #first column was for NAs
night_lowAct_hr_cache <- night_lowAct_hr_cache[,-1] #first column was for NAs
night_lowAct_sk_cache <- night_lowAct_sk_cache[,-1] #first column was for NAs
night_lowAct_st_cache <- night_lowAct_st_cache[,-1] #first column was for NAs

colnames(gsr_cache) <- paste0("gsr_",colnames(gsr_cache))
colnames(hr_cache) <- paste0("hr_",colnames(hr_cache))
colnames(sk_cache) <- paste0("sk_",colnames(sk_cache))
colnames(st_cache) <- paste0("st_",colnames(st_cache))

colnames(rhr_cache) <- paste0("rhr_",colnames(rhr_cache))

colnames(highAct_gsr_cache) <- paste0("highAct_gsr_",colnames(highAct_gsr_cache))
colnames(highAct_hr_cache) <- paste0("highAct_hr_",colnames(highAct_hr_cache))
colnames(highAct_sk_cache) <- paste0("highAct_sk_",colnames(highAct_sk_cache))
colnames(highAct_st_cache) <- paste0("highAct_st_",colnames(highAct_st_cache))

colnames(lowAct_gsr_cache) <- paste0("lowAct_gsr_",colnames(lowAct_gsr_cache))
colnames(lowAct_hr_cache) <- paste0("lowAct_hr_",colnames(lowAct_hr_cache))
colnames(lowAct_sk_cache) <- paste0("lowAct_sk_",colnames(lowAct_sk_cache))
colnames(lowAct_st_cache) <- paste0("lowAct_st_",colnames(lowAct_st_cache))

colnames(day_gsr_cache) <- paste0("day_gsr_",colnames(day_gsr_cache))
colnames(day_hr_cache) <- paste0("day_hr_",colnames(day_hr_cache))
colnames(day_sk_cache) <- paste0("day_sk_",colnames(day_sk_cache))
colnames(day_st_cache) <- paste0("day_st_",colnames(day_st_cache))

colnames(day_rhr_cache) <- paste0("day_rhr_",colnames(day_rhr_cache))

colnames(day_highAct_gsr_cache) <- paste0("day_highAct_gsr_",
                                          colnames(day_highAct_gsr_cache))
colnames(day_highAct_hr_cache) <- paste0("day_highAct_hr_",
                                         colnames(day_highAct_hr_cache))
colnames(day_highAct_sk_cache) <- paste0("day_highAct_sk_",
                                         colnames(day_highAct_sk_cache))
colnames(day_highAct_st_cache) <- paste0("day_highAct_st_",
                                         colnames(day_highAct_st_cache))

colnames(day_lowAct_gsr_cache) <- paste0("day_lowAct_gsr_",
                                         colnames(day_lowAct_gsr_cache))
colnames(day_lowAct_hr_cache) <- paste0("day_lowAct_hr_",
                                        colnames(day_lowAct_hr_cache))
colnames(day_lowAct_sk_cache) <- paste0("day_lowAct_sk_",
                                        colnames(day_lowAct_sk_cache))
colnames(day_lowAct_st_cache) <- paste0("day_lowAct_st_",
                                        colnames(day_lowAct_st_cache))

colnames(night_gsr_cache) <- paste0("night_gsr_",colnames(night_gsr_cache))
colnames(night_hr_cache) <- paste0("night_hr_",colnames(night_hr_cache))
colnames(night_sk_cache) <- paste0("night_sk_",colnames(night_sk_cache))
colnames(night_st_cache) <- paste0("night_st_",colnames(night_st_cache))

colnames(night_rhr_cache) <- paste0("night_rhr_",colnames(night_rhr_cache))

colnames(night_highAct_gsr_cache) <- paste0("night_highAct_gsr_",
                                            colnames(night_highAct_gsr_cache))
colnames(night_highAct_hr_cache) <- paste0("night_highAct_hr_",
                                           colnames(night_highAct_hr_cache))
colnames(night_highAct_sk_cache) <- paste0("night_highAct_sk_",
                                           colnames(night_highAct_sk_cache))
colnames(night_highAct_st_cache) <- paste0("night_highAct_st_",
                                           colnames(night_highAct_st_cache))

colnames(night_lowAct_gsr_cache) <- paste0("night_lowAct_gsr_",
                                           colnames(night_lowAct_gsr_cache))
colnames(night_lowAct_hr_cache) <- paste0("night_lowAct_hr_",
                                          colnames(night_lowAct_hr_cache))
colnames(night_lowAct_sk_cache) <- paste0("night_lowAct_sk_",
                                          colnames(night_lowAct_sk_cache))
colnames(night_lowAct_st_cache) <- paste0("night_lowAct_st_",
                                          colnames(night_lowAct_st_cache))

stats <- cbind(
  ClinWithWear,gsr_cache,hr_cache,sk_cache,st_cache,
  rhr_cache,highAct_gsr_cache,highAct_hr_cache,
  highAct_sk_cache,highAct_st_cache,
  lowAct_gsr_cache,lowAct_hr_cache,
  lowAct_sk_cache,lowAct_st_cache,
  day_gsr_cache,day_hr_cache,day_sk_cache,day_st_cache,
  day_rhr_cache,day_highAct_gsr_cache,day_highAct_hr_cache,
  day_highAct_sk_cache,day_highAct_st_cache,
  day_lowAct_gsr_cache,day_lowAct_hr_cache,
  day_lowAct_sk_cache,day_lowAct_st_cache,
  night_gsr_cache,night_hr_cache,night_sk_cache,night_st_cache,
  night_rhr_cache,night_highAct_gsr_cache,night_highAct_hr_cache,
  night_highAct_sk_cache,night_highAct_st_cache,
  night_lowAct_gsr_cache,night_lowAct_hr_cache,
  night_lowAct_sk_cache,night_lowAct_st_cache)

write.csv(stats,
          paste0("F:\\PhysSubTables_20161111_Normal_20170802\\Basis2016_Clean_Norm_WeekPrior_20180504.csv"),
          row.names=FALSE)


#### DESCRIBE 2-WEEK-PRIOR DATA ####
files <- list.files("F:\\BasisSubsets\\FromBasisData_20161111_Normal_20170802\\2WeekPrior_Files",
                    pattern="*.csv",full.names=TRUE)

gsr_cache <- list()
hr_cache <- list()
sk_cache <- list()
st_cache <- list()

rhr_cache <- list()

highAct_gsr_cache <- list()
highAct_hr_cache <- list()
highAct_sk_cache <- list()
highAct_st_cache <- list()

lowAct_gsr_cache <- list()
lowAct_hr_cache <- list()
lowAct_sk_cache <- list()
lowAct_st_cache <- list()


day_gsr_cache <- list()
day_hr_cache <- list()
day_sk_cache <- list()
day_st_cache <- list()

day_rhr_cache <- list()

day_highAct_gsr_cache <- list()
day_highAct_hr_cache <- list()
day_highAct_sk_cache <- list()
day_highAct_st_cache <- list()

day_lowAct_gsr_cache <- list()
day_lowAct_hr_cache <- list()
day_lowAct_sk_cache <- list()
day_lowAct_st_cache <- list()

night_gsr_cache <- list()
night_hr_cache <- list()
night_sk_cache <- list()
night_st_cache <- list()

night_rhr_cache <- list()

night_highAct_gsr_cache <- list()
night_highAct_hr_cache <- list()
night_highAct_sk_cache <- list()
night_highAct_st_cache <- list()

night_lowAct_gsr_cache <- list()
night_lowAct_hr_cache <- list()
night_lowAct_sk_cache <- list()
night_lowAct_st_cache <- list()
###
i=140

for(i in 1:length(files)){                       
  check <- file.info(files[i])$size
  if(check!=0){
    df <- read.csv(files[i],header=TRUE,sep=",")
    if("Wearable_Account_MD5" %in% names(df)){
      if(length(intersect(unique(df$Wearable_Account_MD5),rem))!=0){
        df <- df[!(df$Wearable_Account_MD5 %in% rem),]
      }
    } else {
      if(!("Wearable_Account_MD5" %in% names(df))){
        print(paste("MD5 not found in",i))
        print(names(df))
        next
      }
    }
    df$Date <- as.POSIXct(paste(df$Timestamp_Local),format="%Y-%m-%d %H:%M:%S")
    emptyCols <- apply(df,2,function(x) all(is.na(x)))
    if(TRUE %in% emptyCols){
      gsr_cache[[i]] <- data.frame(NA)
      hr_cache[[i]] <- data.frame(NA)
      sk_cache[[i]] <- data.frame(NA)
      st_cache[[i]] <- data.frame(NA)
      
      rhr_cache[[i]] <- data.frame(NA)
      
      highAct_gsr_cache[[i]] <- data.frame(NA)
      highAct_hr_cache[[i]] <- data.frame(NA)
      highAct_sk_cache[[i]] <- data.frame(NA)
      highAct_st_cache[[i]] <- data.frame(NA)
      
      lowAct_gsr_cache[[i]] <- data.frame(NA)
      lowAct_hr_cache[[i]] <- data.frame(NA)
      lowAct_sk_cache[[i]] <- data.frame(NA)
      lowAct_st_cache[[i]] <- data.frame(NA)
      
      day_gsr_cache[[i]] <- data.frame(NA)
      day_hr_cache[[i]] <- data.frame(NA)
      day_sk_cache[[i]] <- data.frame(NA)
      day_st_cache[[i]] <- data.frame(NA)
      
      day_rhr_cache[[i]] <- data.frame(NA)
      
      day_highAct_gsr_cache[[i]] <- data.frame(NA)
      day_highAct_hr_cache[[i]] <- data.frame(NA)
      day_highAct_sk_cache[[i]] <- data.frame(NA)
      day_highAct_st_cache[[i]] <- data.frame(NA)
      
      day_lowAct_gsr_cache[[i]] <- data.frame(NA)
      day_lowAct_hr_cache[[i]] <- data.frame(NA)
      day_lowAct_sk_cache[[i]] <- data.frame(NA)
      day_lowAct_st_cache[[i]] <- data.frame(NA)
      
      night_gsr_cache[[i]] <- data.frame(NA)
      night_hr_cache[[i]] <- data.frame(NA)
      night_sk_cache[[i]] <- data.frame(NA)
      night_st_cache[[i]] <- data.frame(NA)
      
      night_rhr_cache[[i]] <- data.frame(NA)
      
      night_highAct_gsr_cache[[i]] <- data.frame(NA)
      night_highAct_hr_cache[[i]] <- data.frame(NA)
      night_highAct_sk_cache[[i]] <- data.frame(NA)
      night_highAct_st_cache[[i]] <- data.frame(NA)
      
      night_lowAct_gsr_cache[[i]] <- data.frame(NA)
      night_lowAct_hr_cache[[i]] <- data.frame(NA)
      night_lowAct_sk_cache[[i]] <- data.frame(NA)
      night_lowAct_st_cache[[i]] <- data.frame(NA)
      next
    }
    if(NROW(df) < 1440){
      gsr_cache[[i]] <- data.frame(NA)
      hr_cache[[i]] <- data.frame(NA)
      sk_cache[[i]] <- data.frame(NA)
      st_cache[[i]] <- data.frame(NA)
      
      rhr_cache[[i]] <- data.frame(NA)
      
      highAct_gsr_cache[[i]] <- data.frame(NA)
      highAct_hr_cache[[i]] <- data.frame(NA)
      highAct_sk_cache[[i]] <- data.frame(NA)
      highAct_st_cache[[i]] <- data.frame(NA)
      
      lowAct_gsr_cache[[i]] <- data.frame(NA)
      lowAct_hr_cache[[i]] <- data.frame(NA)
      lowAct_sk_cache[[i]] <- data.frame(NA)
      lowAct_st_cache[[i]] <- data.frame(NA)
      
      day_gsr_cache[[i]] <- data.frame(NA)
      day_hr_cache[[i]] <- data.frame(NA)
      day_sk_cache[[i]] <- data.frame(NA)
      day_st_cache[[i]] <- data.frame(NA)
      
      day_rhr_cache[[i]] <- data.frame(NA)
      
      day_highAct_gsr_cache[[i]] <- data.frame(NA)
      day_highAct_hr_cache[[i]] <- data.frame(NA)
      day_highAct_sk_cache[[i]] <- data.frame(NA)
      day_highAct_st_cache[[i]] <- data.frame(NA)
      
      day_lowAct_gsr_cache[[i]] <- data.frame(NA)
      day_lowAct_hr_cache[[i]] <- data.frame(NA)
      day_lowAct_sk_cache[[i]] <- data.frame(NA)
      day_lowAct_st_cache[[i]] <- data.frame(NA)
      
      night_gsr_cache[[i]] <- data.frame(NA)
      night_hr_cache[[i]] <- data.frame(NA)
      night_sk_cache[[i]] <- data.frame(NA)
      night_st_cache[[i]] <- data.frame(NA)
      
      night_rhr_cache[[i]] <- data.frame(NA)
      
      night_highAct_gsr_cache[[i]] <- data.frame(NA)
      night_highAct_hr_cache[[i]] <- data.frame(NA)
      night_highAct_sk_cache[[i]] <- data.frame(NA)
      night_highAct_st_cache[[i]] <- data.frame(NA)
      
      night_lowAct_gsr_cache[[i]] <- data.frame(NA)
      night_lowAct_hr_cache[[i]] <- data.frame(NA)
      night_lowAct_sk_cache[[i]] <- data.frame(NA)
      night_lowAct_st_cache[[i]] <- data.frame(NA)
      next
    }
    gsr_cache[[i]] <- data.frame(describe(df$GSR))
    hr_cache[[i]] <- data.frame(describe(df$Heart_Rate))
    sk_cache[[i]] <- data.frame(describe(df$Skin_Temperature_F))
    st_cache[[i]] <- data.frame(describe(df$Steps))
    
    
    source("F:\\R_Commands\\LabelHours.R")
    label.hours(df, "Timestamp_Local")
    
    day_gsr_cache[[i]] <- data.frame(describe(
      df$GSR[which(df$Hour>=8 & df$Hour<20)]))
    day_hr_cache[[i]] <- data.frame(describe(
      df$Heart_Rate[which(df$Hour>=8 & df$Hour<20)]))
    day_sk_cache[[i]] <- data.frame(describe(
      df$Skin_Temperature_F[which(df$Hour>=8 & df$Hour<20)]))
    day_st_cache[[i]] <- data.frame(describe(
      df$Steps[which(df$Hour>=8 & df$Hour<20)]))
    
    night_gsr_cache[[i]] <- data.frame(describe(
      df$GSR[which(df$Hour>=20 | df$Hour<8)]))
    night_hr_cache[[i]] <- data.frame(describe(
      df$Heart_Rate[which(df$Hour>=20 | df$Hour<8)]))
    night_sk_cache[[i]] <- data.frame(describe(
      df$Skin_Temperature_F[which(df$Hour>=20 | df$Hour<8)]))
    night_st_cache[[i]] <- data.frame(describe(
      df$Steps[which(df$Hour>=20 | df$Hour<8)]))
    ###
    
    ## NEW
    
    if(1==1){
      df$lowSteps <- df$Steps
      df$Steps[df$Steps==0] <- NA
      if(all(is.na(df$Steps))){
        print(paste0("Error: No Steps for Candidate #",i))
        
        rhr_cache[[i]] <- data.frame(NA)
        
        highAct_gsr_cache[[i]] <- data.frame(NA)
        highAct_hr_cache[[i]] <- data.frame(NA)
        highAct_sk_cache[[i]] <- data.frame(NA)
        highAct_st_cache[[i]] <- data.frame(NA)
        
        lowAct_gsr_cache[[i]] <- data.frame(NA)
        lowAct_hr_cache[[i]] <- data.frame(NA)
        lowAct_sk_cache[[i]] <- data.frame(NA)
        lowAct_st_cache[[i]] <- data.frame(NA)
        
        day_gsr_cache[[i]] <- data.frame(NA)
        day_hr_cache[[i]] <- data.frame(NA)
        day_sk_cache[[i]] <- data.frame(NA)
        day_st_cache[[i]] <- data.frame(NA)
        
        day_rhr_cache[[i]] <- data.frame(NA)
        
        day_highAct_gsr_cache[[i]] <- data.frame(NA)
        day_highAct_hr_cache[[i]] <- data.frame(NA)
        day_highAct_sk_cache[[i]] <- data.frame(NA)
        day_highAct_st_cache[[i]] <- data.frame(NA)
        
        day_lowAct_gsr_cache[[i]] <- data.frame(NA)
        day_lowAct_hr_cache[[i]] <- data.frame(NA)
        day_lowAct_sk_cache[[i]] <- data.frame(NA)
        day_lowAct_st_cache[[i]] <- data.frame(NA)
        
        night_gsr_cache[[i]] <- data.frame(NA)
        night_hr_cache[[i]] <- data.frame(NA)
        night_sk_cache[[i]] <- data.frame(NA)
        night_st_cache[[i]] <- data.frame(NA)
        
        night_rhr_cache[[i]] <- data.frame(NA)
        
        night_highAct_gsr_cache[[i]] <- data.frame(NA)
        night_highAct_hr_cache[[i]] <- data.frame(NA)
        night_highAct_sk_cache[[i]] <- data.frame(NA)
        night_highAct_st_cache[[i]] <- data.frame(NA)
        
        night_lowAct_gsr_cache[[i]] <- data.frame(NA)
        night_lowAct_hr_cache[[i]] <- data.frame(NA)
        night_lowAct_sk_cache[[i]] <- data.frame(NA)
        night_lowAct_st_cache[[i]] <- data.frame(NA)
        next
        
      } else {
        if(1==1){
          df$Steps[df$Steps==0] <- NA
          #ID steps decile locations
          StDecLocations <- quantile(
            df$Steps, na.rm=TRUE, 
            probs = seq(0.1,0.9,by=0.1))
          if(length(unique(StDecLocations)) != 9){
            check <- which(table(StDecLocations) > 1)
            for(u in 1:length(check)){
              check2 <- which(StDecLocations==names(check)[u])
              theMin <- as.numeric(min(check2))
              for(v in 1:length(check2)){
                if(check2[v] == theMin){
                  next
                } else {
                  StDecLocations[check2[v]] <- sum(
                    StDecLocations[check2[v]],0.0000000000001)
                }
              }
            }
          }
          df$StDecID <- findInterval(df$Steps,
                                     c(-Inf,StDecLocations, Inf))
        }
        #ID heart rate decile locations
        HrDecLocations <- quantile(
          df$Heart_Rate, na.rm=TRUE, 
          probs = seq(0.1,0.9, by=0.1))
        if(length(unique(HrDecLocations)) != 9){
          check <- which(table(HrDecLocations) > 1)
          for(u in 1:length(check)){
            check2 <- which(HrDecLocations==names(check)[u])
            theMin <- as.numeric(min(check2))
            for(v in 1:length(check2)){
              if(check2[v] == theMin){
                next
              } else {
                HrDecLocations[check2[v]] <- sum(
                  HrDecLocations[check2[v]],0.0000000000001)
              }
            }
          }
        }
        df$HrDecID <- findInterval(df$Heart_Rate,
                                   c(-Inf,HrDecLocations,Inf))
        #ID skin temperature decile locations
        SkDecLocations <- quantile(
          df$Skin_Temperature_F, na.rm=TRUE, 
          probs = seq(0.1,0.9,by=0.1))
        if(length(unique(SkDecLocations)) != 9){
          check <- which(table(SkDecLocations) > 1)
          for(u in 1:length(check)){
            check2 <- which(SkDecLocations==names(check)[u])
            theMin <- as.numeric(min(check2))
            for(v in 1:length(check2)){
              if(check2[v] == theMin){
                next
              } else {
                SkDecLocations[check2[v]] <- sum(
                  SkDecLocations[check2[v]],0.0000000000001)
              }
            }
          }
        }
        df$SkDecID <- findInterval(df$Skin_Temperature_F,
                                   c(-Inf,SkDecLocations, Inf))
        #ID gsr decile locations
        GsrDecLocations <- quantile(
          df$GSR, na.rm=TRUE, 
          probs = seq(0.1,0.9,by=0.1))
        if(length(unique(GsrDecLocations)) != 9){
          check <- which(table(GsrDecLocations) > 1)
          for(u in 1:length(check)){
            check2 <- which(GsrDecLocations==names(check)[u])
            theMin <- as.numeric(min(check2))
            for(v in 1:length(check2)){
              if(check2[v] == theMin){
                next
              } else {
                GsrDecLocations[check2[v]] <- sum(
                  GsrDecLocations[check2[v]],0.0000000000001)
              }
            }
          }
        }
        df$GsrDecID <- findInterval(df$GSR,
                                    c(-Inf,GsrDecLocations, Inf))
        
        #SUBSET RESTING HEART RATE PARAMETERS TO "rdf" table
        restingST <- ave(df$lowSteps, df$Wearable_Account_MD5,
                         FUN=function(x) rollapplyr(x, 10, sum, by=1, partial=TRUE))
        restingST[1:10-1] <- c(rep(NA,10-1)) #window-1
        
        #To record resting heart rate as rolling 10-minute mean when sum of steps in last 10 min < 10:
        # restingHR <- ave(df$Heart_Rate, df$Wearable_Account_MD5,
        #                  FUN=function(x) rollapplyr(x, 10, mean, na.rm=TRUE,
        #                                             by=1, partial=TRUE, fill=NA))
        # 
        # restingHR <- restingHR[which(restingST<10 & !is.na(restingST))]
        
        #To record resting HR as all values when sum of steps in last 10 min < 10:
        restingHR <- df$Heart_Rate[which(restingST<10 & !is.na(restingST))]
        
        rhr_cache[[i]] <- data.frame(describe(restingHR))
        
        df_day <- df[which(df$Hour>=8 & df$Hour<20),]
        
        restingST <- ave(df_day$lowSteps, df_day$Wearable_Account_MD5,
                         FUN=function(x) rollapplyr(x, 10, sum, by=1, partial=TRUE)) 
        restingST[1:10-1] <- c(rep(NA,10-1)) #window-1
        
        #To record resting heart rate as rolling 10-minute mean when sum of steps in last 10 min < 10:
        # restingHR <- ave(df$Heart_Rate, df$Wearable_Account_MD5,
        #                  FUN=function(x) rollapplyr(x, 10, mean, na.rm=TRUE,
        #                                             by=1, partial=TRUE, fill=NA))
        # 
        # restingHR <- restingHR[which(restingST<10 & !is.na(restingST))]
        
        #To record resting HR as all values when sum of steps in last 10 min < 10:
        restingHR <- df_day$Heart_Rate[which(restingST<10 & !is.na(restingST))]
        
        # restingHR <- restingHR[which(restingST<10 & !is.na(restingST))] 
        
        day_rhr_cache[[i]] <- data.frame(describe(restingHR))
        rm(df_day)
        
        df_night <- df[which(df$Hour>=20 | df$Hour<8),]
        
        restingST <- ave(df_night$lowSteps, df_night$Wearable_Account_MD5,
                         FUN=function(x) rollapplyr(x, 10, sum, by=1, partial=TRUE))
        restingST[1:10-1] <- c(rep(NA,10-1)) #window-1
        
        # restingHR <- ave(df_night$Heart_Rate, df_night$Wearable_Account_MD5,
        # FUN=function(x) rollapplyr(x, 10, mean, by=1, partial=TRUE)) 
        
        restingHR <- df_night$Heart_Rate[which(restingST<10 & !is.na(restingST))] 
        
        night_rhr_cache[[i]] <- data.frame(describe(restingHR))
        rm(df_night)
        ###
        
        #label high activity (defined as top 10% of Steps)
        if(1==1){
          df$highActivity <- ifelse(
            df$StDecID==10,
            TRUE,
            FALSE)
          #df$highActivity_eventNum <- rleid(df$highActivity)
        }
        
        durationDf <- df[df$highActivity == "TRUE",]
        durationDf$Date <- as.POSIXct(paste(durationDf$Timestamp_Local),
                                      format="%Y-%m-%d %H:%M:%S")
        
        #describe variables during high activity
        highAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        highAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        highAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        highAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        
        
        df_day <- df[which(df$Hour>=8 & df$Hour<20),]
        durationDf_day <- df_day[df_day$highActivity == "TRUE",]
        durationDf_day$Date <- as.POSIXct(paste(durationDf_day$Timestamp_Local),
                                          format="%Y-%m-%d %H:%M:%S")
        
        day_highAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        day_highAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        day_highAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        day_highAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        rm(df_day)
        
        df_night <- df[which(df$Hour>=20 | df$Hour<8),]
        durationDf_day <- df_night[df_night$highActivity == "TRUE",]
        durationDf_day$Date <- as.POSIXct(paste(durationDf_day$Timestamp_Local),
                                          format="%Y-%m-%d %H:%M:%S")
        
        night_highAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        night_highAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        night_highAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        night_highAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        rm(df_night)
        ###
        
        #label low activity (defined as lowest 10% of Steps)
        if(1==1){
          df$lowActivity <- ifelse(
            df$StDecID==1,
            TRUE,
            FALSE)
          #df$lowActivity_eventNum <- rleid(df$lowActivity)
        }
        
        durationDf <- df[df$lowActivity == "TRUE",]
        durationDf$Date <- as.POSIXct(paste(durationDf$Timestamp_Local),
                                      format="%Y-%m-%d %H:%M:%S")
        
        #describe variables during low activity
        lowAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        lowAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        lowAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        lowAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        
        
        df_day <- df[which(df$Hour>=8 & df$Hour<20),]
        durationDf <- df_day[df_day$lowActivity == "TRUE",]
        durationDf$Date <- as.POSIXct(paste(durationDf$Timestamp_Local),
                                      format="%Y-%m-%d %H:%M:%S")
        
        day_lowAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        day_lowAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        day_lowAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        day_lowAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        rm(df_day)
        
        df_night <- df[which(df$Hour>=20 | df$Hour<8),]
        durationDf <- df_night[df_night$lowActivity == "TRUE",]
        durationDf$Date <- as.POSIXct(paste(durationDf$Timestamp_Local),
                                      format="%Y-%m-%d %H:%M:%S")
        
        night_lowAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        night_lowAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        night_lowAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        night_lowAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        rm(df_night)
        ###
      }
    }
  } else {
    gsr_cache[[i]] <- data.frame(NA)
    hr_cache[[i]] <- data.frame(NA)
    sk_cache[[i]] <- data.frame(NA)
    st_cache[[i]] <- data.frame(NA)
    
    rhr_cache[[i]] <- data.frame(NA)
    
    highAct_gsr_cache[[i]] <- data.frame(NA)
    highAct_hr_cache[[i]] <- data.frame(NA)
    highAct_sk_cache[[i]] <- data.frame(NA)
    highAct_st_cache[[i]] <- data.frame(NA)
    
    lowAct_gsr_cache[[i]] <- data.frame(NA)
    lowAct_hr_cache[[i]] <- data.frame(NA)
    lowAct_sk_cache[[i]] <- data.frame(NA)
    lowAct_st_cache[[i]] <- data.frame(NA)
    
    day_gsr_cache[[i]] <- data.frame(NA)
    day_hr_cache[[i]] <- data.frame(NA)
    day_sk_cache[[i]] <- data.frame(NA)
    day_st_cache[[i]] <- data.frame(NA)
    
    day_rhr_cache[[i]] <- data.frame(NA)
    
    day_highAct_gsr_cache[[i]] <- data.frame(NA)
    day_highAct_hr_cache[[i]] <- data.frame(NA)
    day_highAct_sk_cache[[i]] <- data.frame(NA)
    day_highAct_st_cache[[i]] <- data.frame(NA)
    
    day_lowAct_gsr_cache[[i]] <- data.frame(NA)
    day_lowAct_hr_cache[[i]] <- data.frame(NA)
    day_lowAct_sk_cache[[i]] <- data.frame(NA)
    day_lowAct_st_cache[[i]] <- data.frame(NA)
    
    night_gsr_cache[[i]] <- data.frame(NA)
    night_hr_cache[[i]] <- data.frame(NA)
    night_sk_cache[[i]] <- data.frame(NA)
    night_st_cache[[i]] <- data.frame(NA)
    
    night_rhr_cache[[i]] <- data.frame(NA)
    
    night_highAct_gsr_cache[[i]] <- data.frame(NA)
    night_highAct_hr_cache[[i]] <- data.frame(NA)
    night_highAct_sk_cache[[i]] <- data.frame(NA)
    night_highAct_st_cache[[i]] <- data.frame(NA)
    
    night_lowAct_gsr_cache[[i]] <- data.frame(NA)
    night_lowAct_hr_cache[[i]] <- data.frame(NA)
    night_lowAct_sk_cache[[i]] <- data.frame(NA)
    night_lowAct_st_cache[[i]] <- data.frame(NA)
    
    print(i)
    next
  }
  print(i)
}

gsr_cache <- rbind.fill(gsr_cache)
hr_cache <- rbind.fill(hr_cache)
sk_cache <- rbind.fill(sk_cache)
st_cache <- rbind.fill(st_cache)

rhr_cache <- rbind.fill(rhr_cache)

highAct_gsr_cache <- rbind.fill(highAct_gsr_cache)
highAct_hr_cache <- rbind.fill(highAct_hr_cache)
highAct_sk_cache <- rbind.fill(highAct_sk_cache)
highAct_st_cache <- rbind.fill(highAct_st_cache)

lowAct_gsr_cache <- rbind.fill(lowAct_gsr_cache)
lowAct_hr_cache <- rbind.fill(lowAct_hr_cache)
lowAct_sk_cache <- rbind.fill(lowAct_sk_cache)
lowAct_st_cache <- rbind.fill(lowAct_st_cache)

day_gsr_cache <- rbind.fill(day_gsr_cache)
day_hr_cache <- rbind.fill(day_hr_cache)
day_sk_cache <- rbind.fill(day_sk_cache)
day_st_cache <- rbind.fill(day_st_cache)

day_rhr_cache <- rbind.fill(day_rhr_cache)

day_highAct_gsr_cache <- rbind.fill(day_highAct_gsr_cache)
day_highAct_hr_cache <- rbind.fill(day_highAct_hr_cache)
day_highAct_sk_cache <- rbind.fill(day_highAct_sk_cache)
day_highAct_st_cache <- rbind.fill(day_highAct_st_cache)

day_lowAct_gsr_cache <- rbind.fill(day_lowAct_gsr_cache)
day_lowAct_hr_cache <- rbind.fill(day_lowAct_hr_cache)
day_lowAct_sk_cache <- rbind.fill(day_lowAct_sk_cache)
day_lowAct_st_cache <- rbind.fill(day_lowAct_st_cache)

night_gsr_cache <- rbind.fill(night_gsr_cache)
night_hr_cache <- rbind.fill(night_hr_cache)
night_sk_cache <- rbind.fill(night_sk_cache)
night_st_cache <- rbind.fill(night_st_cache)

night_rhr_cache <- rbind.fill(night_rhr_cache)

night_highAct_gsr_cache <- rbind.fill(night_highAct_gsr_cache)
night_highAct_hr_cache <- rbind.fill(night_highAct_hr_cache)
night_highAct_sk_cache <- rbind.fill(night_highAct_sk_cache)
night_highAct_st_cache <- rbind.fill(night_highAct_st_cache)

night_lowAct_gsr_cache <- rbind.fill(night_lowAct_gsr_cache)
night_lowAct_hr_cache <- rbind.fill(night_lowAct_hr_cache)
night_lowAct_sk_cache <- rbind.fill(night_lowAct_sk_cache)
night_lowAct_st_cache <- rbind.fill(night_lowAct_st_cache)

gsr_cache <- gsr_cache[,-1] #first column was for NAs
hr_cache <- hr_cache[,-1] #first column was for NAs
sk_cache <- sk_cache[,-1] #first column was for NAs
st_cache <- st_cache[,-1] #first column was for NAs

rhr_cache <- rhr_cache[,-1] #first column was for NAs

highAct_gsr_cache <- highAct_gsr_cache[,-1] #first column was for NAs
highAct_hr_cache <- highAct_hr_cache[,-1] #first column was for NAs
highAct_sk_cache <- highAct_sk_cache[,-1] #first column was for NAs
highAct_st_cache <- highAct_st_cache[,-1] #first column was for NAs

lowAct_gsr_cache <- lowAct_gsr_cache[,-1] #first column was for NAs
lowAct_hr_cache <- lowAct_hr_cache[,-1] #first column was for NAs
lowAct_sk_cache <- lowAct_sk_cache[,-1] #first column was for NAs
lowAct_st_cache <- lowAct_st_cache[,-1] #first column was for NAs

day_gsr_cache <- day_gsr_cache[,-1] #first column was for NAs
day_hr_cache <- day_hr_cache[,-1] #first column was for NAs
day_sk_cache <- day_sk_cache[,-1] #first column was for NAs
day_st_cache <- day_st_cache[,-1] #first column was for NAs

day_rhr_cache <- day_rhr_cache[,-1] #first column was for NAs

day_highAct_gsr_cache <- day_highAct_gsr_cache[,-1] #first column was for NAs
day_highAct_hr_cache <- day_highAct_hr_cache[,-1] #first column was for NAs
day_highAct_sk_cache <- day_highAct_sk_cache[,-1] #first column was for NAs
day_highAct_st_cache <- day_highAct_st_cache[,-1] #first column was for NAs

day_lowAct_gsr_cache <- day_lowAct_gsr_cache[,-1] #first column was for NAs
day_lowAct_hr_cache <- day_lowAct_hr_cache[,-1] #first column was for NAs
day_lowAct_sk_cache <- day_lowAct_sk_cache[,-1] #first column was for NAs
day_lowAct_st_cache <- day_lowAct_st_cache[,-1] #first column was for NAs

night_gsr_cache <- night_gsr_cache[,-1] #first column was for NAs
night_hr_cache <- night_hr_cache[,-1] #first column was for NAs
night_sk_cache <- night_sk_cache[,-1] #first column was for NAs
night_st_cache <- night_st_cache[,-1] #first column was for NAs

night_rhr_cache <- night_rhr_cache[,-1] #first column was for NAs

night_highAct_gsr_cache <- night_highAct_gsr_cache[,-1] #first column was for NAs
night_highAct_hr_cache <- night_highAct_hr_cache[,-1] #first column was for NAs
night_highAct_sk_cache <- night_highAct_sk_cache[,-1] #first column was for NAs
night_highAct_st_cache <- night_highAct_st_cache[,-1] #first column was for NAs

night_lowAct_gsr_cache <- night_lowAct_gsr_cache[,-1] #first column was for NAs
night_lowAct_hr_cache <- night_lowAct_hr_cache[,-1] #first column was for NAs
night_lowAct_sk_cache <- night_lowAct_sk_cache[,-1] #first column was for NAs
night_lowAct_st_cache <- night_lowAct_st_cache[,-1] #first column was for NAs

colnames(gsr_cache) <- paste0("gsr_",colnames(gsr_cache))
colnames(hr_cache) <- paste0("hr_",colnames(hr_cache))
colnames(sk_cache) <- paste0("sk_",colnames(sk_cache))
colnames(st_cache) <- paste0("st_",colnames(st_cache))

colnames(rhr_cache) <- paste0("rhr_",colnames(rhr_cache))

colnames(highAct_gsr_cache) <- paste0("highAct_gsr_",colnames(highAct_gsr_cache))
colnames(highAct_hr_cache) <- paste0("highAct_hr_",colnames(highAct_hr_cache))
colnames(highAct_sk_cache) <- paste0("highAct_sk_",colnames(highAct_sk_cache))
colnames(highAct_st_cache) <- paste0("highAct_st_",colnames(highAct_st_cache))

colnames(lowAct_gsr_cache) <- paste0("lowAct_gsr_",colnames(lowAct_gsr_cache))
colnames(lowAct_hr_cache) <- paste0("lowAct_hr_",colnames(lowAct_hr_cache))
colnames(lowAct_sk_cache) <- paste0("lowAct_sk_",colnames(lowAct_sk_cache))
colnames(lowAct_st_cache) <- paste0("lowAct_st_",colnames(lowAct_st_cache))

colnames(day_gsr_cache) <- paste0("day_gsr_",colnames(day_gsr_cache))
colnames(day_hr_cache) <- paste0("day_hr_",colnames(day_hr_cache))
colnames(day_sk_cache) <- paste0("day_sk_",colnames(day_sk_cache))
colnames(day_st_cache) <- paste0("day_st_",colnames(day_st_cache))

colnames(day_rhr_cache) <- paste0("day_rhr_",colnames(day_rhr_cache))

colnames(day_highAct_gsr_cache) <- paste0("day_highAct_gsr_",
                                          colnames(day_highAct_gsr_cache))
colnames(day_highAct_hr_cache) <- paste0("day_highAct_hr_",
                                         colnames(day_highAct_hr_cache))
colnames(day_highAct_sk_cache) <- paste0("day_highAct_sk_",
                                         colnames(day_highAct_sk_cache))
colnames(day_highAct_st_cache) <- paste0("day_highAct_st_",
                                         colnames(day_highAct_st_cache))

colnames(day_lowAct_gsr_cache) <- paste0("day_lowAct_gsr_",
                                         colnames(day_lowAct_gsr_cache))
colnames(day_lowAct_hr_cache) <- paste0("day_lowAct_hr_",
                                        colnames(day_lowAct_hr_cache))
colnames(day_lowAct_sk_cache) <- paste0("day_lowAct_sk_",
                                        colnames(day_lowAct_sk_cache))
colnames(day_lowAct_st_cache) <- paste0("day_lowAct_st_",
                                        colnames(day_lowAct_st_cache))

colnames(night_gsr_cache) <- paste0("night_gsr_",colnames(night_gsr_cache))
colnames(night_hr_cache) <- paste0("night_hr_",colnames(night_hr_cache))
colnames(night_sk_cache) <- paste0("night_sk_",colnames(night_sk_cache))
colnames(night_st_cache) <- paste0("night_st_",colnames(night_st_cache))

colnames(night_rhr_cache) <- paste0("night_rhr_",colnames(night_rhr_cache))

colnames(night_highAct_gsr_cache) <- paste0("night_highAct_gsr_",
                                            colnames(night_highAct_gsr_cache))
colnames(night_highAct_hr_cache) <- paste0("night_highAct_hr_",
                                           colnames(night_highAct_hr_cache))
colnames(night_highAct_sk_cache) <- paste0("night_highAct_sk_",
                                           colnames(night_highAct_sk_cache))
colnames(night_highAct_st_cache) <- paste0("night_highAct_st_",
                                           colnames(night_highAct_st_cache))

colnames(night_lowAct_gsr_cache) <- paste0("night_lowAct_gsr_",
                                           colnames(night_lowAct_gsr_cache))
colnames(night_lowAct_hr_cache) <- paste0("night_lowAct_hr_",
                                          colnames(night_lowAct_hr_cache))
colnames(night_lowAct_sk_cache) <- paste0("night_lowAct_sk_",
                                          colnames(night_lowAct_sk_cache))
colnames(night_lowAct_st_cache) <- paste0("night_lowAct_st_",
                                          colnames(night_lowAct_st_cache))

stats <- cbind(
  ClinWithWear,gsr_cache,hr_cache,sk_cache,st_cache,
  rhr_cache,highAct_gsr_cache,highAct_hr_cache,
  highAct_sk_cache,highAct_st_cache,
  lowAct_gsr_cache,lowAct_hr_cache,
  lowAct_sk_cache,lowAct_st_cache,
  day_gsr_cache,day_hr_cache,day_sk_cache,day_st_cache,
  day_rhr_cache,day_highAct_gsr_cache,day_highAct_hr_cache,
  day_highAct_sk_cache,day_highAct_st_cache,
  day_lowAct_gsr_cache,day_lowAct_hr_cache,
  day_lowAct_sk_cache,day_lowAct_st_cache,
  night_gsr_cache,night_hr_cache,night_sk_cache,night_st_cache,
  night_rhr_cache,night_highAct_gsr_cache,night_highAct_hr_cache,
  night_highAct_sk_cache,night_highAct_st_cache,
  night_lowAct_gsr_cache,night_lowAct_hr_cache,
  night_lowAct_sk_cache,night_lowAct_st_cache)

write.csv(stats,
          paste0("F:\\PhysSubTables_20161111_Normal_20170802\\Basis2016_Clean_Norm_2WeekPrior_20180504.csv"),
          row.names=FALSE)


#### DESCRIBE MONTH-PRIOR DATA ####
files <- list.files("F:\\BasisSubsets\\FromBasisData_20161111_Normal_20170802\\MonthPrior_Files",
                    pattern="*.csv",full.names=TRUE)

gsr_cache <- list()
hr_cache <- list()
sk_cache <- list()
st_cache <- list()

rhr_cache <- list()

highAct_gsr_cache <- list()
highAct_hr_cache <- list()
highAct_sk_cache <- list()
highAct_st_cache <- list()

lowAct_gsr_cache <- list()
lowAct_hr_cache <- list()
lowAct_sk_cache <- list()
lowAct_st_cache <- list()


day_gsr_cache <- list()
day_hr_cache <- list()
day_sk_cache <- list()
day_st_cache <- list()

day_rhr_cache <- list()

day_highAct_gsr_cache <- list()
day_highAct_hr_cache <- list()
day_highAct_sk_cache <- list()
day_highAct_st_cache <- list()

day_lowAct_gsr_cache <- list()
day_lowAct_hr_cache <- list()
day_lowAct_sk_cache <- list()
day_lowAct_st_cache <- list()

night_gsr_cache <- list()
night_hr_cache <- list()
night_sk_cache <- list()
night_st_cache <- list()

night_rhr_cache <- list()

night_highAct_gsr_cache <- list()
night_highAct_hr_cache <- list()
night_highAct_sk_cache <- list()
night_highAct_st_cache <- list()

night_lowAct_gsr_cache <- list()
night_lowAct_hr_cache <- list()
night_lowAct_sk_cache <- list()
night_lowAct_st_cache <- list()
###

for(i in 1:length(files)){                       
  check <- file.info(files[i])$size
  if(check!=0){
    df <- read.csv(files[i],header=TRUE,sep=",")
    if("Wearable_Account_MD5" %in% names(df)){
      if(length(intersect(unique(df$Wearable_Account_MD5),rem))!=0){
        df <- df[!(df$Wearable_Account_MD5 %in% rem),]
      }
    } else {
      if(!("Wearable_Account_MD5" %in% names(df))){
        print(paste("MD5 not found in",i))
        print(names(df))
        next
      }
    }
    df$Date <- as.POSIXct(paste(df$Timestamp_Local),format="%Y-%m-%d %H:%M:%S")
    emptyCols <- apply(df,2,function(x) all(is.na(x)))
    if(TRUE %in% emptyCols){
      gsr_cache[[i]] <- data.frame(NA)
      hr_cache[[i]] <- data.frame(NA)
      sk_cache[[i]] <- data.frame(NA)
      st_cache[[i]] <- data.frame(NA)
      
      rhr_cache[[i]] <- data.frame(NA)
      
      highAct_gsr_cache[[i]] <- data.frame(NA)
      highAct_hr_cache[[i]] <- data.frame(NA)
      highAct_sk_cache[[i]] <- data.frame(NA)
      highAct_st_cache[[i]] <- data.frame(NA)
      
      lowAct_gsr_cache[[i]] <- data.frame(NA)
      lowAct_hr_cache[[i]] <- data.frame(NA)
      lowAct_sk_cache[[i]] <- data.frame(NA)
      lowAct_st_cache[[i]] <- data.frame(NA)
      
      day_gsr_cache[[i]] <- data.frame(NA)
      day_hr_cache[[i]] <- data.frame(NA)
      day_sk_cache[[i]] <- data.frame(NA)
      day_st_cache[[i]] <- data.frame(NA)
      
      day_rhr_cache[[i]] <- data.frame(NA)
      
      day_highAct_gsr_cache[[i]] <- data.frame(NA)
      day_highAct_hr_cache[[i]] <- data.frame(NA)
      day_highAct_sk_cache[[i]] <- data.frame(NA)
      day_highAct_st_cache[[i]] <- data.frame(NA)
      
      day_lowAct_gsr_cache[[i]] <- data.frame(NA)
      day_lowAct_hr_cache[[i]] <- data.frame(NA)
      day_lowAct_sk_cache[[i]] <- data.frame(NA)
      day_lowAct_st_cache[[i]] <- data.frame(NA)
      
      night_gsr_cache[[i]] <- data.frame(NA)
      night_hr_cache[[i]] <- data.frame(NA)
      night_sk_cache[[i]] <- data.frame(NA)
      night_st_cache[[i]] <- data.frame(NA)
      
      night_rhr_cache[[i]] <- data.frame(NA)
      
      night_highAct_gsr_cache[[i]] <- data.frame(NA)
      night_highAct_hr_cache[[i]] <- data.frame(NA)
      night_highAct_sk_cache[[i]] <- data.frame(NA)
      night_highAct_st_cache[[i]] <- data.frame(NA)
      
      night_lowAct_gsr_cache[[i]] <- data.frame(NA)
      night_lowAct_hr_cache[[i]] <- data.frame(NA)
      night_lowAct_sk_cache[[i]] <- data.frame(NA)
      night_lowAct_st_cache[[i]] <- data.frame(NA)
      next
    }
    if(NROW(df) < 1440){
      gsr_cache[[i]] <- data.frame(NA)
      hr_cache[[i]] <- data.frame(NA)
      sk_cache[[i]] <- data.frame(NA)
      st_cache[[i]] <- data.frame(NA)
      
      rhr_cache[[i]] <- data.frame(NA)
      
      highAct_gsr_cache[[i]] <- data.frame(NA)
      highAct_hr_cache[[i]] <- data.frame(NA)
      highAct_sk_cache[[i]] <- data.frame(NA)
      highAct_st_cache[[i]] <- data.frame(NA)
      
      lowAct_gsr_cache[[i]] <- data.frame(NA)
      lowAct_hr_cache[[i]] <- data.frame(NA)
      lowAct_sk_cache[[i]] <- data.frame(NA)
      lowAct_st_cache[[i]] <- data.frame(NA)
      
      day_gsr_cache[[i]] <- data.frame(NA)
      day_hr_cache[[i]] <- data.frame(NA)
      day_sk_cache[[i]] <- data.frame(NA)
      day_st_cache[[i]] <- data.frame(NA)
      
      day_rhr_cache[[i]] <- data.frame(NA)
      
      day_highAct_gsr_cache[[i]] <- data.frame(NA)
      day_highAct_hr_cache[[i]] <- data.frame(NA)
      day_highAct_sk_cache[[i]] <- data.frame(NA)
      day_highAct_st_cache[[i]] <- data.frame(NA)
      
      day_lowAct_gsr_cache[[i]] <- data.frame(NA)
      day_lowAct_hr_cache[[i]] <- data.frame(NA)
      day_lowAct_sk_cache[[i]] <- data.frame(NA)
      day_lowAct_st_cache[[i]] <- data.frame(NA)
      
      night_gsr_cache[[i]] <- data.frame(NA)
      night_hr_cache[[i]] <- data.frame(NA)
      night_sk_cache[[i]] <- data.frame(NA)
      night_st_cache[[i]] <- data.frame(NA)
      
      night_rhr_cache[[i]] <- data.frame(NA)
      
      night_highAct_gsr_cache[[i]] <- data.frame(NA)
      night_highAct_hr_cache[[i]] <- data.frame(NA)
      night_highAct_sk_cache[[i]] <- data.frame(NA)
      night_highAct_st_cache[[i]] <- data.frame(NA)
      
      night_lowAct_gsr_cache[[i]] <- data.frame(NA)
      night_lowAct_hr_cache[[i]] <- data.frame(NA)
      night_lowAct_sk_cache[[i]] <- data.frame(NA)
      night_lowAct_st_cache[[i]] <- data.frame(NA)
      next
    }
    gsr_cache[[i]] <- data.frame(describe(df$GSR))
    hr_cache[[i]] <- data.frame(describe(df$Heart_Rate))
    sk_cache[[i]] <- data.frame(describe(df$Skin_Temperature_F))
    st_cache[[i]] <- data.frame(describe(df$Steps))
    
    
    source("F:\\R_Commands\\LabelHours.R")
    label.hours(df, "Timestamp_Local")
    
    day_gsr_cache[[i]] <- data.frame(describe(
      df$GSR[which(df$Hour>=8 & df$Hour<20)]))
    day_hr_cache[[i]] <- data.frame(describe(
      df$Heart_Rate[which(df$Hour>=8 & df$Hour<20)]))
    day_sk_cache[[i]] <- data.frame(describe(
      df$Skin_Temperature_F[which(df$Hour>=8 & df$Hour<20)]))
    day_st_cache[[i]] <- data.frame(describe(
      df$Steps[which(df$Hour>=8 & df$Hour<20)]))
    
    night_gsr_cache[[i]] <- data.frame(describe(
      df$GSR[which(df$Hour>=20 | df$Hour<8)]))
    night_hr_cache[[i]] <- data.frame(describe(
      df$Heart_Rate[which(df$Hour>=20 | df$Hour<8)]))
    night_sk_cache[[i]] <- data.frame(describe(
      df$Skin_Temperature_F[which(df$Hour>=20 | df$Hour<8)]))
    night_st_cache[[i]] <- data.frame(describe(
      df$Steps[which(df$Hour>=20 | df$Hour<8)]))
    ###
    
    ## NEW
    
    if(1==1){
      df$lowSteps <- df$Steps
      df$Steps[df$Steps==0] <- NA
      if(all(is.na(df$Steps))){
        print(paste0("Error: No Steps for Candidate #",i))
        
        rhr_cache[[i]] <- data.frame(NA)
        
        highAct_gsr_cache[[i]] <- data.frame(NA)
        highAct_hr_cache[[i]] <- data.frame(NA)
        highAct_sk_cache[[i]] <- data.frame(NA)
        highAct_st_cache[[i]] <- data.frame(NA)
        
        lowAct_gsr_cache[[i]] <- data.frame(NA)
        lowAct_hr_cache[[i]] <- data.frame(NA)
        lowAct_sk_cache[[i]] <- data.frame(NA)
        lowAct_st_cache[[i]] <- data.frame(NA)
        
        day_gsr_cache[[i]] <- data.frame(NA)
        day_hr_cache[[i]] <- data.frame(NA)
        day_sk_cache[[i]] <- data.frame(NA)
        day_st_cache[[i]] <- data.frame(NA)
        
        day_rhr_cache[[i]] <- data.frame(NA)
        
        day_highAct_gsr_cache[[i]] <- data.frame(NA)
        day_highAct_hr_cache[[i]] <- data.frame(NA)
        day_highAct_sk_cache[[i]] <- data.frame(NA)
        day_highAct_st_cache[[i]] <- data.frame(NA)
        
        day_lowAct_gsr_cache[[i]] <- data.frame(NA)
        day_lowAct_hr_cache[[i]] <- data.frame(NA)
        day_lowAct_sk_cache[[i]] <- data.frame(NA)
        day_lowAct_st_cache[[i]] <- data.frame(NA)
        
        night_gsr_cache[[i]] <- data.frame(NA)
        night_hr_cache[[i]] <- data.frame(NA)
        night_sk_cache[[i]] <- data.frame(NA)
        night_st_cache[[i]] <- data.frame(NA)
        
        night_rhr_cache[[i]] <- data.frame(NA)
        
        night_highAct_gsr_cache[[i]] <- data.frame(NA)
        night_highAct_hr_cache[[i]] <- data.frame(NA)
        night_highAct_sk_cache[[i]] <- data.frame(NA)
        night_highAct_st_cache[[i]] <- data.frame(NA)
        
        night_lowAct_gsr_cache[[i]] <- data.frame(NA)
        night_lowAct_hr_cache[[i]] <- data.frame(NA)
        night_lowAct_sk_cache[[i]] <- data.frame(NA)
        night_lowAct_st_cache[[i]] <- data.frame(NA)
        next
        
      } else {
        if(1==1){
          df$Steps[df$Steps==0] <- NA
          #ID steps decile locations
          StDecLocations <- quantile(
            df$Steps, na.rm=TRUE, 
            probs = seq(0.1,0.9,by=0.1))
          if(length(unique(StDecLocations)) != 9){
            check <- which(table(StDecLocations) > 1)
            for(u in 1:length(check)){
              check2 <- which(StDecLocations==names(check)[u])
              theMin <- as.numeric(min(check2))
              for(v in 1:length(check2)){
                if(check2[v] == theMin){
                  next
                } else {
                  StDecLocations[check2[v]] <- sum(
                    StDecLocations[check2[v]],0.0000000000001)
                }
              }
            }
          }
          df$StDecID <- findInterval(df$Steps,
                                     c(-Inf,StDecLocations, Inf))
        }
        #ID heart rate decile locations
        HrDecLocations <- quantile(
          df$Heart_Rate, na.rm=TRUE, 
          probs = seq(0.1,0.9, by=0.1))
        if(length(unique(HrDecLocations)) != 9){
          check <- which(table(HrDecLocations) > 1)
          for(u in 1:length(check)){
            check2 <- which(HrDecLocations==names(check)[u])
            theMin <- as.numeric(min(check2))
            for(v in 1:length(check2)){
              if(check2[v] == theMin){
                next
              } else {
                HrDecLocations[check2[v]] <- sum(
                  HrDecLocations[check2[v]],0.0000000000001)
              }
            }
          }
        }
        df$HrDecID <- findInterval(df$Heart_Rate,
                                   c(-Inf,HrDecLocations,Inf))
        #ID skin temperature decile locations
        SkDecLocations <- quantile(
          df$Skin_Temperature_F, na.rm=TRUE, 
          probs = seq(0.1,0.9,by=0.1))
        if(length(unique(SkDecLocations)) != 9){
          check <- which(table(SkDecLocations) > 1)
          for(u in 1:length(check)){
            check2 <- which(SkDecLocations==names(check)[u])
            theMin <- as.numeric(min(check2))
            for(v in 1:length(check2)){
              if(check2[v] == theMin){
                next
              } else {
                SkDecLocations[check2[v]] <- sum(
                  SkDecLocations[check2[v]],0.0000000000001)
              }
            }
          }
        }
        df$SkDecID <- findInterval(df$Skin_Temperature_F,
                                   c(-Inf,SkDecLocations, Inf))
        #ID gsr decile locations
        GsrDecLocations <- quantile(
          df$GSR, na.rm=TRUE, 
          probs = seq(0.1,0.9,by=0.1))
        if(length(unique(GsrDecLocations)) != 9){
          check <- which(table(GsrDecLocations) > 1)
          for(u in 1:length(check)){
            check2 <- which(GsrDecLocations==names(check)[u])
            theMin <- as.numeric(min(check2))
            for(v in 1:length(check2)){
              if(check2[v] == theMin){
                next
              } else {
                GsrDecLocations[check2[v]] <- sum(
                  GsrDecLocations[check2[v]],0.0000000000001)
              }
            }
          }
        }
        df$GsrDecID <- findInterval(df$GSR,
                                    c(-Inf,GsrDecLocations, Inf))
        
        #SUBSET RESTING HEART RATE PARAMETERS TO "rdf" table
        restingST <- ave(df$lowSteps, df$Wearable_Account_MD5,
                         FUN=function(x) rollapplyr(x, 10, sum, by=1, partial=TRUE))
        restingST[1:10-1] <- c(rep(NA,10-1)) #window-1
        
        #To record resting heart rate as rolling 10-minute mean when sum of steps in last 10 min < 10:
        # restingHR <- ave(df$Heart_Rate, df$Wearable_Account_MD5,
        #                  FUN=function(x) rollapplyr(x, 10, mean, na.rm=TRUE,
        #                                             by=1, partial=TRUE, fill=NA))
        # 
        # restingHR <- restingHR[which(restingST<10 & !is.na(restingST))]
        
        #To record resting HR as all values when sum of steps in last 10 min < 10:
        restingHR <- df$Heart_Rate[which(restingST<10 & !is.na(restingST))]
        
        rhr_cache[[i]] <- data.frame(describe(restingHR))
        
        df_day <- df[which(df$Hour>=8 & df$Hour<20),]
        
        restingST <- ave(df_day$lowSteps, df_day$Wearable_Account_MD5,
                         FUN=function(x) rollapplyr(x, 10, sum, by=1, partial=TRUE)) 
        restingST[1:10-1] <- c(rep(NA,10-1)) #window-1
        
        #To record resting heart rate as rolling 10-minute mean when sum of steps in last 10 min < 10:
        # restingHR <- ave(df$Heart_Rate, df$Wearable_Account_MD5,
        #                  FUN=function(x) rollapplyr(x, 10, mean, na.rm=TRUE,
        #                                             by=1, partial=TRUE, fill=NA))
        # 
        # restingHR <- restingHR[which(restingST<10 & !is.na(restingST))]
        
        #To record resting HR as all values when sum of steps in last 10 min < 10:
        restingHR <- df_day$Heart_Rate[which(restingST<10 & !is.na(restingST))]
        
        # restingHR <- restingHR[which(restingST<10 & !is.na(restingST))] 
        
        day_rhr_cache[[i]] <- data.frame(describe(restingHR))
        rm(df_day)
        
        df_night <- df[which(df$Hour>=20 | df$Hour<8),]
        
        restingST <- ave(df_night$lowSteps, df_night$Wearable_Account_MD5,
                         FUN=function(x) rollapplyr(x, 10, sum, by=1, partial=TRUE))
        restingST[1:10-1] <- c(rep(NA,10-1)) #window-1
        
        # restingHR <- ave(df_night$Heart_Rate, df_night$Wearable_Account_MD5,
        # FUN=function(x) rollapplyr(x, 10, mean, by=1, partial=TRUE)) 
        
        restingHR <- df_night$Heart_Rate[which(restingST<10 & !is.na(restingST))] 
        
        night_rhr_cache[[i]] <- data.frame(describe(restingHR))
        rm(df_night)
        ###
        
        #label high activity (defined as top 10% of Steps)
        if(1==1){
          df$highActivity <- ifelse(
            df$StDecID==10,
            TRUE,
            FALSE)
          #df$highActivity_eventNum <- rleid(df$highActivity)
        }
        
        durationDf <- df[df$highActivity == "TRUE",]
        durationDf$Date <- as.POSIXct(paste(durationDf$Timestamp_Local),
                                      format="%Y-%m-%d %H:%M:%S")
        
        #describe variables during high activity
        highAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        highAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        highAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        highAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        
        
        df_day <- df[which(df$Hour>=8 & df$Hour<20),]
        durationDf_day <- df_day[df_day$highActivity == "TRUE",]
        durationDf_day$Date <- as.POSIXct(paste(durationDf_day$Timestamp_Local),
                                          format="%Y-%m-%d %H:%M:%S")
        
        day_highAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        day_highAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        day_highAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        day_highAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        rm(df_day)
        
        df_night <- df[which(df$Hour>=20 | df$Hour<8),]
        durationDf_day <- df_night[df_night$highActivity == "TRUE",]
        durationDf_day$Date <- as.POSIXct(paste(durationDf_day$Timestamp_Local),
                                          format="%Y-%m-%d %H:%M:%S")
        
        night_highAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        night_highAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        night_highAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        night_highAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        rm(df_night)
        ###
        
        #label low activity (defined as lowest 10% of Steps)
        if(1==1){
          df$lowActivity <- ifelse(
            df$StDecID==1,
            TRUE,
            FALSE)
          #df$lowActivity_eventNum <- rleid(df$lowActivity)
        }
        
        durationDf <- df[df$lowActivity == "TRUE",]
        durationDf$Date <- as.POSIXct(paste(durationDf$Timestamp_Local),
                                      format="%Y-%m-%d %H:%M:%S")
        
        #describe variables during low activity
        lowAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        lowAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        lowAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        lowAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        
        
        df_day <- df[which(df$Hour>=8 & df$Hour<20),]
        durationDf <- df_day[df_day$lowActivity == "TRUE",]
        durationDf$Date <- as.POSIXct(paste(durationDf$Timestamp_Local),
                                      format="%Y-%m-%d %H:%M:%S")
        
        day_lowAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        day_lowAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        day_lowAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        day_lowAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        rm(df_day)
        
        df_night <- df[which(df$Hour>=20 | df$Hour<8),]
        durationDf <- df_night[df_night$lowActivity == "TRUE",]
        durationDf$Date <- as.POSIXct(paste(durationDf$Timestamp_Local),
                                      format="%Y-%m-%d %H:%M:%S")
        
        night_lowAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        night_lowAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        night_lowAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        night_lowAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        rm(df_night)
        ###
      }
    }
  } else {
    gsr_cache[[i]] <- data.frame(NA)
    hr_cache[[i]] <- data.frame(NA)
    sk_cache[[i]] <- data.frame(NA)
    st_cache[[i]] <- data.frame(NA)
    
    rhr_cache[[i]] <- data.frame(NA)
    
    highAct_gsr_cache[[i]] <- data.frame(NA)
    highAct_hr_cache[[i]] <- data.frame(NA)
    highAct_sk_cache[[i]] <- data.frame(NA)
    highAct_st_cache[[i]] <- data.frame(NA)
    
    lowAct_gsr_cache[[i]] <- data.frame(NA)
    lowAct_hr_cache[[i]] <- data.frame(NA)
    lowAct_sk_cache[[i]] <- data.frame(NA)
    lowAct_st_cache[[i]] <- data.frame(NA)
    
    day_gsr_cache[[i]] <- data.frame(NA)
    day_hr_cache[[i]] <- data.frame(NA)
    day_sk_cache[[i]] <- data.frame(NA)
    day_st_cache[[i]] <- data.frame(NA)
    
    day_rhr_cache[[i]] <- data.frame(NA)
    
    day_highAct_gsr_cache[[i]] <- data.frame(NA)
    day_highAct_hr_cache[[i]] <- data.frame(NA)
    day_highAct_sk_cache[[i]] <- data.frame(NA)
    day_highAct_st_cache[[i]] <- data.frame(NA)
    
    day_lowAct_gsr_cache[[i]] <- data.frame(NA)
    day_lowAct_hr_cache[[i]] <- data.frame(NA)
    day_lowAct_sk_cache[[i]] <- data.frame(NA)
    day_lowAct_st_cache[[i]] <- data.frame(NA)
    
    night_gsr_cache[[i]] <- data.frame(NA)
    night_hr_cache[[i]] <- data.frame(NA)
    night_sk_cache[[i]] <- data.frame(NA)
    night_st_cache[[i]] <- data.frame(NA)
    
    night_rhr_cache[[i]] <- data.frame(NA)
    
    night_highAct_gsr_cache[[i]] <- data.frame(NA)
    night_highAct_hr_cache[[i]] <- data.frame(NA)
    night_highAct_sk_cache[[i]] <- data.frame(NA)
    night_highAct_st_cache[[i]] <- data.frame(NA)
    
    night_lowAct_gsr_cache[[i]] <- data.frame(NA)
    night_lowAct_hr_cache[[i]] <- data.frame(NA)
    night_lowAct_sk_cache[[i]] <- data.frame(NA)
    night_lowAct_st_cache[[i]] <- data.frame(NA)
    
    print(i)
    next
  }
  print(i)
}

gsr_cache <- rbind.fill(gsr_cache)
hr_cache <- rbind.fill(hr_cache)
sk_cache <- rbind.fill(sk_cache)
st_cache <- rbind.fill(st_cache)

rhr_cache <- rbind.fill(rhr_cache)

highAct_gsr_cache <- rbind.fill(highAct_gsr_cache)
highAct_hr_cache <- rbind.fill(highAct_hr_cache)
highAct_sk_cache <- rbind.fill(highAct_sk_cache)
highAct_st_cache <- rbind.fill(highAct_st_cache)

lowAct_gsr_cache <- rbind.fill(lowAct_gsr_cache)
lowAct_hr_cache <- rbind.fill(lowAct_hr_cache)
lowAct_sk_cache <- rbind.fill(lowAct_sk_cache)
lowAct_st_cache <- rbind.fill(lowAct_st_cache)

day_gsr_cache <- rbind.fill(day_gsr_cache)
day_hr_cache <- rbind.fill(day_hr_cache)
day_sk_cache <- rbind.fill(day_sk_cache)
day_st_cache <- rbind.fill(day_st_cache)

day_rhr_cache <- rbind.fill(day_rhr_cache)

day_highAct_gsr_cache <- rbind.fill(day_highAct_gsr_cache)
day_highAct_hr_cache <- rbind.fill(day_highAct_hr_cache)
day_highAct_sk_cache <- rbind.fill(day_highAct_sk_cache)
day_highAct_st_cache <- rbind.fill(day_highAct_st_cache)

day_lowAct_gsr_cache <- rbind.fill(day_lowAct_gsr_cache)
day_lowAct_hr_cache <- rbind.fill(day_lowAct_hr_cache)
day_lowAct_sk_cache <- rbind.fill(day_lowAct_sk_cache)
day_lowAct_st_cache <- rbind.fill(day_lowAct_st_cache)

night_gsr_cache <- rbind.fill(night_gsr_cache)
night_hr_cache <- rbind.fill(night_hr_cache)
night_sk_cache <- rbind.fill(night_sk_cache)
night_st_cache <- rbind.fill(night_st_cache)

night_rhr_cache <- rbind.fill(night_rhr_cache)

night_highAct_gsr_cache <- rbind.fill(night_highAct_gsr_cache)
night_highAct_hr_cache <- rbind.fill(night_highAct_hr_cache)
night_highAct_sk_cache <- rbind.fill(night_highAct_sk_cache)
night_highAct_st_cache <- rbind.fill(night_highAct_st_cache)

night_lowAct_gsr_cache <- rbind.fill(night_lowAct_gsr_cache)
night_lowAct_hr_cache <- rbind.fill(night_lowAct_hr_cache)
night_lowAct_sk_cache <- rbind.fill(night_lowAct_sk_cache)
night_lowAct_st_cache <- rbind.fill(night_lowAct_st_cache)

gsr_cache <- gsr_cache[,-1] #first column was for NAs
hr_cache <- hr_cache[,-1] #first column was for NAs
sk_cache <- sk_cache[,-1] #first column was for NAs
st_cache <- st_cache[,-1] #first column was for NAs

rhr_cache <- rhr_cache[,-1] #first column was for NAs

highAct_gsr_cache <- highAct_gsr_cache[,-1] #first column was for NAs
highAct_hr_cache <- highAct_hr_cache[,-1] #first column was for NAs
highAct_sk_cache <- highAct_sk_cache[,-1] #first column was for NAs
highAct_st_cache <- highAct_st_cache[,-1] #first column was for NAs

lowAct_gsr_cache <- lowAct_gsr_cache[,-1] #first column was for NAs
lowAct_hr_cache <- lowAct_hr_cache[,-1] #first column was for NAs
lowAct_sk_cache <- lowAct_sk_cache[,-1] #first column was for NAs
lowAct_st_cache <- lowAct_st_cache[,-1] #first column was for NAs

day_gsr_cache <- day_gsr_cache[,-1] #first column was for NAs
day_hr_cache <- day_hr_cache[,-1] #first column was for NAs
day_sk_cache <- day_sk_cache[,-1] #first column was for NAs
day_st_cache <- day_st_cache[,-1] #first column was for NAs

day_rhr_cache <- day_rhr_cache[,-1] #first column was for NAs

day_highAct_gsr_cache <- day_highAct_gsr_cache[,-1] #first column was for NAs
day_highAct_hr_cache <- day_highAct_hr_cache[,-1] #first column was for NAs
day_highAct_sk_cache <- day_highAct_sk_cache[,-1] #first column was for NAs
day_highAct_st_cache <- day_highAct_st_cache[,-1] #first column was for NAs

day_lowAct_gsr_cache <- day_lowAct_gsr_cache[,-1] #first column was for NAs
day_lowAct_hr_cache <- day_lowAct_hr_cache[,-1] #first column was for NAs
day_lowAct_sk_cache <- day_lowAct_sk_cache[,-1] #first column was for NAs
day_lowAct_st_cache <- day_lowAct_st_cache[,-1] #first column was for NAs

night_gsr_cache <- night_gsr_cache[,-1] #first column was for NAs
night_hr_cache <- night_hr_cache[,-1] #first column was for NAs
night_sk_cache <- night_sk_cache[,-1] #first column was for NAs
night_st_cache <- night_st_cache[,-1] #first column was for NAs

night_rhr_cache <- night_rhr_cache[,-1] #first column was for NAs

night_highAct_gsr_cache <- night_highAct_gsr_cache[,-1] #first column was for NAs
night_highAct_hr_cache <- night_highAct_hr_cache[,-1] #first column was for NAs
night_highAct_sk_cache <- night_highAct_sk_cache[,-1] #first column was for NAs
night_highAct_st_cache <- night_highAct_st_cache[,-1] #first column was for NAs

night_lowAct_gsr_cache <- night_lowAct_gsr_cache[,-1] #first column was for NAs
night_lowAct_hr_cache <- night_lowAct_hr_cache[,-1] #first column was for NAs
night_lowAct_sk_cache <- night_lowAct_sk_cache[,-1] #first column was for NAs
night_lowAct_st_cache <- night_lowAct_st_cache[,-1] #first column was for NAs

colnames(gsr_cache) <- paste0("gsr_",colnames(gsr_cache))
colnames(hr_cache) <- paste0("hr_",colnames(hr_cache))
colnames(sk_cache) <- paste0("sk_",colnames(sk_cache))
colnames(st_cache) <- paste0("st_",colnames(st_cache))

colnames(rhr_cache) <- paste0("rhr_",colnames(rhr_cache))

colnames(highAct_gsr_cache) <- paste0("highAct_gsr_",colnames(highAct_gsr_cache))
colnames(highAct_hr_cache) <- paste0("highAct_hr_",colnames(highAct_hr_cache))
colnames(highAct_sk_cache) <- paste0("highAct_sk_",colnames(highAct_sk_cache))
colnames(highAct_st_cache) <- paste0("highAct_st_",colnames(highAct_st_cache))

colnames(lowAct_gsr_cache) <- paste0("lowAct_gsr_",colnames(lowAct_gsr_cache))
colnames(lowAct_hr_cache) <- paste0("lowAct_hr_",colnames(lowAct_hr_cache))
colnames(lowAct_sk_cache) <- paste0("lowAct_sk_",colnames(lowAct_sk_cache))
colnames(lowAct_st_cache) <- paste0("lowAct_st_",colnames(lowAct_st_cache))

colnames(day_gsr_cache) <- paste0("day_gsr_",colnames(day_gsr_cache))
colnames(day_hr_cache) <- paste0("day_hr_",colnames(day_hr_cache))
colnames(day_sk_cache) <- paste0("day_sk_",colnames(day_sk_cache))
colnames(day_st_cache) <- paste0("day_st_",colnames(day_st_cache))

colnames(day_rhr_cache) <- paste0("day_rhr_",colnames(day_rhr_cache))

colnames(day_highAct_gsr_cache) <- paste0("day_highAct_gsr_",
                                          colnames(day_highAct_gsr_cache))
colnames(day_highAct_hr_cache) <- paste0("day_highAct_hr_",
                                         colnames(day_highAct_hr_cache))
colnames(day_highAct_sk_cache) <- paste0("day_highAct_sk_",
                                         colnames(day_highAct_sk_cache))
colnames(day_highAct_st_cache) <- paste0("day_highAct_st_",
                                         colnames(day_highAct_st_cache))

colnames(day_lowAct_gsr_cache) <- paste0("day_lowAct_gsr_",
                                         colnames(day_lowAct_gsr_cache))
colnames(day_lowAct_hr_cache) <- paste0("day_lowAct_hr_",
                                        colnames(day_lowAct_hr_cache))
colnames(day_lowAct_sk_cache) <- paste0("day_lowAct_sk_",
                                        colnames(day_lowAct_sk_cache))
colnames(day_lowAct_st_cache) <- paste0("day_lowAct_st_",
                                        colnames(day_lowAct_st_cache))

colnames(night_gsr_cache) <- paste0("night_gsr_",colnames(night_gsr_cache))
colnames(night_hr_cache) <- paste0("night_hr_",colnames(night_hr_cache))
colnames(night_sk_cache) <- paste0("night_sk_",colnames(night_sk_cache))
colnames(night_st_cache) <- paste0("night_st_",colnames(night_st_cache))

colnames(night_rhr_cache) <- paste0("night_rhr_",colnames(night_rhr_cache))

colnames(night_highAct_gsr_cache) <- paste0("night_highAct_gsr_",
                                            colnames(night_highAct_gsr_cache))
colnames(night_highAct_hr_cache) <- paste0("night_highAct_hr_",
                                           colnames(night_highAct_hr_cache))
colnames(night_highAct_sk_cache) <- paste0("night_highAct_sk_",
                                           colnames(night_highAct_sk_cache))
colnames(night_highAct_st_cache) <- paste0("night_highAct_st_",
                                           colnames(night_highAct_st_cache))

colnames(night_lowAct_gsr_cache) <- paste0("night_lowAct_gsr_",
                                           colnames(night_lowAct_gsr_cache))
colnames(night_lowAct_hr_cache) <- paste0("night_lowAct_hr_",
                                          colnames(night_lowAct_hr_cache))
colnames(night_lowAct_sk_cache) <- paste0("night_lowAct_sk_",
                                          colnames(night_lowAct_sk_cache))
colnames(night_lowAct_st_cache) <- paste0("night_lowAct_st_",
                                          colnames(night_lowAct_st_cache))

stats <- cbind(
  ClinWithWear,gsr_cache,hr_cache,sk_cache,st_cache,
  rhr_cache,highAct_gsr_cache,highAct_hr_cache,
  highAct_sk_cache,highAct_st_cache,
  lowAct_gsr_cache,lowAct_hr_cache,
  lowAct_sk_cache,lowAct_st_cache,
  day_gsr_cache,day_hr_cache,day_sk_cache,day_st_cache,
  day_rhr_cache,day_highAct_gsr_cache,day_highAct_hr_cache,
  day_highAct_sk_cache,day_highAct_st_cache,
  day_lowAct_gsr_cache,day_lowAct_hr_cache,
  day_lowAct_sk_cache,day_lowAct_st_cache,
  night_gsr_cache,night_hr_cache,night_sk_cache,night_st_cache,
  night_rhr_cache,night_highAct_gsr_cache,night_highAct_hr_cache,
  night_highAct_sk_cache,night_highAct_st_cache,
  night_lowAct_gsr_cache,night_lowAct_hr_cache,
  night_lowAct_sk_cache,night_lowAct_st_cache)

write.csv(stats,
          paste0("F:\\PhysSubTables_20161111_Normal_20170802\\Basis2016_Clean_Norm_MonthPrior_20180504.csv"),
          row.names=FALSE)


#### DESCRIBE ALL DATA ####
files <- list.files("F:\\BasisSubsets\\FromBasisData_20161111_Normal_20170802\\AllData_Files",
                    pattern="*.csv", full.names=TRUE)

gsr_cache <- list()
hr_cache <- list()
sk_cache <- list()
st_cache <- list()

rhr_cache <- list()

highAct_gsr_cache <- list()
highAct_hr_cache <- list()
highAct_sk_cache <- list()
highAct_st_cache <- list()

lowAct_gsr_cache <- list()
lowAct_hr_cache <- list()
lowAct_sk_cache <- list()
lowAct_st_cache <- list()


day_gsr_cache <- list()
day_hr_cache <- list()
day_sk_cache <- list()
day_st_cache <- list()

day_rhr_cache <- list()

day_highAct_gsr_cache <- list()
day_highAct_hr_cache <- list()
day_highAct_sk_cache <- list()
day_highAct_st_cache <- list()

day_lowAct_gsr_cache <- list()
day_lowAct_hr_cache <- list()
day_lowAct_sk_cache <- list()
day_lowAct_st_cache <- list()

night_gsr_cache <- list()
night_hr_cache <- list()
night_sk_cache <- list()
night_st_cache <- list()

night_rhr_cache <- list()

night_highAct_gsr_cache <- list()
night_highAct_hr_cache <- list()
night_highAct_sk_cache <- list()
night_highAct_st_cache <- list()

night_lowAct_gsr_cache <- list()
night_lowAct_hr_cache <- list()
night_lowAct_sk_cache <- list()
night_lowAct_st_cache <- list()
###

for(i in 1:length(files)){                       
  check <- file.info(files[i])$size
  if(check!=0){
    df <- fread(files[i],header=TRUE,sep=",",stringsAsFactors = FALSE)
    df <- data.frame(df)
    if("Wearable_Account_MD5" %in% names(df)){
      if(length(intersect(unique(df$Wearable_Account_MD5),rem))!=0){
        df <- df[!(df$Wearable_Account_MD5 %in% rem),]
      }
    } else {
      if(!("Wearable_Account_MD5" %in% names(df))){
        print(paste("MD5 not found in",i))
        print(names(df))
        next
      }
    }
    df$Date <- as.POSIXct(paste(df$Timestamp_Local),format="%Y-%m-%d %H:%M:%S")
    emptyCols <- apply(df,2,function(x) all(is.na(x)))
    if(TRUE %in% emptyCols){
      gsr_cache[[i]] <- data.frame(NA)
      hr_cache[[i]] <- data.frame(NA)
      sk_cache[[i]] <- data.frame(NA)
      st_cache[[i]] <- data.frame(NA)
      
      rhr_cache[[i]] <- data.frame(NA)
      
      highAct_gsr_cache[[i]] <- data.frame(NA)
      highAct_hr_cache[[i]] <- data.frame(NA)
      highAct_sk_cache[[i]] <- data.frame(NA)
      highAct_st_cache[[i]] <- data.frame(NA)
      
      lowAct_gsr_cache[[i]] <- data.frame(NA)
      lowAct_hr_cache[[i]] <- data.frame(NA)
      lowAct_sk_cache[[i]] <- data.frame(NA)
      lowAct_st_cache[[i]] <- data.frame(NA)
      
      day_gsr_cache[[i]] <- data.frame(NA)
      day_hr_cache[[i]] <- data.frame(NA)
      day_sk_cache[[i]] <- data.frame(NA)
      day_st_cache[[i]] <- data.frame(NA)
      
      day_rhr_cache[[i]] <- data.frame(NA)
      
      day_highAct_gsr_cache[[i]] <- data.frame(NA)
      day_highAct_hr_cache[[i]] <- data.frame(NA)
      day_highAct_sk_cache[[i]] <- data.frame(NA)
      day_highAct_st_cache[[i]] <- data.frame(NA)
      
      day_lowAct_gsr_cache[[i]] <- data.frame(NA)
      day_lowAct_hr_cache[[i]] <- data.frame(NA)
      day_lowAct_sk_cache[[i]] <- data.frame(NA)
      day_lowAct_st_cache[[i]] <- data.frame(NA)
      
      night_gsr_cache[[i]] <- data.frame(NA)
      night_hr_cache[[i]] <- data.frame(NA)
      night_sk_cache[[i]] <- data.frame(NA)
      night_st_cache[[i]] <- data.frame(NA)
      
      night_rhr_cache[[i]] <- data.frame(NA)
      
      night_highAct_gsr_cache[[i]] <- data.frame(NA)
      night_highAct_hr_cache[[i]] <- data.frame(NA)
      night_highAct_sk_cache[[i]] <- data.frame(NA)
      night_highAct_st_cache[[i]] <- data.frame(NA)
      
      night_lowAct_gsr_cache[[i]] <- data.frame(NA)
      night_lowAct_hr_cache[[i]] <- data.frame(NA)
      night_lowAct_sk_cache[[i]] <- data.frame(NA)
      night_lowAct_st_cache[[i]] <- data.frame(NA)
      next
    }
    if(NROW(df) < 1440){
      gsr_cache[[i]] <- data.frame(NA)
      hr_cache[[i]] <- data.frame(NA)
      sk_cache[[i]] <- data.frame(NA)
      st_cache[[i]] <- data.frame(NA)
      
      rhr_cache[[i]] <- data.frame(NA)
      
      highAct_gsr_cache[[i]] <- data.frame(NA)
      highAct_hr_cache[[i]] <- data.frame(NA)
      highAct_sk_cache[[i]] <- data.frame(NA)
      highAct_st_cache[[i]] <- data.frame(NA)
      
      lowAct_gsr_cache[[i]] <- data.frame(NA)
      lowAct_hr_cache[[i]] <- data.frame(NA)
      lowAct_sk_cache[[i]] <- data.frame(NA)
      lowAct_st_cache[[i]] <- data.frame(NA)
      
      day_gsr_cache[[i]] <- data.frame(NA)
      day_hr_cache[[i]] <- data.frame(NA)
      day_sk_cache[[i]] <- data.frame(NA)
      day_st_cache[[i]] <- data.frame(NA)
      
      day_rhr_cache[[i]] <- data.frame(NA)
      
      day_highAct_gsr_cache[[i]] <- data.frame(NA)
      day_highAct_hr_cache[[i]] <- data.frame(NA)
      day_highAct_sk_cache[[i]] <- data.frame(NA)
      day_highAct_st_cache[[i]] <- data.frame(NA)
      
      day_lowAct_gsr_cache[[i]] <- data.frame(NA)
      day_lowAct_hr_cache[[i]] <- data.frame(NA)
      day_lowAct_sk_cache[[i]] <- data.frame(NA)
      day_lowAct_st_cache[[i]] <- data.frame(NA)
      
      night_gsr_cache[[i]] <- data.frame(NA)
      night_hr_cache[[i]] <- data.frame(NA)
      night_sk_cache[[i]] <- data.frame(NA)
      night_st_cache[[i]] <- data.frame(NA)
      
      night_rhr_cache[[i]] <- data.frame(NA)
      
      night_highAct_gsr_cache[[i]] <- data.frame(NA)
      night_highAct_hr_cache[[i]] <- data.frame(NA)
      night_highAct_sk_cache[[i]] <- data.frame(NA)
      night_highAct_st_cache[[i]] <- data.frame(NA)
      
      night_lowAct_gsr_cache[[i]] <- data.frame(NA)
      night_lowAct_hr_cache[[i]] <- data.frame(NA)
      night_lowAct_sk_cache[[i]] <- data.frame(NA)
      night_lowAct_st_cache[[i]] <- data.frame(NA)
      next
    }
    gsr_cache[[i]] <- data.frame(describe(df$GSR))
    hr_cache[[i]] <- data.frame(describe(df$Heart_Rate))
    sk_cache[[i]] <- data.frame(describe(df$Skin_Temperature_F))
    st_cache[[i]] <- data.frame(describe(df$Steps))
    
    
    source("F:\\R_Commands\\LabelHours.R")
    label.hours(df, "Timestamp_Local")
    
    day_gsr_cache[[i]] <- data.frame(describe(
      df$GSR[which(df$Hour>=8 & df$Hour<20)]))
    day_hr_cache[[i]] <- data.frame(describe(
      df$Heart_Rate[which(df$Hour>=8 & df$Hour<20)]))
    day_sk_cache[[i]] <- data.frame(describe(
      df$Skin_Temperature_F[which(df$Hour>=8 & df$Hour<20)]))
    day_st_cache[[i]] <- data.frame(describe(
      df$Steps[which(df$Hour>=8 & df$Hour<20)]))
    
    night_gsr_cache[[i]] <- data.frame(describe(
      df$GSR[which(df$Hour>=20 | df$Hour<8)]))
    night_hr_cache[[i]] <- data.frame(describe(
      df$Heart_Rate[which(df$Hour>=20 | df$Hour<8)]))
    night_sk_cache[[i]] <- data.frame(describe(
      df$Skin_Temperature_F[which(df$Hour>=20 | df$Hour<8)]))
    night_st_cache[[i]] <- data.frame(describe(
      df$Steps[which(df$Hour>=20 | df$Hour<8)]))
    ###
    
    ## NEW
    
    if(1==1){
      df$lowSteps <- df$Steps
      df$Steps[df$Steps==0] <- NA
      if(all(is.na(df$Steps))){
        print(paste0("Error: No Steps for Candidate #",i))
        
        rhr_cache[[i]] <- data.frame(NA)
        
        highAct_gsr_cache[[i]] <- data.frame(NA)
        highAct_hr_cache[[i]] <- data.frame(NA)
        highAct_sk_cache[[i]] <- data.frame(NA)
        highAct_st_cache[[i]] <- data.frame(NA)
        
        lowAct_gsr_cache[[i]] <- data.frame(NA)
        lowAct_hr_cache[[i]] <- data.frame(NA)
        lowAct_sk_cache[[i]] <- data.frame(NA)
        lowAct_st_cache[[i]] <- data.frame(NA)
        
        day_gsr_cache[[i]] <- data.frame(NA)
        day_hr_cache[[i]] <- data.frame(NA)
        day_sk_cache[[i]] <- data.frame(NA)
        day_st_cache[[i]] <- data.frame(NA)
        
        day_rhr_cache[[i]] <- data.frame(NA)
        
        day_highAct_gsr_cache[[i]] <- data.frame(NA)
        day_highAct_hr_cache[[i]] <- data.frame(NA)
        day_highAct_sk_cache[[i]] <- data.frame(NA)
        day_highAct_st_cache[[i]] <- data.frame(NA)
        
        day_lowAct_gsr_cache[[i]] <- data.frame(NA)
        day_lowAct_hr_cache[[i]] <- data.frame(NA)
        day_lowAct_sk_cache[[i]] <- data.frame(NA)
        day_lowAct_st_cache[[i]] <- data.frame(NA)
        
        night_gsr_cache[[i]] <- data.frame(NA)
        night_hr_cache[[i]] <- data.frame(NA)
        night_sk_cache[[i]] <- data.frame(NA)
        night_st_cache[[i]] <- data.frame(NA)
        
        night_rhr_cache[[i]] <- data.frame(NA)
        
        night_highAct_gsr_cache[[i]] <- data.frame(NA)
        night_highAct_hr_cache[[i]] <- data.frame(NA)
        night_highAct_sk_cache[[i]] <- data.frame(NA)
        night_highAct_st_cache[[i]] <- data.frame(NA)
        
        night_lowAct_gsr_cache[[i]] <- data.frame(NA)
        night_lowAct_hr_cache[[i]] <- data.frame(NA)
        night_lowAct_sk_cache[[i]] <- data.frame(NA)
        night_lowAct_st_cache[[i]] <- data.frame(NA)
        next
        
      } else {
        if(1==1){
          df$Steps[df$Steps==0] <- NA
          #ID steps decile locations
          StDecLocations <- quantile(
            df$Steps, na.rm=TRUE, 
            probs = seq(0.1,0.9,by=0.1))
          if(length(unique(StDecLocations)) != 9){
            check <- which(table(StDecLocations) > 1)
            for(u in 1:length(check)){
              check2 <- which(StDecLocations==names(check)[u])
              theMin <- as.numeric(min(check2))
              for(v in 1:length(check2)){
                if(check2[v] == theMin){
                  next
                } else {
                  StDecLocations[check2[v]] <- sum(
                    StDecLocations[check2[v]],0.0000000000001)
                }
              }
            }
          }
          df$StDecID <- findInterval(df$Steps,
                                     c(-Inf,StDecLocations, Inf))
        }
        #ID heart rate decile locations
        HrDecLocations <- quantile(
          df$Heart_Rate, na.rm=TRUE, 
          probs = seq(0.1,0.9, by=0.1))
        if(length(unique(HrDecLocations)) != 9){
          check <- which(table(HrDecLocations) > 1)
          for(u in 1:length(check)){
            check2 <- which(HrDecLocations==names(check)[u])
            theMin <- as.numeric(min(check2))
            for(v in 1:length(check2)){
              if(check2[v] == theMin){
                next
              } else {
                HrDecLocations[check2[v]] <- sum(
                  HrDecLocations[check2[v]],0.0000000000001)
              }
            }
          }
        }
        df$HrDecID <- findInterval(df$Heart_Rate,
                                   c(-Inf,HrDecLocations,Inf))
        #ID skin temperature decile locations
        SkDecLocations <- quantile(
          df$Skin_Temperature_F, na.rm=TRUE, 
          probs = seq(0.1,0.9,by=0.1))
        if(length(unique(SkDecLocations)) != 9){
          check <- which(table(SkDecLocations) > 1)
          for(u in 1:length(check)){
            check2 <- which(SkDecLocations==names(check)[u])
            theMin <- as.numeric(min(check2))
            for(v in 1:length(check2)){
              if(check2[v] == theMin){
                next
              } else {
                SkDecLocations[check2[v]] <- sum(
                  SkDecLocations[check2[v]],0.0000000000001)
              }
            }
          }
        }
        df$SkDecID <- findInterval(df$Skin_Temperature_F,
                                   c(-Inf,SkDecLocations, Inf))
        #ID gsr decile locations
        GsrDecLocations <- quantile(
          df$GSR, na.rm=TRUE, 
          probs = seq(0.1,0.9,by=0.1))
        if(length(unique(GsrDecLocations)) != 9){
          check <- which(table(GsrDecLocations) > 1)
          for(u in 1:length(check)){
            check2 <- which(GsrDecLocations==names(check)[u])
            theMin <- as.numeric(min(check2))
            for(v in 1:length(check2)){
              if(check2[v] == theMin){
                next
              } else {
                GsrDecLocations[check2[v]] <- sum(
                  GsrDecLocations[check2[v]],0.0000000000001)
              }
            }
          }
        }
        df$GsrDecID <- findInterval(df$GSR,
                                    c(-Inf,GsrDecLocations, Inf))
        
        #SUBSET RESTING HEART RATE PARAMETERS TO "rdf" table
        restingST <- ave(df$lowSteps, df$Wearable_Account_MD5,
                         FUN=function(x) rollapplyr(x, 10, sum, by=1, partial=TRUE))
        restingST[1:10-1] <- c(rep(NA,10-1)) #window-1
        
        #To record resting heart rate as rolling 10-minute mean when sum of steps in last 10 min < 10:
        # restingHR <- ave(df$Heart_Rate, df$Wearable_Account_MD5,
        #                  FUN=function(x) rollapplyr(x, 10, mean, na.rm=TRUE,
        #                                             by=1, partial=TRUE, fill=NA))
        # 
        # restingHR <- restingHR[which(restingST<10 & !is.na(restingST))]
        
        #To record resting HR as all values when sum of steps in last 10 min < 10:
        restingHR <- df$Heart_Rate[which(restingST<10 & !is.na(restingST))]
        
        rhr_cache[[i]] <- data.frame(describe(restingHR))
        
        df_day <- df[which(df$Hour>=8 & df$Hour<20),]
        
        restingST <- ave(df_day$lowSteps, df_day$Wearable_Account_MD5,
                         FUN=function(x) rollapplyr(x, 10, sum, by=1, partial=TRUE)) 
        restingST[1:10-1] <- c(rep(NA,10-1)) #window-1
        
        #To record resting heart rate as rolling 10-minute mean when sum of steps in last 10 min < 10:
        # restingHR <- ave(df$Heart_Rate, df$Wearable_Account_MD5,
        #                  FUN=function(x) rollapplyr(x, 10, mean, na.rm=TRUE,
        #                                             by=1, partial=TRUE, fill=NA))
        # 
        # restingHR <- restingHR[which(restingST<10 & !is.na(restingST))]
        
        #To record resting HR as all values when sum of steps in last 10 min < 10:
        restingHR <- df_day$Heart_Rate[which(restingST<10 & !is.na(restingST))]
        
        # restingHR <- restingHR[which(restingST<10 & !is.na(restingST))] 
        
        day_rhr_cache[[i]] <- data.frame(describe(restingHR))
        rm(df_day)
        
        df_night <- df[which(df$Hour>=20 | df$Hour<8),]
        
        restingST <- ave(df_night$lowSteps, df_night$Wearable_Account_MD5,
                         FUN=function(x) rollapplyr(x, 10, sum, by=1, partial=TRUE))
        restingST[1:10-1] <- c(rep(NA,10-1)) #window-1
        
        # restingHR <- ave(df_night$Heart_Rate, df_night$Wearable_Account_MD5,
        # FUN=function(x) rollapplyr(x, 10, mean, by=1, partial=TRUE)) 
        
        restingHR <- df_night$Heart_Rate[which(restingST<10 & !is.na(restingST))] 
        
        night_rhr_cache[[i]] <- data.frame(describe(restingHR))
        rm(df_night)
        ###
        
        #label high activity (defined as top 10% of Steps)
        if(1==1){
          df$highActivity <- ifelse(
            df$StDecID==10,
            TRUE,
            FALSE)
          #df$highActivity_eventNum <- rleid(df$highActivity)
        }
        
        durationDf <- df[df$highActivity == "TRUE",]
        durationDf$Date <- as.POSIXct(paste(durationDf$Timestamp_Local),
                                      format="%Y-%m-%d %H:%M:%S")
        
        #describe variables during high activity
        highAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        highAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        highAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        highAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        
        
        df_day <- df[which(df$Hour>=8 & df$Hour<20),]
        durationDf_day <- df_day[df_day$highActivity == "TRUE",]
        durationDf_day$Date <- as.POSIXct(paste(durationDf_day$Timestamp_Local),
                                          format="%Y-%m-%d %H:%M:%S")
        
        day_highAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        day_highAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        day_highAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        day_highAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        rm(df_day)
        
        df_night <- df[which(df$Hour>=20 | df$Hour<8),]
        durationDf_day <- df_night[df_night$highActivity == "TRUE",]
        durationDf_day$Date <- as.POSIXct(paste(durationDf_day$Timestamp_Local),
                                          format="%Y-%m-%d %H:%M:%S")
        
        night_highAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        night_highAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        night_highAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        night_highAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        rm(df_night)
        ###
        
        #label low activity (defined as lowest 10% of Steps)
        if(1==1){
          df$lowActivity <- ifelse(
            df$StDecID==1,
            TRUE,
            FALSE)
          #df$lowActivity_eventNum <- rleid(df$lowActivity)
        }
        
        durationDf <- df[df$lowActivity == "TRUE",]
        durationDf$Date <- as.POSIXct(paste(durationDf$Timestamp_Local),
                                      format="%Y-%m-%d %H:%M:%S")
        
        #describe variables during low activity
        lowAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        lowAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        lowAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        lowAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        
        
        df_day <- df[which(df$Hour>=8 & df$Hour<20),]
        durationDf <- df_day[df_day$lowActivity == "TRUE",]
        durationDf$Date <- as.POSIXct(paste(durationDf$Timestamp_Local),
                                      format="%Y-%m-%d %H:%M:%S")
        
        day_lowAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        day_lowAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        day_lowAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        day_lowAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        rm(df_day)
        
        df_night <- df[which(df$Hour>=20 | df$Hour<8),]
        durationDf <- df_night[df_night$lowActivity == "TRUE",]
        durationDf$Date <- as.POSIXct(paste(durationDf$Timestamp_Local),
                                      format="%Y-%m-%d %H:%M:%S")
        
        night_lowAct_gsr_cache[[i]] <- data.frame(describe(
          durationDf$GSR, na.rm=TRUE))
        night_lowAct_hr_cache[[i]] <- data.frame(describe(
          durationDf$Heart_Rate, na.rm=TRUE))
        night_lowAct_sk_cache[[i]] <- data.frame(describe(
          durationDf$Skin_Temperature_F, na.rm=TRUE))
        night_lowAct_st_cache[[i]] <- data.frame(describe(
          durationDf$Steps, na.rm=TRUE))
        rm(df_night)
        
      }
    }
  } else {
    gsr_cache[[i]] <- data.frame(NA)
    hr_cache[[i]] <- data.frame(NA)
    sk_cache[[i]] <- data.frame(NA)
    st_cache[[i]] <- data.frame(NA)
    
    rhr_cache[[i]] <- data.frame(NA)
    
    highAct_gsr_cache[[i]] <- data.frame(NA)
    highAct_hr_cache[[i]] <- data.frame(NA)
    highAct_sk_cache[[i]] <- data.frame(NA)
    highAct_st_cache[[i]] <- data.frame(NA)
    
    lowAct_gsr_cache[[i]] <- data.frame(NA)
    lowAct_hr_cache[[i]] <- data.frame(NA)
    lowAct_sk_cache[[i]] <- data.frame(NA)
    lowAct_st_cache[[i]] <- data.frame(NA)
    
    day_gsr_cache[[i]] <- data.frame(NA)
    day_hr_cache[[i]] <- data.frame(NA)
    day_sk_cache[[i]] <- data.frame(NA)
    day_st_cache[[i]] <- data.frame(NA)
    
    day_rhr_cache[[i]] <- data.frame(NA)
    
    day_highAct_gsr_cache[[i]] <- data.frame(NA)
    day_highAct_hr_cache[[i]] <- data.frame(NA)
    day_highAct_sk_cache[[i]] <- data.frame(NA)
    day_highAct_st_cache[[i]] <- data.frame(NA)
    
    day_lowAct_gsr_cache[[i]] <- data.frame(NA)
    day_lowAct_hr_cache[[i]] <- data.frame(NA)
    day_lowAct_sk_cache[[i]] <- data.frame(NA)
    day_lowAct_st_cache[[i]] <- data.frame(NA)
    
    night_gsr_cache[[i]] <- data.frame(NA)
    night_hr_cache[[i]] <- data.frame(NA)
    night_sk_cache[[i]] <- data.frame(NA)
    night_st_cache[[i]] <- data.frame(NA)
    
    night_rhr_cache[[i]] <- data.frame(NA)
    
    night_highAct_gsr_cache[[i]] <- data.frame(NA)
    night_highAct_hr_cache[[i]] <- data.frame(NA)
    night_highAct_sk_cache[[i]] <- data.frame(NA)
    night_highAct_st_cache[[i]] <- data.frame(NA)
    
    night_lowAct_gsr_cache[[i]] <- data.frame(NA)
    night_lowAct_hr_cache[[i]] <- data.frame(NA)
    night_lowAct_sk_cache[[i]] <- data.frame(NA)
    night_lowAct_st_cache[[i]] <- data.frame(NA)
    
    print(i)
    next
  }
  print(i)
}

gsr_cache <- rbind.fill(gsr_cache)
hr_cache <- rbind.fill(hr_cache)
sk_cache <- rbind.fill(sk_cache)
st_cache <- rbind.fill(st_cache)

rhr_cache <- rbind.fill(rhr_cache)

highAct_gsr_cache <- rbind.fill(highAct_gsr_cache)
highAct_hr_cache <- rbind.fill(highAct_hr_cache)
highAct_sk_cache <- rbind.fill(highAct_sk_cache)
highAct_st_cache <- rbind.fill(highAct_st_cache)

lowAct_gsr_cache <- rbind.fill(lowAct_gsr_cache)
lowAct_hr_cache <- rbind.fill(lowAct_hr_cache)
lowAct_sk_cache <- rbind.fill(lowAct_sk_cache)
lowAct_st_cache <- rbind.fill(lowAct_st_cache)

day_gsr_cache <- rbind.fill(day_gsr_cache)
day_hr_cache <- rbind.fill(day_hr_cache)
day_sk_cache <- rbind.fill(day_sk_cache)
day_st_cache <- rbind.fill(day_st_cache)

day_rhr_cache <- rbind.fill(day_rhr_cache)

day_highAct_gsr_cache <- rbind.fill(day_highAct_gsr_cache)
day_highAct_hr_cache <- rbind.fill(day_highAct_hr_cache)
day_highAct_sk_cache <- rbind.fill(day_highAct_sk_cache)
day_highAct_st_cache <- rbind.fill(day_highAct_st_cache)

day_lowAct_gsr_cache <- rbind.fill(day_lowAct_gsr_cache)
day_lowAct_hr_cache <- rbind.fill(day_lowAct_hr_cache)
day_lowAct_sk_cache <- rbind.fill(day_lowAct_sk_cache)
day_lowAct_st_cache <- rbind.fill(day_lowAct_st_cache)

night_gsr_cache <- rbind.fill(night_gsr_cache)
night_hr_cache <- rbind.fill(night_hr_cache)
night_sk_cache <- rbind.fill(night_sk_cache)
night_st_cache <- rbind.fill(night_st_cache)

night_rhr_cache <- rbind.fill(night_rhr_cache)

night_highAct_gsr_cache <- rbind.fill(night_highAct_gsr_cache)
night_highAct_hr_cache <- rbind.fill(night_highAct_hr_cache)
night_highAct_sk_cache <- rbind.fill(night_highAct_sk_cache)
night_highAct_st_cache <- rbind.fill(night_highAct_st_cache)

night_lowAct_gsr_cache <- rbind.fill(night_lowAct_gsr_cache)
night_lowAct_hr_cache <- rbind.fill(night_lowAct_hr_cache)
night_lowAct_sk_cache <- rbind.fill(night_lowAct_sk_cache)
night_lowAct_st_cache <- rbind.fill(night_lowAct_st_cache)

gsr_cache <- gsr_cache[,-1] #first column was for NAs
hr_cache <- hr_cache[,-1] #first column was for NAs
sk_cache <- sk_cache[,-1] #first column was for NAs
st_cache <- st_cache[,-1] #first column was for NAs

rhr_cache <- rhr_cache[,-1] #first column was for NAs

highAct_gsr_cache <- highAct_gsr_cache[,-1] #first column was for NAs
highAct_hr_cache <- highAct_hr_cache[,-1] #first column was for NAs
highAct_sk_cache <- highAct_sk_cache[,-1] #first column was for NAs
highAct_st_cache <- highAct_st_cache[,-1] #first column was for NAs

lowAct_gsr_cache <- lowAct_gsr_cache[,-1] #first column was for NAs
lowAct_hr_cache <- lowAct_hr_cache[,-1] #first column was for NAs
lowAct_sk_cache <- lowAct_sk_cache[,-1] #first column was for NAs
lowAct_st_cache <- lowAct_st_cache[,-1] #first column was for NAs

day_gsr_cache <- day_gsr_cache[,-1] #first column was for NAs
day_hr_cache <- day_hr_cache[,-1] #first column was for NAs
day_sk_cache <- day_sk_cache[,-1] #first column was for NAs
day_st_cache <- day_st_cache[,-1] #first column was for NAs

day_rhr_cache <- day_rhr_cache[,-1] #first column was for NAs

day_highAct_gsr_cache <- day_highAct_gsr_cache[,-1] #first column was for NAs
day_highAct_hr_cache <- day_highAct_hr_cache[,-1] #first column was for NAs
day_highAct_sk_cache <- day_highAct_sk_cache[,-1] #first column was for NAs
day_highAct_st_cache <- day_highAct_st_cache[,-1] #first column was for NAs

day_lowAct_gsr_cache <- day_lowAct_gsr_cache[,-1] #first column was for NAs
day_lowAct_hr_cache <- day_lowAct_hr_cache[,-1] #first column was for NAs
day_lowAct_sk_cache <- day_lowAct_sk_cache[,-1] #first column was for NAs
day_lowAct_st_cache <- day_lowAct_st_cache[,-1] #first column was for NAs

night_gsr_cache <- night_gsr_cache[,-1] #first column was for NAs
night_hr_cache <- night_hr_cache[,-1] #first column was for NAs
night_sk_cache <- night_sk_cache[,-1] #first column was for NAs
night_st_cache <- night_st_cache[,-1] #first column was for NAs

night_rhr_cache <- night_rhr_cache[,-1] #first column was for NAs

night_highAct_gsr_cache <- night_highAct_gsr_cache[,-1] #first column was for NAs
night_highAct_hr_cache <- night_highAct_hr_cache[,-1] #first column was for NAs
night_highAct_sk_cache <- night_highAct_sk_cache[,-1] #first column was for NAs
night_highAct_st_cache <- night_highAct_st_cache[,-1] #first column was for NAs

night_lowAct_gsr_cache <- night_lowAct_gsr_cache[,-1] #first column was for NAs
night_lowAct_hr_cache <- night_lowAct_hr_cache[,-1] #first column was for NAs
night_lowAct_sk_cache <- night_lowAct_sk_cache[,-1] #first column was for NAs
night_lowAct_st_cache <- night_lowAct_st_cache[,-1] #first column was for NAs

colnames(gsr_cache) <- paste0("gsr_",colnames(gsr_cache))
colnames(hr_cache) <- paste0("hr_",colnames(hr_cache))
colnames(sk_cache) <- paste0("sk_",colnames(sk_cache))
colnames(st_cache) <- paste0("st_",colnames(st_cache))

colnames(rhr_cache) <- paste0("rhr_",colnames(rhr_cache))

colnames(highAct_gsr_cache) <- paste0("highAct_gsr_",colnames(highAct_gsr_cache))
colnames(highAct_hr_cache) <- paste0("highAct_hr_",colnames(highAct_hr_cache))
colnames(highAct_sk_cache) <- paste0("highAct_sk_",colnames(highAct_sk_cache))
colnames(highAct_st_cache) <- paste0("highAct_st_",colnames(highAct_st_cache))

colnames(lowAct_gsr_cache) <- paste0("lowAct_gsr_",colnames(lowAct_gsr_cache))
colnames(lowAct_hr_cache) <- paste0("lowAct_hr_",colnames(lowAct_hr_cache))
colnames(lowAct_sk_cache) <- paste0("lowAct_sk_",colnames(lowAct_sk_cache))
colnames(lowAct_st_cache) <- paste0("lowAct_st_",colnames(lowAct_st_cache))

colnames(day_gsr_cache) <- paste0("day_gsr_",colnames(day_gsr_cache))
colnames(day_hr_cache) <- paste0("day_hr_",colnames(day_hr_cache))
colnames(day_sk_cache) <- paste0("day_sk_",colnames(day_sk_cache))
colnames(day_st_cache) <- paste0("day_st_",colnames(day_st_cache))

colnames(day_rhr_cache) <- paste0("day_rhr_",colnames(day_rhr_cache))

colnames(day_highAct_gsr_cache) <- paste0("day_highAct_gsr_",
                                          colnames(day_highAct_gsr_cache))
colnames(day_highAct_hr_cache) <- paste0("day_highAct_hr_",
                                         colnames(day_highAct_hr_cache))
colnames(day_highAct_sk_cache) <- paste0("day_highAct_sk_",
                                         colnames(day_highAct_sk_cache))
colnames(day_highAct_st_cache) <- paste0("day_highAct_st_",
                                         colnames(day_highAct_st_cache))

colnames(day_lowAct_gsr_cache) <- paste0("day_lowAct_gsr_",
                                         colnames(day_lowAct_gsr_cache))
colnames(day_lowAct_hr_cache) <- paste0("day_lowAct_hr_",
                                        colnames(day_lowAct_hr_cache))
colnames(day_lowAct_sk_cache) <- paste0("day_lowAct_sk_",
                                        colnames(day_lowAct_sk_cache))
colnames(day_lowAct_st_cache) <- paste0("day_lowAct_st_",
                                        colnames(day_lowAct_st_cache))

colnames(night_gsr_cache) <- paste0("night_gsr_",colnames(night_gsr_cache))
colnames(night_hr_cache) <- paste0("night_hr_",colnames(night_hr_cache))
colnames(night_sk_cache) <- paste0("night_sk_",colnames(night_sk_cache))
colnames(night_st_cache) <- paste0("night_st_",colnames(night_st_cache))

colnames(night_rhr_cache) <- paste0("night_rhr_",colnames(night_rhr_cache))

colnames(night_highAct_gsr_cache) <- paste0("night_highAct_gsr_",
                                            colnames(night_highAct_gsr_cache))
colnames(night_highAct_hr_cache) <- paste0("night_highAct_hr_",
                                           colnames(night_highAct_hr_cache))
colnames(night_highAct_sk_cache) <- paste0("night_highAct_sk_",
                                           colnames(night_highAct_sk_cache))
colnames(night_highAct_st_cache) <- paste0("night_highAct_st_",
                                           colnames(night_highAct_st_cache))

colnames(night_lowAct_gsr_cache) <- paste0("night_lowAct_gsr_",
                                           colnames(night_lowAct_gsr_cache))
colnames(night_lowAct_hr_cache) <- paste0("night_lowAct_hr_",
                                          colnames(night_lowAct_hr_cache))
colnames(night_lowAct_sk_cache) <- paste0("night_lowAct_sk_",
                                          colnames(night_lowAct_sk_cache))
colnames(night_lowAct_st_cache) <- paste0("night_lowAct_st_",
                                          colnames(night_lowAct_st_cache))

stats <- cbind(
  ClinWithWear,gsr_cache,hr_cache,sk_cache,st_cache,
  rhr_cache,highAct_gsr_cache,highAct_hr_cache,
  highAct_sk_cache,highAct_st_cache,
  lowAct_gsr_cache,lowAct_hr_cache,
  lowAct_sk_cache,lowAct_st_cache,
  day_gsr_cache,day_hr_cache,day_sk_cache,day_st_cache,
  day_rhr_cache,day_highAct_gsr_cache,day_highAct_hr_cache,
  day_highAct_sk_cache,day_highAct_st_cache,
  day_lowAct_gsr_cache,day_lowAct_hr_cache,
  day_lowAct_sk_cache,day_lowAct_st_cache,
  night_gsr_cache,night_hr_cache,night_sk_cache,night_st_cache,
  night_rhr_cache,night_highAct_gsr_cache,night_highAct_hr_cache,
  night_highAct_sk_cache,night_highAct_st_cache,
  night_lowAct_gsr_cache,night_lowAct_hr_cache,
  night_lowAct_sk_cache,night_lowAct_st_cache)

write.csv(stats,
          paste0("F:\\PhysSubTables_20161111_Normal_20170802\\Basis2016_Clean_Norm_AllData_20180504.csv"),
          row.names=FALSE)


#### END OF SCRIPT ####
