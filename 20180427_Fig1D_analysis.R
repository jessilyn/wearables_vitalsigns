#### REQUIRED
require(data.table)
require(ggplot2)
require(plyr)
require(psych)
require(zoo)
require( lubridate )

# FUNCTIONS
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

#### READ IN & CLEAN DATA
#dir = "/srv/gsfs0/projects/snyder/jdunn/framework_paper/SECURE_data" # Path to the directory with data
#dir = "/Users/jessilyn/Desktop/framework_paper/weartals"
dir = "/Users/jessilyn/Desktop/framework_paper/Figure1/Fig1C/Ryans_input_files"

# df <- fread(paste0(dir, "/BasisData_20161111_PostSummerAddOns_Cleaned_NotNormalized_20170928.csv"),
df.source <- fread(paste0(dir, "/BasisData_20161111_PostSummerAddOns_Cleaned_NotNormalized_20180427.csv"),
                        header=TRUE,sep=",",stringsAsFactors = FALSE) # 38009228 observations
df<-df.source[!is.na(df.source$iPOP_ID),] # only keep wearables data that has an associated iPOP ID; 
df[,6] <- apply(df[,6], 2, remove_outliers) # clean data based on HR, Skin Temp, Steps
df <- df[which(!is.na(df[,6]))] # remove observations where HR = NA
df$Date <- as.POSIXct(paste(df$Timestamp_Local),format="%Y-%m-%d %H:%M:%S")
df2<-df

##### SAMPLE DEMOGRAPHICS 
inven <- fread(paste0(dir, "/ClinWearDemo_SamplePop.csv"),
               header=TRUE,sep=",",stringsAsFactors = FALSE)
vitals <- fread(paste0(dir,"/Vitals.csv"),
                header=TRUE,sep=',',stringsAsFactors=FALSE)
unq_vitals_iPOPs <- unique(vitals$HIMC_ID)
vitals$RESULT_TIME<-as.Date(vitals$RESULT_TIME)
colnames(vitals)[1:2] <- c("iPOP_ID", "Date")

###############
#  Figure 1D  #
###############

### explore how time of day and resting thresholds affect comparison of mean and variance between watch RHR and clinic pulse
restingDf <- c() 
restingDf.all <- c() # keep all resting data for boxplots later
#restingDf.all.byTime <- list() # keep all resting data by time of day for boxplots later
window=10 # define time window for resting threshold
maxsteps <- 1 #define max steps for resting threshold
#indiv.stats <- data.frame()
indiv.means <- c()
indiv.sd <- c()
indiv.clinDay.means <- data.frame()
indiv.clinDay.sd <- data.frame()
test.iPOP_ID <- unique(df$iPOP_ID)[1:5]

for(i in test.iPOP_ID){
#for(i in unique(df$iPOP_ID)){
    print(i)
    subDf <- df[which(df$iPOP_ID %in% i),] #pull data per individual - fix this to be by iPOP ID not by MD5
    restingST<-c()
    restingST[1:window]<-"NA" # remove 1st x observations because we dont know what happened prior to putting the watch on
    restingST[window+1:dim(subDf)[1]] <- rollapply(subDf$Steps, width=list(1:window), sum)
    restingST <- as.numeric(restingST); restingST <- restingST[1:dim(subDf)[1]]
    restingDf <- subDf[restingST<maxsteps & subDf$Steps<maxsteps & !is.na(restingST)] # in the previous time window of X min plus the current minute,there are < maxsteps steps 
    indiv.means[i] <- mean(na.omit(restingDf$Heart_Rate)) # mean RHR for all days/times for individual i
    indiv.sd[i] <- sd(na.omit(restingDf$Heart_Rate)) # RHR var for all days/times for individual i
    restingDf.all <- rbind(restingDf.all, restingDf) # store all resting data for boxplots
}  
restingDf.all <- restingDf.all[,c(10,11,6,7,8,3)]
names(restingDf.all)<- c("iPOP_ID","Date", "restingHR", "restingSkinTemp", "restingSteps", "DateTime" )
restingDf.all$Date<-as.Date(restingDf.all$Date)

# all data
ggplot(restingDf.all2) +
  geom_boxplot(aes(x=iPOP_ID, y=restingHR))

#merge vitals with resting HR
restingDf.vitals <- merge(restingDf.all,vitals,by=c("iPOP_ID","Date"))
restingDf.vitals$DateTime<-as.POSIXct(restingDf.vitals$DateTime, format="%Y-%m-%d %H:%M:%OS")
restingDf.vitals <- restingDf.vitals[order(restingDf.vitals$DateTime),] 
restingDf.vitals$restingHR <- as.numeric(restingDf.vitals$restingHR)
restingDf.vitals$restingSkinTemp <- as.numeric(restingDf.vitals$restingSkinTemp)
wear_vals <- aggregate(restingHR ~ iPOP_ID + Date, mean, na.rm=TRUE, data=test) # mean of 
vital_vals <- aggregate(Pulse ~ iPOP_ID + Date, mean, na.rm=TRUE, data=test) #
plotDf <- merge(wear_vals,vital_vals)

# compare all resting watch data with all vitals data
restingDf.compare <- cbind(restingDf.vitals$restingHR, restingDf.vitals$Pulse)
colnames(restingDf.compare) <- c("wHR", "cHR")
boxplot(restingDf.compare) + ylab("Heart Rate")


for (j in 1:6){ # vary the hour of the day
  subDfClinDay <- with( restingDf , restingDf[ hour( Date ) >= j+2 & hour( Date ) < j+3 , ] ) #pull data only from specific time window
  indiv.clinDay.means[i,j] <- mean(na.omit(subDfClinDay$Heart_Rate)) # mean RHR for all days/times for individual i
  indiv.clinDay.sd[i,j] <- var(na.omit(subDfClinDay$Heart_Rate)) # RHR var for all days/times for individual i
  #restingDf.all.byTime[[j]] <- rbind(restingDf.all.byTime[[j]], subDfClinDay) # store all resting data by start time for boxplots
  # generate table with headers iPOP_ID, RHR_mean, RHR_stdev, time window (e.g. 3-4am)
}


  #write.csv(plotDf, paste0(dir,"/20180416/20180416_3am_resting10minHRvPulse_plot.csv")) 
  
  # generate scatterplots with individuals by color for each time window
  p[j] <- ggplot(plotDf, aes(x=restingHR,y=Pulse,col=as.factor(substr(iPOP_ID,9,12)))) +
    geom_point() +
    #geom_smooth(method="glm", formula =y~x, se=F) +
    labs(title="Clinical Pulse vs.\n Wearable Resting\n Heart Rate (6-7am) Mean ",x="Resting Heart Rate",y="Pulse") +
    annotate("segment",x=-Inf,xend=Inf,y=-Inf,yend=Inf,
             lwd=2, color="blue", alpha=.25) +
    guides(col=guide_legend("ID")) +
    xlim(40, 100) +
    ylim(40, 100)+
    theme(plot.title=element_text(face="bold",colour="black",size=14),
          axis.title.x=element_text(face="bold",colour="black",size=14),
          axis.text.x=element_text(face="bold",colour="black",size=12,angle=55,vjust=0.9,hjust=1),
          axis.title.y=element_text(face="bold",colour="black",size=14),
          axis.text.y=element_text(face="bold",colour="black",size=12),
          axis.ticks.length = unit(.2,"cm"),
          legend.title=element_text(face="bold", colour="black", size=14),
          legend.text=element_text(face="bold", colour="black", size=12),
          panel.background=element_rect(fill="grey94"))
}

# differences between RHR mean and RHR var from late night (e.g. 3-4am) vs later morning (e.g. 8-9am)
varClinHR<-aggregate(vitals$Pulse, list(vitals$HIMC_ID), var)
dfVar <- merge(varWatchHR[[1]], varWatchHR[[6]], by="Group.1") # combine different timeframes into 1 data frame
dfVar <- merge(dfVar, varClinHR, by="Group.1")
anova(dfVar[,2], dfVar[,3], dfVar[,4], var.equal=FALSE, paired=TRUE)
boxplot(dfVar[,3], dfVar[,2], dfVar[,4], outline = FALSE,
        ylab="Heart Rate Variance", las = 2, names = c("Day", "Night", "Clin"))

dfAvg <- merge(meanWatchHR[[1]], meanWatchHR[[6]], by="Group.1") # combine different timeframes into 1 data frame
dfAvg <- merge(dfAvg, varClinHR, by="Group.1")
t.test(dfAvg[,2], dfAvg[,3], var.equal=FALSE, paired=TRUE)
boxplot(dfAvg[,3], dfAvg[,2], vitals$Pulse, outline = FALSE,
        ylab="Heart Rate Mean", las = 2, names = c("Day", "Night", "Clin"))
