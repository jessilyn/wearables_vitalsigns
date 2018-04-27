# Figure 1D script - 2018-04-27
# open this script from the weartals github repo
dir = "../SECURE_data"
#dir = "/srv/gsfs0/projects/snyder/jdunn/framework_paper/SECURE_data" # For use on scg

## Hypothesis: 
# 1) Watch RHR < Clinic Pulse (white coat syndrome)
# 2) Watch variance per individual < clinic pulse variance per individual (power of many tests)

### Goal: explore how time of day and resting thresholds affect comparison of mean and variance between watch RHR and clinic pulse

#### REQUIRED
require(data.table)
require(ggplot2)
require(plyr)
require(psych)
require(zoo)
require( lubridate )
library("ggthemes")
source("ggplot-theme.R")

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
vitals <- fread(paste0(dir,"/Vitals_for_Fig1D.csv"), #there are two different vitals.csv files; one is for 30k and one is for iPOP - make sure you use the correct one
                header=TRUE,sep=',',stringsAsFactors=FALSE)
unq_vitals_iPOPs <- unique(vitals$HIMC_ID)
vitals$RESULT_TIME<-as.Date(vitals$RESULT_TIME)
colnames(vitals)[1:2] <- c("iPOP_ID", "Date")

###############
#  Figure 1D  #
###############
restingDf <- c() 
restingDf.all <- c() # keep all resting data for boxplots later
#restingDf.all.byTime <- list() # keep all resting data by time of day for boxplots later
window=10 # define time window for resting threshold
maxsteps <- 1 #define max steps for resting threshold
indiv.means <- c()
indiv.sd <- c()
indiv.clinDay.means <- data.frame()
indiv.clinDay.sd <- data.frame()

for(i in unique(df$iPOP_ID)){
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

#merge vitals with resting HR
restingDf.vitals <- merge(restingDf.all,vitals,by=c("iPOP_ID","Date"))
restingDf.vitals$DateTime<-as.POSIXct(restingDf.vitals$DateTime, format="%Y-%m-%d %H:%M:%OS")
restingDf.vitals <- restingDf.vitals[order(restingDf.vitals$DateTime),] 
restingDf.vitals$restingHR <- as.numeric(restingDf.vitals$restingHR)
restingDf.vitals$restingSkinTemp <- as.numeric(restingDf.vitals$restingSkinTemp)
restingDf.vitals$Pulse <- as.numeric(restingDf.vitals$Pulse)

#########
# PLOTS #
#########

# compare all resting watch data with all vitals data
restingDf.compare <- cbind(restingDf.vitals$restingHR, restingDf.vitals$Pulse)
colnames(restingDf.compare) <- c("Resting wHR", "cHR")
boxplot(restingDf.compare, ylab="Heart Rate", col = "darkred") 
t.test(restingDf.vitals$restingHR, restingDf.vitals$Pulse, var.equal=FALSE, paired=TRUE) # test for significance

#compare within individual means and variability of watch data to clinical data
delta.RHR.pulse<-restingDf.vitals$Pulse - restingDf.vitals$restingHR; hist(delta.RHR.pulse, breaks=100, col="darkred", main = "cHR - rwHR (All Clinic Visits, Watch data from all Days)")
means<-aggregate(restingDf.vitals, list(restingDf.vitals$iPOP_ID), mean) #check why different from indiv.means
sd<-aggregate(restingDf.vitals, list(restingDf.vitals$iPOP_ID), sd)
delta.mean.RHR.pulse<-means$Pulse - means$restingHR ; hist(delta.mean.RHR.pulse, breaks=100, col="darkred", main = "cHR - rwHR (Individual Means)")
delta.sd.RHR.pulse<-sd$Pulse - sd$restingHR ; hist(delta.sd.RHR.pulse, breaks=100, col="darkred", main = "cHR - rwHR (Individual Stdevs)")

# by individual
library(reshape2)
restingDf.vitals.melt <- melt(restingDf.vitals,id.vars='iPOP_ID', measure.vars=c('restingHR','Pulse'))
ggplot(restingDf.vitals.melt) +
  geom_boxplot(aes(x=iPOP_ID, y=value, color=variable), outlier.shape=NA) +
  labs(title="Clinical Pulse vs.\n Wearable Resting\n Heart Rate ",x="Resting Heart Rate",y="Pulse") +
  weartals_theme

###############################
# Differences by Hour of Day  #
###############################

for (j in 1:6){ # vary the hour of the day
  subDfClinDay <- with( restingDf , restingDf[ hour( Date ) >= j+2 & hour( Date ) < j+3 , ] ) #pull data only from specific time window
  indiv.clinDay.means[i,j] <- mean(na.omit(subDfClinDay$Heart_Rate)) # mean RHR for all days/times for individual i
  indiv.clinDay.sd[i,j] <- var(na.omit(subDfClinDay$Heart_Rate)) # RHR var for all days/times for individual i
}
