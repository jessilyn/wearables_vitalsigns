source("experiment/load-data.R")
source("experiment/helper-functions.R")

####################
#### Figure 1  #####
####################
iPOPdaysMonitored <- read.csv("/Users/jessilyn/Desktop/framework_paper/Figure1/Slide 2/slide2_C_participant_data_summary.csv",
                  header=TRUE,sep=',',stringsAsFactors=FALSE)
# restrict to wearables only people
iPOPdaysMonitored <- iPOPdaysMonitored[iPOPdaysMonitored$iPOP_ID %in% wearables.people,]
mean(iPOPdaysMonitored$Days_monitored_by_clinic)/365 # 3.3 years of clinic monitoring
mean(iPOPdaysMonitored$Total_NumOfClinMeasures) # avg of 42 clinic visits
mean(iPOPdaysMonitored$Days_monitored_by_basis) # avg of 343 clinic visits
mean(iPOPdaysMonitored$Days_overlapping_between_basis_and_clinic_monitoring) # avg of 313 clinic visits

#number of iPOPpers with > 50 clinic visits
sum(table(iPOPcorDf.demo$iPOP_ID) > 50)


iPOP.table <- fread(paste0(dir, "ClinWearDemo_SamplePop.csv"),
header=TRUE,sep=",",stringsAsFactors = FALSE)
# restrict to wearables only people
iPOP.table <- iPOP.table[iPOP.table$iPOP_ID %in% wearables.people,]
iPOP.table$iPOP_ID <- as.factor(iPOP.table$iPOP_ID)
iPOP.table$Gender <- as.factor(iPOP.table$Gender)
iPOP.table$Ethn <- as.factor(iPOP.table$Ethn)
iPOP.table$AgeIn2016 <- as.numeric(iPOP.table$AgeIn2016)
table(iPOP.table[1:54,]$Ethn)
female <- iPOP.table[iPOP.table$Gender=="F"]
max(female$AgeIn2016);  min(female$AgeIn2016); mean(female$AgeIn2016) # max age of female particpants

male <- iPOP.table[iPOP.table$Gender=="M"]
male <- male[-max(dim(male)),]
max(male$AgeIn2016);  min(male$AgeIn2016); mean(male$AgeIn2016) # max age of male particpants



###############
#  Figure 1B  #
############### 
# expanded analysis in 20180605_Fig1D_analysis.R in the whitecoat github directory
df <- fread(paste0(dir, "/BasisData_20161111_PostSummerAddOns_Cleaned_NotNormalized_20180427.csv"),
                   header=TRUE,sep=",",stringsAsFactors = FALSE,
                   select=c("Timestamp_Local","Heart_Rate","Skin_Temperature_F",
                            "Steps","iPOP_ID")) # 38009228 observations
df<-df[!is.na(df$iPOP_ID),] # only keep wearables data that has an associated iPOP ID; 
df[,"Heart_Rate"] <- apply(df[,"Heart_Rate"], 2, remove_outliers) # clean data based on HR (TODO: later also clean on Skin Temp, Steps)
df <- df[which(!is.na(df[,"Heart_Rate"])),] # remove observations where HR = NA
# restrict to daytime values only (between 6am-10pm)
#df$Timestamp_Local<-as.POSIXct(df$Timestamp_Local) # takes forever
df$Timestamp_Local<-fastPOSIXct(df$Timestamp_Local) # takes a very long time
#daytime.df <- with( df, df[ hour( Timestamp_Local ) >= 6 & hour( Timestamp_Local ) < 22 , ] ) # pull data only from specific time window; store hourly resting data for boxplots
#daytime.df <- with( df, df[ hour( Timestamp_Local ) >= 20 & hour( Timestamp_Local ) < 24 , ] ) # pull data only from specific time window; store hourly resting data for boxplots
#daytime.df <- with( df, df[ hour( Timestamp_Local ) >= 24 | hour( Timestamp_Local ) >= 0 & hour( Timestamp_Local ) < 1, ] ) # pull data only from specific time window; store hourly resting data for boxplots

#below is actually nighttime for the test mike asked for
daytime.df <- with( df, df[ hour( Timestamp_Local ) >= 19 & hour( Timestamp_Local ) < 21 , ] ) # pull data only from specific time window; store hourly resting data for boxplots

vitals <- iPOPvitals
vitals$Clin_Result_Date<-as.Date(vitals$Clin_Result_Date, "%Y-%m-%d")
colnames(vitals)[1:5] <- c("iPOP_ID", "Date", "BP", "Pulse", "Temp")
windows=c(60, 120, 180, 240) # define time windows with no steps for resting threshold (10,60,120, etc)

dayPrior = FALSE
idx=0
HR.personal.sd	<- c()
wRHR.mean	<- c()
wRHR.sd	<- c()
wRHR.num.obs	<- c()
wRTemp.mean	<- c()
wRTemp.sd	<- c()
wRTemp.num.obs	<- c()
Temp.personal.sd	<- c()
for (window in windows){
  idx=idx+1
  restingDf <- c() 
  restingDf.all <- list() # keep all resting data for boxplots later
  maxsteps <- 1 #define max steps for resting threshold
  indiv.means <- c()
  indiv.sd <- c()
  
  for(i in unique(daytime.df$iPOP_ID)){
    subDf <- daytime.df[which(daytime.df$iPOP_ID %in% i),] #pull data per individual
    if (dim(subDf)[1] > window) {
    print(i)
    restingST<-c()
    restingST <- rollapplyr(subDf$Steps, width=window, by=1, sum, partial=TRUE)
    restingST[1:window-1]<-"NA" # remove 1st x observations because we dont know what happened prior to putting the watch on
    restingST <- as.numeric(restingST)  # Expected warning: "In as.numeric(restingST) : NAs introduced by coercion"
    restingDf <- subDf[restingST<maxsteps & !is.na(restingST)] # in the previous time window of X min plus the current minute,there are < maxsteps steps 
    indiv.means[i] <- mean(restingDf$Heart_Rate, na.rm=TRUE) # mean RHR for all days/times for individual i
    indiv.sd[i] <- sd(restingDf$Heart_Rate, na.rm=TRUE) # RHR var for all days/times for individual i
    restingDf.all[[i]] <- restingDf # store all resting data for boxplots
    }
  }
  
  restingDf.all <- rbindlist(restingDf.all)
  restingDf.all$Date <- as.Date(restingDf.all$Timestamp_Local)
  restingDf.all <- restingDf.all[,c("iPOP_ID","Date","Heart_Rate","Skin_Temperature_F","Steps","Timestamp_Local")]
  names(restingDf.all) <- c("iPOP_ID","Date","restingHR","restingSkinTemp","restingSteps","DateTime")
  means.by.id<-aggregate(restingDf.all, list(restingDf.all$iPOP_ID), na.omit(mean)) 
  sd.by.id<-aggregate(restingDf.all, list(restingDf.all$iPOP_ID), sd) 
  HR.personal.sd[idx] <- mean(na.omit(sd.by.id$restingHR)) # personal / intra-individual SD
  wRHR.mean[idx] <- mean(na.omit(restingDf.all$restingHR)) # wRHR; mean 65.23 +/- 10.41, n=1,198,040 measurements
  wRHR.sd[idx] <-sqrt(var((na.omit(restingDf.all$restingHR))))  # sd wRHR
  wRHR.num.obs[idx] <- length(na.omit(restingDf.all$restingHR))
  wRTemp.mean[idx] <- mean(na.omit(restingDf.all$restingSkinTemp)) # wRTemp; 90.89 +/- 3.09, n=1,181,648 measurements
  wRTemp.sd[idx] <-sd(na.omit(restingDf.all$restingSkinTemp)) # sd wRTemp
  wRTemp.num.obs[idx] <- length(na.omit(restingDf.all$restingSkinTemp))
  Temp.personal.sd[idx] <- mean(na.omit(sd.by.id$restingSkinTemp))
  
  #Optional: use day-prior rather than day-of wearable data for comparison:
  if(dayPrior){
    restingDf.all$Date <- restingDf.all$Date + days(1)
  }

  restingDf.vitals <- merge(restingDf.all,vitals,by=c("iPOP_ID","Date"))
  restingDf.vitals$DateTime<-as.POSIXct(restingDf.vitals$DateTime)
  restingDf.vitals <- restingDf.vitals[order(restingDf.vitals$DateTime),] 
  cols <- c("restingHR","restingSkinTemp","Pulse","Temp") #subset columns to convert
  restingDf.vitals[,(cols) := lapply(.SD, function(x) as.numeric(as.character(x))), .SDcols = cols]
  df.name <- paste0("restingDf.vitals.", window)
  assign(df.name, restingDf.vitals) # to store data frame for each resting window definition
}


ggplot()+
  geom_line(aes(x=windows, y=wRHR.mean), color="red") +
  geom_point(aes(x=windows, y=wRHR.mean), color="red") +
  # geom_line(aes(x=windows, y=wRTemp.mean), color="blue") +
  # geom_point(aes(x=windows, y=wRTemp.mean), color="blue") +
  xlab("Resting Time Window (seconds)") +
  ylab("Mean wRHR") # or wRTemp
  
cols <- c("red","blue","green","purple")
ggplot()+
  geom_line(aes(x=windows, y=wRHR.sd), color="red") +
  geom_point(aes(x=windows, y=wRHR.sd), color="red") +
  geom_line(aes(x=windows, y=HR.personal.sd), color="blue") +
  geom_point(aes(x=windows, y=HR.personal.sd), color="blue") +
  geom_line(aes(x=windows, y=rep(cHR.sd, length(windows))), color="coral") +
  geom_point(aes(x=windows, y=rep(cHR.sd, length(windows))), color="coral") +
  geom_line(aes(x=windows, y=rep(cHR.individual.sd, length(windows))), color="skyblue") +
  geom_point(aes(x=windows, y=rep(cHR.individual.sd, length(windows))), color="skyblue") +
  xlab("Resting Time Window (seconds)") +
  ylab("HR SD") # or wRTemp
  
wRHR.mean
wRHR.sd
wRHR.num.obs
HR.personal.sd
wRTemp.mean
wRTemp.sd
wRTemp.num.obs
Temp.personal.sd

for(window in windows){
  restingDf.vitals <- eval(as.name(paste0("restingDf.vitals.",window)))

  # resting values to go in the text of the manuscript
  mean(restingDf.vitals$restingHR)
  sd(restingDf.vitals$restingHR)
  mean(na.omit(restingDf.vitals$restingSkinTemp))
  sd(na.omit(restingDf.vitals$restingSkinTemp))

  # wRHR from *specific day* of clinic visit only; aggregate wRHR into daily values corresponding to the clinic date
  # compare all resting watch data with all vitals data - this was fixed to aggregate wRHR into daily values corresponding to the clinic date
  options(datatable.optimize=1) #need this here; otherwsie won't handle character class
  rhr.daily.means <- restingDf.vitals[, lapply(.SD, mean, na.rm=TRUE), by=c("iPOP_ID","Date")]
  options(datatable.optimize=0)
  numObs <- dim(rhr.daily.means)[1]
  numPeople <- length(unique(rhr.daily.means$iPOP_ID))
  restingDf.compare <- cbind(rhr.daily.means$restingHR, rhr.daily.means$Pulse, rhr.daily.means$restingSkinTemp, rhr.daily.means$Temp)
  colnames(restingDf.compare) <- c("Resting wHR", "cHR", "Resting wSkinTemp", "cTemp")
  rhr.daily.means.id <- cbind(rhr.daily.means$iPOP_ID, rhr.daily.means$restingHR, rhr.daily.means$Pulse, rhr.daily.means$restingSkinTemp, rhr.daily.means$Temp)
  rhr.daily.means.id <- as.data.frame(rhr.daily.means.id)
  rhr.daily.means.id[,1 ]<- as.factor(as.character(rhr.daily.means.id[,1 ])); 
  rhr.daily.means.id[,2 ]<- as.numeric(as.character(rhr.daily.means.id[,2 ])); 
  rhr.daily.means.id[,3 ]<- as.numeric(as.character(rhr.daily.means.id[,3 ])); 
  rhr.daily.means.id[,4 ]<- as.numeric(as.character(rhr.daily.means.id[,4 ]));
  rhr.daily.means.id[,5 ]<- as.numeric(as.character(rhr.daily.means.id[,5 ])); 
  colnames(rhr.daily.means.id) <- c("iPOP_ID", "restingHR", "Pulse", "restingSkinTemp", "Temp")
  
  means<-aggregate(rhr.daily.means.id, list(rhr.daily.means.id$iPOP_ID), mean) # check this compare to indiv means
  sd<-aggregate(rhr.daily.means.id, list(rhr.daily.means.id$iPOP_ID), sd)
  delta.daily.mean.RHR.pulse<-means$Pulse - means$restingHR
  delta.daily.sd.RHR.pulse<-sd$Pulse - sd$restingHR 
  hist(delta.daily.mean.RHR.pulse, col="darkred", main = paste0("Delta Mean cHR - Mean wRHR)"))
  hist(delta.daily.sd.RHR.pulse, col="darkred", main = paste0("Delta StdDev cHR - StdDev wRHR)"))
  
  rhr.daily.means.id$idx <- as.numeric(rhr.daily.means.id$iPOP_ID)
  
  # Scatter plot HR
  p1 <- ggplot(rhr.daily.means.id,
         aes(x=restingHR,y=Pulse,col=as.factor(rhr.daily.means.id$idx))) +
             #col=as.factor(substr(iPOP_ID,9,12)))) +
    geom_point() +
    #labs(title=paste0("Clinical Pulse vs. Wearable RHR Mean (",window,"min resting)"),x="Resting Heart Rate",y="Pulse") +
    labs(title=NULL,x="wRHR",y="cHR") +
    annotate("segment",x=-Inf,xend=Inf,y=-Inf,yend=Inf,
             lwd=1, color="blue", alpha=.25) +
    #guides(col=guide_legend("Subject ID")) +
    xlim(40, 100) +
    ylim(40, 100)+
    theme(plot.title=element_text(face="bold",colour="black",size=16),
          axis.title.x=element_text(face="bold",colour="black",size=16),
          axis.text.x=element_text(face="bold",colour="black",size=16,angle=55,vjust=0.9,hjust=1),
          axis.title.y=element_text(face="bold",colour="black",size=16),
          axis.text.y=element_text(face="bold",colour="black",size=16),
          axis.ticks.length = unit(.2,"cm"),
          #legend.position="none",
          #legend.title=element_text(face="bold", colour="black", size=16),
          #legend.text=element_text(face="bold", colour="black", size=16),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
          #panel.background=element_rect(fill="grey94"))
  
  ## scatter plot skin temp
  p2 <- ggplot(rhr.daily.means.id,
         aes(x=restingSkinTemp,y=Temp,col=as.factor(rhr.daily.means.id$idx))) +
             #col=as.factor(substr(iPOP_ID,9,12)))) +
    geom_point() +
    #labs(title=paste0("Clinical Temp vs. Wearable Temp (",window,"min resting)"), x="Resting Skin Temp",y="Core Temperature") +
    labs(title=NULL, x="wRTemp",y="cTemp") +
    annotate("segment",x=-Inf,xend=Inf,y=-Inf,yend=Inf,
             lwd=1, color="blue", alpha=.25) +
    guides(col=guide_legend("Subject ID")) +
    xlim(92, 100) +
    ylim(92, 100)+
    theme(plot.title=element_text(face="bold",colour="black",size=16),
          axis.title.x=element_text(face="bold",colour="black",size=16),
          axis.text.x=element_text(face="bold",colour="black",size=16,angle=55,vjust=0.9,hjust=1),
          axis.title.y=element_text(face="bold",colour="black",size=16),
          axis.text.y=element_text(face="bold",colour="black",size=16),
          axis.ticks.length = unit(.2,"cm"),
          #legend.position="none",
          legend.title=element_text(face="bold", colour="black", size=16),
          legend.text=element_text(face="bold", colour="black", size=16),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
          #panel.background=element_rect(fill="grey94"))
  
  grid.arrange(p1, p2, nrow = 2)
# save as a 7x4.5 pdf
  }

##########
# Fig 1C #
##########
hist(iPOPdaysMonitored$Days_monitored_by_clinic, col="grey", breaks=10,
     xlab = "Time Monitored by Clinic (Days)", main = NULL, font.lab=2,lwd=2,font=2)
hist(iPOPdaysMonitored$Days_monitored_by_clinic, col="red", breaks=10,
     xlab = "Time Monitored by Clinic (Days)", main = NULL, font.lab=2,lwd=2,font=2, lty="blank")
hist(iPOPdaysMonitored$Total_NumOfClinMeasures, col="grey", breaks=10,
     xlab = "Number of Clinic Visits / Person", main = NULL, font.lab=2,lwd=2,font=2)
hist(iPOPdaysMonitored$Days_monitored_by_basis, col="grey", breaks=20,
     xlab = "Time Monitored by Watch (Days)", main = NULL, font.lab=2,lwd=2,font=2)
mean(iPOPdaysMonitored$Total_NumOfClinMeasures)
mean(iPOPdaysMonitored$Days_monitored_by_clinic)

###############
# Fig 1D  top #
###############

length(iPOPvitals$Pulse[!is.na(iPOPvitals$Pulse)]) # number of cHR measurements in iPOP cohort
mean(iPOPvitals$Pulse[!is.na(iPOPvitals$Pulse)]) # mean cHR; 71.54 +/- 9.92, n=1644
cHR.sd <- sqrt(var(iPOPvitals$Pulse[!is.na(iPOPvitals$Pulse)])) # stdev of cHR
means<-aggregate(iPOPvitals$Pulse,list(iPOPvitals$iPOP_ID), mean) # check this compare to indiv means
sd<-aggregate(iPOPvitals$Pulse, list(iPOPvitals$iPOP_ID), sd)
# personal sd:
cHR.individual.sd <- mean(na.omit(sd$x)) # intra-individual SD 6.913

length(iPOPvitals$Temp[!is.na(iPOPvitals$Temp)]) # number of cTemp measurements in iPOP cohort
mean(iPOPvitals$Temp[!is.na(iPOPvitals$Temp)]) # mean cTemp; 97.84 +/- 0.38, n=1136
cTemp.sd <- sqrt(var(iPOPvitals$Temp[!is.na(iPOPvitals$Temp)])) # stdev of cTemp
means<-aggregate(iPOPvitals$Temp,list(iPOPvitals$iPOP_ID), mean) # check this compare to indiv means
sd<-aggregate(iPOPvitals$Temp, list(iPOPvitals$iPOP_ID), sd)
cTemp.individual.sd <- mean(na.omit(sd$x)) # intra-individual SD 0.2536

pdf(file = paste0(dir, "../Figure1/Figure1D_hists.pdf"))
par(mfrow = c(2,2), mai = c(0.7, 0.7, 0.7, 0.7))
hist(iPOPvitals$Pulse, col="tomato3", , border="tomato4", breaks=50,
     xlab = "cHR", xlim=c(50,200),
     main = NULL, font.lab=2,lwd=2,font=2)
hist(iPOPvitals$Temp, col="turquoise3", border="turquoise4", breaks=10,
     xlab = "cTemp", xlim=c(65,105),
     main = NULL, font.lab=2,lwd=2,font=2)
#####################
#  Figure 1D Bottom #
#####################

# resting HR and ST from Fig 1B data - need to run code for Fig 1B first
options(scipen=10)
hist(restingDf.all$restingHR, col="tomato3", border="tomato4", breaks=50,
     xlab = "wRHR", xlim=c(50,200),
     main = NULL, font.lab=2,lwd=2,font=2)
scale_y_continuous()
hist(restingDf.all$restingSkinTemp, col="turquoise3", border="turquoise4", breaks=46,
     xlab = "wRTemp", xlim=c(65,105),
     main = NULL, font.lab=2,lwd=2,font=2)
dev.off()

dfFigOne <- fread(paste0(paste0(dir, "BasisData_20161111_PostSummerAddOns_Cleaned_NotNormalized_20180427.csv")),
            header=TRUE,sep=",",stringsAsFactors = FALSE)

dfFigOne$Heart_Rate <- remove_outliers(dfFigOne$Heart_Rate) # clean data based on HR (TODO: later also clean on Skin Temp, Steps)
length(dfFigOne$Heart_Rate[!is.na(dfFigOne$Heart_Rate)]) # number of wHR measurements in iPOP cohort
mean(dfFigOne$Heart_Rate[!is.na(dfFigOne$Heart_Rate)]) # mean wHR mean 74.31 +/- 15.17, n=25,341,508
sqrt(var(dfFigOne$Heart_Rate[!is.na(dfFigOne$Heart_Rate)])) # stdev of wHR

# hist(dfFigOne$Heart_Rate, col="darkred", breaks=100,
#      xlab = "wHR",
#      main = NULL, font.lab=2,lwd=2,font=2,
#      xlim=c(0,200))

dfFigOne$Skin_Temperature_F <- remove_outliers(dfFigOne$Skin_Temperature_F) # clean data based on HR (TODO: later also clean on Skin Temp, Steps)
length(dfFigOne$Skin_Temperature_F[!is.na(dfFigOne$Skin_Temperature_F)]) # number of wTemp measurements in iPOP cohort
mean(dfFigOne$Skin_Temperature_F[!is.na(dfFigOne$Skin_Temperature_F)]) # mean wTemp mean 88.57 +/- 3.74, n=27,136,802
sqrt(var(dfFigOne$Skin_Temperature_F[!is.na(dfFigOne$Skin_Temperature_F)])) # stdev of wTemp 
# hist(dfFigOne$Skin_Temperature_F, col="darkgrey", breaks=100,
#      xlab = "wTemp", xlim=c(65,105),
#      main = NULL, font.lab=2,lwd=2,font=2)

#characterize the iPOP data set
length(na.omit(iPOPvitals$Temp)) + length(na.omit(iPOPvitals$Pulse)) # total number of clinical vital signs measured
describe(iPOPlabs[names(iPOPlabs) %in% allClin]) # summary of clinical labs data
length(unique(wear$iPOP_ID)) # num people in iPOP wearables dataset

#############################
#    Suppl. Table 1A and B  #
#############################
# make table for vitals (Suppl. Table 1A)
#describe(iPOPvitals)
# make table for labs
#describe(iPOPlabs)

source("experiment/population-models.R")
source("experiment/group-comparison.R")
source("experiment/cca.R")

##############
#  Figure 2F #
##############

rf.features <-read.table("/Users/jessilyn/Desktop/framework_paper/SECURE_data/20180622/20180621_DayPrior_noDemog_RF_Features.csv",
                  header=FALSE,sep=',',stringsAsFactors=FALSE)
colnames(rf.features) <- c("test", "cv.run", "iPOP_ID", "feature", "coefficient")
rf.feature.summaries <- as.data.frame(summarise(group_by(rf.features, test, feature),
                                                   mean=mean(coefficient), sd=sd(coefficient)))
top.models <- c("HCT", "HGB", "RBC", "MONOAB", "GLU", "UALB",  "CL", "A1C")
top.rf.features <- c()
for (i in top.models) {
  rf.feature.subset<-rf.feature.summaries[rf.feature.summaries$test %in% i,] 
  rf.feature.sorted <- rf.feature.subset[order(rf.feature.subset$mean, decreasing=TRUE),]
  top.rf.features <- rbind(top.rf.features, head(rf.feature.sorted[1:10,]))
}

rf.feature.summaries[rf.feature.summaries$test %in% top.models,]


##############
#  Figure 3A #
##############

hist(table(corDf$ANON_ID), col="red", breaks=200, xlab = "Number of Clinic Visits / Person",
     main = NULL, font.lab=2,lwd=2,font=2,lty="blank",
     xlim = c(0,350)) # dist. of clinic visits in 30k cohort
hist(table(corDf$ANON_ID)[table(corDf$ANON_ID)>50], col="red", breaks=200, xlab = "Number of Clinic Visits / Person",
     main = NULL, font.lab=2,lwd=2,font=2,lty="blank",
     xlim = c(50,350)) # dist. of clinic visits in 30k cohort
describe(as.matrix(table(corDf$ANON_ID))) # mean & median number visits in 30k cohort
mean(na.omit(corDf$Pulse)) # mean pulse 77.51
sd(na.omit(corDf$Pulse)) # sd pulse 14.12
mean(na.omit(corDf$Temp)) # mean temp 97.96
sd(na.omit(corDf$Temp)) # sd temp 0.50
length(na.omit(corDf$Pulse)) # 86,515
length(na.omit(corDf$Temp)) # 75,187

# duration of time monitored in 30K dataset:
maxDate <-as.Date(as.matrix(tapply(corDf$Clin_Result_Date, corDf$ANON_ID, max)))
minDate <- as.Date(tapply(corDf$Clin_Result_Date, corDf$ANON_ID, min))
duration <- as.numeric(maxDate-minDate)
withDuration <- cbind(as.data.frame(table(corDf$ANON_ID)), duration)
describe(duration) # mean & median number of days of monitoring in 30k cohort
hist(withDuration$duration, col="red", breaks=200, xlab = "Time Monitored by Clinic (Days)",
     main = NULL, font.lab=2,lwd=2,font=2, lty="blank")
hist(withDuration$duration[withDuration$Freq > 50], col="red", breaks=200, xlab = "Time Monitored by Clinic (Days)",
     main = NULL, font.lab=2,lwd=2,font=2, lty="blank")

#characterize the 30k data set
length(unique(corDf$ANON_ID)) # num people in 30k dataset where both labs and vitals exist
length(unique(labs$ANON_ID)) # num people in 30k dataset
length(na.omit(labs$Clin_Result_Date)) # num lab tests (in the 50 labs we explored) in 30k dataset
as.matrix(table(labs$LAB_NAME)) # number of each clinical lab
length(na.omit(vitals$Temp)) + length(na.omit(vitals$Pulse)) # total number of clinical vital signs measured
#304 people have more than 50 observations per person
length(table(corDf$ANON_ID)[table(corDf$ANON_ID)>50])

##############
#  Figure 3B #
##############
pdf(file = paste0(dir, "../Figure3/Figure3B_hists.pdf"))
par(mfrow = c(2,2))
hist(corDf$Pulse, col="tomato3", border="tomato4", breaks=50,
     xlab = "cHR", xlim=c(50,200),
     main = NULL, font.lab=2,lwd=2,font=2)
hist(corDf$Temp, col="turquoise3", border="turquoise4", breaks=5,
     xlab = "cTemp", xlim=c(65,105),
     main = NULL, font.lab=2,lwd=2,font=2)
dev.off()

##############
#  Figure 3XX #
##############
#30 K Univariate Correlation Fit Plots by Lukasz/Jessie
vitalVars <- which(names(corDf) %in% c("Pulse","Temp"))
allClin <- c("A1C","AG","ALB","ALKP","ALT","AST","BASO",
             "BASOAB","BUN","CA","CHOL","CHOLHDL","CL","CO2",
             "CR","EOS","EOSAB","ESR", "GLOB","GLU_byMeter",
             "GLU_fasting","GLU_nonFasting","GLU_SerPlas",
             "GLU_wholeBld","HCT","HDL",
             "HGB","HSCRP","IGM","K","LDL_Calc", "LDL_Direct","LDLHDL","LYM","LYMAB",
             "MCH","MCHC","MCV","MONO","MONOAB","NA.","NEUT",
             "NEUTAB","NHDL","PLT","PROCALCITONIN", "RBC","RDW","TBIL","TGL","TP","TroponinI","WBC")
clinVars <- which(names(corDf) %in% allClin)
#clin subset of the top 10 most predictive models from bivariate analysis:
# clinTopTen <- c("GLU_fasting","CR","HSCRP", "NEUTAB","NEUT","LYM", "RDW","ALB","AG", "PLT","PROCALCITONIN", "ESR")
# clinTopTen <- c("NA." , "NEUT", "HSCRP", "RBC", "LDLHDL", "ALB", "NHDL", "HGB", "GLU_fasting", "CL", "LYM")
clin.WBCs<- c("NEUT", "LYM", "BASO","MONO","EOS",
              "NEUTAB", "LYMAB", "BASOAB","MONOAB","EOSAB",) 
summary.pulse<-list()
summary.Temp<-list()
r.squared <-c()
plots <- list()
idx=0
for (j in clin.WBCs){
  idx=idx+1
  ## 30k All data scatterplots for Fig 3D and 3E
  pname <- paste0("Plot-",i)
  p<-ggplot(corDf, aes_string(y = corDf[[j]], x = corDf$Pulse)) +
    #ggplot(corDf, aes(y = corDf[[j]], x = corDf$Temp)) +
    stat_density_2d(aes(fill = ..level..), geom = 'polygon') +
    #scale_fill_viridis_c(name = "density") +
    geom_point(shape = '.') +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1.5, col="darkred") +
    theme(axis.title=element_text(face="bold",size="14"),axis.text=element_text(size=16,face="bold"), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    ylab(paste0(c(j ," Bin")))+
    xlab("cHR")
  ggsave(paste0(pname,".png"),p)
  plots[[idx]] = p   
}
p <- grid.arrange(grobs=plots,ncol=5)

clin.WBCs<- c("NEUT", "LYM", "BASO","MONO","EOS",
              "NEUTAB", "LYMAB", "BASOAB","MONOAB","EOSAB") 
#clin.WBCs<- c("HSCRP")
summary.pulse<-list()
summary.Temp<-list()
r.squared <-c()


## All points with curves
plots <- list()
idx=0
for (j in clin.WBCs){
  idx=idx+1
  ## 30k All data scatterplots for Fig 3D and 3E
  #pname <- paste0("Plot-",i)
  p<-ggplot(
    corDf, aes_string(y = corDf[[j]], x = corDf$Pulse)) +
    #corDf, aes_string(y = corDf[[j]], x = corDf$Temp)) +
    stat_density_2d(aes(fill = ..level..), geom = 'polygon') +
    #scale_fill_viridis_c(name = "density") +
    geom_point(shape = '.') +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1.5, col="darkred") +
    theme(axis.title=element_text(face="bold",size="14"),axis.text=element_text(size=16,face="bold"), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    ylab(paste0(c(j ," Bin")))+
    xlab("cHR")
  #ggsave(paste0(pname,".png"),p)
  plots[[idx]] = p   
}

p <- grid.arrange(grobs=plots,ncol=5)

## Just the binned curves
plots1 <- list()
plots2 <- list()
idx=0
for (j in clin.WBCs){
  idx=idx+1
  corDf$bin2<-ntile(corDf[[j]], 40)
  # for Temp - point plot of bin values (looks like line of points)
  # corDf2 <- summarySE(corDf, measurevar="Temp", groupvars="bin2", na.rm=TRUE)
  # p1<- ggplot(corDf2, aes(x=bin2, y=Temp)) +
  #   geom_point(stat="identity", fill="darkblue") +
  #   geom_errorbar(aes(ymin=Temp-se, ymax=Temp+se), width=.4) +
  #   xlab(paste(c(j, "bins", sep=" ")))+
  #   scale_y_continuous(limits = c(97,99))+
  #   theme(text = element_text(size=9),
  #         axis.text.x = element_text(angle = 60, hjust = 1))
  # # For Pulse
  corDf2 <- summarySE(corDf, measurevar="Pulse", groupvars="bin2", na.rm=TRUE)
  # barplot of bin values (looks like the line of points but with bars instead)
  # p1 <- ggplot(corDf2, aes(x=bin2, y=Pulse)) +
  #   geom_bar(stat="identity", fill="darkred") +
  #   geom_errorbar(aes(ymin=Pulse-se, ymax=Pulse+se), width=.2) +
  #   xlab(paste(c(j, "bins", sep=" "))) +
  #   theme(text = element_text(size=9),
  #         axis.text.x = element_text(angle = 60, hjust = 1))
  # print(paste0(j, ": number of data points in bin = ", sum(corDf$bin2 %in% "2")))
  # quadratic models of bins
  model <-lm(corDf2$Pulse  ~ corDf2$bin2 + I((corDf2$bin2)^2))
  p1 <- ggplot(corDf2, aes(y = bin2, x = Pulse)) +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1.5, col="darkred") +
    theme(axis.title=element_text(face="bold",size="11"), axis.text.x = element_text(angle = 60, hjust = 1), axis.text=element_text(size=11,face="bold"), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    #geom_point(col="black") +
    ylab(paste0(c(j ," Bin")))
  # summary.pulse <- summary(lm(corDf2$Pulse ~ corDf2$bin2 + I(corDf2$bin2^2)))
  # r.squared[j] <- summary.pulse$adj.r.squared
  corDf2 <- summarySE(corDf, measurevar="Temp", groupvars="bin2", na.rm=TRUE)
  p2<-ggplot(corDf2, aes(y = bin2, x = Temp)) +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1.5, col="darkblue") +
    #geom_point(col="black") +
    #ylim(c(96,98.5))+
    theme(axis.title=element_text(face="bold",size="11"), axis.text.x = element_text(angle = 60, hjust = 1), axis.text=element_text(size=11,face="bold"), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    ylab(paste0(c(j ," Bin")))
  # # summary.Temp <- summary(lm(corDf2$Temp ~ corDf2$bin2 + I(corDf2$bin2^2)))
  # # r.squared[j] <- summary.Temp$adj.r.squared
  plots1[[idx]] = p1
  plots2[[idx]] = p2
}
p <- grid.arrange(grobs=plots1,ncol=5)
p2 <- grid.arrange(grobs=plots2,ncol=5)
as.matrix(r.squared)


#####################
#  Figure 3F and 3G #
#####################

#for (i in clinTopTen){
i <- "NEUT"
pulse.diff <- c()
temp.diff <- c()
pulse.fourth.quartile <- c()
pulse.num.fourth.quartile <- c()
pulse.third.quartile <- c()
pulse.num.third.quartile <- c()
pulse.second.quartile <- c()
pulse.num.second.quartile <- c()
pulse.first.quartile <- c()
pulse.num.first.quartile <- c()
temp.fourth.quartile <- c()
temp.num.fourth.quartile <- c()
temp.third.quartile <- c()
temp.num.third.quartile <- c()
temp.second.quartile <- c()
temp.num.second.quartile <- c()
temp.first.quartile <- c()
temp.num.first.quartile <- c()
idx=0
ptm <- proc.time()
for (j in unique(corDf$ANON_ID)){
  idx=idx+1
  #create personalized quartiles for each person/measurement type; this step takes a very very long time
  person <- corDf[corDf$ANON_ID == j,]
  if (sum(!is.na(person[,i])) >= 4 & sum(!is.na(person$Pulse)) >= 4){
    print(paste0(idx, " : ", j))
    person$bins2 <- ntile(person[,i], 4)
    #get pulse values when the lab measurement for that person is in their lowest or highest quartile
    pulse.fourth.quartile[j] <- mean(person$Pulse[person$bins2 >= 4])
    pulse.num.fourth.quartile[j] <-length(person$Pulse[person$bins2 >= 4 ])
    pulse.third.quartile[j] <- mean(person$Pulse[person$bins2 >= 3  &  person$bins2 < 4 ])
    pulse.num.third.quartile[j] <- length(person$Pulse[person$bins2 >= 3  &  person$bins2 < 4 ])
    pulse.second.quartile[j] <- mean(person$Pulse[person$bins2 >= 2  &  person$bins2 < 3 ])
    pulse.num.second.quartile[j] <- length(person$Pulse[person$bins2 >= 2  &  person$bins2 < 3 ])
    pulse.first.quartile[j] <- mean(person$Pulse[person$bins2 <= 1 ])
    pulse.num.first.quartile[j] <-length(person$Pulse[person$bins2 <= 1 ])
    # make a way to save this for each i
    
    #get temp values when the lab measurement for that person is in their lowest or highest quantile
    temp.fourth.quartile[j] <- mean(person$Temp[person$bins2 >= 4 ])
    temp.num.fourth.quartile[j] <-length(person$Temp[person$bins2 >= 4 ])
    temp.third.quartile[j] <- mean(person$Temp[person$bins2 >= 3  &  person$bins2 < 4 ])
    temp.num.third.quartile[j] <- length(person$Temp[person$bins2 >= 3  &  person$bins2 < 4 ])
    temp.second.quartile[j] <- mean(person$Temp[person$bins2 >= 2  &  person$bins2 < 3 ])
    temp.num.second.quartile[j] <- length(person$Temp[person$bins2 >= 2  &  person$bins2 < 3 ])
    temp.first.quartile[j] <- mean(person$Temp[person$bins2 <= 1 ])
    temp.num.first.quartile[j] <-length(person$Temp[person$bins2 <= 1 ])
    # make a way to save this for each i
    
  }
}
proc.time() - ptm

personalQuartiles<-cbind(as.matrix(pulse.first.quartile), as.matrix(pulse.second.quartile), as.matrix(pulse.third.quartile),as.matrix(pulse.fourth.quartile),
                         as.matrix(temp.first.quartile), as.matrix(temp.second.quartile), as.matrix(temp.third.quartile), as.matrix(temp.fourth.quartile))
personalQuartiles <- personalQuartiles[(!is.na(personalQuartiles[,2]) & !is.na(personalQuartiles[,5])),] #remove NAs
#names(personalQuartiles) <- c("pulse.first.quartile", "pulse.second.quartile", "pulse.third.quartile", "pulse.fourth.quartile", "temp.first.quartile", "temp.second.quartile", "temp.third.quartile","temp.fourth.quartile")
pulse.diff<-as.matrix(personalQuartiles[,4] - personalQuartiles[,1])
hist(pulse.diff, breaks=100, col="darkred", main=paste0("Mean Pulse (1st - 4th quartile of ",i, " values)"))
boxplot(pulse.diff, col="darkred", outline=FALSE, main=paste0("Mean Pulse (1st - 4th quartile of ",i, " values)"))
temp.diff<-as.matrix(personalQuartiles[,8] - personalQuartiles[,5])
hist(temp.diff, breaks=100, col="darkblue", main=paste0("Mean Temp (1st - 4th quartile of ",i, " values)"))
boxplot(temp.diff, col="darkblue", outline=FALSE, main=paste0("Mean Temp (1st - 4th quartile of ",i, " values)"))


# data in play: temp.diff.neut and pulse.diff.neut and temp.diff.lym and pulse.diff.lym
hist(temp.diff.neut, breaks=100, main="Temperature Difference Between Personalized  4th and 1st Quartile of Neutrophil Levels", xlab="Temperature Difference", ylab="Number of Individuals", border="black", col="darkblue")
hist(pulse.diff.neut, breaks=100, main="Pulse Difference Between Personalized 4th and 1st Quartile of Neutrophil Levels", xlab="Pulse Difference", ylab="Number of Individuals", border="black", col="darkred")
hist(temp.diff.lym, breaks=100, main="Temperature Difference Between Personalized 4th and 1st Quartile of Lymphocyte Levels", xlab="Temperature Difference", ylab="Number of Individuals", border="black", col="darkblue")
hist(pulse.diff.lym, breaks=100, main="Pulse Difference Between Personalized 4th and 1st Quartile of Lymphocyte Levels", xlab="Pulse Difference", ylab="Number of Individuals", border="black", col="darkred")

write.csv(temp.diff.lym, "~/Desktop/tempdifflym.csv")
write.csv(pulse.diff.lym, "~/Desktop/pulsedifflym.csv")

length(temp.diff.neut[!is.na(temp.diff.neut)])
#  }
#}

##########################################
#    Figure 3XX; Suppl. Table 2 and 3        #
##########################################
# create ranked list of clinical laboratory tests by the correlation coefficients between observed and predicted values; checked by Jessie on 2017-12-20
# predicted values from simple bivariate models of (lab test ~ pulse + temp) using 30k dataset
# Do 10-fold cross validation at the subject level (e.g. each test set contains 1/10 of the people in the 28k dataset)
# RUN 30K CORRELATIONS between labs and vitals

names(corDf)[names(corDf) %in% "GLU_SerPlas"] <-"GLU"  # fix names to be same between iPOP and 30K datasets ; number of NAs for each GLU: GLU_nonFasting (113472), GLU_wholeBld (111726), GLU_SerPlas (30949), GLU_byMeter (NA = 101012), GLU_fasting (110303)
names(corDf)[names(corDf)  %in% "LDL_Calc"] <-"LDL"  # fix names to be same between iPOP and 30K datasets ; corDf$LDL_Calc range = wear$LDL range
options("scipen"=100, "digits"=4)
models=c(" ~ Pulse", # univariate with pulse only
         " ~ Temp",   # univariate with temp only
         " ~ Pulse + Temp", # bivariate with pulse + temp
         " ~ Pulse + I(Pulse^2)",
         " ~ Temp + I(Temp^2)", " ~ Pulse + I(Pulse^2) + Temp + I(Temp^2)" )
cv.runs <- 50
models.corr.coefs <- c()
models.pct.var <- c()

for (i in 1:cv.runs){ #50 fold cross validation (10% test set; 90% training set)
  print(i)
  ANON_ID = corDf$ANON_ID # Remember the list of subjects
  corDf.tmp = corDf[,-c(1,2)]  #remove ANON_ID and Clin_Result_Date
  corDf.tmp <- subset(corDf.tmp, select=-c(ALCRU, CR)) # all values for ALCRU tests are NA, only 20 values for CR are not NA
  nms = names(subset(corDf.tmp, select=-c(Pulse, Temp)))

  # Do cross-validation
  subjects = unique(ANON_ID)
  n = length(subjects) # total num of observations
  test = sample(n)[1:floor(n*0.1)] # 10% of subjects are held for testing
  test.subj = subjects[test]
  test.mask = ANON_ID %in% test.subj

    for (nm in top.names){ # for each of the 50 clinical lab tests
      print(nm)
      tmp=0
      corDf2 = data.frame(labtest = corDf.tmp[[nm]], Pulse = corDf.tmp$Pulse, Temp = corDf.tmp$Temp) # prepare data for LM
      #df <- cbind(corDf2[[i]], corDf2[,c("Pulse", "Temp")])
      corDf2 <- na.omit(corDf2)
      test.data <- na.omit(corDf2[test.mask,])
      train.data <-na.omit(corDf2[!test.mask,])
        for (k in 1:length(models)){
          model<-lm(as.formula(paste0("labtest",models[k])),data=train.data)
          m <- summary(model) # quadratic univariate with pulse or temp only
          model.null <- lm(as.formula(paste0("labtest"," ~ 1")),data=train.data)
          # r[tmp,tmp2]<-m$adj.r.squared # matrix of r-squared values for each left-one-out model
          # p[tmp,tmp2]<-1-pf(m$fstatistic[1],m$fstatistic[2],m$fstatistic[3]) # matrix of p-squared values for each left-one-out model
          numTrainObs<-length(train.data$Pulse) # train: the number of each clinical lab test that has corresponding vital signs
          numTestObs<-length(test.data[,1]) #  test: the number of each clinical lab test that has corresponding vital signs
          pred=predict(model, newdata=test.data)# prediction on test data set
          pred.null=predict(model.null, newdata=test.data)# prediction on test data set
          #rsq.pred = 1 - (mean( pred - test.data[,1])**2 ) / var( (test.data[,1]) ) # test r.sq
          if (length(pred)<1){next}
          r.pred = cor(pred, test.data[,1], use = "complete.obs") # test r.sq
          rssm <- sum((test.data[,1] - pred)^2)
          rss0 <- sum((test.data[,1]- pred.null)^2)
          sqrt.pct.var <- sqrt(1- (rssm/rss0))
          name.rsq <- paste("model.mean.rsq", k, sep = ".")
          models.corr.coefs <- rbind(models.corr.coefs,
                                     c(model = name.rsq, cv.step = i, test = nm, corr.coef = r.pred, sqrt.pct.var = sqrt.pct.var, numTestObs = numTestObs, numTrainObs = numTrainObs))
    }
  }
}

# models.corr.coefs <-read.csv("/Users/jessilyn/Desktop/framework_paper/Figure3/Fig3C/20180503_pct_var_30k_noDemog.csv")
corr.coefs <- as.data.frame(models.corr.coefs)
corr.coefs$cv.step <- as.numeric(as.character(corr.coefs$cv.step))
corr.coefs$corr.coef <- as.numeric(as.character(corr.coefs$corr.coef))
corr.coefs$sqrt.pct.var <- as.numeric(as.character(corr.coefs$sqrt.pct.var))

library(dplyr)
model.corr.coefs <- (corr.coefs %>%
  group_by(test, model) %>% 
  summarise_at(vars("sqrt.pct.var"), funs(mean,sd)))
model.corr.coefs$model <- mapvalues(model.corr.coefs$model, from = c("model.mean.rsq.1", "model.mean.rsq.2", "model.mean.rsq.3", "model.mean.rsq.4", "model.mean.rsq.5", "model.mean.rsq.6"), to = c("~ Pulse", "~ Temp", "~ Pulse + Temp", " ~ Pulse + P^2", " ~ Temp + T^2", " ~ Pulse + P^2 + Temp + T^2"))
model.corr.coefs <- na.omit(model.corr.coefs)
model.corr.coefs$test  = factor(model.corr.coefs$test, levels=pull(model.corr.coefs[order(-model.corr.coefs$mean),][,1]))
model.corr.coefs$mean <- pmax(model.corr.coefs$mean, 0)

# plot how rsq changes with the different models, and add in error bars from sd.plot
ggplot(model.corr.coefs, aes(x=test, y=mean, group=model, col=as.factor(model.corr.coefs$model))) +
  theme(legend.title = element_blank())+
  geom_point() +
  #guides(fill=guide_legend(title="Model")) +
  xlab("Clinical Laboratory Test") + 
  #ylab(expression(atop("Cross-Validated", paste( "Cor Coef (+/- SD)")))) +
  ylab(expression(atop("Cross-Validated", paste( "Sqrt of % Variance Explained (+/- SD)")))) +
  theme(axis.title=element_text(face="bold",size="12"),axis.text=element_text(size=12,face="bold"), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 60, hjust = 1),
        axis.text.y = element_text(hjust = 1)) +
  ylim(0,0.5) +
  scale_fill_discrete(name="Model")+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.5)
  #, position=position_dodge(.7))

write.table(models.corr.coefs, "/Users/jessilyn/Desktop/framework_paper/Figure3/Fig3C/20180504_pct_var_30k_noDemog.csv",row.names=FALSE,col.names=TRUE, sep=",")
#write.table(models.corr.coefs, "../SECURE_data/20180503_pct_var_30k_noDemog.csv",row.names=FALSE,col.names=FALSE, sep=",")

#############################################################
#    Figure 3C + Demographics + BloodPressure + Respiration #
#############################################################
library(caret)
library(plyr)
#function to pull out lm model p-values
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}
names(corDf)[names(corDf) %in% "GLU_SerPlas"] <-"GLU"  # fix names to be same between iPOP and 30K datasets ; number of NAs for each GLU: GLU_nonFasting (113472), GLU_wholeBld (111726), GLU_SerPlas (30949), GLU_byMeter (NA = 101012), GLU_fasting (110303)
names(corDf)[names(corDf)  %in% "LDL_Calc"] <-"LDL"  # fix names to be same between iPOP and 30K datasets ; corDf$LDL_Calc range = wear$LDL range
corDf.demog <- merge(thirtyKdemog, corDf, by="ANON_ID")
corDf.demog$Gender <- as.factor(corDf.demog$Gender)
corDf.demog$Ethn <- as.factor(corDf.demog$Ethn)

# make sure you have sufficient # of tests
summaries <- summary(corDf.demog) 
to.remove <-c() 
for (i in 6:dim(summaries)[2]){
  if ( #as.numeric(unlist(strsplit(summaries[7,][i], ":"))[2]) != "NA" &
    as.numeric(unlist(strsplit(summaries[7,][i], ":"))[2]) > (dim(corDf.demog)[1] - .005*dim(corDf.demog)[1])){ #remove anything that is missing X% of our total # of observations 
    print(i)
    to.remove <- c(to.remove, names(summaries[7,][i]))
  }
}
to.remove <- gsub("\\s", "", to.remove)
corDf.demog <- corDf.demog[ , -which(names(corDf.demog) %in% c(to.remove))]

cv.runs <- 50
folds <- createFolds(factor(corDf.demog$Ethn), k = cv.runs, list = FALSE) # break data into (cv.runs) folds with equal proportion of ethnicities in each fold - if it becomes unbalanced sometimes one ethnicity will appear in training and note in test and it breaks the pipeline

#check that your folds work the way you expect
corDf.demog$cv.folds <- folds; # ddply(corDf.demog, 'cv.folds', summarise, prop=sum(Ethn=="White")) # check that this equals table(corDf.tmp$Ethn) / cv.runs

options("scipen"=100, "digits"=4)
models=c(" ~ Pulse", # univariate with pulse only
         " ~ Temp", # univariate with temp only
         " ~ Systolic", # univariate with sys only
         " ~ Diastolic",   # univariate with dias only
         " ~ Respiration", # univariate with resp only
         " ~ Pulse + Temp + Systolic + Diastolic + Respiration", # this is the total possible info we can gain from vitals
         " ~ Age + Gender + Ethn", # this is the total possible info we can gain from demog
         " ~ Pulse + Temp + Systolic + Diastolic + Respiration + Age + Gender + Ethn") # this is the total possible info we can gain from vitals and demog

         # " ~ Systolic + Diastolic + Respiration + Age + Gender + Ethn", # trivariate - this is the info we are losing by not having a wearable that measures these things
         # " ~ Pulse + I(Pulse^2) + Temp + I(Temp^2)
         # + Systolic + I(Systolic^2) + Diastolic + I(Diastolic^2) + Respiration + I(Respiration^2) + Age + Gender + Ethn" ) # this is the total possible info we can gain from vitals

# for random forest:
models=c(" ~ Pulse + Temp + Systolic + Diastolic + Respiration", # this is the total possible info we can gain from vitals
         " ~ Pulse + Temp + Systolic + Diastolic + Respiration + Age + Gender + Ethn") # this is the total possible info we can gain from vitals and demog

models.corr.coefs <- c()
cv.runs = 1
nms
for (i in 1:cv.runs){ #50 fold cross validation (10% test set; 90% training set)
  print(i)
  corDf.tmp = corDf.demog[corDf.demog$cv.folds==i,]  #remove ANON_ID and Clin_Result_Date & demographics
  # ANON_ID = corDf.tmp$ANON_ID # Remember the list of subjects
  corDf.tmp = corDf.tmp[,-c(1,5)]  #remove ANON_ID and Clin_Result_Date & demographics
  nms = names(subset(corDf.tmp, select=-c(Pulse, Temp, Systolic, Diastolic, Respiration, Age, Gender, Ethn)))
  
  # Do stratified cross-validation per subject
  # split into training and test
  #folds <- createFolds(factor(corDf.tmp$Ethn), k = 10, list = FALSE) # break data into (10% training, 90% test) folds with equal proportion of ethnicities in each fold - if it becomes unbalanced sometimes one ethnicity will appear in training and note in test and it breaks the pipeline
  # for random forest
  folds <- createFolds(factor(corDf.tmp$Ethn), k = 3000, list = FALSE) # ~50 entries per fold; folds with equal proportion of ethnicities in each fold - if it becomes unbalanced sometimes one ethnicity will appear in training and note in test and it breaks the pipeline
  corDf.tmp$test.train <- folds
  
  # subjects = unique(ANON_ID)
  # n = length(subjects) # total num of observations
  # test = sample(n)[1:floor(n*0.1)] # 10% of subjects are held for testing
  # test.subj = subjects[test]
  # test.mask = ANON_ID %in% test.subj
  
  for (nm in nms){ # for each of the 50 clinical lab tests
    print(nm)
    #tmp=0
    corDf2 = data.frame(labtest = corDf.tmp[[nm]], Pulse = corDf.tmp$Pulse, Temp = corDf.tmp$Temp,
                        Systolic = corDf.tmp$Systolic, Diastolic = corDf.tmp$Diastolic, Respiration = corDf.tmp$Respiration,
                        Age = corDf.tmp$Age, Gender = corDf.tmp$Gender, Ethn = corDf.tmp$Ethn, test.train = corDf.tmp$test.train
                         ) # prepare data for LM
    #df <- cbind(corDf2[[i]], corDf2[,c("Pulse", "Temp")])
    corDf2 <- na.omit(corDf2)
    # train.data <- corDf2[corDf2$test.train<9,] 
    # test.data <-corDf2[corDf2$test.train==10,] # training set is ~10% of total set, but not exactly because it is balancing by ethnicities
    # 
    # for random forest
    train.data <- corDf2[corDf2$test.train<180,] # 60% training, 40% test data
    test.data <-corDf2[corDf2$test.train>=180 & corDf2$test.train<300,] # training set is ~10% of total set, but not exactly because it is balancing by ethnicities
    #train.data<-sample(train.data, 450, replace=FALSE) # comment out if not doing RF
    
    t<-as.data.frame(table(train.data$Ethn)) # if there is an ethnicity that has zero entries in the training data
    test.data <- test.data[!(test.data$Ethn %in% as.character(t[t[,2]<1,][,1])),] #remove that ethnicity from the test data
    for (k in 1:length(models)){
      # model<-lm(as.formula(paste0("labtest",models[k])),data=train.data)
      # p.val <- lmp(model)
      # m <- summary(model) # quadratic univariate with pulse or temp only
      # model.null <- lm(as.formula(paste0("labtest"," ~ 1")),data=train.data)
      
      #for random forest
      model<-randomForest(as.formula(paste0("labtest",models[k])),data=train.data)
      model.null <- lm(as.formula(paste0("labtest"," ~ 1")),data=train.data)
      
      # r[tmp,tmp2]<-m$adj.r.squared # matrix of r-squared values for each left-one-out model
      # p[tmp,tmp2]<-1-pf(m$fstatistic[1],m$fstatistic[2],m$fstatistic[3]) # matrix of p-squared values for each left-one-out model
      numTrainObs<-length(train.data$Pulse) # train: the number of each clinical lab test that has corresponding vital signs
      numTestObs<-length(test.data[,1]) # test: the number of each clinical lab test that has corresponding vital signs
      pred=predict(model, newdata=test.data)# prediction on test data set
      pred.null=predict(model.null, newdata=test.data)# prediction on test data set
      #rsq.pred = 1 - (mean( pred - test.data[,1])**2 ) / var( (test.data[,1]) ) # test r.sq
      if (length(pred)<1){next}
      r.pred = cor(pred, test.data[,1], use = "complete.obs") # test r.sq
      rssm <- sum((test.data[,1] - pred)^2)
      rss0 <- sum((test.data[,1]- pred.null)^2)
      sqrt.pct.var <- sqrt(1- (rssm/rss0))
      if ((1- (rssm/rss0)) <0) {sqrt.pct.var <- 0}
      name.rsq <- paste("model.mean.rsq", k, sep = ".")
      # for lm, add in pval = p.val below
      models.corr.coefs <- rbind(models.corr.coefs,
                                 c(model = name.rsq, cv.step = i, test = nm, corr.coef = r.pred, sqrt.pct.var = sqrt.pct.var, numTestObs = numTestObs, numTrainObs = numTrainObs))
    }
  }
}

#20180506_model_compare_30k_withDemog.csv
# 20180801_30k_RF_noIDnoDemog.csv
write.table(models.corr.coefs, "/Users/jessilyn/Desktop/framework_paper/Figure3/Fig3C/20180804_cVS_noID_RF_train_test_60_40.csv",row.names=FALSE,col.names=TRUE, sep=",")

corr.coefs <- as.data.frame(models.corr.coefs)
corr.coefs$cv.step <- as.numeric(as.character(corr.coefs$cv.step))
corr.coefs$corr.coef <- as.numeric(as.character(corr.coefs$corr.coef))
corr.coefs$sqrt.pct.var <- as.numeric(as.character(corr.coefs$sqrt.pct.var))
corr.coefs$numTestObs <- as.numeric(as.character(corr.coefs$numTestObs))
corr.coefs$numTrainObs <- as.numeric(as.character(corr.coefs$numTrainObs))
mean.numTrainObs <- (corr.coefs %>%
                       group_by(test, model) %>% 
                       summarise_at(vars("numTrainObs"), funs(mean)))
mean.numTestObs <- (corr.coefs %>%
                       group_by(test, model) %>% 
                       summarise_at(vars("numTestObs"), funs(mean)))
meanNumObs <- merge(mean.numTrainObs, mean.numTestObs, by = c("test", "model"))
write.table(meanNumObs, "/Users/jessilyn/Desktop/framework_paper/Figure3/Fig3C/20180804_NumObs_summarized_cVS_noID_RF_train_test_60_40.csv",row.names=FALSE,col.names=TRUE, sep=",")

library(dplyr)
## plot corr coefs or sqrt pct var explained (need to change code to plot second one)
model.corr.coefs <- (corr.coefs %>%
                       group_by(test, model) %>% 
                       summarise_at(vars("sqrt.pct.var"), funs(mean,sd))) # change to summarise_at(vars("sqrt.pct.var") or "corr.coef"
model.corr.coefs$model <- mapvalues(model.corr.coefs$model, from = c("model.mean.rsq.1", "model.mean.rsq.2"), 
                                    #from = c("model.mean.rsq.1", "model.mean.rsq.2", "model.mean.rsq.3", "model.mean.rsq.4", "model.mean.rsq.5", "model.mean.rsq.6", "model.mean.rsq.7", "model.mean.rsq.8"), 
                                    to = c("~ All Vitals", "~ All Vitals + Demographics"))
                                    # "~ Pulse","~ Temp","~ Systolic", "~ Diastolic", "~ Respiration", "~ All Vitals", "~ Demographics", "~ All Vitals + Demographics"))
                                    # to = c("~ Systolic", "~ Diastolic", "~ Respiration", "~ Systolic + Diastolic + Respiration", "~ Pulse + P^2 + Temp + T^2 + Systolic + S^2) + Diastolic + D^2 + Respiration + R^2"))
model.corr.coefs <- na.omit(model.corr.coefs)
model.corr.coefs$test  = factor(model.corr.coefs$test, levels=pull(model.corr.coefs[order(-model.corr.coefs$mean),][,1]))
model.corr.coefs$mean <- pmax(model.corr.coefs$mean, 0)
model.corr.coefs.top.names <- model.corr.coefs[model.corr.coefs$test %in% top.names,]

write.table(model.corr.coefs.top.names, "/Users/jessilyn/Desktop/framework_paper/Figure3/Fig3C/20180804_summarized_cVS_noID_RF_train_test_60_40.csv",row.names=FALSE,col.names=TRUE, sep=",")

# plot how rsq changes with the different models, and add in error bars from sd.plot
ggplot(model.corr.coefs.top.names, aes(x=test, y=mean, group=model, col=as.factor(model.corr.coefs.top.names$model))) +
  theme(legend.title = element_blank())+
  geom_point(position=position_dodge(.05)) +
  #guides(fill=guide_legend(title="Model")) +
  xlab("Clinical Laboratory Test") + 
  #ylab(expression(atop("Cross-Validated", paste( "Cor Coef (+/- SD)")))) + # if corr coefs
  ylab(expression(atop("Cross-Validated", paste( "Sqrt of % Variance Explained (+/- SD)")))) + # if sqrt pct var
  theme(axis.title=element_text(face="bold",size="12"),axis.text=element_text(size=12,face="bold"), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 60, hjust = 1),
        axis.text.y = element_text(hjust = 1)) +
  ylim(0,0.7) +
  scale_fill_discrete(name="Model")+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.5, position=position_dodge(.05))


# Delta Corr Coeff between Demographics Only and Vitals + Demographics:
# Which clinical lab models benefit the most by the addition of vital signs?
delta.corr.coef <- model.corr.coefs.top.names[model.corr.coefs.top.names$model %in% "~ All Vitals + Demographics",][,3] - model.corr.coefs.top.names[model.corr.coefs.top.names$model %in% "~ Demographics",][,3]
delta.corr.coef<-cbind(as.data.frame(model.corr.coefs.top.names[model.corr.coefs.top.names$model %in% "~ All Vitals + Demographics",][,1]), delta.corr.coef)
delta.corr.coef$test  = factor(delta.corr.coef$test, levels=delta.corr.coef[order(-delta.corr.coef$mean),][,1])
ggplot(delta.corr.coef, aes(x=test, y=mean))+
  geom_point() +
  theme(legend.title = element_blank())+
  xlab("Clinical Laboratory Test") + 
  #ylab(expression(atop("Delta Cor Coef ", paste( "(~ Vitals + Demog ) - (~ Demog )")))) + # if corr coefs
  ylab(expression(atop("Delta Sqrt % Var Explained ", paste( "(~ Vitals + Demog ) - (~ Demog )")))) + 
  theme(axis.title=element_text(face="bold",size="12"),axis.text=element_text(size=12,face="bold"), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 60, hjust = 1),
        axis.text.y = element_text(hjust = 1))
  #ylim(0,0.5)
  
## get mean RPVE for each model:

allModelResults <- read.csv("/Users/jessilyn/Desktop/framework_paper/Figure3/Fig3C/20180505_model_compare_30k_withDemog.csv",
         header =TRUE, stringsAsFactors = FALSE)
models <- unique(allModelResults$model)

# mean RVPE for the univariate models 
for (i in models){
print(i)
sub <- allModelResults[allModelResults$model %in% i,]
print(c("mean" , mean(sub$sqrt.pct.var)))
print(c("sd" , sd(sub$sqrt.pct.var)))
print(c("numTestObs", mean(sub$numTestObs)))
print(c("numTrainingObs", mean(sub$numTrainObs)))
}

# increase in RVPE from demog-only to cVS + demog for top models:
delta.corr.coef[order(delta.corr.coef$mean, decreasing = TRUE),]

source("experiment/individual-models.R")

###############
#  Figure 5A #
###############
# Visits vs R^2
generate5A = function(clin,dataset = "30k",min_visits=10,cap=200){
  if (dataset == "iPOP"){
    identifier = "iPOP_ID"
    corDf.tmp = iPOPcorDf[!is.na(iPOPcorDf[[clin]]),]
  }
  else{
    identifier = "ANON_ID"
    corDf.tmp = corDf[!is.na(corDf[[clin]]),]
  }
  
  corDf.tmp = corDf.tmp[!is.na(corDf.tmp[["Pulse"]]),]
  corDf.tmp = corDf.tmp[!is.na(corDf.tmp[["Temp"]]),]
  
  # Here we select people with the largest number of observations
  toppat = table(corDf.tmp[[identifier]])
  toppat = toppat[toppat > min_visits]
  toppat = names(toppat)
  toppat = toppat[1:min(cap,length(toppat))]
  
  dd = corDf.tmp[corDf.tmp[[identifier]] %in% toppat ,c(identifier,"Temp","Pulse",clin,"Clin_Result_Date")]
  
  # Compute R for individual models
  res = c()
  for (pat in toppat){
    frm = paste0(clin," ~ Pulse + Pulse^2 + Temp + Temp^2")
    
    # Crossval
    allpreds = c()
    for (i in 1:10){
      ind.data = dd[dd[[identifier]] == pat,]
      nsmpl = nrow(ind.data)
      train.idx = sample(nsmpl)[1:floor(0.8 * nsmpl)]
      model = lm(frm, data = ind.data[train.idx,])
      preds = predict(model,ind.data[-train.idx,])
      
      allpreds = rbind(allpreds, cbind(ind.data[-train.idx,clin], preds))
    }
    
    err = sqrt(1 - var(allpreds[,1] - allpreds[,2])/var(allpreds[,1] - mean(allpreds[,1])))
    vis = sum(dd[[identifier]] == pat)
    span = max(as.Date(ind.data$Clin_Result_Date)) - min(as.Date(ind.data$Clin_Result_Date))
    res = rbind(res, c(vis, span, err))
  }
  dres = data.frame(span = res[,2]/365, r = res[,3])
  ggplot(dres, aes(span, r)) + 
    weartals_theme + theme(text = element_text(size=25)) +
    geom_point(size=2) + 
    geom_smooth(method="lm", formula = y ~ x, size=1)
  ggsave(paste0("plots/Figure-5A-",clin,"-",dataset,".png"),width = 9,height = 6,units = "in")
}
generate5A("CHOL","30k",cap=100,min_visits = 10)

#####################################
#   Figure 5A (Timecourse from 2D)  #
#####################################

#Run after running individual time course from 2C (reading in various files for wear by timespans) that produced pct_var, corr_coeffs, & num_Records files   #
weartals_theme = theme_bw() + theme(text = element_text(size=18), panel.border = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
# read in each of the corr_coeffs from the different time windows
# had to manually add back in headers in the with demog files that I ran on scg: 20180327_corr_coeffs_AllData.csv , 20180327_corr_coeffs_MonthPrior.csv, 20180327_corr_coeffs_TwoWeekPrior.csv, 20180327_corr_coeffs_WeekPrior.csv,20180327_corr_coeffs_DayPrior.csv, 20180327_corr_coeffs_ThreeDayPrior.csv, 
# save as pdf 4x12.5"
# data <-read.table("/Users/jessilyn/Desktop/framework_timecourse/with_resting_bugfix_and_demographics/20180420_corr_coeffs_AllData_demog.csv",
#                   header=TRUE,sep=',',stringsAsFactors=FALSE)
fig.2c.df <-read.csv("/Users/jessilyn/Desktop/framework_timecourse/with_restingbugfix_demog_pctdev/20180507_AllData_pct_var.csv",
                     header=TRUE,sep=',',stringsAsFactors=FALSE)
fig.2c.plot <- melt(fig.2c.df)
fig.2c.plot[,3][is.nan(fig.2c.plot[,3])] <- 0 #replace % var explained of NaN w/ 0
fig.2c$test <- fig.2c.plot[order(-fig.2c.plot[,3]),] # reorder by LM Vitals
fig.2c$test = factor(fig.2c$test, levels = order(-fig.2c.plot[,3]))

# Plot the % var explained
ggplot(fig.2c, aes(x=test, y=value, color = variable)) + geom_point(size = 5, aes(shape=variable, color=variable)) +
  weartals_theme +
  ylim(0,1) +
  scale_shape_discrete(breaks=c("vitals", "lasso", "rf"),
                       labels=c("LM vitals", "LASSO", "RF")) +
  scale_color_discrete(breaks=c("vitals", "lasso", "rf"),
                       labels=c("LM vitals", "LASSO", "RF")) +
  labs(x = "Lab tests",y = expression(paste("Sqrt of % Variance Explained"))) 


### THE PART BELOW IS UNDER CONSTRUCTION
# Look at differences between DayPrior and AllData
data.allData <-read.table("/Users/jessilyn/Desktop/framework_timecourse/with_restingbugfix_demog_pctdev/20180503_pct_var_Dayprior.csv",
                          header=TRUE,sep=',',stringsAsFactors=FALSE)
data.dayPrior <-read.table("/Users/jessilyn/Desktop/framework_timecourse/with_restingbugfix_demog_pctdev/20180507_AllData_pct_var.csv",
                           header=TRUE,sep=',',stringsAsFactors=FALSE)
#data.dayPrior$model<- gsub("vitals", "vitals.ipop", data$model)
#data$r_squared <- pmax(data$r_squared, 0)
#data.nodemog$r_squared <- pmax(data$r_squared, 0)
#df <- merge(data.dayPrior, data.allData, by = c("model", "test"))
#colnames(df)[3:4] <- c("DayPrior", "AllData")
df <- data.dayPrior[,3:4] - data.allData[,3:4]
df2 <- melt(df)
df2<- df2[order(-df2$value),]
df2<-cbind(data.dayPrior[,1], df2)
colnames(df2)<-c("test", "model", "pct.var")
ggplot(df2, aes(test,pct.var, color = model)) + geom_point(size = 5, aes(shape=model, color=model)) +
  weartals_theme + 
  ylim(-0.5,0.5) +
  scale_shape_discrete(breaks=c("rf", "lasso"),
                       labels=c("RF", "LASSO")) +
  scale_color_discrete(breaks=c("rf", "lasso"),
                       labels=c("RF", "LASSO")) +
  labs(x = "Lab tests",y = expression(atop("Increase in Corr Coeff", paste("by using Day Prior vs All Watch Data"))))

### THE PART BELOW IS UNDER CONSTRUCTION
# make the case that if we could do the RF etc on all data (dont need to be individualized models) and we could combine the individualized models we could do an awesome job at preciting the clinical labs.
# can we create one more layer of mixed effects models in the iPOP analysis here?

# Look at differences between with and without demog
data.nodemog <-read.table("/Users/jessilyn/Desktop/framework_timecourse/with_resting_bugfix_no_demographics/20180420_corr_coeffs_TwoWeekPrior.csv",
                          header=TRUE,sep=',',stringsAsFactors=FALSE)
data <-read.table("/Users/jessilyn/Desktop/framework_timecourse/with_resting_bugfix_and_demographics/20180420_corr_coeffs_TwoWeekPrior_demog.csv",
                  header=TRUE,sep=',',stringsAsFactors=FALSE)
data$model<- gsub("vitals", "vitals.ipop", data$model)
#data$r_squared <- pmax(data$r_squared, 0)
#data.nodemog$r_squared <- pmax(data$r_squared, 0)
df <- merge(data, data.nodemog, by = c("model", "test"))
colnames(df)[3:4] <- c("r_w_demog", "r_no_demog")
df$delta <- df[,3] - df[,4]
df<- df[order(-df$delta),]

ggplot(df, aes(test,delta, color = model)) + geom_point(size = 5, aes(shape=model, color=model)) +
  weartals_theme + 
  ylim(-0.5,0.5) +
  scale_shape_discrete(breaks=c("all-rf", "lasso-rf", "all-lm", "lasso-lm", "vitals"),
                       labels=c("RF all variables", "RF + LASSO", "LM all variables", "LM + LASSO", "LM vitals")) +
  scale_color_discrete(breaks=c("all-rf", "lasso-rf", "all-lm", "lasso-lm", "vitals"),
                       labels=c("RF all variables", "RF + LASSO", "LM all variables", "LM + LASSO", "LM vitals")) +
  labs(x = "Lab tests",y = expression(atop("Increase in Corr Coeff", paste("by adjusting for demographics"))))

###############
#  Figure 5B #
###############
getEvents = function(dres, codes)
{
  dres$date = as.Date(dres$date)
  dres$change = c(0, dres$slope[2:length(dres$slope)] - dres$slope[2:length(dres$slope) - 1])
  dres$change = abs(dres$change)
  dres$time_diff = c(0, dres$date[2:length(dres$slope)] - dres$date[2:length(dres$slope) - 1])
  newpat = c(TRUE,dres$identifier[2:length(dres$slope)] != dres$identifier[2:length(dres$slope) - 1])
  dres = dres[!newpat,]
  dres = dres[order(-dres$change),]
  
  res.events = c()
  
  for(i in 1:10){
    events = codes[as.character(dres$identifier[i]) == codes$ANON_ID,]
    events$dist = abs(events$date - dres$date[i])
    events = events[order(events$dist),]
    res.events = rbind(res.events, events[1:10,])
  }
  res.events
}


# Temporal evolution of the estimate of the mean
generate5C = function(clin,vit,dataset = "30k",window=50,filter = NULL,col = 1)
  {
  if (dataset == "iPOP"){
    identifier = "iPOP_ID"
    corDf.tmp = iPOPcorDf[!is.na(iPOPcorDf[[clin]]),]
  }
  else{
    identifier = "ANON_ID"
    corDf.tmp = corDf[!is.na(corDf[[clin]]),]
  }
  
  corDf.tmp = corDf.tmp[!is.na(corDf.tmp[[vit]]),]
  if (!is.null(filter)){
    rows = corDf.tmp[[identifier]] %in% filter
    corDf.tmp = corDf.tmp[rows,]
  }

  # Here we select people with the largest number of observations
  toppat = table(corDf.tmp[[identifier]])
  toppat = names(sort(-toppat))[1:length(filter)]

  dd = corDf.tmp[corDf.tmp[[identifier]] %in% toppat, c(identifier,"Clin_Result_Date",vit,clin)]
  
  dates = c()
  slopes = c()
  rsquared = c()
  ids = c()
  vtrue = c()
  vpred = c()
  vitals = c("Pulse","Temp","Systolic","Diastolic","Respiration")
  
  for (pat in toppat){
    dd.pat = dd[dd[[identifier]] == pat,]
    for (i in (window+1):(nrow(dd.pat)-1) ){
      model = lm(formula(paste(clin,"~",paste(vitals,sep=" + "))), data = dd.pat[(i-window):i, ])
      slopes = c(slopes, model$coefficients[1])
      rsquared = c(rsquared, sqrt(summary(model)$r.squared))
      dates = c(dates, dd.pat$Clin_Result_Date[i])
      vtrue = c(vtrue, dd.pat[i+1,clin])
      vpred = c(vpred, predict(model, newdata = dd.pat[i+1,]) )
      ids = c(ids, pat)
    }
  }
  
  dres = data.frame(date = as.Date(as.POSIXct(dates)), slope = slopes, identifier = ids, rsquared = rsquared, true = vtrue,
                    predicted = vpred)
  dres$time = as.numeric(difftime(dres$date,min(dres$date),units="days")) / 365.0
  
  dres.pred = gather(dres, values, measurement, true:predicted, factor_key=TRUE)
  
  plt_pred = ggplot(dres.pred, aes(time, measurement, group = values, color = values)) + 
    weartals_theme + theme(text = element_text(size=25)) +
    geom_line(size=1.3) +
    geom_point(size=2) 
  print(plt_pred)
  
  plt_slope = ggplot(dres, aes(time, slope, group = identifier, color = identifier)) + 
    weartals_theme + theme(text = element_text(size=25)) +
    geom_line(size=1.3) +
    geom_point(size=2) 
  write.table(dres, file=paste0("data/Figure-5C-",clin,"-",vit,"-",pat,".csv"),sep = ',')
  
  cols = gg_color_hue(3)
  
  
  plt_rsq = ggplot(dres, aes(time, rsquared, group = identifier, color = identifier)) + 
    weartals_theme + theme(text = element_text(size=25)) +
    ylab(expression(sqrt("Variance explained")))
  
  if (dataset != "iPOP"){
    plt_rsq = plt_rsq + geom_point(size=4, colour=cols[col]) +
      geom_line(size=2, colour=cols[col]) 
  }
  else{
    plt_rsq = plt_rsq + geom_point(size=4) +
      geom_line(size=1.3) + ylim(c(0,0.6)) + theme(legend.position = "none")
  }
  
  events = NULL
  if (dataset != "iPOP")
    events = getEvents(dres, codes)
  
  if (dataset == "iPOP"){
    ggsave(paste0("plots/Figure-5C-",clin,'-',vit,"-",window,"-",dataset,"-slopes.png"), 
          plot = plt_slope, width = 6, height = 6,units = "in")
    ggsave(paste0("plots/Figure-5C-",clin,'-',vit,"-",window,"-",dataset,"-rsqured.png"),
           plot=plt_rsq,width = 6, height = 6,units = "in")
    ggsave(paste0("plots/Figure-5C-",clin,'-',vit,"-",window,"-",dataset,"-predictions.png"),
           plot=plt_pred,width = 6, height = 6,units = "in")
  }
  list(dres = dres, plt_slope = plt_slope, plt_rqs = plt_rsq, plt_pred = plt_pred, events = events)
}

visits = aggregate(iPOPcorDf$iPOP_ID,by=list(iPOPcorDf$iPOP_ID),length)
visits = visits[order(-visits$x),]
pats = visits[1:1,1]
dres = generate5C("HCT","Pulse","iPOP",
                  window = 30,filter=pats,1)

## Load codes (initial)
codes = read.csv("../SECURE_data/SECURE/initial_MI.csv",stringsAsFactors=FALSE,header = FALSE)
colnames(codes) = c("ANON_ID","ICD_DATE","ICD9","ICD10","DX_NAME")
codes$date = as.Date(as.POSIXct(codes$ICD_DATE,format="%d-%b-%Y")) + 1 # POSIX no time mapped by Date to the previous day so add 1

# Find Patients with enough visits
getTop10Patients = function(){
  pats = unique(codes$ANON_ID)
  corDf.tmp = corDf[corDf$ANON_ID %in% pats,]
  counts = aggregate(corDf.tmp$ANON_ID, by=list(corDf.tmp$ANON_ID), FUN=length)
  counts = counts[order(-counts$x),]
  
  npats = 10
  pats = counts[counts[,2] > 50,1][1:10] # top 5 with at least 50 visits
  pats
}
#pats = getTop10Patients()

generate5Cevents = function(pats,col,dataset="30k"){
  dres = generate5C("HCT","Pulse",dataset,
                    window = 30,filter=pats,col)
  
  if(dataset == "30k"){
    codes_pats = codes[codes$ANON_ID %in% pats,]
    ids.tmp = c("!",codes_pats$ANON_ID)
    first = c(ids.tmp[-1] != ids.tmp[-length(ids.tmp)])
    last = c(first[-1],TRUE)
    
    # correct for D-148 (last event instead of first)
    if (pats == "D-148"){
      first[] = FALSE
      first[6] = TRUE
    }
    codes_pats$time = as.numeric(codes_pats$date - min(dres$dres$date))/365
    codes_pats = codes_pats[first,]
  }
  else{#Mike
    times = c(7,43,52,64)
    first = rep(FALSE, length(dres$dres$time))
    first[times] = TRUE
    codes_pats = list(time = as.numeric(dres$dres$time[times]), ICD10 = NULL)
    print(dres$dres$date[times])
  }
  filename = paste0("plots/Figure-5C-rsqured-",pats,".png")
  
  plt_cur = dres$plt_rqs + theme(legend.position="none")
  for (evid in 1:sum(first)){
    plt_cur = plt_cur + geom_vline(xintercept = codes_pats$time[evid],color="red",size=1) +
    geom_text(aes_q(x=codes_pats$time[evid], label=paste0("\n",codes_pats$ICD10[evid]),
                    y=max(dres$dres$rsquared) - sd(dres$dres$rsquared)/2),
                    colour="black", angle=90, text=element_text())
  }
  
  ## Other events (TODO: very manual for now...)
  print(pats)
  if (pats == "D-148"){
    eid = 28
    plt_cur = plt_cur + geom_vline(xintercept = dres$dres$time[eid],color="blue",size=1)
    print(dres$dres$date[eid])
  }
  if (pats == "D-145"){
    for (eid in c(12, 17)){
      plt_cur = plt_cur + geom_vline(xintercept = dres$dres$time[eid],color="blue",size=1)
      print(dres$dres$date[eid])
    }
  }
  if (pats == "PD-6145"){
    for (eid in c(25)){
      plt_cur = plt_cur + geom_vline(xintercept = dres$dres$time[eid],color="blue",size=1)
      print(dres$dres$date[eid])
    }
  }
  
  print(plt_cur)
  ggsave(paste0(filename),
         plot=plt_cur,width = 6,height = 6,units = "in")
  filename = paste0("plots/Figure-5C-pred-",pats,".png")
  ggsave(paste0(filename),
         plot=dres$plt_pred,width = 6,height = 6,units = "in")
  
  dres
}

pats = c("D-145","D-148","PD-6145") #, "N-3683")
for (i in 1:length(pats))
  generate5Cevents(pats[i],i)

generate5D = function(pat = "1636-69-001",lab.test = "HCT"){
## 1636-69-001


nobs = 25
neval = 3
nstart = 6
shift = 2

## How much data we need for the estimates
corDf.tmp = iPOPcorDf[!is.na(iPOPcorDf[[lab.test]]),]
corDf.tmp = corDf.tmp[!is.na(corDf.tmp[["Pulse"]]),]
corDf.tmp = corDf.tmp[!is.na(corDf.tmp[["Temp"]]),]
corDf.tmp = corDf.tmp[!is.na(corDf.tmp[["systolic"]]),]
corDf.tmp = corDf.tmp[!is.na(corDf.tmp[["diastolic"]]),]
tbl = table(corDf.tmp$iPOP_ID)
patids = names(tbl[tbl >= nobs + neval])
wall = which(corDf.tmp$iPOP_ID == patids[1])
half2 = wall[ceiling(length(wall)/2):length(wall)]
corDf.tmp[half2,]$iPOP_ID = "1636-69-001-late"
patids = c("1636-69-001-late", patids)
patids = patids[1:2]

# Here we select people with the largest number of observations
vts = c("Pulse","Temp","systolic","diastolic") #,"Respiration")

res = matrix(1, length(patids), nobs)
for (i in nstart:nobs){
  for (j in 1:length(patids)){
    pat = patids[j]
    d = corDf.tmp[corDf.tmp$iPOP_ID %in% pat, c(lab.test,vts)]
    ntest = nrow(d) - shift
    
    train = shift + (ntest - neval + 1 - i):(ntest-neval)
    test = shift + (ntest - neval + 1):ntest
    
    model = lm(paste0(lab.test," ~ ."),data=d[train,])
    
    sm = summary(model)
    pp = predict(model, newdata=d[ntest,])
    err = mean((pp - d[(ntest - neval + 1):ntest,lab.test])**2) / mean((mean(d[train,lab.test]) - d[test,lab.test])**2)
    res[j,i] = err
  }
}
mse = colMeans(res[,1:nobs])
mse[mse > 1] = 1
df = data.frame(observations = 1:nobs, RPVE = sqrt(1 - mse))
#plot(df,ylab="RPVE of HCT prediction",xlab="observation used for the model")

plt = ggplot(df, aes(observations, RPVE)) + 
  geom_point(size=3) +
  weartals_theme + theme(text = element_text(size=25)) +
  theme(legend.position="none") +
  scale_x_continuous(breaks = pretty(df$observations, n = 13)) 
print(plt)
ggsave(paste0("plots/Figure-5D-",pat,"-",lab.test,".png"),width = 6, height = 6)
write.table(df, file=paste0("data/Figure-5D-",pat,"-",lab.test,".csv"))
df
}
res = generate5D("1636-69-001", "HCT")

###############
#   Figure 5 #
###############
# run after reading in and cleaning data and running Figure 2D section to get top.names

weartals_theme = theme_bw() + theme(text = element_text(size=18), panel.border = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

#corr.coefs <-read.table("../SECURE_data/20180322_ranked_models_test_lm.csv",row.names=1, sep=",")
corr.coefs <-read.table("../SECURE_data/20180403_ranked_models_ipop_lm.csv",row.names=1, sep=",")
top.names<-rownames(corr.coefs) # names of lab tests from either the 30k or the iPOP simple bivariate models
top.names<-top.names[top.names %in% names(wear)] # only keep the lab names that are also present in the iPOP data

## Univariate Mixed-effect: True vs predicted 
# !! Only patients with at least min_visits = 20

min_visits = 20
mm.corr.coefs <- c()
lr.corr.coefs <- c()
id.corr.coefs <- c()
clin.idx <- c()
#for (i in 1:length(top.names)){
for (i in 1:4){
  clin = top.names[i]
  patients = sort(table(corDf[!is.na(corDf[[clin]]),]$ANON_ID))
  labs.vitals.tmp = corDf[corDf$ANON_ID %in% names(patients[patients > min_visits]),]
  labs.vitals.tmp$ANON_ID = factor(labs.vitals.tmp$ANON_ID)
  
  nn = nrow(labs.vitals.tmp)
  smp = sample(nn)
  test = smp[(1+floor(nn*0.9)):nn]
  train = smp[1:floor(nn*0.9)]
  
  frm = paste0(clin," ~ Pulse + Temp + (Pulse + Temp|ANON_ID)")
  print(frm)
  if (nrow(labs.vitals.tmp[train,]) && length(unique(labs.vitals.tmp[train,]$ANON_ID)) > 1){
    clin.idx <-c(clin.idx, clin)
    mm = lmer(frm, data = labs.vitals.tmp[train,])
    cf = coef(mm)
    vit = "Pulse"
    #qq = qplot(cf$ANON_ID[vit], geom="histogram")  + weartals_theme + xlab(paste0(top8[i]," ~ ",vit)) + ylab("count")
    #print(qq) 
    #, vp = viewport(layout.pos.row = matchidx$row,
    #                         layout.pos.col = matchidx$col))
    tt = labs.vitals.tmp[test,clin]
  
    # Evaluate LR model
    frm = paste0(clin," ~ Pulse + Temp")
    m0 = lm(frm, labs.vitals.tmp[train,])
    pp = predict(m0, newdata = labs.vitals.tmp[test,])
    #plot(pp, tt)
    lr.corr.coefs <- c(lr.corr.coefs, cor(pp,tt,use = "na.or.complete")) # corr coef of LR model
    
    # Evaluate MM model
    pp = predict(mm, newdata = labs.vitals.tmp[test,])
    #plot(pp, tt)
    mm.corr.coefs<-c(mm.corr.coefs, cor(pp,tt,use = "na.or.complete")) # corr coef of MM model
    
    # Evaluate LR model with ID
    frm = paste0(clin," ~ ANON_ID")
    m0 = lm(frm, labs.vitals.tmp[train,])
    pp = predict(m0, newdata = labs.vitals.tmp[test,])
    #plot(pp, tt)
    id.corr.coefs <- c(id.corr.coefs, cor(pp,tt,use = "na.or.complete")) # corr coef of LR model only with patient ID
  }
}
indiv.corr.coefs <- cbind(lr.corr.coefs, mm.corr.coefs, id.corr.coefs)
rownames(indiv.corr.coefs) <- clin.idx
write.table(indiv.corr.coefs, "../SECURE_data/20180329_indiv_30k_corr_coeffs.csv",row.names=TRUE,col.names=TRUE, sep=",")
data <-read.table("../SECURE_data/20180330/20180329_indiv_30k_corr_coeffs.csv",
                  header=TRUE,sep=',',stringsAsFactors=FALSE)
d <- melt(data, id.vars="X")
ggplot(d, aes(x=X, y=value, col=variable, shape=variable))+
  geom_point(cex=2.5) + 
  weartals_theme




####################
# Suppl. Figure 1  #
####################
iPOPtopTen <- c( "PLT",
                 "ALKP",
                 "BUN",
                 "MONOAB",
                 "HSCRP",
                 "GLU",
                 "GLOB",
                 "A1C",
                 "WBC",
                 "IGM" )

pList <- list(); j=0  
for (i in iPOPtopTen){
  j=j+1
  call <-paste0("iPOPcorDf$",i)
  df <- cbind(iPOPcorDf[[i]], iPOPcorDf[,c("Pulse", "Temp")])
  df <- na.omit(df)
  #pList[[j]] <- 
  print(ggplot(df, aes(x = df$Pulse, y = df[,1])) +
        geom_point(col="black", pch=19, cex=0.5) +
        stat_smooth(method = "lm", formula = y  ~ x + I(x^2), size = 1.5, col="darkred") +
        theme(axis.title=element_text(face="bold",size="14"),axis.text=element_text(size=16,face="bold"), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
        xlab("cHR") + ylab(i)) }
#grid.arrange(pList[[1]],pList[[2]],pList[[3]],pList[[4]],pList[[5]],pList[[6]], ncol=2,top="Main Title")
#####
# iPOP binning plot figures
#####
summary.pulse<-list()
summary.Temp<-list()
r.squared <-c()
for (j in clinTopTen){
  iPOPcorDf$bin2<-ntile(iPOPcorDf[[j]], 40)
  # for Temp
  # corDf2 <- summarySE(corDf, measurevar="Temp", groupvars="bin2", na.rm=TRUE)
  # print(ggplot(corDf2, aes(x=bin2, y=Temp)) +
  # geom_point(stat="identity", fill="darkblue") +
  #   geom_errorbar(aes(ymin=Temp-se, ymax=Temp+se), width=.4) +
  #   xlab(paste(c(j, "bins", sep=" ")))+
  #   scale_y_continuous(limits = c(97,99)) 
  # + theme(text = element_text(size=9),
  #         axis.text.x = element_text(angle = 60, hjust = 1)))
  # For Pulse
  iPOPcorDf2 <- summarySE(iPOPcorDf, measurevar="Pulse", groupvars="bin2", na.rm=TRUE)
  # print(ggplot(corDf2, aes(x=bin2, y=Pulse)) +
  #  geom_bar(stat="identity", fill="darkred") +
  # geom_errorbar(aes(ymin=Pulse-se, ymax=Pulse+se), width=.2) +
  # xlab(paste(c(j, "bins", sep=" ")))
  # + theme(text = element_text(size=9),
  #         axis.text.x = element_text(angle = 60, hjust = 1)))
  print(paste0(j, ": number of data points in bin = ", sum(iPOPcorDf$bin2 %in% "2")))
  print(ggplot(iPOPcorDf2, aes(x = bin2, y = Pulse)) +
          stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1.5, col="darkred") +
          theme(axis.title=element_text(face="bold",size="14"),axis.text=element_text(size=16,face="bold"), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
          geom_point(col="black") +
          xlab(paste0(c(j ," Bin"))))
  summary.pulse <- summary(lm(iPOPcorDf2$Pulse ~ iPOPcorDf2$bin2 + I(iPOPcorDf2$bin2^2)))
  r.squared[j] <- summary.pulse$adj.r.squared
  # print(ggplot(corDf2, aes(x = bin2, y = Temp)) +
  #         stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1.5, col="darkblue") +
  #         geom_point(col="black") +
  #         #ylim(c(96,98.5))+
  #         theme(axis.title=element_text(face="bold",size="14"),axis.text=element_text(size=16,face="bold"), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  #         xlab(paste0(c(j ," Bin"))))
  # summary.Temp <- summary(lm(corDf2$Temp ~ corDf2$bin2 + I(corDf2$bin2^2)))
  # r.squared[j] <- summary.Temp$adj.r.squared
  
}
as.matrix(r.squared)



##################
# Suppl. Fig 2A  #
##################
plot(corDf$Pulse ~ corDf$NEUT, pch='.',
     xlab="Neutrophils", ylab="Pulse", font.lab=2,lwd=2,font=2)
abline(lm(corDf$Pulse ~ corDf$NEUT+ (corDf$NEUT)^2), col="blue",lwd=4)

#############################
#    Suppl. Table 2 and 3   #
#############################
# allClin <- c("A1C","AG","ALB","ALKP","ALT","AST","BASO",
#              "BASOAB","BUN","CA","CHOL","CHOLHDL","CL","CO2",
#              "CR","EOS","EOSAB","ESR", "GLOB","GLU_byMeter",
#              "GLU_fasting","GLU_nonFasting","GLU_SerPlas",
#              "GLU_wholeBld","HCT","HDL",
#              "HGB","HSCRP","IGM","K","LDL_Calc", "LDL_Direct","LDLHDL","LYM","LYMAB",
#              "MCH","MCHC","MCV","MONO","MONOAB","NA.","NEUT",
#              "NEUTAB","NHDL","PLT","PROCALCITONIN", "RBC","RDW","TBIL","TGL","TP","TroponinI","WBC")# RUN 30K CORRELATIONS 
allClin <- c("A1C","AG","ALB","ALKP","ALT","AST","BASO",
             "BASOAB","BUN","CA","CHOL","CHOLHDL","CL","CO2",
             "CR","EOS","EOSAB","ESR", "GLOB",
             "GLU_fasting","HCT","HDL",
             "HGB","HSCRP","IGM","K","LDL_Direct","LDLHDL","LYM","LYMAB",
             "MCH","MCHC","MCV","MONO","MONOAB","NA.","NEUT",
             "NEUTAB","NHDL","PLT","RBC","RDW","TBIL","TGL","TP","WBC")# RUN 30K CORRELATIONS

# for each lab run a multiple regression:
r<-c()
p<-c()
fstat <-c()
degfree <- c()
tot= 0 
for (i in allClin){
  call <-paste0("corDf$",i)
  df <- cbind(corDf[[i]], corDf[,c("Pulse", "Temp")])
  df <- na.omit(df)
  #tot = tot + length(df$Pulse)
  print(c(i , length(df$Pulse))) # the number of each clinical lab test that has corresponding vital signs
  m <- summary(lm(df[,1] ~ df$Pulse + df$Temp)) # bivariate with pulse + temp
  #m <- summary(lm(df[,1] ~ df$Pulse)) # univariate with pulse or temp only
  #m <- summary(lm(df[,1] ~ df$Temp + I(df$Temp^2))) # quadratic univariate with pulse or temp only
  r[i]<-m$adj.r.squared 
  fstat[i]<-m$fstatistic
  p[i]<-1-pf(m$fstatistic[1],m$fstatistic[2],m$fstatistic[3])
  degfree[i]<-m$df
}

options("scipen"=100, "digits"=4)
str(data.frame(as.list(r)))
str(data.frame(as.list(p))); p<-sort(p) 
str(data.frame(as.list(fstat)))
tot # total number of labs that have clin vitals measures corresponding to it

# num lab tests in iPOP dataset
tot <- 0; for (i in 7:56){
  tmp <- length(as.matrix(na.omit(wear[i]))); tot <- tot + tmp}; tot 

# num vital signs in iPOP dataset
tot <- 0; for (i in 7:56){tmp <- length(as.matrix(na.omit(wear[i]))); tot <- tot + tmp}; tot




##############
#  Figure 2B #
##############
# make boxplots for the top best correlated clinic values with vital signs in the 30k cohort

#topHits <- c("GLU_fasting","HSCRP","ESR", "NEUT","RDW","LYM", "ALB", "PROCALCITONIN")
#topHits <- c("HSCRP","NEUTAB","NEUT","LYM", "PLT", "TroponinI", "ESR", "PROCALCITONIN")

for (i in clinTopTen){
  #quartile
  #below<-corDf$Temp[corDf[i] < summary(corDf[i])[5]]
  #above<-corDf$Temp[corDf[i] > summary(corDf[i])[5]]
  #decile
  below<- corDf$Temp[sapply(ntile(corDf[i], 40) <= 1, isTRUE)]
  #fifth<- corDf$Temp[ntile(corDf[i], 40) <= 6 & ntile(corDf[i], 40) >= 4 & !is.na(ntile(corDf[i], 40))]
  #tenth<- corDf$Temp[ntile(corDf[i], 40) <= 11 & ntile(corDf[i], 40) >= 9 & !is.na(ntile(corDf[i], 40))]
  above<- corDf$Temp[sapply(ntile(corDf[i], 40) >= 40, isTRUE)]
  print(paste0(i, ": number in upper is ", length(below), " and number of lower is ", length(above)))
  pval<-round(unlist(t.test(below, above)[3]), digits=3)
  #boxplot(below,above, outline=FALSE, main=paste(i,", P=", pval, sep=""), names=c("Below 3rd Quartile","Above 3rd Quartile"), ylab="Temp")
  #boxplot(below,fifth, tenth, above, outline=FALSE, main=paste(i,", P=", pval, sep=""), names=c("Below 1st ventile","fifth ventile", "tenth ventile","Above 10th ventile"), ylab="Temp")
  boxplot(below, above, outline=FALSE, main=paste(i,", P=", pval, sep=""), names=c("Lowest","Highest"), ylab="Temp", col="lavenderblush4")
  #color for skin temp is lavenderblush4
}

# check diabetes ones:
for (i in topHits){
  normal<- corDf$Temp[corDf$GLU_fasting < 100]
  prediabetes<- corDf$Temp[corDf$GLU_fasting > 100 & corDf$GLU_fasting < 110]
  diabetes<- corDf$Temp[ntile(corDf[i], 40) <= 11 & ntile(corDf[i], 40) >= 9 & !is.na(ntile(corDf[i], 40))]
  print(paste0(i, ": number in upper is ", length(below), " and number of lower is ", length(above)))
  pval<-round(unlist(t.test(below, above)[3]), digits=3)
  #boxplot(below,above, outline=FALSE, main=paste(i,", P=", pval, sep=""), names=c("Below 3rd Quartile","Above 3rd Quartile"), ylab="Temp")
  #boxplot(below,fifth, tenth, above, outline=FALSE, main=paste(i,", P=", pval, sep=""), names=c("Below 1st ventile","fifth ventile", "tenth ventile","Above 10th ventile"), ylab="Temp")
  boxplot(below, above, outline=FALSE, main=paste(i,", P=", pval, sep=""), names=c("Below 1st ventile","Above 10th ventile"), ylab="Temp")
}



############
# Figure 4 #
############
library(lme4)
topNeut <- names(head(sort(pulse.num.top.quartile, decreasing = TRUE), 20)) #the 20 people that had the most measurements of NEUT (between 67 and 171 measurements)
p1 <- ggplot(data = corDf[corDf$ANON_ID %in% topNeut,], aes(x = NEUT, y = Temp, colour = ANON_ID)) +       
  #geom_point() + 
  #geom_smooth(method='lm',formula=y~x, se = FALSE)
  geom_smooth(method='loess',formula=y~x, se = FALSE)
#  geom_smooth(method = "lm", formula = y ~ splines::bs(x, 2), se = FALSE)
model <- lmList(NEUT ~ Temp | ANON_ID, data=corDf)

na.omit(corDf) %>% 
  group_by(ANON_ID) %>% 
  do({
    mod = lm(NEUT ~ Temp, data = .)
    data.frame(Intercept = coef(mod)[1],
               Slope = coef(mod)[2])
  })



#########################
# Supplementary Table 1 #
#########################
# get mean +/- SE for each of the 50 clin values
library("psych")
thirtyk.summary <- describe(corDf)
ipop.corDf.summary <- describe(iPOPcorDf)
ipop.demog.summary <- describe(iPOPdemographics$AgeIn2016)
table(iPOPdemographics)
write.csv(thirtyk.summary, "/Users/jessilyn/Desktop/framework_paper/Suppl_Table_1/supplTable1.csv")
write.csv(ipop.corDf.summary, "/Users/jessilyn/Desktop/framework_paper/Suppl_Table_1/supplTable2.csv")

raw.wear <- read.csv(paste0(dir,
                            "BasisData_20161111_PostSummerAddOns_Cleaned_NotNormalized_20180427.csv"),
                     header=TRUE,sep=',',stringsAsFactors=FALSE)

describe(raw.wear$GSR)
describe(raw.wear$Heart_Rate)
describe(raw.wear$Steps)

# get p-values and rsquared of univariate correlations
vitalVars <- which(names(corDf) %in% c("Pulse","Temp"))
results <- corr.test(corDf[,3:56],
                     corDf[,c(vitalVars)],
                     method="pearson",adjust="fdr")
#### CREATE DATA FRAME(S) SUMMARIZING RESULTS ####
rCols <- paste0(dimnames(results$r)[[2]],"_R")
pCols <- paste0(dimnames(results$p)[[2]],"_P")
nCols <- paste0(dimnames(results$n)[[2]],"_N")
allCors_R <- data.frame(placeHolder=rep(NA,length(allClin)))
rownames(allCors_R) <- dimnames(results$r)[[1]]
for(i in 1:length(rCols)){
  allCors_R[,i] <- results$r[,i]
  names(allCors_R)[i] <- rCols[i]
}

allCors_P <- data.frame(placeHolder=rep(NA,length(allClin)))
rownames(allCors_P) <- dimnames(results$p)[[1]]
for(i in 1:length(pCols)){
  allCors_P[,i] <- results$p[,i]
  names(allCors_P)[i] <- pCols[i]
}

allCors_N <- data.frame(placeHolder=rep(NA,length(allClin)))
rownames(allCors_N) <- dimnames(results$n)[[1]]
for(i in 1:length(nCols)){
  allCors_N[,i] <- results$n[,i]
  names(allCors_N)[i] <- nCols[i]
}

allCors <- cbind(allCors_R,allCors_P,allCors_N)

newColOrder <- c("Pulse","Temp")

tmp <- allCors
for(i in 1:length(newColOrder)){
  if(i == 1){
    cache <- grep(newColOrder[i],names(allCors))
    tmp[,c(i:sum(0+3))] <- allCors[,c(cache)]
    names(tmp)[c(i:sum(0+3))] <- c(names(allCors)[c(cache)])
    count <- sum(0+3)
  } else {
    if(i != 1){
      cache <- grep(paste0("^",newColOrder[i]),names(allCors))
      tmp[,c(sum(count+1):sum(count+3))] <- allCors[,c(cache)]
      names(tmp)[c(sum(count+1):sum(count+3))] <- c(names(allCors)[c(cache)])
      count <- sum(count+3)
    }
  }
}

allCors <- tmp
sigCors_Ranked <- allCors

#class(sigCors_Ranked$Pulse_P)
sigCors_Ranked <- sigCors_Ranked[
  which(sigCors_Ranked$Pulse_P < 0.05),]

sigCors_Ranked <- sigCors_Ranked[
  order(abs(sigCors_Ranked$Pulse_R),decreasing = TRUE),]

#### SAVE DATA FRAME(S) SUMMARIZING RESULTS ####

write.csv(allCors,
          "~/Desktop/20170810_allCors_VitalsVsLabs.csv",
          row.names=TRUE)

write.csv(sigCors_Ranked,
          "~/Desktop/20170810_sigCorsRanked_VitalsVsLabs.csv",
          row.names=TRUE)

################################
#  Currently not used in paper #
################################

# RUN iPOP CORRELATIONS between labs and vitals
options("scipen"=100, "digits"=4)
models=c(" ~ Pulse", # univariate with pulse only
         " ~ Temp",   # univariate with temp only
         " ~ Pulse + Temp", # bivariate with pulse + temp
         " ~ Pulse + I(Pulse^2)",
         " ~ Temp + I(Temp^2)" )

for (k in 1:length(models)){
  print(k)
  r<-matrix(ncol = length(unique(iPOPcorDf$iPOP_ID)),nrow = length(allClin),
            dimnames=list(
              c(allClin),
              c(unique(iPOPcorDf$iPOP_ID))))
  rsq.pred <-r; p<-r; fstat <-r; degfree <- r; numObs <-r;
  tmp=0
  for (i in allClin){ # for each of the 50 clinical lab tests
    tmp=tmp+1 # counter for index of allClin
    tmp2=0    # counter for index of iPOP_ID
    for (j in unique(iPOPcorDf$iPOP_ID)){ 
      tmp2=tmp2+1  # counter for index of iPOP_ID
      iPOPcorDf2 <- iPOPcorDf[!(iPOPcorDf$iPOP_ID %in% j),] # leave one person out
      df <- cbind(iPOPcorDf2[[i]], iPOPcorDf2[,c("Pulse", "Temp")])
      df <- na.omit(df)
      model<-lm(as.formula(paste0(i,models[k])),data=iPOPcorDf2)
      m <- summary(model) # quadratic univariate with pulse or temp only
      # r[tmp,tmp2]<-m$adj.r.squared # matrix of r-squared values for each left-one-out model
      # p[tmp,tmp2]<-1-pf(m$fstatistic[1],m$fstatistic[2],m$fstatistic[3]) # matrix of p-squared values for each left-one-out model
      numObs[tmp,tmp2]<-length(df$Pulse) # the number of each clinical lab test that has corresponding vital signs
      iPOPcorDf3 <- iPOPcorDf[(iPOPcorDf$iPOP_ID %in% j),] # test set (the one person that was left out)
      df3 <- cbind(iPOPcorDf3[[i]], iPOPcorDf3[,c("Pulse", "Temp")])
      df3 <- na.omit(df3)
      pred=predict(model, newdata=df3)# prediction on test person
      rsq.pred[tmp,tmp2] = 1 - (mean( (pred - df3[,1])**2 ) / var( (df[,1]) )) # test r.sq
    }
  }
  name.rsq <- paste("model.mean.rsq", k, sep = "")
  assign(name.rsq, data.frame(model = name.rsq, test = allClin, means = rowMeans(rsq.pred), sd =apply(rsq.pred, 1, sd))) 
}

rsq.plot<- as.data.frame(as.list((rbind(model.mean.rsq1, model.mean.rsq2, model.mean.rsq3, model.mean.rsq4, model.mean.rsq5))))
rsq.plot$model <- mapvalues(rsq.plot$model, from = c("model.mean.rsq1", "model.mean.rsq2", "model.mean.rsq3", "model.mean.rsq4", "model.mean.rsq5"), to = c("~ Pulse", "~ Temp", "~ Pulse + Temp", " ~ Pulse + I(Pulse^2)", " ~ Temp + I(Temp^2)"))

# plot how rsq changes with the different models, and add in error bars from sd.plot
ggplot(rsq.plot, aes(x=test, y=means, group=model, col=as.factor(rsq.plot$model))) +
  geom_point() +
  theme(axis.title=element_text(face="bold",size="12"),axis.text=element_text(size=12,face="bold"), panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 60, hjust = 1)) +
  xlab("Clinical Laboratory Test") + ylab("Cross-Validated R-squared (+/- SD)") +
  labs(linetype='Model')
guides(guide_legend(title="Model"))
#guides(fill=guide_legend(title="Model"))
scale_fill_discrete(name="Model")+
  geom_errorbar(aes(ymin=means-sd, ymax=means+sd), width=0.7,
                position=position_dodge(.7)) + ylim(0,1.5)

tot # total number of labs that have clin vitals measures corresponding to it
# num lab tests in iPOP dataset
tot <- 0; for (i in 7:56){
  tmp <- length(as.matrix(na.omit(wear[i]))); tot <- tot + tmp}; tot 

# num vital signs in iPOP dataset
tot <- 0; for (i in 7:56){tmp <- length(as.matrix(na.omit(wear[i]))); tot <- tot + tmp}; tot



#### END ####

## REVISION CODE ##

#Hematologic correlations

# PLT + GLOB + TP + HGB + HCT + RDW + MCH + MCV + RBC + MCHC
subsample <- corDf[sample(nrow(corDf), size = 10000, replace = FALSE),]
pairs(~ HGB + HCT + RBC,data=subsample, 
      main="CBC Correlations")
pairs(~ HGB + HCT + RBC + MONOAB + A1C + GLU_SerPlas + PLT + CL, data=subsample, 
      main="Top Lab Test Correlations")

blood = na.omit(corDf[,c("ANON_ID","Clin_Result_Date","HGB","HCT")])
model.blood <- lm(HGB ~ HCT, data = blood)
summary(model.blood)
#plot(model.blood)


colnames(icd)[colnames(icd)=="ICD_DATE"] <- "Clin_Result_Date"
colnames(cpt)[colnames(cpt)=="CPT_DATE"] <- "Clin_Result_Date"
icd$Clin_Result_Date<- as.character(icd$Clin_Result_Date)
cpt$Clin_Result_Date<- as.character(cpt$Clin_Result_Date)

blood$residuals <- model.blood$residuals ## Add the residuals to the data.frame
o <- order(blood$residuals^2, decreasing=T) ## Reorder to put largest first
top.residuals <- blood[o[1:10],]
top.residuals$Clin_Result_Date <- as.Date(top.residuals$Clin_Result_Date)
top.residuals$Clin_Result_Date<- as.character(top.residuals$Clin_Result_Date)
hgb.hct.divergence <- merge(top.residuals, icd, by = c("ANON_ID", "Clin_Result_Date"))

plot(HGB ~ HCT, data = blood)
abline(model.blood, col = "red")
points(HGB ~ HCT, data = top.residuals, col="red", pch=19)
