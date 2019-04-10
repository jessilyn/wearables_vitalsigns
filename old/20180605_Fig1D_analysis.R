
# Figure 1D script - 2018-04-27
# open this script from the weartals github repo

#set working directory to directory of this script (RStudio solution)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# ^Having a set working directory will be important for later graph output.

#set data input directory
dir.in = "../SECURE_data"
#dir.in = "/srv/gsfs0/projects/snyder/jdunn/framework_paper/SECURE_data" # For use on scg

#set data output directory (e.g. for graph PNGs, t-test outputs)
dir.out.graphs = "./graphs/20180605"
dir.out.analyses = "./analyses/20180605"

## Hypotheses: 
# 1) Watch RHR < Clinic Pulse (white coat syndrome)
# 2) Watch variance per individual < clinic pulse variance per individual (power of many tests)

### Goal: explore how time of day and resting thresholds affect comparison of mean and variability between watch RHR and clinic pulse

#### REQUIRED
require(data.table) #fread
require(fasttime) #fastPOSIXct
require(ggplot2) #ggplot
require(lubridate)
require(plyr)
require(psych) #describe
require(reshape2) #melt
require(zoo) #rollapply
require("ggthemes")
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

fastDate <- function(x, tz=NULL)
  #as.Date(fastPOSIXct(x, tz=tz))
  as.Date(x, tz=tz)

#### READ IN & CLEAN DATA
df.source <- fread(paste0(dir.in, "/BasisData_20161111_PostSummerAddOns_Cleaned_NotNormalized_20180427.csv"),
                   header=TRUE,sep=",",stringsAsFactors = FALSE,
                   select=c("Timestamp_Local","Heart_Rate","Skin_Temperature_F",
                            "Steps","iPOP_ID")) # 38009228 observations
df<-df.source[!is.na(df.source$iPOP_ID),] # only keep wearables data that has an associated iPOP ID; 
df[,"Heart_Rate"] <- apply(df[,"Heart_Rate"], 2, remove_outliers) # clean data based on HR (TODO: later also clean on Skin Temp, Steps)
df <- df[which(!is.na(df[,"Heart_Rate"])),] # remove observations where HR = NA

vitals <- fread(paste0(dir.in,"/Vitals_for_Fig1D.csv"), #there are two different vitals.csv files; one is for 30k and one is for iPOP - make sure you use the correct one
                header=TRUE,sep=',',stringsAsFactors=FALSE)
vitals$RESULT_TIME<-as.Date(vitals$RESULT_TIME)
colnames(vitals)[1:2] <- c("iPOP_ID", "Date")

###############
#  Figure 1D  #
###############
#windows=120
windows=c(10,60,120,240) # define time windows with no steps for resting threshold

#To check day-prior, supply TRUE or FALSE here:
#If TRUE, merges clinical based on day-prior, and also adds "dayPrior" to output file names.
dayPrior = FALSE
for (window in windows){
  restingDf <- c() 
  restingDf.all <- list() # keep all resting data for boxplots later
  maxsteps <- 1 #define max steps for resting threshold
  indiv.means <- c()
  indiv.sd <- c()
  for(i in unique(df$iPOP_ID)){
    print(i)
    subDf <- df[which(df$iPOP_ID %in% i),] #pull data per individual
    restingST<-c()
    restingST <- rollapplyr(subDf$Steps, width=window, by=1, sum, partial=TRUE)
    restingST[1:window-1]<-"NA" # remove 1st x observations because we dont know what happened prior to putting the watch on
    restingST <- as.numeric(restingST)  # Expected warning: "In as.numeric(restingST) : NAs introduced by coercion"
    restingDf <- subDf[restingST<maxsteps & !is.na(restingST)] # in the previous time window of X min plus the current minute,there are < maxsteps steps 
    indiv.means[i] <- mean(restingDf$Heart_Rate, na.rm=TRUE) # mean RHR for all days/times for individual i
    # ^ why slightly different from means$restingHR below?
    # ^ Several reasons: 
    #   1) When merging vitals and df on row #94, all.x=TRUE wasn't used.
    #      This meant we went from having 1,854,454 values for stat estimation,
    #      down to just 40,593; so we lost 1,813,861 data observations there.
    #      This effected estimates. However, in this particular case, we
    #      probably don't want all.x=TRUE because we want to compare wearable
    #      and clinical values from the same date range for comparison accuracy.
    #   2) Not all iPOPs are present in both vitals and df.
    #      This changed the order between indiv.means and means$restingHR.
    #   3) Also, the one below was being resorted by DateTime (line ~#95), then by aggregate.
    #      This again changed the order between indiv.means and means$restingHR.
    # ^ A solution to resolve differences and compare these is on line #95.
    indiv.sd[i] <- sd(restingDf$Heart_Rate, na.rm=TRUE) # RHR var for all days/times for individual i
    restingDf.all[[i]] <- restingDf # store all resting data for boxplots
  }
  
  restingDf.all <- rbindlist(restingDf.all)
  restingDf.all$Date <- as.Date(restingDf.all$Timestamp_Local)
  restingDf.all <- restingDf.all[,c("iPOP_ID","Date","Heart_Rate","Skin_Temperature_F","Steps","Timestamp_Local")]
  names(restingDf.all) <- c("iPOP_ID","Date","restingHR","restingSkinTemp","restingSteps","DateTime")
  
  # #### Tangent: This shows how to make indiv.means = restingDf$Heart_Rate
  # #    To save space, some lines below are explained afterward.
  # restingDf.vitals <- merge(restingDf.all,vitals,by=c("iPOP_ID","Date"),all.x = TRUE)
  # restingDf.vitals$DateTime<-fastPOSIXct(restingDf.vitals$DateTime)
  # restingDf.vitals <- restingDf.vitals[order(restingDf.vitals$DateTime),] 
  # cols <- c("restingHR","restingSkinTemp","Pulse") #subset columns to convert
  # restingDf.vitals[,(cols) := lapply(.SD, function(x) as.numeric(as.character(x))), .SDcols = cols]
  # options(datatable.optimize=1)
  # means <- restingDf.vitals[, lapply(.SD, mean, na.rm=TRUE), by=iPOP_ID]
  # options(datatable.optimize=0)
  # # Code to resolve descriptive stat differences 
  # x <- sapply(unique(df$iPOP_ID), function(x) grep(x,unique(means$iPOP_ID)))
  # iPOPInVitals <- sapply(x, length)
  # #subset indiv.means by overlapping vitals/df iPOPs 
  # test <- indiv.means[which(iPOPInVitals==1)]
  # #reorder indiv.means by same order as restingDf (by DateTime)
  # x <- unlist(x)
  # #see if revised indiv.means are now identical to means$restingHR:
  # identical(as.numeric(test[order(x)]),means$restingHR)
  # #should now return TRUE
  # #### End tangent. 
  # # ^ We'll actually want to discard this though. I just wanted to show how.
  # rm(restingDf.vitals,cols,means,x,iPOPInVitals,test)
  
  #Optional: use day-prior rather than day-of wearable data for comparison:
  if(dayPrior){
    restingDf.all$Date <- restingDf.all$Date + days(1)
  }
  # ^ All this does is advance each day by 1 if we set dayPrior to TRUE.
  #   This simple change allows us to reuse most of our code while testing.
  
  # We do happen to want only the dates that overlap for now, so:
  #merge vitals with resting HR
  restingDf.vitals <- merge(restingDf.all,vitals,by=c("iPOP_ID","Date"))
  restingDf.vitals$DateTime<-as.POSIXct(restingDf.vitals$DateTime)
  restingDf.vitals <- restingDf.vitals[order(restingDf.vitals$DateTime),] 
  cols <- c("restingHR","restingSkinTemp","Pulse") #subset columns to convert
  restingDf.vitals[,(cols) := lapply(.SD, function(x) as.numeric(as.character(x))), .SDcols = cols]
  df.name <- paste0("restingDf.vitals.", window)
  assign(df.name, restingDf.vitals) # to store data frame for each resting window definition
}

######################
# PLOTS AND ANALYSES #
######################

#Both graphs and analyses (e.g. t-tests) are saved to dir.out specified above.
# multiple ways to compare watch data to clinic pulse:
# 1) summary of wRHR from *all days* of watch monitoring (use indiv.means and indiv.sd)
# 2) summary of wRHR from *specific day* of clinic visit only (use rhr.daily.means & restingDf.compare)
# 3) summary of wRHR from *all clinic days* of watch monitoring (use means)


### figure in the paper is window<-windows[4] (resting for 240 min from same day as clinic visit)
for(window in windows){
  
  restingDf.vitals <- eval(as.name(paste0("restingDf.vitals.",window)))
  
  # restrict to daytime values only (between 6am-10pm)
  #restingDf.vitals.2 <- with( restingDf.vitals, restingDf.vitals[ hour( DateTime ) >= 6 & hour( DateTime ) < 22 , ] ) # pull data only from specific time window; store hourly resting data for boxplots
  #restingDf.vitals <-restingDf.vitals.2
  
  # scenario 2: wRHR from *specific day* of clinic visit only; aggregate wRHR into daily values corresponding to the clinic date
  # compare all resting watch data with all vitals data - this was fixed to aggregate wRHR into daily values corresponding to the clinic date
  options(datatable.optimize=1) #need this here; otherwsie won't handle character class
  rhr.daily.means <- restingDf.vitals[, lapply(.SD, mean, na.rm=TRUE), by=c("iPOP_ID","Date")]
  options(datatable.optimize=0)
  numObs <- dim(rhr.daily.means)[1]
  numPeople <- length(unique(rhr.daily.means$iPOP_ID))
  restingDf.compare <- cbind(rhr.daily.means$restingHR, rhr.daily.means$Pulse)
  colnames(restingDf.compare) <- c("Resting wHR", "cHR")
  rhr.daily.means.id <- cbind(rhr.daily.means$iPOP_ID, rhr.daily.means$restingHR, rhr.daily.means$Pulse)
  rhr.daily.means.id <- as.data.frame(rhr.daily.means.id)
  rhr.daily.means.id[,1 ]<- as.factor(as.character(rhr.daily.means.id[,1 ])); 
  rhr.daily.means.id[,2 ]<- as.numeric(as.character(rhr.daily.means.id[,2 ])); 
  rhr.daily.means.id[,3 ]<- as.numeric(as.character(rhr.daily.means.id[,3 ])); 
  colnames(rhr.daily.means.id) <- c("iPOP_ID", "restingHR", "Pulse")
  
  means<-aggregate(rhr.daily.means.id, list(rhr.daily.means.id$iPOP_ID), mean) # check this compare to indiv means
  sd<-aggregate(rhr.daily.means.id, list(rhr.daily.means.id$iPOP_ID), sd)
  delta.daily.mean.RHR.pulse<-means$Pulse - means$restingHR
  delta.daily.sd.RHR.pulse<-sd$Pulse - sd$restingHR 
  hist(delta.daily.mean.RHR.pulse, col="darkred", main = paste0("Delta Mean cHR - Mean wRHR)"))
  hist(delta.daily.sd.RHR.pulse, col="darkred", main = paste0("Delta StdDev cHR - StdDev wRHR)"))
  
  # Scatter plot HR
  ggplot(rhr.daily.means.id,
         aes(x=restingHR,y=Pulse,col=as.factor(substr(iPOP_ID,9,12)))) +
    geom_point() +
    labs(title=paste0("Clinical Pulse vs. Wearable RHR Mean (",window,"min resting)"),x="Resting Heart Rate",y="Pulse") +
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
  
  ## scatter plot skin temp
  ggplot(rhr.daily.means.id,
         aes(x=restingSkinTemp,y=Temp,col=as.factor(substr(iPOP_ID,9,12)))) +
    geom_point() +
    labs(title=paste0("Clinical Temp vs. Wearable Temp (",window,"min resting)"),
         x="Resting Skin Temp",y="Core Temperature") +
    annotate("segment",x=-Inf,xend=Inf,y=-Inf,yend=Inf,
             lwd=2, color="blue", alpha=.25) +
    guides(col=guide_legend("ID")) +
    xlim(90, 100) +
    ylim(90, 100)+
    theme(plot.title=element_text(face="bold",colour="black",size=14),
          axis.title.x=element_text(face="bold",colour="black",size=14),
          axis.text.x=element_text(face="bold",colour="black",size=12,angle=55,vjust=0.9,hjust=1),
          axis.title.y=element_text(face="bold",colour="black",size=14),
          axis.text.y=element_text(face="bold",colour="black",size=12),
          axis.ticks.length = unit(.2,"cm"),
          legend.title=element_text(face="bold", colour="black", size=14),
          legend.text=element_text(face="bold", colour="black", size=12),
          panel.background=element_rect(fill="grey94"))
  
  #run t-test
  # scenario 2: wRHR from *specific day* of clinic visit only
  p<-t.test(rhr.daily.means$restingHR, rhr.daily.means$Pulse, var.equal=FALSE, paired=TRUE) # test for significance
  p$method <- paste0("Paired T-Test between Wearable and Clinical (",window,"min Window)")

  #output t-test
  out<-capture.output(p)
  cat(out,file=paste0(dir.out.analyses,"t-test_output_",window,"minWindow",ifelse(dayPrior,"_DayPrior",""),".txt"),sep="\n",append=FALSE)

  #output boxplot
  png(file=paste0(dir.out.graphs,"boxplot_sameDay_WearVsClin_rhr_",window,"minWindow",ifelse(dayPrior,"_DayPrior",""),".png"),
      width=11.2,height=7,res=300, units = "in", pointsize=12)
  boxplot(restingDf.compare, ylab="Heart Rate", col = "darkred",
          main=paste0("Resting Window = ",window," min \n",numObs,
                      " observations; ",numPeople," People\n",
                      "p-value: ", p$p.value, "\n"))
  dev.off()

  #TODO: Get folks with more than 3 clinic pulse measurements that have corresponding watch data. Plot 1 value per day for the watch and clinic visit and compare variability in that way (dont include every RHR measurement from the watch over the course of that day)
  # aggregate(Pulse ~ iPOP_ID, data = restingDf.vitals, FUN = function(x){NROW(x)})
  # aggregate(Pulse ~ iPOP_ID+Date, data = restingDf.vitals, FUN = function(x){mean(x)})

  #change data from wide to long for plotting
  restingDf.vitals.melt <- melt(restingDf.vitals,id.vars='iPOP_ID', measure.vars=c('restingHR','Pulse'))
  rhr.daily.means.melt <- melt(rhr.daily.means.id, id.vars='iPOP_ID', measure.vars=c('restingHR','Pulse'))
  #output boxplot by iPOP ID
  png(file=paste0(dir.out.graphs,"boxplot_sameDay_byID_rhr_",window,"minWindow",ifelse(dayPrior,"_DayPrior",""),".png"),
      width=11.2,height=7,res=300, units = "in", pointsize=12)
  #scenario 1
  print(ggplot(restingDf.vitals.melt) +
    geom_boxplot(aes(x=iPOP_ID, y=value, color=variable), outlier.shape=NA) +
      ylim(c(50,100))+
    labs(title=paste0(window," min: Clinical Pulse vs.\n Wearable Resting\n Heart Rate "),x="Resting Heart Rate",y="Pulse") +
    theme(axis.title=element_text(face="bold",size="12"),axis.text=element_text(size=12,face="bold"), panel.background = element_blank(), axis.line = element_line(colour = "black"),
          axis.text.x = element_text(angle = 60, hjust = 1)))
  
  # scenario 2 <- this one goes in the paper
  print(ggplot(rhr.daily.means.melt) +
          ylim(c(50,100))+
          geom_boxplot(aes(x=iPOP_ID, y=value, color=variable), outlier.shape=NA) +
          labs(title=paste0(window," min: Clinical Pulse vs.\n Wearable Resting\n Heart Rate "),x="Resting Heart Rate",y="Pulse") +
          theme(axis.title=element_text(face="bold",size="12"),axis.text=element_text(size=12,face="bold"), panel.background = element_blank(), axis.line = element_line(colour = "black"),
                axis.text.x = element_text(angle = 60, hjust = 1)))
  dev.off()

  #compare values of within individual means and variability of watch data to clinical data
  delta.RHR.pulse<-restingDf.vitals$Pulse - restingDf.vitals$restingHR

  #output histogram of same-day clinical and watch data
  png(file=paste0(dir.out.graphs,"histogram_sameDay_ClinWear_rhr_",window,"minWindow",ifelse(dayPrior,"_DayPrior",""),".png"),
      width=11.2,height=7,res=300, units = "in", pointsize=12)
  hist(delta.RHR.pulse, breaks=100, col="darkred", main = paste0("cHR - rwHR (Same-Day Clinic Visits, Watch Data at ",window,"min Window)"))
  dev.off()

  # scenario 3: summary of wRHR from *all clinic* days of watch monitoring
  #get mean and SD by iPOP
  options(datatable.optimize=1)
  means <- restingDf.vitals[, lapply(.SD, mean, na.rm=TRUE), by=iPOP_ID]
  # ^ Quicker data table solution for aggregating all columns by iPOP.
  sd <- restingDf.vitals[, lapply(.SD, sd, na.rm=TRUE), by=iPOP_ID]
  # ^ Quicker data table solution for aggregating all columns by iPOP.
  options(datatable.optimize=0)

  # scenario 3: get delta (clin-wear)
  delta.mean.RHR.pulse<-means$Pulse - means$restingHR #hist(delta.mean.RHR.pulse, breaks=100, col="darkred", main = "cHR - rwHR (Individual Means)")
  delta.sd.RHR.pulse<-sd$Pulse - sd$restingHR #hist(delta.sd.RHR.pulse, breaks=100, col="darkred", main = "cHR - rwHR (Individual Stdevs)")
  dfDelta <- data.frame(cbind(iPOP_ID=means$iPOP_ID, delta.mean.RHR.pulse, delta.sd.RHR.pulse))
  dfDelta <- transform(dfDelta, delta.mean.RHR.pulse = as.numeric(delta.mean.RHR.pulse),
                       delta.sd.RHR.pulse = as.numeric(delta.sd.RHR.pulse))
  
  # scenario 3: summary of wRHR from *all clinic* days of watch monitoring
  p3<-t.test(means$restingHR, means$Pulse, var.equal=FALSE, paired=TRUE) # test for significance of difference between means
  p3.1<-t.test(sd$restingHR, sd$Pulse, var.equal=FALSE, paired=TRUE) # test for significance of difference between stdev
  p3$method <- paste0("Paired T-Test between Wearable and Clinical (",window,"min Window)")
  p3.1$method <- paste0("Paired T-Test between Wearable and Clinical (",window,"min Window)")
  
  # scenario 3: summary of wRHR from *all clinic* days of watch monitoring
  #output scatterplot of within-individual means & variability
  png(file=paste0(dir.out.graphs,"scatterplot_withinPerson_delta_rhrMeansAndSDs_",window,"minWindow",ifelse(dayPrior,"_DayPrior",""),".png"),
      width=11.2,height=7,res=300, units = "in", pointsize=12)
  print(ggplot(dfDelta) +
    geom_point(aes(x=iPOP_ID, y=delta.mean.RHR.pulse), color="blue") +
    geom_point(aes(x=iPOP_ID, y=delta.sd.RHR.pulse), color="orange") +
    labs(title = paste0(window,"min resting window"), y="Clinical Pulse \n - \n Wearable \n Resting Heart Rate") +
    theme(axis.title=element_text(face="bold",size="12"),
          axis.text=element_text(size=12,face="bold"),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(angle = 60, hjust = 1)) +
    geom_hline(yintercept=0, color="red"))
  dev.off()

  ###############################
  # Differences by Hour of Day  #
  ###############################
  
  # TODO: look into how much data there is for each window (e.g. shouldnt be much when the resting window definition is 2hr, for example)
  subDfClinDay <-list()
  png(file=paste0(dir.out.graphs,"boxplotGrid_sameDay_timeOfDayComparisons_WearVsClin_",window,"minWindow",ifelse(dayPrior,"_DayPrior",""),".png"),
      width=11.2,height=7,res=300, units = "in", pointsize=12)
  par(mfrow=c(2,4))
  for (j in 1:8){ # vary the hour of the day
    subDfClinDay[[j]] <- with( restingDf.vitals, restingDf.vitals[ hour( DateTime ) >= j+2 & hour( DateTime ) < j+3 , ] ) # pull data only from specific time window; store hourly resting data for boxplots
    rhr.daily.means <- subDfClinDay[[j]][, lapply(.SD, mean, na.rm=TRUE), by=c("iPOP_ID","Date")]
    # ^ seems like more measurements than there should be if the resting HR definition is as stringent as is meant to be
    restingDf.compare <- cbind(rhr.daily.means$restingHR, rhr.daily.means$Pulse) 
    colnames(restingDf.compare) <- c("Resting wHR", "cHR")
    numObs <-dim(rhr.daily.means)[1]
    title=(paste0((j+2), " - ",(j+3), " am\n Num Obs = ", numObs))
    boxplot(restingDf.compare, ylab="Heart Rate", col = "darkred", main=title) 
    sig.test<-t.test(restingDf.vitals$restingHR, restingDf.vitals$Pulse, var.equal=FALSE, paired=TRUE) # test wHR v cHR for significance
    delta.RHR.pulse<-restingDf.compare[,1] - restingDf.compare[,2];
    means<-aggregate(restingDf.vitals, list(restingDf.vitals$iPOP_ID), mean) # check this compare to indiv means
    sd<-aggregate(restingDf.vitals, list(restingDf.vitals$iPOP_ID), sd)
    delta.mean.RHR.pulse<-means$Pulse - means$restingHR
    delta.sd.RHR.pulse<-sd$Pulse - sd$restingHR 
    print(c("sig.test",sig.test))
  
    
  }
  mtext(paste0(window,"min resting window"), side = 3, line = -2, outer = TRUE)
  dev.off()
  
  #####################################
  # Differences by Weekend v Weekday  #
  #####################################
  # TODO: include weekend vs weekday analysis
  
}
  
# In case you want to graph data when all.x=TRUE option is applied,
# run line #122 with all.x=TRUE; then run lines #123-152; then run #167-177.

# #store vector of iPOP IDs with both clinical and wearable data
# iPOPsubset <- aggregate(variable ~ iPOP_ID, 
#                         function(x) length(unique(x)), 
#                         data=restingDf.vitals.melt[complete.cases(restingDf.vitals.melt)])
# iPOPsubset <- iPOPsubset[which(iPOPsubset$variable==2),1]
# 
# #plot version with only iPOPs in vitals
# ggplot(restingDf.vitals.melt[restingDf.vitals.melt$iPOP_ID %in% iPOPsubset]) +
#   geom_boxplot(aes(x=iPOP_ID, y=value, color=variable), outlier.shape=NA) +
#   labs(title="Clinical Pulse vs.\n Wearable Resting\n Heart Rate ",x="Resting Heart Rate",y="Pulse") +
#   weartals_theme
