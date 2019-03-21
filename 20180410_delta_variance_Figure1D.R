#### REQUIRED
# require(data.table)
# require(ggplot2)
# require(plyr)
# require(psych)
# require(zoo)
# 
# #### DATA
# # Path to the directory with data
# dir = "/srv/gsfs0/projects/snyder/jdunn/framework_paper/SECURE_data"
# 
# # df <- fread(paste0(dir, "/BasisData_20161111_PostSummerAddOns_Cleaned_NotNormalized_20170928.csv"),
# #             header=TRUE,sep=",",stringsAsFactors = FALSE)
# df <- fread("/Users/jessilyn/Desktop/framework_paper/Figure1/Fig1C/Ryans_input_files/BasisData_20161111_PostSummerAddOns_Cleaned_NotNormalized_20170928.csv",
#             header=TRUE,sep=",",stringsAsFactors = FALSE)
# ##### SAMPLE DEMOGRAPHICS
# # inven <- fread(paste0(dir, "/ClinWearDemo_SamplePop.csv"),
# #                header=TRUE,sep=",",stringsAsFactors = FALSE)
# inven <- fread("/Users/jessilyn/Desktop/framework_paper/Figure1/Fig1C/Ryans_input_files/ClinWearDemo_SamplePop.csv",
#                header=TRUE,sep=",",stringsAsFactors = FALSE)
# inven <- inven[!duplicated(inven),]
# 
# unq <- unique(df$Wearable_Account_MD5)
# 
# cache <- sapply(inven$MD5s_SortedWithSerial, function(x) unique(unlist(strsplit(x,","))))
# names(cache) <- inven$iPOP_ID
# 
# #This creates a list of "unq" indices which match known subject MD5s
# #(i.e. It links MD5s present in the Basis table with associated iPOP IDs.)
# links <- lapply(cache, function(i) which(unq %in% i))
# 
# #Calculate number of days monitored by wearable for each subj.
# subj_wearObs_timespan <- c()
# for(i in 1:length(links)){
#   subj_wearObs_timespan[i] <- difftime(
#     max(df$Timestamp_Local[which(df$Wearable_Account_MD5 %in% unq[links[[i]]])],na.rm=TRUE),
#     min(df$Timestamp_Local[which(df$Wearable_Account_MD5 %in% unq[links[[i]]])],na.rm=TRUE),
#     units="days")
# }
# 
# #Name days of obs. vector by iPOP
# names(subj_wearObs_timespan) <- names(links)
# 
# #Create descriptive stats for number of days monitored
# describe(subj_wearObs_timespan)
# 
# #Create table of days monitored by subj.
# wear_dm <- data.frame("iPOP_ID"=names(subj_wearObs_timespan),
#                       "Wearable_Days_Monitored"=subj_wearObs_timespan)
# 
# ##### SAMPLE WEARABLE DESCRIPTIVES
# wear_stats <- rbind(describe(df$GSR),describe(df$Heart_Rate),
#                     describe(df$Skin_Temperature_F),describe(df$Steps))
# wear_stats$vars <- c("GSR","Heart_Rate","Skin_Temperature_F","Steps")
# 
# # write.csv(wear_stats,paste0("E:\\Basis_Data\\Supplemental\\",
# #                             "slide2_D_SamplePopulation_WearableDescriptives.csv"),row.names=FALSE)
# 
# ##################
# #  Figure 1D Top #
# ##################
# 
# #### WHITE COAT GRAPH (Slide 14: B)
# vitals <- fread("/Users/jessilyn/Desktop/framework_paper/Figure1/Fig1C/Ryans_input_files/Vitals.csv",
#                 header=TRUE,sep=',',stringsAsFactors=FALSE)
# # vitals <- fread(paste0(dir,"/Vitals.csv"),
# #   header=TRUE,sep=',',stringsAsFactors=FALSE)
# 
# unq_vitals_iPOPs <- unique(vitals$HIMC_ID)
# length(which(names(links) %in% unq_vitals_iPOPs))
# df$Date <- as.POSIXct(paste(df$Timestamp_Local),format="%Y-%m-%d %H:%M:%S")
# 
# #
# restingDf <- data.frame("iPOP_ID"=NA,"Date"=NA,"restingHR"=NA)
# require( lubridate )
# #for(i in 1:length(links)){
# for(i in 1:3){
#   print(unq[links[[i]]])
#   subDf <- df[which(df$Wearable_Account_MD5 %in% unq[links[[i]]]),]
#   #subDfMorning <- df[which("Date"=unlist(strsplit(restingDf$Date,",") %in% c("7"*,"8"*)),] # new line added to look only at 7-8am HR and ST
#   subDfMorning <- with( subDf , subDf[ hour( Date ) >= 5 & hour( Date ) < 6 , ] )
#   subDf <- subDfMorning
#   restingDate <- subDf$Date #### ADDED
#   
#   restingMD5 <- subDf$Wearable_Account_MD5 #### ADDED
#   
#   restingST <- ave(subDf$Steps, subDf$Wearable_Account_MD5,
#                    FUN=function(x) rollapplyr(x, 10, sum, na.rm=TRUE, 
#                                               by=1, partial=TRUE, fill=NA)) #### ADDED
#   
#   restingHR <- ave(subDf$Heart_Rate, subDf$Wearable_Account_MD5,
#                    FUN=function(x) rollapplyr(x, 10, mean, na.rm=TRUE, 
#                                               by=1, partial=TRUE, fill=NA)) #### ADDED
#   
#   restingDate <- restingDate[which(restingST<10 | !is.na(restingST))]
#   restingMD5 <- restingMD5[which(restingST<10 | !is.na(restingST))]
#   restingHR <- restingHR[which(restingST<10 | !is.na(restingST))]
#   
#   restingDf[i,1] <- paste(restingMD5,collapse=",")
#   restingDf[i,2] <- paste(restingDate,collapse=",")
#   restingDf[i,3] <- paste(restingHR,collapse=",")
# }
# 
# restingHR_Df <- data.frame("iPOP_ID"=unlist(strsplit(restingDf$iPOP_ID,",")),
#                            "Date"=unlist(strsplit(restingDf$Date,",")),
#                            "restingHR"=unlist(strsplit(restingDf$restingHR,",")),
#                            stringsAsFactors = FALSE)
# 
# #link MD5s with iPOPs
# tmp <- sapply(restingHR_Df$iPOP_ID, 
#               function(i) names(cache)[which(lapply(cache, function(q) which(i %in% q))==1)])
# restingHR_Df$iPOP_ID <- tmp
# 
# #merge vitals with resting HR
# class(vitals$HIMC_ID)
# class(restingHR_Df$iPOP_ID)
# restingHR_Df$Date <- as.character(as.Date(restingHR_Df$Date))
# 
# 
# test <- merge(restingHR_Df,vitals,by.x=c("iPOP_ID","Date"),by.y=c("HIMC_ID","RESULT_TIME"),allow.cartesian = TRUE)
# 
# #plot
# test$restingHR <- as.numeric(test$restingHR)
# test$Date <- as.Date(test$Date)
# 
# wear_vals <- aggregate(restingHR ~ iPOP_ID + Date, mean, na.rm=TRUE, data=test)
# vital_vals <- aggregate(Pulse ~ iPOP_ID + Date, mean, na.rm=TRUE, data=test)
# 
# d <- names(table(vital_vals$iPOP_ID)[table(vital_vals$iPOP_ID)>15])
# subsetVital_vals <- vital_vals[vital_vals$iPOP_ID %in% d,] # should be length 230 bc sum(vital_vals$iPOP_ID %in% d)
# plotDf <- merge(wear_vals,subsetVital_vals)
# plotDf <- merge(wear_vals,vital_vals)
#write.csv(plotDf, paste0(dir,"/20180409/20180409_5am_restingHRvPulse_plot.csv"))


#iPOP demographics
iPOPdemographics <- read.csv("/Users/jessilyn/Desktop/framework_paper/SECURE_data/SECURE_ClinWearDemo_SamplePop.csv",
                             header=TRUE,sep=',',stringsAsFactors=FALSE)

plotDf <-read.csv("/Users/jessilyn/Desktop/framework_paper/Figure1/Fig1D/Fig1D_timecourse/20180409_3am_restingHRvPulse_plot.csv")
library(reshape2)
plotDf.m <- melt(plotDf,id.vars='iPOP_ID', measure.vars=c('restingHR','Pulse'))
tt <- table(plotDf.m$iPOP_ID)
plotDf.m.many <- subset(plotDf.m, iPOP_ID %in% names(tt[tt > 4])) # try restricting this analysis to people who have more than x clinic visits (e.g. 5 visits)
plotDf.demo <- subset(iPOPdemographics, iPOP_ID %in% names(tt[tt > 4]))

########
#  clin vs wearable dotplot

ggplot(plotDf, aes(x=restingHR,y=Pulse,col=as.factor(substr(iPOP_ID,9,12)))) +
  geom_point() +
  #geom_smooth(method="glm", formula =y~x, se=F) +
  labs(title="Clinical Pulse vs.\n Wearable Resting\n Heart Rate (5-6am) Mean ",x="Resting Heart Rate",y="Pulse") +
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



#try coloring x axis labels by gender - right now this doesnt work.
plotDf.m.many <-merge(plotDf.m.many, plotDf.demo[,1:4], by="iPOP_ID")
plotDf.m.many$Gender <- as.factor(plotDf.m.many$Gender)
numColors <- length(levels(plotDf.m.many$Gender)) # How many colors you need
myPalette <- c("red" ,"blue")
names(myPalette) <- levels(plotDf.m.many$Gender) # Give every color an appropriate name
#plotDf.m.many <- plotDf.m.many[!(plotDf.m.many$iPOP_ID %in% "1636-69-090"),] # for the 3-4am and 8-9am comparison

p <- ggplot(plotDf.m.many) +
  geom_boxplot(aes(x=iPOP_ID, y=value, color=variable)) +
  labs(title="Clinical Pulse vs.\n Wearable Resting\n Heart Rate (8-9am) Mean ",x="Resting Heart Rate",y="Pulse") +
  theme(plot.title=element_text(face="bold",colour="black",size=14),
        axis.title.x=element_text(face="bold",colour="black",size=14),
        #axis.text.x=element_text(face="bold",colour=myPalette[plotDf.m.many$Gender],size=12,angle=55,vjust=0.9,hjust=1),
        axis.text.x=element_text(face="bold",colour="black",size=12,angle=55,vjust=0.9,hjust=1),
        axis.title.y=element_text(face="bold",colour="black",size=14),
        axis.text.y=element_text(face="bold",colour="black",size=12),
        axis.ticks.length = unit(.2,"cm"),
        legend.title=element_text(face="bold", colour="black", size=14),
        legend.text=element_text(face="bold", colour="black", size=12),
        panel.background=element_rect(fill="grey94")) +
  ylim(c(45,100))
p

p <- ggplot(plotDf.m.many) +
  geom_boxplot(aes(x=iPOP_ID, y=value, color=variable)) +
  labs(title="Clinical Pulse vs.\n Wearable Resting\n Heart Rate (8-9am) Mean ",x="Resting Heart Rate",y="Pulse") +
  theme(plot.title=element_text(face="bold",colour="black",size=14),
        axis.title.x=element_text(face="bold",colour="black",size=14),
        axis.text.x=element_text(face="bold",colour=cols$Gender,size=12,angle=55,vjust=0.9,hjust=1),
        axis.title.y=element_text(face="bold",colour="black",size=14),
        axis.text.y=element_text(face="bold",colour="black",size=12),
        axis.ticks.length = unit(.2,"cm"),
        legend.title=element_text(face="bold", colour="black", size=14),
        legend.text=element_text(face="bold", colour="black", size=12),
        panel.background=element_rect(fill="grey94"))
p

#ggplot(plotDf, aes(x=restingHR,y=Pulse,col=as.factor(substr(iPOP_ID,9,12)))) +
ggplot(plotDf) +
  geom_boxplot(aes(x=as.factor(substr(iPOP_ID,9,12)),y=Pulse), col="blue")+
  geom_boxplot(aes(x=as.factor(substr(iPOP_ID,9,12)),y=restingHR), col="red") +
  #geom_point() +
  #geom_smooth(method="glm", formula =y~x, se=F) +
  labs(title="Clinical Pulse vs.\n Wearable Resting\n Heart Rate (3-4am) Mean ",x="Resting Heart Rate",y="Pulse") +
  # annotate("segment",x=-Inf,xend=Inf,y=-Inf,yend=Inf,
  #          lwd=2, color="blue", alpha=.25) +
  #guides(col=guide_legend("ID")) +
  #xlim(40, 100) +
  #ylim(40, 100)+
  theme(plot.title=element_text(face="bold",colour="black",size=14),
        axis.title.x=element_text(face="bold",colour="black",size=14),
        axis.text.x=element_text(face="bold",colour="black",size=12,angle=55,vjust=0.9,hjust=1),
        axis.title.y=element_text(face="bold",colour="black",size=14),
        axis.text.y=element_text(face="bold",colour="black",size=12),
        axis.ticks.length = unit(.2,"cm"),
        legend.title=element_text(face="bold", colour="black", size=14),
        legend.text=element_text(face="bold", colour="black", size=12),
        panel.background=element_rect(fill="grey94"))


#####
#### delta variance (clinic - watch)

library(data.table)
WatchVar <- setDT(plotDf.m.many[plotDf.m.many$variable=="restingHR",])[, list(WatchVariance=var(value)) , by = iPOP_ID]
ClinVar <-  setDT(plotDf.m.many[plotDf.m.many$variable=="Pulse",])[, list(ClinVariance=var(value)) , by = iPOP_ID]
VarDiff <- ClinVar$ClinVariance - WatchVar$WatchVariance
names(VarDiff) <- WatchVar$iPOP_ID
VarDiff2 <- cbind(read.table(text = names(VarDiff)), V3 = VarDiff)
VarDiff2$V1 = factor(VarDiff2$V1, levels = names(sort(tt, decreasing=TRUE)))

ggplot(VarDiff2, aes(x=V1,y=V3)) +
  geom_point() + 
  ylab("Delta Variance (Clinic - Watch)") +
  xlab(NULL) +
  theme(plot.title=element_text(face="bold",colour="black",size=14),
        axis.title.x=element_text(face="bold",colour="black",size=14),
        axis.text.x=element_text(face="bold",colour="black",size=12,angle=55,vjust=0.9,hjust=1),
        axis.title.y=element_text(face="bold",colour="black",size=14),
        axis.text.y=element_text(face="bold",colour="black",size=12),
        axis.ticks.length = unit(.2,"cm"),
        legend.title=element_text(face="bold", colour="black", size=14),
        legend.text=element_text(face="bold", colour="black", size=12),
        panel.background=element_rect(fill="grey94")) +
  geom_hline(yintercept = 0, col="red")

visit.counts <- data.frame(tt)
df.visits <- merge(visit.counts, VarDiff2, by.x = "Var1", by.y = "V1")
ggplot(df.visits, aes(x=Freq, y=V3))+
  geom_point() +
  xlab("Number of Clinic Visits")+
  ylab("Delta Variance (Clinic - Watch)")+
  geom_hline(yintercept = 0, color="red")



#####
#### delta means (clinic - watch)

WatchMean <- setDT(plotDf.m.many[plotDf.m.many$variable=="restingHR",])[, list(WatchMean=mean(value)) , by = iPOP_ID]
ClinMean <-  setDT(plotDf.m.many[plotDf.m.many$variable=="Pulse",])[, list(ClinMean=mean(value)) , by = iPOP_ID]
MeanDiff <- ClinMean$ClinMean - WatchMean$WatchMean
names(MeanDiff) <- WatchMean$iPOP_ID
MeanDiff2 <- cbind(read.table(text = names(MeanDiff)), V3 = MeanDiff)
MeanDiff2$V1 = factor(MeanDiff2$V1, levels = names(sort(tt, decreasing=TRUE)))

ggplot(MeanDiff2, aes(x=V1,y=V3)) +
  geom_point() + 
  ylab("Delta Mean Pulse (Clinic - Watch)") +
  xlab(NULL) +
  theme(plot.title=element_text(face="bold",colour="black",size=14),
        axis.title.x=element_text(face="bold",colour="black",size=14),
        axis.text.x=element_text(face="bold",colour="black",size=12,angle=55,vjust=0.9,hjust=1),
        axis.title.y=element_text(face="bold",colour="black",size=14),
        axis.text.y=element_text(face="bold",colour="black",size=12),
        axis.ticks.length = unit(.2,"cm"),
        legend.title=element_text(face="bold", colour="black", size=14),
        legend.text=element_text(face="bold", colour="black", size=12),
        panel.background=element_rect(fill="grey94")) +
  geom_hline(yintercept = 0, col="red")

visit.counts <- data.frame(tt)
df.visits <- merge(visit.counts, MeanDiff2, by.x = "Var1", by.y = "V1")
ggplot(df.visits, aes(x=Freq, y=V3))+
  geom_point() +
  xlab("Number of Clinic Visits")+
  ylab("Delta Mean (Clinic - Watch)")+
  geom_hline(yintercept = 0, color="red")