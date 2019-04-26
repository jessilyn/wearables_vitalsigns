
# Framework Paper Figures
# built from 20170905_thirtyk.R

#### OUTPUT: Figures for paper

#### LIBRARY DEPENDENCIES:
library(ggplot2)
library(data.table)
library(psych)
library(lme4)
library(MuMIn)
library(Rmisc)
library(gridExtra)
library(grid)
library(dplyr)


#### FILE DEPENDENCIES: 
#iPOP wearables/clinical combined data
wear <- read.csv("/Users/jessilyn/Documents/Career_Development/Mentoring/RyanRunge/20170803_FINAL_LASSOS/Basis2016_Norm0824_WeekPrior.csv",
                 header=TRUE,sep=',',stringsAsFactors=FALSE)
# iPOP vitals
iPOPvitals <- read.csv(
  paste0("/Users/jessilyn/Documents/Research/iPOP/Clinical/STRIDE/20170717_sixth_stride_data_dump/vitals.csv"),
  header=TRUE,sep=',',stringsAsFactors=FALSE)

#iPOP Labs
iPOPlabs <- read.csv(
  paste0("/Users/jessilyn/Documents/Research/iPOP/Clinical/STRIDE/20170717_sixth_stride_data_dump/lab_results_20170717.csv"),
  header=TRUE,sep=',',stringsAsFactors=FALSE)

# 30k vitals
vitals <- fread("/Users/jessilyn/Documents/Career_Development/Mentoring/RyanRunge/20170803_FINAL_LASSOS/30k_diabetes_cohort/20170831_larger_dataset/all_vitals.csv",
                header=TRUE,sep=',',stringsAsFactors=FALSE)
vitals <- data.frame(vitals)
# 30k labs
labs <- fread("/Users/jessilyn/Documents/Career_Development/Mentoring/RyanRunge/20170803_FINAL_LASSOS/30k_diabetes_cohort/20170831_larger_dataset/all_labs.csv",
              header=TRUE,sep=',',stringsAsFactors=FALSE)
# cleaned 30K file
corDf <- read.csv("/Users/jessilyn/Documents/Career_Development/Mentoring/RyanRunge/20170803_FINAL_LASSOS/20170905_Cleaned_joined_30k_labs_vitals.csv",
                   header=TRUE,sep=',',stringsAsFactors=FALSE)

####################
#### Figure 1  #####
####################
#characterize the 30k data set
length(unique(corDf$ANON_ID)) # num people in 30k dataset where both labs and vitals exist
length(unique(labs$ANON_ID)) # num people in 30k dataset
length(na.omit(labs$Clin_Result_Date)) # num lab tests (in the 50 labs we explored) in 30k dataset
as.matrix(table(labs$LAB_NAME)) # number of each clinical lab
length(na.omit(vitals$Temp)) + length(na.omit(vitals$Pulse)) # total number of clinical vital signs measured
#304 people have more than 50 observations per person
length(table(corDf$ANON_ID)[table(corDf$ANON_ID)>50])

#characterize the iPOP data set
length(na.omit(iPOPvitals$X.Temp.)) + length(na.omit(iPOPvitals$X.Pulse.)) # total number of clinical vital signs measured
length(unique(wear$iPOP_ID)) # num people in iPOP wearables dataset
# num lab tests in iPOP dataset
tot <- 0; for (i in 7:56){
tmp <- length(as.matrix(na.omit(wear[i]))); tot <- tot + tmp}; tot 

# num vital signs in iPOP dataset
tot <- 0; for (i in 7:56){tmp <- length(as.matrix(na.omit(wear[i]))); tot <- tot + tmp}; tot

#### RUN CORRELATIONS ####

# for each lab run a multiple regression:
p<-c()
fstat <-c()
degfree <- c()
tot= 0 
for (i in allClin){
  call <-paste0("corDf$",i)
  df <- cbind(corDf[[i]], corDf[,c("Pulse", "Temp")])
  df <- na.omit(df)
  #tot = tot + df
  print(i) # the number of each clinical lab test that has corresponding vital signs
  #print(c(i , length(df$Pulse))) # the number of each clinical lab test that has corresponding vital signs
  p[i]<-summary(lm(df[,1] ~ df$Pulse + df$Temp))$r.squared 
  #fstat[i]<-summary(lm(df[,1] ~ df$Pulse + df$Temp))$fstatistic
  #degfree[i]<-summary(lm(df[,1] ~ df$Pulse + df$Temp))$df
  }
options("scipen"=100, "digits"=4)
p<-sort(p); str(data.frame(as.list(p)))
fstat<-sort(fstat); str(data.frame(as.list(fstat)))
degfree<-sort(degfree); str(data.frame(as.list(degfree)))
tot # total number of labs that have clin vitals measures corresponding to it

str(data.frame(as.list(b)))

labs.pca <- prcomp(na.omit(corDf[,3:53]),
                 center = TRUE,
                 scale. = TRUE) 

# make boxplots for the top best correlated clinic values with vital signs in the 30k cohort

topHits <- c("GLU_Fasting","HSCRP","ESR", "NEUT","RDW","LYM", "ALB", "PROCALCITONIN")
#topHits <- c("HSCRP","NEUTAB","NEUT","LYM", "PLT", "TroponinI", "ESR", "PROCALCITONIN")

for (i in topHits){
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
boxplot(below, above, outline=FALSE, main=paste(i,", P=", pval, sep=""), names=c("Lowest","Highest"), ylab="Temp")
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


##############
#  Figure 2A #
##############
#Univariate Correlations by Ryan 
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
results <- corr.test(corDf[,c(clinVars)],
                     corDf[,c(vitalVars)],
                     method="pearson",adjust="fdr")

#clin subset of the top 10 most predictive models from bivariate analysis:
clinTopTen <- c("GLU_fasting","CR","HSCRP", "NEUTAB","NEUT","LYM", "RDW","ALB","AG", "PLT","PROCALCITONIN", "ESR")

# boxplots #http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#Helper functions

for (j in clinTopTen){
  corDf$bin2<-ntile(corDf[[j]], 40)
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
 corDf2 <- summarySE(corDf, measurevar="Pulse", groupvars="bin2", na.rm=TRUE)
 print(ggplot(corDf2, aes(x=bin2, y=Pulse)) +
  geom_bar(stat="identity", fill="darkred") +
 geom_errorbar(aes(ymin=Pulse-se, ymax=Pulse+se), width=.2) +
 xlab(paste(c(j, "bins", sep=" ")))
 + theme(text = element_text(size=9),
         axis.text.x = element_text(angle = 60, hjust = 1)))
 print(paste0(j, ": number of data points in bin = ", sum(corDf$bin2 %in% "2")))
}

##############
#  Figure 3A #
##############


#for (i in clinTopTen){
  i <- "NEUT"
  pulse.diff <- c()
  temp.diff <- c()
  pulse.top.quartile <- c()
  pulse.num.top.quartile <- c()
  pulse.bottom.quartile <- c()
  pulse.num.bottom.quartile <- c()
  temp.top.quartile <- c()
  temp.num.top.quartile <- c()
  temp.bottom.quartile <- c()
  temp.num.bottom.quartile <- c()
  
  ptm <- proc.time()
  for (j in corDf$ANON_ID){
    #create personalized quartiles for each person/measurement type; this step takes a very very long time
      corDf$bins2[corDf$ANON_ID %in% j] <- ntile(corDf[,i][corDf$ANON_ID %in% j], 4)
    #get pulse values when the lab measurement for that person is in their lowest or highest quantile
      pulse.top.quartile[j] <- mean(corDf$Pulse[corDf$bins2 >= 4 & corDf$ANON_ID %in% j])
      pulse.num.top.quartile[j] <-length(corDf$Pulse[corDf$bins2 >= 4 & corDf$ANON_ID %in% j])
      pulse.bottom.quartile[j] <- mean(corDf$Pulse[corDf$bins2 <= 1 & corDf$ANON_ID %in% j])
      pulse.num.bottom.quartile[j] <-length(corDf$Pulse[corDf$bins2 <= 1 & corDf$ANON_ID %in% j])
      # make a way to save this for each i
      pulse.diff[j]<-pulse.top.quartile - pulse.bottom.quartile
    
    #get temp values when the lab measurement for that person is in their lowest or highest quantile
      temp.top.quartile[j] <- mean(corDf$Temp[corDf$bins2 >= 4 & corDf$ANON_ID %in% j])
      temp.num.top.quartile[j] <-length(corDf$Temp[corDf$bins2 >= 4 & corDf$ANON_ID %in% j])
      temp.bottom.quartile[j] <- mean(corDf$Temp[corDf$bins2 <= 1 & corDf$ANON_ID %in% j])
      temp.num.bottom.quartile[j] <-length(corDf$Temp[corDf$bins2 <= 1 & corDf$ANON_ID %in% j])
      # make a way to save this for each i
      temp.diff[j]<-temp.top.quartile - temp.bottom.quartile
  }
  proc.time() - ptm

# data in play: temp.diff.neut and pulse.diff.neut and temp.diff.lym and pulse.diff.lym
  hist(temp.diff.neut, breaks=100, main="Temperature Difference Between Personalized  4th and 1st Quartile of Neutrophil Levels", xlab="Temperature Difference", ylab="Number of Individuals", border="black", col="darkblue")
  hist(pulse.diff.neut, breaks=100, main="Pulse Difference Between Personalized 4th and 1st Quartile of Neutrophil Levels", xlab="Pulse Difference", ylab="Number of Individuals", border="black", col="darkred")
  hist(temp.diff.lym, breaks=100, main="Temperature Difference Between Personalized 4th and 1st Quartile of Lymphocyte Levels", xlab="Temperature Difference", ylab="Number of Individuals", border="black", col="darkblue")
  hist(pulse.diff.lym, breaks=100, main="Pulse Difference Between Personalized 4th and 1st Quartile of Lymphocyte Levels", xlab="Pulse Difference", ylab="Number of Individuals", border="black", col="darkred")
  
  write.csv(temp.diff.lym, "~/Desktop/tempdifflym.csv")
  write.csv(pulse.diff.lym, "~/Desktop/pulsedifflym.csv")
  
  length(temp.diff.neut[!is.na(temp.diff.neut)])
#  }
}

