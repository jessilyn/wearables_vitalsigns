require(fasttime) #fastPOSIXct
library(reshape2)
library("docopulae")

# make vector of unique iPOP_ID and Date combos
dir <- "/Users/jessilyn/Desktop/framework_paper/manuscript_drafts/SUBMISSION/REVISION/Fitbit_study/vitals_fitbit_study/"
filenames <- list.files(path = dir, pattern = NULL)
fileVec<- colsplit(string=filenames, pattern="_", names=c("iPOP_ID", "Type", "Date1", "Date2"))
# match HR and steps files
for (i in 1:length(fileVec$Type)){
      hr_filename <- paste0(dir, fileVec[j,1],"_",fileVec[j,2],"_",fileVec[j,3],"_",fileVec[j,4])
      steps_filename <- paste0(dir, fileVec[i,1],"_Steps_",fileVec[i,3],"_",fileVec[i,4])
    if (file.size(hr_filename) > 0) {
      fitbitHR <- read.csv(hr_filename,
                           header=TRUE, sep='\t', stringsAsFactors=FALSE, row.names=NULL) } 
      #else next
    if (file.size(steps_filename) > 0) {
      fitbitSteps <- read.csv(steps_filename,
                              header=TRUE, sep='\t', stringsAsFactors=FALSE, row.names=NULL) } 
      #else next
      df <- merge(fitbitHR,
                  fitbitSteps,
                  by=c("date_time", "uid"))
      # df[,"Heart_Rate"] <- apply(df[,"Heart_Rate"], 2, remove_outliers) # clean data based on HR (TODO: later also clean on Skin Temp, Steps)
      # restrict to daytime values only (between 6am-10pm)
      df$date_time<-fastPOSIXct(df$date_time) # takes a very long time

}
