
#### PURPOSE: Check correlations between vitals and other clinical labs.

#### DEPENDENCIES: Vitals and Labs data; psych libary from R.

#### OUTPUT: Two CSV files to desktop with correlation results.

#### REQUIRED LIBRARIES (i.e. PACKAGES) ####
#install.packages("psych")
require(psych)

#### READ DATA ####
dir = "./"

vitals <- read.csv(
  paste0(dir,"SECURE_data/vitals.csv"),
  header=TRUE,sep=',',stringsAsFactors=FALSE)

labs <- read.csv(
  paste0("./","SECURE_data/lab_results_20170717.csv"),
  header=TRUE,sep=',',stringsAsFactors=FALSE)


# wear <- read.csv(
#   paste0("./","SECURE_data/Basis2016_Norm0824_WeekPrior.csv"),
#   header=TRUE,sep=',',stringsAsFactors=FALSE)
wear <- read.csv(
  paste0("./","SECURE_data/Basis2016_Cleaned_NotNorm0824_WeekPrior.csv"),
  header=TRUE,sep=',',stringsAsFactors=FALSE)

#### PREP VITALS DATA ####

#Rename columns
names(vitals)[which(names(vitals)=="HIMCID")] <- "iPOP_ID"
names(vitals)[which(names(vitals)=="RECORDED_TIME")] <- "Clin_Result_Date"
names(vitals)[which(names(vitals)=="X.Pulse.")] <- "Pulse"
names(vitals)[which(names(vitals)=="X.Temp.")] <- "Temp"

#Reformat dates
vitals$Clin_Result_Date <- format(
  as.Date(vitals$Clin_Result_Date, "%d-%b-%Y"), "%Y-%m-%d")

#Make correlation variables numeric
vitals[,c("Pulse","Temp")] <- apply(
  vitals[,c("Pulse","Temp")], 2,
  function(x) as.numeric(as.character(x)))

#Describe
describe(vitals$Pulse)
describe(vitals$Temp)

#Plot hist
hist(vitals$Pulse)
hist(vitals$Temp)

#Transform to normal (in this case sqrt pulse; winsor Temp)
describe(sqrt(vitals$Pulse))
vitals$Pulse <- sqrt(vitals$Pulse)
describe(winsor(vitals$Temp, trim = 0.05))
vitals$Temp <- winsor(vitals$Temp, trim = 0.05)

#Recheck normalization
describe(vitals$Pulse)
describe(vitals$Temp)

#### PREP LABS DATA ####

#Rename columns
names(labs)[which(names(labs)=="HIMC_ID")] <- "iPOP_ID"
names(labs)[which(names(labs)=="RESULT_TIME")] <- "Clin_Result_Date"

#Reformat dates
labs$Clin_Result_Date <- format(
  as.Date(labs$Clin_Result_Date, "%d-%b-%Y"), "%Y-%m-%d")

#Make correlation variables numeric
allClin <- c("A1C","AG","ALB","ALCRU","ALKP","ALT","AST","BASO",
             "BASOAB","BUN","CA","CHOL","CHOLHDL","CL","CO2",
             "CR","EGFR","EOS","EOSAB","GLOB","GLU","HCT","HDL",
             "HGB","HSCRP","IGM","K","LDL","LDLHDL","LYM","LYMAB",
             "MCH","MCHC","MCV","MONO","MONOAB","NA.","NEUT",
             "NEUTAB","NHDL","PLT","RBC","RDW","TBIL","TGL","TP",
             "UALB","UALBCR","WBC")
#Leave out UALBCRINTP, as it is more categorical than continuous.

for(i in 1:length(allClin)){ #this removes non-numeric characters
  cache <- labs[,c(allClin[i])]
  cache <- gsub("[^0-9.]","",cache) #this keeps decimals
  labs[,c(allClin[i])] <- cache
}

labs[,c(allClin)] <- apply(
  labs[,c(allClin)], 2,
  function(x) as.numeric(as.character(x)))

#Describe
nonNormal <- list()
for(i in 1:length(allClin)){
  if(describe(
    labs[,allClin[i]])$skew>1 | describe(
      labs[,allClin[i]])$kurtosis>1){
    print(allClin[i])
    print(describe(labs[,allClin[i]]))
    nonNormal[[i]] <- allClin[i]
  }
}

#Transform to normal 
nonNormal <- unlist(nonNormal)

describe(labs[,nonNormal[1]])
describe(sqrt(labs[,nonNormal[1]]))
labs[,nonNormal[1]] <- sqrt(labs[,nonNormal[1]])

describe(labs[,nonNormal[2]])
describe(log10(labs[,nonNormal[2]]))
labs[,nonNormal[2]] <- log10(labs[,nonNormal[2]])

describe(labs[,nonNormal[3]])
describe(log10(labs[,nonNormal[3]]))
labs[,nonNormal[3]] <- log10(labs[,nonNormal[3]])

describe(labs[,nonNormal[4]])
describe(log10(labs[,nonNormal[4]]))
labs[,nonNormal[4]] <- log10(labs[,nonNormal[4]])

describe(labs[,nonNormal[5]]+1)
describe(log10(labs[,nonNormal[5]]+1))
labs[,nonNormal[5]] <- log10(labs[,nonNormal[5]]+1)

describe(labs[,nonNormal[6]])
describe(sqrt(labs[,nonNormal[6]]))
labs[,nonNormal[6]] <- sqrt(labs[,nonNormal[6]])

describe(labs[,nonNormal[7]])
describe(sqrt(labs[,nonNormal[7]]))
labs[,nonNormal[7]] <- sqrt(labs[,nonNormal[7]])

describe(labs[,nonNormal[8]])
describe(log10(labs[,nonNormal[8]]))
labs[,nonNormal[8]] <- log10(labs[,nonNormal[8]])

describe(labs[,nonNormal[9]])
describe(log10(labs[,nonNormal[9]]+1))
labs[,nonNormal[9]] <- log10(labs[,nonNormal[9]]+1)

describe(labs[,nonNormal[10]])
describe(log10(labs[,nonNormal[10]]+1))
labs[,nonNormal[10]] <- log10(labs[,nonNormal[10]]+1)

describe(labs[,nonNormal[11]])
describe(log10(labs[,nonNormal[11]]))
labs[,nonNormal[11]] <- log10(labs[,nonNormal[11]])

describe(labs[,nonNormal[12]])
describe(log10(labs[,nonNormal[12]]))
labs[,nonNormal[12]] <- log10(labs[,nonNormal[12]])

describe(labs[,nonNormal[13]])
describe(log10(labs[,nonNormal[13]]))
labs[,nonNormal[13]] <- log10(labs[,nonNormal[13]])

describe(labs[,nonNormal[14]])
describe(log10(labs[,nonNormal[14]]))
labs[,nonNormal[14]] <- log10(labs[,nonNormal[14]])

describe(labs[,nonNormal[15]])
describe(log10(labs[,nonNormal[15]]))
labs[,nonNormal[15]] <- log10(labs[,nonNormal[15]])

describe(labs[,nonNormal[16]])
describe(log10(labs[,nonNormal[16]]))
labs[,nonNormal[16]] <- log10(labs[,nonNormal[16]])

describe(labs[,nonNormal[17]])
describe(sqrt(
  sum(max(labs[,nonNormal[17]],na.rm=TRUE)+1)-labs[,nonNormal[17]]))
labs[,nonNormal[17]] <- sqrt(
  sum(max(labs[,nonNormal[17]],na.rm=TRUE)+1)-labs[,nonNormal[17]])

describe(labs[,nonNormal[18]])
describe(sqrt(
  sum(max(labs[,nonNormal[18]],na.rm=TRUE)+1)-labs[,nonNormal[18]]))
labs[,nonNormal[18]] <- sqrt(
  sum(max(labs[,nonNormal[18]],na.rm=TRUE)+1)-labs[,nonNormal[18]])

describe(labs[,nonNormal[19]])
describe(sqrt(labs[,nonNormal[19]]))
labs[,nonNormal[19]] <- sqrt(labs[,nonNormal[19]])

describe(labs[,nonNormal[20]])
describe(sqrt(labs[,nonNormal[20]]))
labs[,nonNormal[20]] <- sqrt(labs[,nonNormal[20]])

describe(labs[,nonNormal[21]])
describe(log10(labs[,nonNormal[21]]))
labs[,nonNormal[21]] <- log10(labs[,nonNormal[21]])

describe(labs[,nonNormal[22]])
describe(log10(labs[,nonNormal[22]]))
labs[,nonNormal[22]] <- log10(labs[,nonNormal[22]])

describe(labs[,nonNormal[23]])
describe(sqrt(labs[,nonNormal[23]]))
labs[,nonNormal[23]] <- sqrt(labs[,nonNormal[23]])

describe(labs[,nonNormal[24]])
describe(log10(labs[,nonNormal[24]]))
labs[,nonNormal[24]] <- log10(labs[,nonNormal[24]])

describe(labs[,nonNormal[25]])
describe(log10(labs[,nonNormal[25]]))
labs[,nonNormal[25]] <- log10(labs[,nonNormal[25]])

describe(labs[,nonNormal[26]])
describe(log10(labs[,nonNormal[26]]))
labs[,nonNormal[26]] <- log10(labs[,nonNormal[26]])

describe(labs[,nonNormal[27]])
describe(log10(labs[,nonNormal[27]]))
labs[,nonNormal[27]] <- log10(labs[,nonNormal[27]])

describe(labs[,nonNormal[28]])
describe(log10(labs[,nonNormal[28]]))
labs[,nonNormal[28]] <- log10(labs[,nonNormal[28]])

describe(labs[,nonNormal[29]])
describe(log10(labs[,nonNormal[29]]))
labs[,nonNormal[29]] <- log10(labs[,nonNormal[29]])

#### PREP WEARABLE DATA ####

#create list of all wearable vars
allWear <- names(wear)[sum(which(names(wear)=="WBC")+1):NCOL(wear)]

#mean
keywords <- c("hr", "mean")
matches <- sapply(keywords, grepl, allWear, ignore.case=TRUE)
matches <- data.frame(matches)
varNames <- allWear[with(matches, which(hr==TRUE & 
                                          mean==TRUE))]

#for following, use varNames[c(1,7,10,25,26)] for week prior
# and varNames[c(1,7,10,43,44)] for all data
#varNames
hr1 <- varNames[c(1)]
st1 <- sub("hr","st",hr1)
gs1 <- sub("hr","gsr",hr1)
sk1 <- sub("hr","sk",hr1)

rh1 <- varNames[c(2)]

ah1 <- varNames[c(3)]
at1 <- sub("hr","st",ah1)
ag1 <- sub("hr","gsr",ah1)
ak1 <- sub("hr","sk",ah1)

lh1 <- varNames[c(4)]
lt1 <- sub("hr","st",lh1)
lg1 <- sub("hr","gsr",lh1)
lk1 <- sub("hr","sk",lh1)

hr2 <- sub("mean","sd",hr1)
st2 <- sub("mean","sd",st1)
gs2 <- sub("mean","sd",gs1)
sk2 <- sub("mean","sd",sk1)

rh2 <- sub("mean","sd",rh1)

ah2 <- sub("mean","sd",ah1)
at2 <- sub("mean","sd",at1)
ag2 <- sub("mean","sd",ag1)
ak2 <- sub("mean","sd",ak1)

lh2 <- sub("mean","sd",lh1)
lt2 <- sub("mean","sd",lt1)
lg2 <- sub("mean","sd",lg1)
lk2 <- sub("mean","sd",lk1)

hr3 <- sub("mean","median",hr1)
st3 <- sub("mean","median",st1)
gs3 <- sub("mean","median",gs1)
sk3 <- sub("mean","median",sk1)

rh3 <- sub("mean","median",rh1)

ah3 <- sub("mean","median",ah1)
at3 <- sub("mean","median",at1)
ag3 <- sub("mean","median",ag1)
ak3 <- sub("mean","median",ak1)

lh3 <- sub("mean","median",lh1)
lt3 <- sub("mean","median",lt1)
lg3 <- sub("mean","median",lg1)
lk3 <- sub("mean","median",lk1)

hr4 <- sub("mean","kurtosis",hr1)
st4 <- sub("mean","kurtosis",st1)
gs4 <- sub("mean","kurtosis",gs1)
sk4 <- sub("mean","kurtosis",sk1)

rh4 <- sub("mean","kurtosis",rh1)

ah4 <- sub("mean","kurtosis",ah1)
at4 <- sub("mean","kurtosis",at1)
ag4 <- sub("mean","kurtosis",ag1)
ak4 <- sub("mean","kurtosis",ak1)

lh4 <- sub("mean","kurtosis",lh1)
lt4 <- sub("mean","kurtosis",lt1)
lg4 <- sub("mean","kurtosis",lg1)
lk4 <- sub("mean","kurtosis",lk1)

allWear <- c(hr1,hr2,hr3,hr4,
             st1,st2,st3,st4,
             gs1,gs2,gs3,gs4,
             sk1,sk2,sk3,sk4,
             rh1,rh2,rh3,rh4,
             ah1,ah2,ah3,ah4,
             at1,at2,at3,at4,
             ag1,ag2,ag3,ag4,
             ak1,ak2,ak3,ak4,
             lh1,lh2,lh3,lh4,
             lt1,lt2,lt3,lt4,
             lg1,lg2,lg3,lg4,
             lk1,lk2,lk3,lk4)

rm(hr1,hr2,hr3,hr4,
   st1,st2,st3,st4,
   gs1,gs2,gs3,gs4,
   sk1,sk2,sk3,sk4,
   rh1,rh2,rh3,rh4,
   ah1,ah2,ah3,ah4,
   at1,at2,at3,at4,
   ag1,ag2,ag3,ag4,
   ak1,ak2,ak3,ak4,
   lh1,lh2,lh3,lh4,
   lt1,lt2,lt3,lt4,
   lg1,lg2,lg3,lg4,
   lk1,lk2,lk3,lk4)

wear <- wear[,c("iPOP_ID","Clin_Result_Date",allWear)]

for(i in 1:length(allWear)){ #this removes non-numeric characters
  cache <- wear[,c(allWear[i])]
  cache <- gsub("[^0-9.]","",cache) #this keeps decimals
  wear[,c(allWear[i])] <- cache
}

wear[,c(allWear)] <- apply(
  wear[,c(allWear)], 2,
  function(x) as.numeric(as.character(x)))

#identify and remove columns where sum = 0 (all NA)
emptyVar <- c(names(wear[,-c(1:2)])[which(
  apply(wear[,-c(1:2)],2,sum,na.rm=TRUE)==0)])

if(length(emptyVar)!=0){
  wear <- wear[,-c(which(names(wear)==emptyVar))]
}

allWear <- allWear[-c(which(allWear==emptyVar))]

#### FORMAT DATA FRAME FOR CORRELATION MATRIX ####

#Merge data
corDf <- merge(labs[,c("iPOP_ID","Clin_Result_Date",allClin)],
               vitals[,c("iPOP_ID","Clin_Result_Date",
                         "Pulse","Temp")],
               by=c("iPOP_ID","Clin_Result_Date"))

corDf <- merge(corDf,wear,
               by=c("iPOP_ID","Clin_Result_Date"),
               all.x = TRUE)

#Separate ID/Date columns from corDf
ID_Date <- corDf[,c("iPOP_ID","Clin_Result_Date")]
corDf <- corDf[,-c(which(
  names(corDf) %in% c("iPOP_ID","Clin_Result_Date")))]

#### RUN CORRELATIONS ####

#Run
vitalVars <- which(names(corDf) %in% c("Pulse","Temp"))
wearVars <- which(names(corDf) %in% c(allWear))
clinVars <- c(1:NCOL(corDf[-c(vitalVars,wearVars)]))

results <- corr.test(corDf[,c(clinVars)],
                     corDf[,c(vitalVars,wearVars)],
                     method="pearson",adjust="fdr")

#### VIEW CORRELATION RESULTS ####

#View correlations
results$r

#View p-values
results$p

#View sample size for each test
results$n

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

newColOrder <- c("Pulse","Temp",allWear)

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

#### END ####