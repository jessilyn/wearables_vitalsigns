

#Start script

#purpose: create sample size & top factor tables from Lasso

#require
require(dummies) #makes dummy vars by ID
require(ggplot2) #plot linear model
require(glmnet) #Lasso
require(plotmo) #bells and whistles
require(psych)
require(zoo) #label year/quarter

fullpath="~/Desktop/YourPath/"
fullpath="./SECURE_data/"

#read
full <- read.csv(
  paste0(fullpath,
         "../Basis2016_Clean_Norm_WeekPrior_20171020.csv"),
  header=TRUE,sep=',',
  stringsAsFactors = FALSE)

#add missing model info:
modelMissing <- unique(full[is.na(full$Model),"iPOP_ID"])

full$Model[which(full$iPOP_ID==modelMissing[1])] <- "Peak"
full$Model[which(full$iPOP_ID==modelMissing[2])] <- "Peak"
full$Model[which(full$iPOP_ID==modelMissing[3])] <- "B1"
full$Model[which(full$iPOP_ID==modelMissing[4] & 
                   full$Clin_Result_Date >= "2014-09-15" & 
                   full$Clin_Result_Date <= "2014-12-07")] <- "B1"
full$Model[which(full$iPOP_ID==modelMissing[4] & 
                   full$Clin_Result_Date < "2014-09-15" | 
                   full$Clin_Result_Date > "2014-12-07")] <- "Peak"
full$Model[which(full$iPOP_ID==modelMissing[5])] <- "Peak"
full$Model[which(full$iPOP_ID==modelMissing[6])] <- "B1"
full$Model[which(full$iPOP_ID==modelMissing[7])] <- "B1"
full$Model[which(full$iPOP_ID==modelMissing[8])] <- "B1"

#zip <- read.csv("~/Desktop/SECURE_1636 Participant Zip Codes.csv",
#                header=TRUE,sep=',',stringsAsFactors=FALSE)

#full <- merge(full,zip,
#              by.x="iPOP_ID",by.y="Participant.ID",all.x=TRUE)

full <- full[!is.na(full$Model),]
#full <- full[!is.na(full$Zip.Code),]

#add HR algo and seasonal dummy variables
full$HR_AlgoChange <- rep(NA,NROW(full))
#full$Season <- rep(NA,NROW(full))
full$HR_AlgoChange[which(full$Clin_Result_Date<"2015-05-20")] <- 0
full$HR_AlgoChange[which(full$Clin_Result_Date>="2015-05-20")] <- 1
#full$Season <- as.yearqtr(full$Clin_Result_Date, format = "%Y-%m-%d")

full$HR_AlgoChange <- as.factor(full$HR_AlgoChange)
#full$Season <- as.factor(full$Season)
full$Gender <- as.factor(full$Gender)
full$Ethn <- as.factor(full$Ethn)
full$Model <- as.factor(full$Model)
#full$Zip.Code <- as.factor(full$Zip.Code)

HR_AlgoChange <- data.frame(model.matrix( ~ HR_AlgoChange - 1, data=full))
#season <- data.frame(model.matrix( ~ Season - 1, data=full))
gender <- data.frame(model.matrix( ~ Gender - 1, data=full))
ethn <- data.frame(model.matrix( ~ Ethn - 1, data=full))
model <- data.frame(model.matrix( ~ Model - 1, data=full))
#zips <- data.frame(model.matrix( ~ Zip.Code - 1, data=full))

full <- cbind(
  full,
  ethn,
  gender,
  HR_AlgoChange,
  #season,
  #zips,
  model)

cref <- read.csv(
  paste0(fullpath,
         "../clin.csv"),
  header=FALSE,sep=',',
  stringsAsFactors = FALSE)

#Add demographics to phys. subset table
#unq <- unique(full$iPOP_ID)

#### Now run Lasso.

#note demo variables
#allDemo <- names(full)[3:6]

allDemo <- c(
  "AgeIn2016",
  names(ethn)[-which(apply(ethn,2,sum)==min(apply(ethn,2,sum)))],
  names(gender)[-which(apply(gender,2,sum)==min(apply(gender,2,sum)))],
  names(HR_AlgoChange)[-which(apply(HR_AlgoChange,2,sum)==min(apply(HR_AlgoChange,2,sum)))],
  #names(season)[-which(apply(season,2,sum)==min(apply(season,2,sum)))],
  #names(zips)[-which(apply(zips,2,sum)==min(apply(zips,2,sum)))],
  names(model)[-which(apply(model,2,sum)==min(apply(model,2,sum)))]
)

#create list of clinical factors to predict
allClin <- names(full)[7:56]
#find intersect of wanted and available clinical results
#allClin <- intersect(allClin,gsub(" ","",cref[,1]))

#create list of all wearable vars
allWear <- names(full)[57:NCOL(full)]

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

###NEW 0822
dhr1 <- varNames[c(5)]
dst1 <- sub("hr","st",dhr1)
dgs1 <- sub("hr","gsr",dhr1)
dsk1 <- sub("hr","sk",dhr1)

drh1 <- varNames[c(6)]

dah1 <- varNames[c(7)]
dat1 <- sub("hr","st",dah1)
dag1 <- sub("hr","gsr",dah1)
dak1 <- sub("hr","sk",dah1)

dlh1 <- varNames[c(8)]
dlt1 <- sub("hr","st",dlh1)
dlg1 <- sub("hr","gsr",dlh1)
dlk1 <- sub("hr","sk",dlh1)

dhr2 <- sub("mean","sd",dhr1)
dst2 <- sub("mean","sd",dst1)
dgs2 <- sub("mean","sd",dgs1)
dsk2 <- sub("mean","sd",dsk1)

drh2 <- sub("mean","sd",drh1)

dah2 <- sub("mean","sd",dah1)
dat2 <- sub("mean","sd",dat1)
dag2 <- sub("mean","sd",dag1)
dak2 <- sub("mean","sd",dak1)

dlh2 <- sub("mean","sd",dlh1)
dlt2 <- sub("mean","sd",dlt1)
dlg2 <- sub("mean","sd",dlg1)
dlk2 <- sub("mean","sd",dlk1)

dhr3 <- sub("mean","median",dhr1)
dst3 <- sub("mean","median",dst1)
dgs3 <- sub("mean","median",dgs1)
dsk3 <- sub("mean","median",dsk1)

drh3 <- sub("mean","median",drh1)

dah3 <- sub("mean","median",dah1)
dat3 <- sub("mean","median",dat1)
dag3 <- sub("mean","median",dag1)
dak3 <- sub("mean","median",dak1)

dlh3 <- sub("mean","median",dlh1)
dlt3 <- sub("mean","median",dlt1)
dlg3 <- sub("mean","median",dlg1)
dlk3 <- sub("mean","median",dlk1)

dhr4 <- sub("mean","kurtosis",dhr1)
dst4 <- sub("mean","kurtosis",dst1)
dgs4 <- sub("mean","kurtosis",dgs1)
dsk4 <- sub("mean","kurtosis",dsk1)

drh4 <- sub("mean","kurtosis",drh1)

dah4 <- sub("mean","kurtosis",dah1)
dat4 <- sub("mean","kurtosis",dat1)
dag4 <- sub("mean","kurtosis",dag1)
dak4 <- sub("mean","kurtosis",dak1)

dlh4 <- sub("mean","kurtosis",dlh1)
dlt4 <- sub("mean","kurtosis",dlt1)
dlg4 <- sub("mean","kurtosis",dlg1)
dlk4 <- sub("mean","kurtosis",dlk1)

nhr1 <- varNames[c(5)]
nhr1 <- sub("day","night",nhr1)
nst1 <- sub("hr","st",nhr1)
ngs1 <- sub("hr","gsr",nhr1)
nsk1 <- sub("hr","sk",nhr1)

nrh1 <- varNames[c(6)]
nrh1 <- sub("day","night",nrh1)

nah1 <- varNames[c(7)]
nah1 <- sub("day","night",nah1)
nat1 <- sub("hr","st",nah1)
nag1 <- sub("hr","gsr",nah1)
nak1 <- sub("hr","sk",nah1)

nlh1 <- varNames[c(8)]
nlh1 <- sub("day","night",nlh1)
nlt1 <- sub("hr","st",nlh1)
nlg1 <- sub("hr","gsr",nlh1)
nlk1 <- sub("hr","sk",nlh1)

nhr2 <- sub("mean","sd",nhr1)
nst2 <- sub("mean","sd",nst1)
ngs2 <- sub("mean","sd",ngs1)
nsk2 <- sub("mean","sd",nsk1)

nrh2 <- sub("mean","sd",nrh1)

nah2 <- sub("mean","sd",nah1)
nat2 <- sub("mean","sd",nat1)
nag2 <- sub("mean","sd",nag1)
nak2 <- sub("mean","sd",nak1)

nlh2 <- sub("mean","sd",nlh1)
nlt2 <- sub("mean","sd",nlt1)
nlg2 <- sub("mean","sd",nlg1)
nlk2 <- sub("mean","sd",nlk1)

nhr3 <- sub("mean","median",nhr1)
nst3 <- sub("mean","median",nst1)
ngs3 <- sub("mean","median",ngs1)
nsk3 <- sub("mean","median",nsk1)

nrh3 <- sub("mean","median",nrh1)

nah3 <- sub("mean","median",nah1)
nat3 <- sub("mean","median",nat1)
nag3 <- sub("mean","median",nag1)
nak3 <- sub("mean","median",nak1)

nlh3 <- sub("mean","median",nlh1)
nlt3 <- sub("mean","median",nlt1)
nlg3 <- sub("mean","median",nlg1)
nlk3 <- sub("mean","median",nlk1)

nhr4 <- sub("mean","kurtosis",nhr1)
nst4 <- sub("mean","kurtosis",nst1)
ngs4 <- sub("mean","kurtosis",ngs1)
nsk4 <- sub("mean","kurtosis",nsk1)

nrh4 <- sub("mean","kurtosis",nrh1)

nah4 <- sub("mean","kurtosis",nah1)
nat4 <- sub("mean","kurtosis",nat1)
nag4 <- sub("mean","kurtosis",nag1)
nak4 <- sub("mean","kurtosis",nak1)

nlh4 <- sub("mean","kurtosis",nlh1)
nlt4 <- sub("mean","kurtosis",nlt1)
nlg4 <- sub("mean","kurtosis",nlg1)
nlk4 <- sub("mean","kurtosis",nlk1)
###

### NEW: Add dummy variables by ID
dummies <- dummy('iPOP_ID', full, sep = "_")
#full <- cbind(full, dummies)
iPOPvars <- grep("iPOP_ID_.*",names(full),value=TRUE)
###

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
             lk1,lk2,lk3,lk4,
             dhr1,dhr2,dhr3,dhr4,
             dst1,dst2,dst3,dst4,
             dgs1,dgs2,dgs3,dgs4,
             dsk1,dsk2,dsk3,dsk4,
             drh1,drh2,drh3,drh4,
             dah1,dah2,dah3,dah4,
             dat1,dat2,dat3,dat4,
             dag1,dag2,dag3,dag4,
             dak1,dak2,dak3,dak4,
             dlh1,dlh2,dlh3,dlh4,
             dlt1,dlt2,dlt3,dlt4,
             dlg1,dlg2,dlg3,dlg4,
             dlk1,dlk2,dlk3,dlk4,
             nhr1,nhr2,nhr3,nhr4,
             nst1,nst2,nst3,nst4,
             ngs1,ngs2,ngs3,ngs4,
             nsk1,nsk2,nsk3,nsk4,
             nrh1,nrh2,nrh3,nrh4,
             nah1,nah2,nah3,nah4,
             nat1,nat2,nat3,nat4,
             nag1,nag2,nag3,nag4,
             nak1,nak2,nak3,nak4,
             nlh1,nlh2,nlh3,nlh4,
             nlt1,nlt2,nlt3,nlt4,
             nlg1,nlg2,nlg3,nlg4,
             nlk1,nlk2,nlk3,nlk4,
             iPOPvars)

#subset main table by variables wanted for analysis
testDf <- full[,c(allDemo,allClin,allWear)]

#create empty lists for future data frames
dfDevExp.1se <- vector(mode = "list", length=length(allClin))
dfList <- vector(mode = "list", length=length(allClin))
dfList2 <- vector(mode = "list", length=length(allClin))
dfList3 <- vector(mode = "list", length=length(allClin))
dfDevExp.min <- vector(mode = "list", length=length(allClin))
dfList4 <- vector(mode = "list", length=length(allClin))
dfList5 <- vector(mode = "list", length=length(allClin))
dfList6 <- vector(mode = "list", length=length(allClin))
sigCors <- vector(mode = "list", length=length(allClin))

#run Lasso
for(idx in 1:length(allClin)){
  
  #select clinical test
  #clinTest <- as.numeric(which(allClin=="A1C"))
  
  if(allClin[idx]=="X.NA.."){
    next
  }
  if(allClin[idx]=="UALBCRINTP"){
    next
  }
  
  unqIDs <- unique(full$iPOP_ID)
  
  for(id in 1:length(unqIDs)){
    #subset main table by variables wanted for analysis
    testDf <- full[,
                   c(allClin[idx],
                     allWear,
                     allDemo)]
    
    #remove > and < signs to make columns numeric
    testDf[,1] <- gsub("[< | >]", "", testDf[,1])
    testDf[,1] <- as.numeric(testDf[,1])
    
    #check that class of clinical results is always numeric for test
    #apply(testDf[,1:50], 2, function(x) class(x))
    
    #remove NAs from testDf
    for(j in 1:length(allClin[1])){
      #remove rows according to NAs in clinical outcome column
      tmp <- testDf[!is.na(testDf[,j]),]
      #then remove columns with all NAs
      tmp <- tmp[, colSums(is.na(tmp)) != nrow(tmp)]
      #next remove the rest of the NAs (glmnet doesn't handle NAs)
      tmp <- na.omit(tmp)
      #note: imputation may be an alternative
    }
    
    #store iPOP ID and clinical date
    IDandDate <- full[rownames(tmp),1:2]
    
    #scale data
    m <- apply(tmp[,-c(which(names(tmp) %in% c(allDemo,iPOPvars)))],2,mean,na.rm=TRUE)
    s <- apply(tmp[,-c(which(names(tmp) %in% c(allDemo,iPOPvars)))],2,sd,na.rm=TRUE)
    normTmp <- scale(tmp[,-c(which(names(tmp) %in% c(allDemo,iPOPvars)))], m, s)
    tmp <- cbind(normTmp,tmp[,c(which(names(tmp) %in% c(allDemo,iPOPvars)))])
    
    #correct AgeIn2016 to account for the clinical test date
    for(t in 1:NROW(tmp[,"AgeIn2016"])){
      cache <- tmp[t,"AgeIn2016"]
      date <- IDandDate[t,"Clin_Result_Date"]
      newAge <- sum(2016-as.numeric(substr(date,1,4)))
      tmp[t,"AgeIn2016"] <- tmp[t,"AgeIn2016"]-newAge
    }
    
    #also scale age, the only demo needing scaling
    m <- mean(tmp[,"AgeIn2016"],na.rm=TRUE)
    s <- sd(tmp[,"AgeIn2016"],na.rm=TRUE)
    tmp[,"AgeIn2016"] <- scale(tmp[,"AgeIn2016"], m, s)
    
    #check for all-NaN columns; these were all-zero columns beforehand.
    #change all-NaN columns to zero ONLY WHERE IT WAS ORIGINALLY ZERO.
    
    #tmp[, colSums(is.na(tmp)) == nrow(tmp)] <- 0
    
    #separate outcome into clinical outcome and predictive factors
    outcome <- tmp[,1]
    predictors <- tmp[,-c(1)]
    rm(tmp)
    
    #optional:
    #predictors <- predictors[,c(
    #  which(!names(predictors) %in% allDemo[-c(1:23)]))]
    
    #make sure all predictors are numeric
    predictors[] <- apply(predictors,2,
                          function(x) as.numeric(as.character(x)))
    #apply(predictors,2,class)
    
    #identify columns where sum = 0
    if(length(which(apply(predictors,2,sum,na.rm=TRUE)==0))!=0){
      predictors <- predictors[,-c(which(
        apply(predictors,2,sum,na.rm=TRUE)==0))]
    }
    
    # #print correlations for predictors vs outcome
    # for(c in 1:NCOL(predictors)){
    #   testFactor <- predictors[,c]
    #   if(abs(describe(testFactor)$skew) < 3 && abs(describe(testFactor)$kurtosis) < 3){
    #     #print("yes")
    #     sig <- cor.test(outcome,testFactor)$p.value
    #     corEst <- cor.test(outcome,testFactor)$estimate
    #     if(sig < .05){
    #       sigCors[[idx]][c] <- paste("Sig. pearson cor between",
    #                                  names(predictors)[c],
    #                                  "and",
    #                                  allClin[idx],
    #                                  "==",
    #                                  cor.test(outcome,testFactor)$estimate)
    #       #print(c)
    #     } else {
    #       if(sig > .05){
    #         sigCors[[idx]][c] <- paste(names(predictors)[c],
    #                                    "pearson cor wasn't significant.")
    #       }
    #       #print(c)
    #     }
    #   } else {
    #     if(describe(testFactor)$min == 0 && describe(testFactor)$max == 1){
    #       #print("yes")
    #       sig <- cor.test(outcome,testFactor,method="kendall")$p.value
    #       corEst <- cor.test(outcome,testFactor,method="kendall")$estimate
    #       if(sig < .05){
    #         sigCors[[idx]][c] <- paste("Sig. kendall's tau cor between",
    #                                    names(predictors)[c],
    #                                    "and",
    #                                    allClin[idx],
    #                                    "==",
    #                                    cor.test(outcome,testFactor,
    #                                             method="kendall")$estimate)
    #         #print(c)
    #       } else {
    #         if(sig > .05){
    #           sigCors[[idx]][c] <- paste(
    #             names(predictors)[c],
    #             "kendall's tau cor wasn't significant.")
    #         }
    #         #print(c)
    #       }
    #     } else {
    #       #print("yes")
    #       sig <- cor.test(outcome,testFactor,method="kendall")$p.value
    #       corEst <- cor.test(outcome,testFactor,method="kendall")$estimate
    #       if(sig < .05){
    #         sigCors[[idx]][c] <- paste("Sig. kendall's tau cor between",
    #                                    names(predictors)[c],
    #                                    "and",
    #                                    allClin[idx],
    #                                    "==",
    #                                    cor.test(outcome,testFactor,
    #                                             method="kendall")$estimate)
    #         #print(c)
    #       } else {
    #         if(sig > .05){
    #           sigCors[[idx]][c] <- paste(
    #             names(predictors)[c],
    #             "kendall's tau cor wasn't significant.")
    #         }
    #         #print(c)
    #       }
    #       #sigCors[[k]][c] <- paste(names(predictors)[c],
    #       #                         "factor wasn't normal.")
    #       #print(c)
    #     }
    #   }
    # }
    
    #data.frame(sigCors[[idx]])
    # sigCorsRelevant <- data.frame(cors=sigCors[[idx]])
    # sigCorsRelevant$cors <- as.character(sigCorsRelevant$cors)
    # cache <- which(substr(sigCorsRelevant$cors,1,3)=="Sig")
    # sigCorsRelevant <- sigCorsRelevant[cache,]
    # x <- sapply(sigCorsRelevant,function(x) gsub(".*==","",x))
    # cache <- order(abs(as.numeric(x)),decreasing=TRUE)
    # sigCorsRelevant <- data.frame(sigCorsRelevant)
    # sigCorsRelevant <- sigCorsRelevant$sigCorsRelevant[cache]
    # sigCorsRelevant <- data.frame(sigCorsRelevant)
    
    # Leave one out!
    loo.mask = (unqIDs[id] != IDandDate[,1])
    if (sum(!loo.mask) > 0){

    #decide on number for nfolds from number of obs per subject
    frq <- as.vector(table(as.character(IDandDate$iPOP_ID[loo.mask])))
    
    #optional argument for leave-one-out CV method
    n <- length(frq)
    #as.numeric(length(outcome))
    
    #optional argument to specify folds for CV
    folds <- rep(1:length(frq),frq[1:length(frq)])
    
    #run cross-validation
    CV <- cv.glmnet(x=as.matrix(predictors[loo.mask,]),y=as.matrix(outcome[loo.mask]),
                    standardize.response=FALSE,
                    family="gaussian",
                    nfolds=n,
                    foldid=folds,
                    nlambda=100)

    # predict on the remaining one user
    pred = predict(CV,newx = as.matrix(predictors[!loo.mask,]))
    sdev.ratio = mean((pred - outcome[!loo.mask])**2) / mean(outcome[!loo.mask]**2)
    
    #view fit
    #theFit <- data.frame(Df=CV$glmnet.fit$df,
    #                     Dev=CV$glmnet.fit$dev.ratio,
    #                     Lambda=CV$glmnet.fit$lambda)
    
    # #plot Sig. correlations from pre-constructed matrix
    # print(paste("Sig. Factor Correlations Pre-Lasso for", allClin[idx], "model:"))
    # cat("\n")
    # print(sigCorsRelevant[1:NROW(sigCorsRelevant),])
    # 
    # print("####################################################################")
    # 
    #plot CV
    #print(paste("Cross-validation plot for", allClin[idx], "model:"))
    #plot(CV)
    #title(main="Lambda Min. & Lambda Min. + 1se\n", font.main = 1)
    
    #print("####################################################################")
    
    #plot CV lambda (norm)
    #print(paste("Cross-validation normal lambdas for", allClin[idx], "model:"))
    #plot_glmnet(CV$glmnet.fit, xvar="norm", label=NCOL(predictors))
    
    #print("####################################################################")
    
    #plot CV lambda (norm & log)
    #print(paste("Cross-validation normal lambdas & log lambdas for", allClin[idx], "model:"))
    #plot_glmnet(CV$glmnet.fit, xvar="rlambda", label=NCOL(predictors))
    
    #print("####################################################################")
    
    #plot CV deviance explained
    #print(paste("Cross-validation deviance explained for", allClin[idx], "model:"))
    #plot_glmnet(CV$glmnet.fit, xvar="dev", label=NCOL(predictors))
    
    #print("####################################################################")
    
    #extract non-zero coefficients at lambda.1se
    ##print(paste("Extracted non-zero coefficients for", allClin[idx], "model:"))
    options(scipen=999)
    clinical <- allClin[idx]
    
    statNames <- unlist(dimnames(coef(CV$glmnet.fit,s=CV$lambda.1se))[1])
    
    lambda1se <- matrix(coef(CV$glmnet.fit, s=CV$lambda.1se),ncol=1)
    lambda1se <- data.frame(lambda1se)
    colnames(lambda1se) <- allClin[idx]
    lambda1se$StatName <- statNames
    lambda1se <- lambda1se[,c(2,1)]
    cache <- lambda1se[lambda1se[,clinical]!=0 & lambda1se$StatName!="(Intercept)",
                       c("StatName",clinical)]
    ##print(paste("Factors in Lambda.1se Model:"))
    
    ##print(cache[order(abs(cache[,clinical]),decreasing = TRUE),])
    
    lambdaMin <- matrix(coef(CV$glmnet.fit, s=CV$lambda.min),ncol=1)
    lambdaMin <- data.frame(lambdaMin)
    colnames(lambdaMin) <- allClin[idx]
    lambdaMin$StatName <- statNames
    lambdaMin <- lambdaMin[,c(2,1)]
    cache <- lambdaMin[lambdaMin[,clinical]!=0 & lambdaMin$StatName!="(Intercept)",
                       c("StatName",clinical)]
    ##print(paste("Factors in Lambda.min Model:"))
    ##print(cache[order(abs(cache[,clinical]),decreasing = TRUE),])
    
    #table the sample demographics, # of obs. per subj., and the top factors
    
    #save Df, %dev, and Lambda stats from each model
    theFit <- data.frame(ClinTest=allClin[idx],
                         Df=CV$glmnet.fit$df,
                         Dev=CV$glmnet.fit$dev.ratio,
                         Lambda=CV$glmnet.fit$lambda)
    
    dfDevExp.1se[[idx]][[id]] <- data.frame(theFit[theFit$Lambda==CV$lambda.1se,])
    
    #first create list to store tables
    dfList[[idx]][[id]] <- data.frame(
      "Clin_Test"=allClin[idx],
      "Total_Obs"=sum(frq),
      "Num_Subjs"=length(frq)
    )
    #assign(paste0('nDemographics', idx), obsPerSubj)
    
    dfList2[[idx]][[id]] <- data.frame(
      "Clin_Test"=rep(allClin[idx],length(frq)),
      "Subj"=names(table(as.character(IDandDate$iPOP_ID[!loo.mask]))),
      "Num_Obs"=as.vector(table(as.character(IDandDate$iPOP_ID[!loo.mask])))
    )
    #assign(paste0('obsPerSubj', idx), obsPerSubj)
    
    dfList3[[idx]][[id]] <- data.frame(
      "Clin_Test"=rep(allClin[idx],length(
        names(
          sort(
            coef(CV$glmnet.fit, s=CV$lambda.1se)[-1,1],decreasing=TRUE)))),
      "Factor"=names(
        sort(coef(CV$glmnet.fit, s=CV$lambda.1se)[-1,1],decreasing=TRUE)),
      "Rank"=seq(1:length(
        sort(coef(CV$glmnet.fit, s=CV$lambda.1se)[-1,1],decreasing=TRUE))),
      "Coefficient"=as.numeric(
        coef(CV$glmnet.fit, s=CV$lambda.1se)[-1,1][
          order(abs(coef(CV$glmnet.fit, s=CV$lambda.1se)[-1,1]),
                decreasing=TRUE)]
      )
    )
    
    dfDevExp.min[[idx]][[id]] <- data.frame(theFit[theFit$Lambda==CV$lambda.min,])
    
    #first create list to store tables
    dfList4[[idx]][[id]] <- data.frame(
      "Clin_Test"=allClin[idx],
      "Total_Obs"=sum(frq),
      "Num_Subjs"=length(frq)
    )
    #assign(paste0('nDemographics', idx), obsPerSubj)
    
    dfList5[[idx]][[id]] <- data.frame(
      "Clin_Test"=rep(allClin[idx],length(frq)),
      "Subj"=names(table(as.character(IDandDate$iPOP_ID[!loo.mask]))),
      "Num_Obs"=as.vector(table(as.character(IDandDate$iPOP_ID[!loo.mask])))
    )
    #assign(paste0('obsPerSubj', idx), obsPerSubj)
    
    dfList6[[idx]][[id]] <- data.frame(
      "Clin_Test"=rep(allClin[idx],length(
        names(
          sort(
            coef(CV$glmnet.fit, s=CV$lambda.min)[-1,1],decreasing=TRUE)))),
      "Factor"=names(
        sort(coef(CV$glmnet.fit, s=CV$lambda.min)[-1,1],decreasing=TRUE)),
      "Rank"=seq(1:length(
        sort(coef(CV$glmnet.fit, s=CV$lambda.min)[-1,1],decreasing=TRUE))),
      "Coefficient"=as.numeric(
        coef(CV$glmnet.fit, s=CV$lambda.min)[-1,1][
          order(abs(coef(CV$glmnet.fit, s=CV$lambda.min)[-1,1]),
                decreasing=TRUE)]
      )
    )
    #assign(paste0('topFactors', idx), topFactors)
    
    print(paste("ID",unqIDs[id],sdev.ratio))
    }
  } #ea iPOP
  
  print(allClin[idx])
  
}

#Write %dev results to csv (1se)
#dfDevExp.1se
newtest <- dfDevExp.1se
newtest <- unlist(newtest, recursive = FALSE)
newtest <- do.call("rbind", newtest)
newtest$iPOP_LeftOut <- rep(unqIDs,49)
write.csv(newtest,
          paste0("~/Desktop/YourPath/",
                 "Basis2016_Clean_Norm_WeekPrior_1sePctDev_20171027.csv"),
          row.names=FALSE)

#Write total obs and number of subjects list to csv (1se)
#dfList
newtest <- dfList
newtest <- unlist(newtest, recursive = FALSE)
newtest <- do.call("rbind", newtest)
newtest$iPOP_LeftOut <- rep(unqIDs,49)
write.csv(newtest,
          paste0("~/Desktop/YourPath/",
                 "Basis2016_Clean_Norm_WeekPrior_1seTotObsAndSubjs_20171027.csv"),
          row.names=FALSE)

#Write number of obs/subj list to csv (1se)
#dfList2
newtest <- dfList2
newtest <- unlist(newtest, recursive = FALSE)
newtest <- do.call("rbind", newtest)
#newtest$iPOP_LeftOut <- rep(unqIDs,ea=49)
write.csv(newtest,
          paste0("~/Desktop/YourPath/",
                 "Basis2016_Clean_Norm_WeekPrior_1seNumObsPerSubj_20171027.csv"),
          row.names=FALSE)

#Write factor rank results to csv (1se)
#dfList3
newtest <- dfList3
newtest <- unlist(newtest, recursive = FALSE)
newtest <- do.call("rbind", newtest)
#newtest$iPOP_LeftOut <- rep(unqIDs,49)
write.csv(newtest,
          paste0("~/Desktop/YourPath/",
                 "Basis2016_Clean_Norm_WeekPrior_1seFactRank_20171027.csv"),
          row.names=FALSE)

#Write %dev results to csv (min)
#dfDevExp.min
newtest <- dfDevExp.min
newtest <- unlist(newtest, recursive = FALSE)
newtest <- do.call("rbind", newtest)
newtest$iPOP_LeftOut <- rep(unqIDs,49)
write.csv(newtest,
          paste0("~/Desktop/YourPath/",
                 "Basis2016_Clean_Norm_WeekPrior_MinPctDev_20171027.csv"),
          row.names=FALSE)

#Write total obs and number of subjects list to csv (min)
#dfList4
newtest <- dfList4
newtest <- unlist(newtest, recursive = FALSE)
newtest <- do.call("rbind", newtest)
newtest$iPOP_LeftOut <- rep(unqIDs,49)
write.csv(newtest,
          paste0("~/Desktop/YourPath/",
                 "Basis2016_Clean_Norm_WeekPrior_MinTotObsAndSubjs_20171027.csv"),
          row.names=FALSE)

#Write number of obs/subj list to csv (min)
#dfList5
newtest <- dfList5
newtest <- unlist(newtest, recursive = FALSE)
newtest <- do.call("rbind", newtest)
#newtest$iPOP_LeftOut <- rep(unqIDs,49)
write.csv(newtest,
          paste0("~/Desktop/YourPath/",
                 "Basis2016_Clean_Norm_WeekPrior_MinNumObsPerSubj_20171027.csv"),
          row.names=FALSE)

#Write factor rank results to csv (min)
#dfList6
newtest <- dfList6
newtest <- unlist(newtest, recursive = FALSE)
newtest <- do.call("rbind", newtest)
#newtest$iPOP_LeftOut <- rep(unqIDs,49)
write.csv(newtest,
          paste0("~/Desktop/YourPath/",
                 "Basis2016_Clean_Norm_WeekPrior_MinFactRank_20171027.csv"),
          row.names=FALSE)

# #Write %dev results to csv
# devEx.1se <- do.call(rbind,dfDevExp.1se)
# devEx.1se <- devEx.1se[order(devEx.1se$Dev, decreasing=TRUE),]
# write.csv(devEx.1se,
#           paste0("~/Desktop/YourPath/",
#                  "Basis2016_Clean_Norm_AllData_1sePctDev_20171020.csv"),
#           row.names=FALSE)
# 
# #Write %dev results to csv
# devEx.Min <- do.call(rbind,dfDevExp.min)
# devEx.Min <- devEx.Min[order(devEx.Min$Dev, decreasing=TRUE),]
# write.csv(devEx.Min,
#           paste0("~/Desktop/YourPath/",
#                  "Basis2016_Clean_Norm_AllData_MinPctDev_20171020.csv"),
#           row.names=FALSE)





