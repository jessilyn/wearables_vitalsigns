---
  output: pdf_document
fontsize: 11pt
geometry: margin=1in
header-includes:
  - \usepackage{caption}
- \usepackage{helvet}
- \usepackage{hyperref}
- \usepackage{fancyhdr}
- \usepackage{textgreek}
- \usepackage{MnSymbol}
- \usepackage{bm}
- \usepackage{xcolor}
- \usepackage{setspace}
- \usepackage{linegoal}
- \usepackage{ocgx}
- \usepackage{tikz}
- \pagestyle{fancy}
- \lhead{}
- \rhead{\hyperlink{toc}{Contents}{}{\textcolor[RGB]{140,21,21}{$\bm\circlearrowleft$}}}
---
  [//]:This is how to leave a comment. This is optional code: \setlength\parindent{11pt}
\hypersetup{colorlinks, breaklinks, linkcolor=[RGB]{0,0,0}}
\renewcommand{\familydefault}{\sfdefault}
\renewcommand{\headrulewidth}{0pt}
\normalfont
\singlespacing
\thispagestyle{empty}
\par\text{ }
\begin{center}
\includegraphics{/Users/Home/Desktop/Ongoing_Projects/Jessie_MobilizeCenter/Stanford_Graphics/AOI_title.png}
\end{center}
\par\text{ }
\par\text{ }
\par\text{ }
\par\text{ }
\par\text{ }
\par\text{ }
\par\text{ }
\par\text{ }
\par\text{ }
\par\text{ }
\par\text{ }
\par\text{ }
\par\text{ }
\par\text{ }
\par\text{ }
\par\text{ }
\par\text{ }
\par\text{ }
\begin{center}
\Huge
\par\textbf{Ryan Runge}
\par\textbf{July 28, 2017}
\end{center}

\doublespacing

\newpage
\hypertarget{toc}{}
\thispagestyle{plain}
\begin{center}
\tableofcontents
\end{center}

\newpage
\begin{center}
\section{Individual Results from 22 NOT-Normalized, Single-Gaussian Lassos}
\end{center}
```{r eval=TRUE,echo=FALSE,message=FALSE,warning=FALSE}
#Start script

#purpose: create sample size & top factor tables from Lasso

#require
require(ggplot2) #plot linear model
require(glmnet) #Lasso
require(plotmo) #bells and whistles
require(psych)
require(zoo) #label year/quarter


###############
#  Figure 2C  #
###############

# creates ranked list of clinical laboratory tests by the %var explained in simple LM; LOO cross validation at the subject level 

source("ggplot-theme.R") # just to make things look nice
#top.names<-c("MONOAB", "HGB", "HCT", "MCHC") # for testing model on small subset

####
# CODE FOR SIMPLE LM
#
sum.vectors.in.list <- function(x) {
  c(sum(na.omit(x)))
}

rsq.all = c()
pct.var.all = c()
vitals.variables <- c("Pulse", "Temp", "AgeIn2016", "Gender", "Ethn") # "BMI", "systolic", "diastolic", 
iPOPcorDf.demo <- merge(iPOPcorDf, iPOPdemographics[1:4], by="iPOP_ID")
patients = unique(iPOPcorDf$iPOP_ID)

val.true <- rep(list(NA),length(top.names)) # list of vectors to store true values; each vector is for 1 clinical lab
val.pred <- rep(list(NA),length(top.names)) # list of vectors to store trained model predicted values; each vector is for 1 clinical lab
val.null.pred <- rep(list(NA),length(top.names)) # list of vectors to store null model predicted values; each vector is for 1 clinical lab
# p.value<-list()  # TODO: decide if this is a relevant parameter to collect - I think not...?
num.true <- rep(list(NA),length(top.names)) # number of observations per individual per clinic test

for (k in 1:length(patients)){
  train <- patients[patients != patients[k]]
  test <- patients[patients == patients[k]]
  cat("Patient",patients[k],"\n") # LOO
  dat.train<-iPOPcorDf.demo[ iPOPcorDf.demo$iPOP_ID %in% train, ] # subset input data by training set
  dat.test<-iPOPcorDf.demo[ iPOPcorDf.demo$iPOP_ID %in% test, ] # subset input data by testing set
  
  for (l in 1:length(top.names)){
    cat("Test",top.names[l],"\n")
    # create training set
    x.train<-dat.train[,colnames(dat.train) %in% c(top.names[l], vitals.variables)] # subset input data by lab: only take current lab test of interest
    x.train<- na.omit(x.train) # skip nas and nans ## TODO: the way this script is written, you will lose a lot of data because you take the number of lab visits down to the test with the minimum number of visits. However, if you do na.omit after the next line, you have to change your matrix to accept dynamic number of row entries. Not sure how to do this yet, so for now just reducing the data amount by a lot. 
    predictors <- as.data.frame(x.train[,colnames(x.train) %in% c(vitals.variables)]) # later add in demographics
    outcome <- as.matrix(x.train[,colnames(x.train) %in% top.names[l]]) # matrix of outcome for model building # tried adding as.numeric after as.matrix() but that introduced new issues
    # create test set
    x.test<-dat.test[,colnames(dat.test) %in% c(top.names[l], vitals.variables)] # subset input data by lab: only take current lab test of interest
    x.test<- na.omit(x.test) # skip nas and nans ## TODO: SEE ABOVE na.omit FOR ISSUE WITH THIS
    val.true[[l]] = c(val.true[[l]], x.test[,top.names[l]]) # true values of left out person
    num.true[[l]]<-c(num.true[[l]],length(x.test[,top.names[l]])) # number of test observations for left out person
    fml = paste("cbind(",paste(top.names[l],collapse=" , "),") ~",paste(vitals.variables,collapse=" + "))
    fml.null = paste(top.names[l]," ~ 1")
    bivar.lm.model = lm(as.formula(fml), data = x.train) # build the model
    val.pred[[l]] = c(val.pred[[l]], predict(bivar.lm.model, newdata = x.test)) # predict on trained model
    bivar.null.lm.model<-lm(as.formula(fml.null), data = x.train) # create null model for hypothesis testing and for calculating RSS0
    val.null.pred[[l]] = c(val.null.pred[[l]], predict(bivar.null.lm.model, newdata = x.test)) # predict on null model
    # t<- anova(bivar.null.lm.model, bivar.lm.model) # to get p-values for model
    # p.value[[l]] <- as.numeric(t[2,][["Pr(>F)"]])  # to get p-values for model
  }
}
num.test.obs <- lapply(num.true, sum.vectors.in.list)

rsq.vitals = c()
rssm.vitals = c()
rss0.vitals = c()
pct.var.explained = c()
num.Records.check <- c()
for (j in 1:length(top.names)){
  rsq.vitals = c(rsq.vitals, cor(val.pred[[j]], val.true[[j]], use = "complete.obs"))
  rssm.vitals = sum(na.omit((val.true[[j]] - val.pred[[j]])^2))
  rss0.vitals = sum(na.omit((val.true[[j]] - val.null.pred[[j]])^2))
  pct.var.explained = c(pct.var.explained, (1 - ( rssm.vitals / rss0.vitals )))
  num.Records.check <- c(num.Records.check, (length(val.pred[[j]])-1)) # same as num.test.obs
}
names(rsq.vitals) = top.names
names(pct.var.explained) = top.names
sqrt.pct.var <- sqrt(pct.var.explained)

####
# CODE FOR LASSO, RF

#clean wear data frame
wear[,8:length(names(wear))] <- apply(
  wear[,8:length(names(wear))], 2,
  function(x) as.numeric(as.character(x)))
wear$Gender <- as.factor(wear$Gender)
wear$Ethn <- as.factor(wear$Ethn)
wear.variables <- unlist(read.table("FinalLasso_153WearableFactors.csv", stringsAsFactors = FALSE)) # the table of model features we want to work with
demo.variables <- c("AgeIn2016", "Gender", "Ethn")

val.true <- rep(list(NA),length(top.names)) # list of vectors to store true values; each vector is for 1 clinical lab
null.val.pred <- rep(list(NA),length(top.names))  # list of vectors to store nullmodel-predicted values; each vector is for 1 clinical lab
lasso.val.pred <- rep(list(NA),length(top.names)) # list of vectors to store lasso-trainedmodel-predicted values; each vector is for 1 clinical lab
rf.val.pred <- rep(list(NA),length(top.names))  # list of vectors to store rf-trainedmodel-predicted values; each vector is for 1 clinical lab
num.Records <- c()
for (k in 1:length(patients)){
  train <- patients[patients != patients[k]]
  test <- patients[patients == patients[k]]
  cat("Patient",patients[k],"\n") # LOO
  dat.train<- wear[ wear$iPOP_ID %in% train, ] # subset input data by training set
  dat.test<-wear[ wear$iPOP_ID %in% test, ] # subset input data by testing set
  for (l in 1:length(top.names)){
    cat("Test",top.names[l],"\n")
    x.train<-dat.train[,colnames(dat.train) %in% c(top.names[l], wear.variables, demo.variables)] # subset input data by lab: only take current lab test of interest
    x.train<- na.omit(x.train) # skip nas and nans ## TODO: the way this script is written, you will lose a lot of data because you take the number of lab visits down to the test with the minimum number of visits. However, if you do na.omit after the next line, you have to change your matrix to accept dynamic number of row entries. Not sure how to do this yet, so for now just reducing the data amount by a lot. 
    predictors <- as.data.frame(x.train[,colnames(x.train) %in% c(wear.variables, demo.variables)]) # later add in demographics
    outcome <- as.matrix(x.train[,colnames(x.train) %in% top.names[l]]) # matrix of outcome for model building # tried adding as.numeric after as.matrix() but that introduced new issues
    
    # create test set
    x.test<-dat.test[,colnames(dat.test) %in% c(top.names[l], wear.variables, demo.variables)] # subset input data by lab: only take current lab test of interest
    x.test<- na.omit(x.test) # skip nas and nans ## TODO: SEE ABOVE na.omit FOR ISSUE WITH THIS
    val.true[[l]] = c(val.true[[l]], x.test[,top.names[l]]) # true values of left out person
    
    num.Records <- rbind(num.Records, c(IPOP_ID=patients[k], test=top.names[l], TrainingObs=length(outcome), TestObs=length(x.test[,top.names[l]])))
    
    rf.variables.to.use = c(wear.variables, demo.variables) # rf variables (use all)
    
    ## run lasso for variable selection
    n <- as.numeric(length(outcome)) #optional argument for leave-one-out CV method for nfold
    x_train <- model.matrix( ~ .-1, as.data.frame(predictors))
    glm.res = cv.glmnet(x=x_train,y=outcome,
                        standardize.response=FALSE,
                        family="gaussian",
                        nfolds=n,
                        nlambda=100)
    lasso.variables.to.use = rownames(glm.res$glmnet.fit$beta[abs(glm.res$glmnet.fit$beta[,25]) > 1e-10,]) # TODO: this is an arbitrary rule for now
    # check if Gender* / Ethn* selected into LASSO models
    ethn.sel = grep("^Ethn",lasso.variables.to.use)
    gend.sel = grep("^Gender",lasso.variables.to.use)
    
    # remove Gender* and add Gender if present
    # remove Ethn* and add Ethn if present
    torm = c(ethn.sel, gend.sel)
    if (length(torm) > 0){
      lasso.variables.to.use = lasso.variables.to.use[-torm]
      if (length(ethn.sel) > 0){
        lasso.variables.to.use = c("Ethn",lasso.variables.to.use) }
      if (length(gend.sel) > 0){
        lasso.variables.to.use = c("Gender",lasso.variables.to.use) # variables.to.use contains all variables selected by LASSO    
      }        
    }
    
    # build null, lasso, and rf models
    set.seed(1)
    null.fml = paste(top.names[l]," ~ 1")
    null.model = lm(as.formula(null.fml), data = x.train) # create null model for hypothesis testing and for calculating RSS0
    null.val.pred[[l]] = c(null.val.pred[[l]], predict(null.model, newdata = x.test)) # predict on null model
    
    lasso.fml = paste("cbind(",paste(top.names[l],collapse=" , "),") ~",paste(lasso.variables.to.use,collapse=" + "))
    lasso.model = lm(as.formula(lasso.fml), data = x.train) # , weights = labs.wear$weight) # TODO: do we need to include weights?
    lasso.val.pred[[l]] = c(lasso.val.pred[[l]], predict(lasso.model, newdata = x.test)) # predict on trained model
    
    rf.fml = paste("cbind(",paste(top.names[l],collapse=" , "),") ~",paste(rf.variables.to.use,collapse=" + "))
    rf.model = randomForest(as.formula(rf.fml), data = x.train)  #weights = labs.wear$weight) # TODO: do we need to include weights?
    rf.val.pred[[l]] = c(rf.val.pred[[l]], predict(rf.model, newdata = x.test)) # predict on left out person
    
    # t<- anova(bivar.null.lm.model, bivar.lm.model) # to get p-values for model
    # p.value[[l]] <- as.numeric(t[2,][["Pr(>F)"]])  # to get p-values for model
    
    ### add in Ryan's code to extract lasso details here ###
    
    
    
    
  }
}

## calculate correlation coefficients and pct var explained by the models
rsq.lasso = c()
rssm.lasso = c()
rss0.lasso = c()
lasso.pct.var.explained = c()
lasso.num.Records <- c()
rsq.rf = c()
rssm.rf = c()
rss0.rf = c()
rf.pct.var.explained = c()
rf.num.Records <- c()

for (j in 1:length(top.names)){
  #lasso
  rsq.lasso = c(rsq.lasso, cor(lasso.val.pred[[j]], val.true[[j]], use = "complete.obs"))
  rssm.lasso = sum(na.omit((val.true[[j]] - lasso.val.pred[[j]])^2))
  rss0.lasso = sum(na.omit((val.true[[j]] - null.val.pred[[j]])^2))
  lasso.pct.var.explained = c(lasso.pct.var.explained, (1 - ( rssm.lasso / rss0.lasso )))
  lasso.num.Records <- c(num.Records, length(lasso.val.pred[[j]]))
  #rf
  rsq.rf = c(rsq.rf, cor(rf.val.pred[[j]], val.true[[j]], use = "complete.obs"))
  rssm.rf = sum(na.omit((val.true[[j]] - rf.val.pred[[j]])^2))
  rss0.rf = sum(na.omit((val.true[[j]] - null.val.pred[[j]])^2))
  rf.pct.var.explained = c(rf.pct.var.explained, (1 - ( rssm.rf / rss0.rf )))
  rf.num.Records <- c(num.Records, length(rf.val.pred[[j]]))
  
}
names(rsq.lasso) = top.names
names(lasso.pct.var.explained) = top.names
lasso.sqrt.pct.var <- sqrt(lasso.pct.var.explained)

names(rsq.rf) = top.names
names(rf.pct.var.explained) = top.names
rf.sqrt.pct.var <- sqrt(rf.pct.var.explained)

fig.2c.df <- cbind(rownames(as.data.frame(sqrt.pct.var)), as.data.frame(sqrt.pct.var), as.data.frame(lasso.sqrt.pct.var), as.data.frame(rf.sqrt.pct.var), row.names=NULL)

colnames(fig.2c.df)<-c("test", "vitals", "lasso", "rf")
fig.2c.df$test = factor(fig.2c.df$test, levels = as.factor(names(sqrt.pct.var)[order(-sqrt.pct.var)]))

fig.2c.corr.coefs <- cbind(rownames(as.data.frame(rsq.vitals)), as.data.frame(rsq.vitals), as.data.frame(rsq.lasso), as.data.frame(rsq.rf), row.names=NULL)
colnames(fig.2c.corr.coefs)<-c("test", "vitals", "lasso", "rf")
fig.2c.corr.coefs$test = factor(fig.2c.corr.coefs$test, levels = as.factor(names(rsq.vitals)[order(-rsq.vitals)]))
fig.2c.corr.coefs[fig.2c.corr.coefs<0]=0 # clamp to zero

fig.2c.plot <- melt(fig.2c.corr.coefs)
fig.2c.plot[,3][is.nan(fig.2c.plot[,3])] <- 0 #replace % var explained of NaN w/ 0
fig.2c <- fig.2c.plot[order(-fig.2c.plot[,3]),] # reorder by LM Vitals

num.Records <- as.data.frame(num.Records)
num.Records <- transform(num.Records, TrainingObs = as.numeric(TrainingObs), 
                         TestObs = as.numeric(TestObs))

# Plot the % var explained
ggplot(fig.2c, aes(x=test, y=value, color = variable)) + geom_point(size = 5, aes(shape=variable, color=variable)) +
  weartals_theme +
  ylim(0,1) +
  scale_shape_discrete(breaks=c("vitals", "lasso", "rf"),
                       labels=c("LM vitals", "LASSO", "RF")) +
  scale_color_discrete(breaks=c("vitals", "lasso", "rf"),
                       labels=c("LM vitals", "LASSO", "RF")) +
  labs(x = "Lab tests",y = expression(paste("Sqrt of % Variance Explained"))) 

# store the results
write.table(num.Records, "../SECURE_data/20180506_num_Records_DayPrior.csv",row.names=FALSE,col.names=FALSE, sep=",")
write.table(fig.2c.df, "../SECURE_data/20180506_pct_var_Dayprior.csv",row.names=FALSE,col.names=c("test", "vitals", "lasso", "rf"), sep=",")
write.table(fig.2c.corr.coefs, "../SECURE_data/20180506_corr_coefs_Dayprior.csv",row.names=FALSE,col.names=c("test", "vitals", "lasso", "rf"), sep=",")
write.table(num.Records, "../SECURE_data/20180507/20180507_Dayprior_num_Records.csv",row.names=FALSE,col.names=FALSE, sep=",")
write.table(num.Records.check, "../SECURE_data/20180507/20180507_Dayprior_num_Records_check.csv",row.names=FALSE,col.names=FALSE, sep=",")
write.table(rf.num.Records.check, "../SECURE_data/20180507/20180507_Dayprior_RF_num_Records.csv",row.names=FALSE,col.names=FALSE, sep=",")
write.table(lasso.num.Records, "../SECURE_data/20180507/20180507_Dayprior_LASSO_num_Records.csv",row.names=FALSE,col.names=FALSE, sep=",")















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
for(k in 1:length(allClin)){
  
  #check for all-NaN columns; these were all-zero columns beforehand.
  #change all-NaN columns to zero ONLY WHERE IT WAS ORIGINALLY ZERO.
  
  #tmp[, colSums(is.na(tmp)) == nrow(tmp)] <- 0
  
  #separate outcome into clinical outcome and predictive factors
  outcome <- tmp[,1]
  predictors <- tmp[,-c(1)]

  #apply(predictors,2,class)

  #print correlations for predictors vs outcome
  for(c in 1:NCOL(predictors)){
    testFactor <- predictors[,c]
    if(abs(describe(testFactor)$skew) < 3 && abs(describe(testFactor)$kurtosis) < 3){
      #print("yes")
      sig <- cor.test(outcome,testFactor)$p.value
      corEst <- cor.test(outcome,testFactor)$estimate
      if(sig < .05){
        sigCors[[k]][c] <- paste("Sig. pearson cor between",
                                 names(predictors)[c],
                                 "and",
                                 allClin[k],
                                 "==",
                                 cor.test(outcome,testFactor)$estimate)
        #print(c)
      } else {
        if(sig > .05){
          sigCors[[k]][c] <- paste(names(predictors)[c],
                                   "pearson cor wasn't significant.")
        }
        #print(c)
      }
    } else {
      if(describe(testFactor)$min == 0 && describe(testFactor)$max == 1){
        #print("yes")
        sig <- cor.test(outcome,testFactor,method="kendall")$p.value
        corEst <- cor.test(outcome,testFactor,method="kendall")$estimate
        if(sig < .05){
          sigCors[[k]][c] <- paste("Sig. kendall's tau cor between",
                                   names(predictors)[c],
                                   "and",
                                   allClin[k],
                                   "==",
                                   cor.test(outcome,testFactor,
                                            method="kendall")$estimate)
          #print(c)
        } else {
          if(sig > .05){
            sigCors[[k]][c] <- paste(
              names(predictors)[c],
              "kendall's tau cor wasn't significant.")
          }
          #print(c)
        }
      } else {
        #print("yes")
        sig <- cor.test(outcome,testFactor,method="kendall")$p.value
        corEst <- cor.test(outcome,testFactor,method="kendall")$estimate
        if(sig < .05){
          sigCors[[k]][c] <- paste("Sig. kendall's tau cor between",
                                   names(predictors)[c],
                                   "and",
                                   allClin[k],
                                   "==",
                                   cor.test(outcome,testFactor,
                                            method="kendall")$estimate)
          #print(c)
        } else {
          if(sig > .05){
            sigCors[[k]][c] <- paste(
              names(predictors)[c],
              "kendall's tau cor wasn't significant.")
          }
          #print(c)
        }
        #sigCors[[k]][c] <- paste(names(predictors)[c],
        #                         "factor wasn't normal.")
        #print(c)
      }
    }
  }
  
  #data.frame(sigCors[[k]])
  sigCorsRelevant <- data.frame(sigCors[[k]])
  sigCorsRelevant$sigCors..k.. <- as.character(sigCorsRelevant$sigCors..k..)
  cache <- which(substr(sigCorsRelevant$sigCors..k..,1,3)=="Sig")
  sigCorsRelevant <- sigCorsRelevant[cache,]
  x <- sapply(sigCorsRelevant,function(x) gsub(".*==","",x))
  cache <- order(x,decreasing=TRUE)
  sigCorsRelevant <- data.frame(sigCorsRelevant)
  sigCorsRelevant <- sigCorsRelevant$sigCorsRelevant[cache]
  sigCorsRelevant <- data.frame(sigCorsRelevant)
  
  #decide on number for nfolds from number of obs per subject
  frq <- as.vector(table(as.character(IDandDate$iPOP_ID)))
  
  #optional argument for leave-one-out CV method
  n <- length(frq)
  #as.numeric(length(outcome))
  
  #optional argument to specify folds for CV
  folds <- rep(1:length(frq),frq[1:length(frq)])
  
  #run cross-validation
  CV <- cv.glmnet(x=as.matrix(predictors),y=outcome,
                  standardize.response=FALSE,
                  family="gaussian",
                  nfolds=n,
                  foldid=folds,
                  nlambda=100)
  
  #view fit
  #theFit <- data.frame(Df=CV$glmnet.fit$df,
  #                     Dev=CV$glmnet.fit$dev.ratio,
  #                     Lambda=CV$glmnet.fit$lambda)
  
  #plot Sig. correlations from pre-constructed matrix
  print(paste("Sig. Factor Correlations Pre-Lasso for", allClin[k], "model:"))
  cat("\n")
  print(sigCorsRelevant[1:NROW(sigCorsRelevant),])
  
  print("####################################################################")
  
  #plot CV
  print(paste("Cross-validation plot for", allClin[k], "model:"))
  plot(CV)
  title(main="Lambda Min. & Lambda Min. + 1se\n", font.main = 1)
  
  print("####################################################################")
  
  #plot CV lambda (norm)
  print(paste("Cross-validation normal lambdas for", allClin[k], "model:"))
  plot_glmnet(CV$glmnet.fit, xvar="norm", label=NCOL(predictors))
  
  print("####################################################################")
  
  #plot CV lambda (norm & log)
  print(paste("Cross-validation normal lambdas & log lambdas for", allClin[k], "model:"))
  plot_glmnet(CV$glmnet.fit, xvar="rlambda", label=NCOL(predictors))
  
  print("####################################################################")
  
  #plot CV deviance explained
  print(paste("Cross-validation deviance explained for", allClin[k], "model:"))
  plot_glmnet(CV$glmnet.fit, xvar="dev", label=NCOL(predictors))
  
  print("####################################################################")
  
  #extract non-zero coefficients at lambda.1se
  print(paste("Extracted non-zero coefficients for", allClin[k], "model:"))
  options(scipen=999)
  clinical <- allClin[k]
  
  statNames <- unlist(dimnames(coef(CV$glmnet.fit,s=CV$lambda.1se))[1])
  
  lambda1se <- matrix(coef(CV$glmnet.fit, s=CV$lambda.1se),ncol=1)
  lambda1se <- data.frame(lambda1se)
  colnames(lambda1se) <- allClin[k]
  lambda1se$StatName <- statNames
  lambda1se <- lambda1se[,c(2,1)]
  cache <- lambda1se[lambda1se[,clinical]!=0 & lambda1se$StatName!="(Intercept)",
                     c("StatName",clinical)]
  print(paste("Factors in Lambda.1se Model:"))
  
  print(cache[order(abs(cache[,clinical]),decreasing = TRUE),])
  
  lambdaMin <- matrix(coef(CV$glmnet.fit, s=CV$lambda.min),ncol=1)
  lambdaMin <- data.frame(lambdaMin)
  colnames(lambdaMin) <- allClin[k]
  lambdaMin$StatName <- statNames
  lambdaMin <- lambdaMin[,c(2,1)]
  cache <- lambdaMin[lambdaMin[,clinical]!=0 & lambdaMin$StatName!="(Intercept)",
                     c("StatName",clinical)]
  print(paste("Factors in Lambda.min Model:"))
  print(cache[order(abs(cache[,clinical]),decreasing = TRUE),])
  
  #table the sample demographics, # of obs. per subj., and the top factors
  
  #save Df, %dev, and Lambda stats from each model
  theFit <- data.frame(ClinTest=allClin[k],
                       Df=CV$glmnet.fit$df,
                       Dev=CV$glmnet.fit$dev.ratio,
                       Lambda=CV$glmnet.fit$lambda)
  
  dfDevExp.1se[[k]] <- data.frame(theFit[theFit$Lambda==CV$lambda.1se,])
  
  #first create list to store tables
  dfList[[k]] <- data.frame(
    "Clin_Test"=allClin[k],
    "Total_Obs"=n,
    "Num_Subjs"=length(frq)
  )
  #assign(paste0('nDemographics', k), obsPerSubj)
  
  dfList2[[k]] <- data.frame(
    "Clin_Test"=rep(allClin[k],length(frq)),
    "Subj"=1:length(frq),
    "Num_Obs"=frq
  )
  #assign(paste0('obsPerSubj', k), obsPerSubj)
  
  dfList3[[k]] <- data.frame(
    "Clin_Test"=rep(allClin[k],length(
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
  
  dfDevExp.min[[k]] <- data.frame(theFit[theFit$Lambda==CV$lambda.min,])
  
  #first create list to store tables
  dfList4[[k]] <- data.frame(
    "Clin_Test"=allClin[k],
    "Total_Obs"=n,
    "Num_Subjs"=length(frq)
  )
  #assign(paste0('nDemographics', k), obsPerSubj)
  
  dfList5[[k]] <- data.frame(
    "Clin_Test"=rep(allClin[k],length(frq)),
    "Subj"=1:length(frq),
    "Num_Obs"=frq
  )
  #assign(paste0('obsPerSubj', k), obsPerSubj)
  
  dfList6[[k]] <- data.frame(
    "Clin_Test"=rep(allClin[k],length(
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
  #assign(paste0('topFactors', k), topFactors)
  
}
```

\newpage
\begin{center}
\section{Overview of All 22 NOT-Normalized, Single-Gaussian Lassos}
\end{center}
```{r eval=TRUE,echo=FALSE,message=FALSE,warning=FALSE}
#merge each data frame list
merged_nDevExp.1se <- do.call("rbind", dfDevExp.1se)

merged_nDemographics.1se <- do.call("rbind", dfList)
merged_obsPerSubj.1se <- do.call("rbind", dfList2)
merged_topFactors.1se <- do.call("rbind", dfList3)
merged_topFactors.1se <- merged_topFactors.1se[merged_topFactors.1se$Coefficient!=0,]

merged_RankedDevExp.1se <- merged_nDevExp.1se[order(
  merged_nDevExp.1se$Dev,decreasing = TRUE),]

merged_nDevExp.min <- do.call("rbind", dfDevExp.min)

merged_nDemographics.min <- do.call("rbind", dfList4)
merged_obsPerSubj.min <- do.call("rbind", dfList5)
merged_topFactors.min <- do.call("rbind", dfList6)
merged_topFactors.min <- merged_topFactors.min[merged_topFactors.min$Coefficient!=0,]

merged_RankedDevExp.min <- merged_nDevExp.min[order(
  merged_nDevExp.min$Dev,decreasing = TRUE),]

#print tables
print("Single-Lasso Models Ranked by %Dev Explained (Lambda.1se)")
print(merged_RankedDevExp.1se)

print("####################################################################")

print("Factors of Most Extreme (High or Low) Non-Zero Coefficients (Lambda.1se)")
print(merged_topFactors.1se)

print("####################################################################")

print("Factors of Most Extreme (High or Low) Non-Zero Coefficients (Lambda.1se)")
print(merged_obsPerSubj.1se)

#print tables
print("Single-Lasso Models Ranked by %Dev Explained (Lambda.min)")
print(merged_RankedDevExp.min)

print("####################################################################")

print("Factors of Most Extreme (High or Low) Non-Zero Coefficients (Lambda.min)")
print(merged_topFactors.min)

print("####################################################################")

print("Factors of Most Extreme (High or Low) Non-Zero Coefficients (Lambda.min)")
print(merged_obsPerSubj.min)
```

\newpage
\begin{center}
\section{Objectives}
\end{center}
$\bullet$ Detail the methods/data for each Lasso run
\par\text{}$\bullet$ Get a viable "Figure 2D" (from paper prep slides)
\par\text{}$\bullet$ Investigate Basis spikes over time
\par\text{}$\bullet$ Add QSART to subset tables
\par\text{}$\bullet$ Format and describe empatica data
\par\text{}$\bullet$ Resume CGM analysis
\par\text{}$\bullet$ Functional clustering of glucose data

\newpage
\begin{center}
\section{Issues}
\end{center}
$\bullet$ None to report
