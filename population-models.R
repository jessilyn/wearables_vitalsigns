library(MASS)
library("reshape2")
library("ggthemes")
library("ggplot2")
library(dplyr)
library(reshape2)
library(randomForest)
library("glmnet")

# Path to the directory with data
dir = "../SECURE_data/"

#source("../20171214_thirtyk_PaperFigures.R") # loads the data
source("load-data.R") # loads the data
source("ggplot-theme.R") # just to make things look nice

#######################
## Population models ##
#######################
v = t(read.csv("top.csv",row.names = 1))
top = as.vector(v)
names(top) = colnames(v)

# order according to accuracy of a simple vitals model (stored in 'top.csv' and it might not be accurate ordering)
top = sort(top,decreasing = TRUE)
top.names = names(top)

# top.names <- c("A1C","AG","ALB","ALCRU","ALKP","ALT","AST","BASO",
#              "BASOAB","BUN","CA","CHOL","CHOLHDL","CL","CO2",
#              "CR","EGFR","EOS","EOSAB","ESR", "GLOB","GLU","HCT","HDL",
#              "HGB","HSCRP","IGM","K","LDL","LDLHDL","LYM","LYMAB",
#              "MCH","MCHC","MCV","MONO","MONOAB","NA.","NEUT",
#              "NEUTAB","NHDL","PLT", "RBC","RDW","TBIL","TGL","TP","UALB","UALBCR","WBC")

# Select variables: iPop_ID, Clin_Result_Data and top variables 
labs.clin = labs[,colnames(labs) %in% c(colnames(labs)[1:2], top.names)]

# Merge by iPop_ID and exam date: labs+vitals, wearables+vitals, wearables+labs
labs.wear.full = merge(data.frame(labs.clin), data.frame(wear), by.x = c("iPOP_ID","Clin_Result_Date"), by.y = c("iPOP_ID","Clin_Result_Date") )
labs.vitals = merge(data.frame(labs.clin), data.frame(vitals), by.x = c("iPOP_ID","Clin_Result_Date"), by.y = c("iPOP_ID","Clin_Result_Date") )
wear.vitals = merge(data.frame(wear), data.frame(vitals), by.x = c("iPOP_ID","Clin_Result_Date"), by.y = c("iPOP_ID","Clin_Result_Date") )
labs.wear = labs.wear.full

#!! Summarize data per subject by taking the mean of variables
# Separate predictors and ids
labs.wear.full = data.frame(labs.wear.full)
labs.wear = aggregate(labs.wear.full[,-c(1,2)], by = list(labs.wear.full$iPOP_ID), function(x){mean(x,na.rm=TRUE)} )
labs.wear.uid = labs.wear[,1]

# From now on we have independent [??]

# Linear models for iPOP vitals ~ labs
#labs.vitals[,names(labs.vitals) %in% top.names]
models.vitals = lm(as.matrix(labs.vitals[,names(labs.vitals) %in% top.names]) ~ as.matrix(labs.vitals[,c("Pulse","Temp")]))
models.vitals.s = summary(models.vitals)

# Choose best R from iPOP vitals ~ labs
rsq.vitals = c()

for (i in 1:length(top.names))
  print(top.names, sqrt(models.vitals.s[[i]]$adj.r.squared))
  rsq.vitals = c(rsq.vitals, sqrt(models.vitals.s[[i]]$adj.r.squared)) # not crossvalidated but very elementary models here!
names(rsq.vitals) = top.names

## WEARABLES -- build models for wearables and crossvalidate
rsq.all = rsq.vitals
# Mean impute
#for(i in 1:ncol(labs.wear))
#  labs.wear[is.na(labs.wear[,i]), i] <- mean(labs.wear[,i], na.rm = TRUE)


wear.names = list(#tiny = c("st_mean","rhr_mean"),
  #smpl = c("st_mean","st_sd","rhr_mean","hr_sd"),
  #smpl.hr = c("sk_mean","gsr_mean", "st_mean","st_sd","rhr_mean","hr_sd"),
  #smpl.hr.gsr = c("hr_mean","sk_mean","gsr_mean","st_mean","rhr_mean","hr_sd","sk_sd","gsr_sd","st_sd","rhr_sd"),
  #goodGLOB = c("hr_kurtosis","hr_mean", "hr_median", "st_mean", "st_sd", "gsr_median", "rhr_mean", "highAct_hr_sd",
  #         "highAct_st_mean", "highAct_st_median", "highAct_st_kurtosis", "highAct_gsr_mean", "highAct_gsr_median",
  #         "highAct_sk_mean", "highAct_sk_median", "highAct_sk_kurtosis", "lowAct_st_mean", "lowAct_st_median"),
  vars = c(),
  all = colnames(wear)[3:53])

for (j in 1:length(wear.names))
{
  # LOO
  patients = unique(labs.wear.uid)
  col.wear = c(1:2, which(colnames(wear) %in% wear.names[[j]])) 
  col.vitals = c(1:2, 4:5)
  
  # Build to matrices: predictev vs true
  val.true = c()
  val.pred = list(lm=c(),rf=c())
  
  # Linear models for wearables
  for (k in 1:length(patients)){
    loo.mask = patients[k] == labs.wear.uid
    
    ######################
    ## Build random forest and linear models
    # We will predict one by one, let's create a vector of tests
    res.true = c()
    res.pred = list(lm=c(),rf=c())
    
    print(patients[k])
    for (l in 1:length(top.names)){
      # skip nas and nans
      nas = cbind(as.matrix(labs.wear[,wear.names[["all"]]]),as.matrix(labs.wear[,top.names[l],drop=FALSE]))
      na.rows = rowMeans(nas)
      drop.rows = (is.na(na.rows) | is.nan(na.rows))
      if (j == 1){     # adaptive choice of variables
        glm.res = cv.glmnet(x=as.matrix(labs.wear[!loo.mask & !drop.rows,wear.names[["all"]]]),y=as.matrix(labs.wear[!loo.mask & !drop.rows,top.names[l],drop=FALSE]),
                            standardize.response=FALSE,
                            family="gaussian",
                            nlambda=100)
        wear.names[["vars"]] = rownames(glm.res$glmnet.fit$beta[abs(glm.res$glmnet.fit$beta[,25]) > 1e-10,])
      }

      # Random forest
      fml = paste("cbind(",paste(top.names[l],collapse=" , "),") ~",paste(wear.names[[j]],collapse=" + "))
      models.wear.rf = randomForest(as.formula(fml),
                       data = labs.wear[!loo.mask & !drop.rows,],
                       weights = labs.wear$weight)
      res.true = cbind(res.true, as.matrix(labs.wear[loo.mask,top.names[l]]))
      res.pred[["rf"]] = cbind(res.pred[["rf"]], predict(models.wear.rf, newdata = labs.wear[loo.mask & !drop.rows,]))
      
      # LM      
      models.wear.lm = lm(as.formula(fml),
                                 data = labs.wear[!loo.mask & !drop.rows,],
                                 weights = labs.wear$weight)
      res.pred[["lm"]] = cbind(res.pred[["lm"]], predict(models.wear.lm, newdata = labs.wear[loo.mask & !drop.rows,]))
      
    }
    if (!nrow(res.pred[["lm"]])){
      res.pred[["lm"]] = NA
      res.pred[["rf"]] = NA
    }
    # Add predictions and true values for the patient k
    val.true = rbind(val.true, res.true)
    val.pred[["lm"]] = rbind(val.pred[["lm"]], res.pred[["lm"]])
    val.pred[["rf"]] = rbind(val.pred[["rf"]], res.pred[["rf"]])
    # ----
  }
  
  # Diagonal of correlation matrix of pred and true is the correlation for a specific test
  rsq.wear = diag(cor(val.true,val.pred[["rf"]],use = "pairwise.complete.obs"))
  names(rsq.wear) = top.names
  rsq.all = rbind(rsq.all, rsq.wear)
  rownames(rsq.all)[nrow(rsq.all)] = paste(names(wear.names)[j],"rf",sep="-")

  # Diagonal of correlation matrix of pred and true is the correlation for a specific test
  if (names(wear.names)[j] != "all"){
    colnames(val.pred[["lm"]])= NULL
    colnames(val.true)= NULL
    rsq.wear = diag(cor(val.true,as.matrix(val.pred[["lm"]]),use = "pairwise.complete.obs"))
    names(rsq.wear) = top.names
    rsq.all = rbind(rsq.all, rsq.wear)
    rownames(rsq.all)[nrow(rsq.all)] = paste(names(wear.names)[j],"lm",sep="-")
  }
}
rownames(rsq.all)[1] = "vitals"
df = data.frame(rsq.all)
df[df<0] = 0 # clamp correlations to 0
df$name = rownames(rsq.all)

# Plot the correlations
data = melt(df, id = "name")
colnames(data) = c("model","test","r_squared")

png('figure2C.png',width = 1700, height = 600,res=120)
vitals_res = data[data$model == "vitals",]
data$test = factor(data$test, levels = vitals_res$test[order(-vitals_res$r_squared)])
ggplot(data, aes(test,r_squared, color = model)) + geom_point(size = 5, aes(shape=model, color=model)) +
  weartals_theme + 
  scale_shape_discrete(breaks=c("all-rf", "vars-rf", "all-lm", "vars-lm", "vitals"),
                       labels=c("RF all variables", "RF + LASSO", "LM all variables", "LM + LASSO", "LM vitals")) +
  scale_color_discrete(breaks=c("all-rf", "vars-rf", "all-lm", "vars-lm", "vitals"),
                       labels=c("RF all variables", "RF + LASSO", "LM all variables", "LM + LASSO", "LM vitals")) +
  labs(x = "Lab tests",y = expression(paste("correlation"))) + ggtitle("Model comparison")
dev.off()