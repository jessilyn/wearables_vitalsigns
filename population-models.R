library(MASS)
library("reshape2")
library("ggthemes")
library(dplyr)
library(reshape2)
library(randomForest)

# Path to the directory with data
dir = "./SECURE_data/"

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

# From now on we have independent

# Linear models for vitals
labs.vitals[,names(labs.vitals) %in% top.names]
models.vitals = lm(as.matrix(labs.vitals[,names(labs.vitals) %in% top.names]) ~ as.matrix(labs.vitals[,c("Pulse","Temp")]))
models.vitals.s = summary(models.vitals)

# Choose best
rsq.vitals = c()

for (i in 1:length(top.names))
  rsq.vitals = c(rsq.vitals, sqrt(models.vitals.s[[i]]$adj.r.squared)) # not crossvalidated but very elementary models here!
names(rsq.vitals) = top.names

## WEARABLES -- build models for wearables and crossvalidate
rsq.all = rsq.vitals
wear.names = list(#tiny = c("st_mean","rhr_mean"),
                  #smpl = c("st_mean","st_sd","rhr_mean","hr_sd"),
                  #smpl.hr = c("sk_mean","gsr_mean", "st_mean","st_sd","rhr_mean","hr_sd"),
                  #smpl.hr.gsr = c("hr_mean","sk_mean","gsr_mean","st_mean","rhr_mean","hr_sd","sk_sd","gsr_sd","st_sd","rhr_sd"),
                  all = colnames(wear)[3:53])

for(i in 1:ncol(labs.wear))
  labs.wear[is.na(labs.wear[,i]), i] <- mean(labs.wear[,i], na.rm = TRUE)

for (j in 1:length(wear.names))
{
  # LOO
  patients = unique(labs.wear.uid)
  col.wear = c(1:2, which(colnames(wear) %in% wear.names[[j]])) 
  col.vitals = c(1:2, 4:5)
  
  # Build to matrices: predictev vs true
  val.true = c()
  val.pred = c()
  
  print(wear.names[j])
  # Linear models for wearables
  for (k in 1:length(patients)){
    loo.mask = patients[k] == labs.wear.uid
    
    ## Build linear regression
    # fml = paste("cbind(",paste(top.names,collapse=" , "),") ~",paste(wear.names[[j]],collapse=" + "))
    # models.wear = lm(formula = fml,
    #                  data = labs.wear[!loo.mask,],
    #                  na.action = na.omit,
    #                  weights = labs.wear$weight)
    # val.true = rbind(val.true, as.matrix(labs.wear[loo.mask,top.names[1]]))
    # val.pred = rbind(val.pred, predict(models.wear, newdata = labs.wear[loo.mask,]))
    ## ----
    
    ######################
    ## Build random forest
    # We will predict one by one, let's create a vector of tests
    res.true = c()
    res.pred = c()
    
    print(patients[k])
    for (l in 1:length(top.names)){
      fml = paste("cbind(",paste(top.names[l],collapse=" , "),") ~",paste(wear.names[[j]],collapse=" + "))
      models.wear = randomForest(as.formula(fml),
                       data = labs.wear[!loo.mask,],
                       weights = labs.wear$weight)
      res.true = c(res.true, as.matrix(labs.wear[loo.mask,top.names[l]]))
      res.pred = c(res.pred, predict(models.wear, newdata = labs.wear[loo.mask,]))
    }
    # Add predictions and true values for the patient k
    val.true = rbind(val.true, res.true)
    val.pred = rbind(val.pred, res.pred)
    # ----
  }
  
  # Diagonal of correlation matrix of pred and true is the correlation for a specific test
  rsq.wear = diag(cor(val.true,val.pred,use = "pairwise.complete.obs"))
  names(rsq.wear) = top.names
  rsq.all = rbind(rsq.all, rsq.wear)
  rownames(rsq.all)[nrow(rsq.all)] = names(wear.names)[j]
}
rownames(rsq.all)[1] = "vitals"
df = data.frame(rsq.all)
df[df<0] = 0 # clamp correlations to 0
df$name = rownames(rsq.all)

# Plot the correlations
data = melt(df, id = "name")
colnames(data) = c("model","test","r_squared")
ggplot(data, aes(test,r_squared, color = model)) + geom_point(size = 5, aes(shape=model, color=model)) +
  weartals_theme +
  labs(x = "Lab tests",y = expression(paste("correlation"))) + ggtitle("Model comparison")
