library(MASS)
library("reshape2")
library("ggthemes")
library(dplyr)
library(reshape2)

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

### Garbage ####
# Corelations
# cors = cor(labs.wear[,-1],use="pairwise.complete.obs")
# cors[order(abs(cors[,"hr_mean"]),decreasing = TRUE),c("hr_mean","st_mean")]
# ww = function(x){
#   res = sqrt(length(x))/max(0.001,sd(x,na.rm = TRUE))
#   if (is.na(res))
#     1
#   else
#     res
# }
################

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
  rsq.vitals = c(rsq.vitals, models.vitals.s[[i]]$adj.r.squared) # not crossvalidated but very elementary models here!
names(rsq.vitals) = top.names

## WEARABLES -- build models for wearables and crossvalidate
rsq.all = rsq.vitals
wear.names = list(tiny = c("st_mean","rhr_mean"),
                  smpl = c("st_mean","st_sd","rhr_mean","hr_sd"),
                  smpl.hr = c("sk_mean","gsr_mean", "st_mean","st_sd","rhr_mean","hr_sd"),
                  smpl.hr.gsr = c("hr_mean","sk_mean","gsr_mean","st_mean","rhr_mean","hr_sd","sk_sd","gsr_sd","st_sd","rhr_sd"))

for (j in 1:length(wear.names))
{
  # LOO
  patients = unique(labs.wear.uid)
  col.wear = c(1:2, which(colnames(wear) %in% wear.names[[j]])) 
  col.vitals = c(1:2, 4:5)
  
  val.true = c()
  val.pred = c()
  # Linear models for wearables
  for (k in 1:length(patients)){
    loo.mask = patients[k] == labs.wear.uid
    # models.wear = lm(as.matrix(labs.wear[!loo.mask,top.names]) ~ as.data.frame(labs.wear[!loo.mask,wear.names[[j]]]),
    #                  na.action = na.omit,
    #                  weights = labs.wear$weight)
    fml = paste("cbind(",paste(top.names,collapse=" , "),") ~",paste(wear.names[[j]],collapse=" + "))
    models.wear = lm(formula = fml,
                     data = labs.wear[!loo.mask,],
                     na.action = na.omit,
                     weights = labs.wear$weight)
    val.true = rbind(val.true, as.matrix(labs.wear[loo.mask,top.names]))
    val.pred = rbind(val.pred, predict(models.wear, newdata = labs.wear[loo.mask,]))
  }
  rsq.wear = diag(cor(val.true,val.pred,use = "pairwise.complete.obs"))
  names(rsq.wear) = top.names
  rsq.all = rbind(rsq.all, rsq.wear)
  rownames(rsq.all)[nrow(rsq.all)] = names(wear.names)[j]
}
rownames(rsq.all)[1] = "vitals"
df = data.frame(rsq.all)
df[df<0] = 0
df$name = rownames(rsq.all)

#qplot(variable, value, colour = name, data = melt(df, id = "name"))
data = melt(df, id = "name")
colnames(data) = c("model","test","r_squared")
ggplot(data, aes(test,r_squared, color = model)) + geom_point(size = 5, aes(shape=model, color=model)) +
  weartals_theme +
  labs(x = "Lab tests",y = expression(paste("correlation"))) + ggtitle("Model comparison")
