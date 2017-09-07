library(MASS)
source("20170830_CorMatrix_Vitals_Labs_Wear.R")
v = t(read.csv("top.csv",row.names = 1))
top = as.vector(v)
names(top) = colnames(v)
top = sort(top,decreasing = TRUE)[1:10]
top.names = names(top)
top.names[1] = "GLU"

# Select variables
labs.clin = labs[,colnames(labs) %in% c(colnames(labs)[1:2], top.names)]

# Merge by clinical test
labs.wear = merge(data.frame(labs.clin), data.frame(wear), by.x = c("iPOP_ID","Clin_Result_Date"), by.y = c("iPOP_ID","Clin_Result_Date") )[,-(1:2)]
labs.vitals = merge(data.frame(labs.clin), data.frame(vitals), by.x = c("iPOP_ID","Clin_Result_Date"), by.y = c("iPOP_ID","Clin_Result_Date") )[,-(1:2)]

# Corelations
cors = cor(labs.wear,use="pairwise.complete.obs")
cors[order(abs(cors[,"hr_mean"]),decreasing = TRUE),c("hr_mean","st_mean")]

# Linear models for vitals
models.vitals = lm(as.matrix(labs.vitals[,top.names]) ~ as.matrix(labs.vitals[,c("Pulse","Temp")]))
models.vitals.s = summary(models.vitals)

# Choose best
rsq.vitals = c()
for (i in 1:length(top.names))
  rsq.vitals = c(rsq.vitals, models.vitals.s[[i]]$adj.r.squared)
names(rsq.vitals) = top.names
#rsq.vitals = rsq.vitals[order(rsq.vitals, decreasing = TRUE)]

## WEARABLES
rsq.all = rsq.vitals[1:10]
wear.names = list(c("st_mean","st_sd","rhr_mean","hr_sd"),
#  c("sk_mean","gsr_mean", "st_mean","st_sd","rhr_mean","hr_sd"),
#  c("sk_mean","gsr_mean","st_mean","rhr_mean","hr_sd"),
  c("hr_mean","sk_mean","gsr_mean","st_mean","rhr_mean","hr_sd","sk_sd","gsr_sd","st_sd","rhr_sd"))

for (j in 1:length(wear.names))
{
  col.wear = c(1:2, which(colnames(wear) %in% wear.names[[j]])) 
  col.vitals = c(1:2, 4:5)
  
  # Linear models for wearables
  models.wear = lm(as.matrix(labs.wear[,top.names]) ~ as.matrix(labs.wear[,wear.names[[j]]]))
  
  models.wear.s = summary(models.wear)
  
  rsq.wear = c()
  for (i in 1:length(top.names))
    rsq.wear = c(rsq.wear, models.wear.s[[i]]$adj.r.squared)
  names(rsq.wear) = top.names
  rsq.all = rbind(rsq.all, rsq.wear)
}
rownames(rsq.all) = c("Vitals","Simple","Full")
df = data.frame(rsq.all)
df$name = rownames(rsq.all)
library("reshape2")
library("ggthemes")
#qplot(variable, value, colour = name, data = melt(df, id = "name"))
data = melt(df, id = "name")
colnames(data) = c("model","test","r_squared")
ggplot(data, aes(test,r_squared, color = model))  +  geom_point(size = 5, aes(shape=model, color=model)) +
  theme_bw() + theme(text = element_text(size=18), panel.border = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Lab tests",y = expression(paste("adjusted ",R^{2}))) + ggtitle("Model comparison")
  #+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) # verbose


#+ theme_minimal()
library("ggplot2")
