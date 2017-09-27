library(MASS)
source("old/20170830_CorMatrix_Vitals_Labs_Wear.R")
library("reshape2")
library("ggthemes")
source("ggplot-theme.R")

v = t(read.csv("top.csv",row.names = 1))
top = as.vector(v)
names(top) = colnames(v)
top = sort(top,decreasing = TRUE)[1:10]
top.names = names(top)
top.names[1] = "GLU"

# Select variables
labs.clin = labs[,colnames(labs) %in% c(colnames(labs)[1:2], top.names)]

# Merge by clinical test
labs.wear.full = merge(data.frame(labs.clin), data.frame(wear), by.x = c("iPOP_ID","Clin_Result_Date"), by.y = c("iPOP_ID","Clin_Result_Date") )
labs.vitals = merge(data.frame(labs.clin), data.frame(vitals), by.x = c("iPOP_ID","Clin_Result_Date"), by.y = c("iPOP_ID","Clin_Result_Date") )
wear.vitals = merge(data.frame(wear), data.frame(vitals), by.x = c("iPOP_ID","Clin_Result_Date"), by.y = c("iPOP_ID","Clin_Result_Date") )
labs.wear = labs.wear.full

# Corelations
cors = cor(labs.wear,use="pairwise.complete.obs")
cors[order(abs(cors[,"hr_mean"]),decreasing = TRUE),c("hr_mean","st_mean")]

library(dplyr)
library(reshape2)
ww = function(x){
  res = sqrt(length(x))/max(0.001,sd(x,na.rm = TRUE))
  if (is.na(res))
    1
  else
    res
}
labs.wear <- labs.wear.full %>% group_by(iPOP_ID) %>% summarise(count=n(),
                                                                sdd=sd(CR,na.rm = TRUE),
                                                                weight=sqrt(n()),
                                                                GLU=mean(GLU,na.rm=TRUE),
                                                                CR=mean(CR,na.rm=TRUE),
                                                                HSCRP=mean(HSCRP,na.rm=TRUE),
                                                                NEUTAB=mean(NEUTAB,na.rm=TRUE),
                                                                NEUT=mean(NEUT,na.rm=TRUE),
                                                                LYM=mean(LYM,na.rm=TRUE),
                                                                RDW=mean(RDW,na.rm=TRUE),
                                                                ALB=mean(ALB,na.rm=TRUE),
                                                                AG=mean(AG,na.rm=TRUE),
                                                                PLT=mean(PLT,na.rm=TRUE),
                                                                hr_mean=mean(hr_mean,na.rm=TRUE),
                                                                sk_mean=mean(sk_mean,na.rm=TRUE),
                                                                gsr_mean=mean(gsr_mean,na.rm=TRUE),
                                                                st_mean=mean(st_mean,na.rm=TRUE),
                                                                rhr_mean=mean(rhr_mean,na.rm=TRUE),
                                                                hr_sd=mean(hr_sd,na.rm=TRUE),
                                                                sk_sd=mean(sk_sd,na.rm=TRUE),
                                                                gsr_sd=mean(gsr_sd,na.rm=TRUE),
                                                                st_sd=mean(st_sd,na.rm=TRUE),
                                                                rhr_sd=mean(rhr_sd,na.rm=TRUE))
#labs.wear <- labs.wear.full %>% group_by(iPOP_ID) %>% sample_n(10000,replace = TRUE)
#labs.wear$weight = sqrt(labs.wear$count) / sd(labs.wear$sdd)

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
simple.model = c("st_mean","st_sd","rhr_mean","hr_sd")
wear.names = list(simple.model,
#  c("sk_mean","gsr_mean", "st_mean","st_sd","rhr_mean","hr_sd"),
#  c("sk_mean","gsr_mean","st_mean","rhr_mean","hr_sd"),
  c("hr_mean","sk_mean","gsr_mean","st_mean","rhr_mean","hr_sd","sk_sd","gsr_sd","st_sd","rhr_sd"))


for (j in 1:length(wear.names))
{
  col.wear = c(1:2, which(colnames(wear) %in% wear.names[[j]])) 
  col.vitals = c(1:2, 4:5)
  
  # Linear models for wearables
  models.wear = lm(as.matrix(labs.wear[,top.names]) ~ as.matrix(labs.wear[,wear.names[[j]]]),na.action = na.omit,weights = labs.wear$weight)
  
  models.wear.s = summary(models.wear)
  
  rsq.wear = c()
  for (i in 1:length(top.names))
    rsq.wear = c(rsq.wear, models.wear.s[[i]]$adj.r.squared)
  names(rsq.wear) = top.names
  rsq.all = rbind(rsq.all, rsq.wear)
}
rownames(rsq.all) = c("Vitals","Simple","Full")
df = data.frame(rsq.all)
df[df<0] = 0
df$name = rownames(rsq.all)

library("lme4")
m1.lme4 = lmer(GLU ~ 1 + rhr_mean + st_mean + (rhr_mean|iPOP_ID),
               data = labs.wear.full)
summary(m1.lme4)
plot(m1.lme4)
MuMIn::r.squaredGLMM(m1.lme4)

#qplot(variable, value, colour = name, data = melt(df, id = "name"))
data = melt(df, id = "name")
colnames(data) = c("model","test","r_squared")
ggplot(data, aes(test,r_squared, color = model)) + geom_point(size = 5, aes(shape=model, color=model)) +
  weartals_theme +
  labs(x = "Lab tests",y = expression(paste("adjusted ",R^{2}))) + ggtitle("Model comparison")
  #+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) # verbose

## Individual models
head(df)

## LABS PER PATIENT
table(labs.wear[,1])

# People with many observations
participants.visit = sort(table(labs.wear[,1]),decreasing = TRUE)
participants = names(participants.visit[participants.visit>50])

model.p = list()
for (i in 1:length(participants)){
  labs.wear.p = labs.wear[labs.wear$iPOP_ID %in% participants[i],]
  model.p[[i]] = lm(as.matrix(labs.wear.p[,top.names]) ~ as.matrix(labs.wear.p[,simple.model]),na.action = na.omit)
}
library("abind")
res = c()
for (i in 1:length(participants)){
  res = abind(res, model.p[[i]]$coefficients,along = 3)
}
apply(res, c(1,2), function(x){mean(x,na.rm = TRUE)})
apply(res, c(1,2), function(x){sd(x,na.rm = TRUE)})

for (i in 1:length(model.p)){
  print(summary(model.p[[i]])[[2]]$adj.r.squared)
}



# People with many observations
participants.wear = sort(table(labs.wear[!is.na(labs.wear$hr_mean),1]),decreasing = TRUE)
participants.w = names(participants.wear[participants.wear>20])

labs.vitals.top = labs.vitals[labs.vitals$iPOP_ID %in% participants.w,]
labs.wear.top = labs.wear[labs.wear$iPOP_ID %in% participants.w,]


library(lme4)
library("MuMIn")
library("sjPlot")
m1.lme4 = lmer(GLU ~ Pulse + Temp + (1|iPOP_ID),
               data = labs.vitals.top)
sjp.glmer(m1.lme4, type = "fe.cor")
summary(m1.lme4)
MuMIn::r.squaredGLMM(m1.lme4)

source("ggplot-theme.R")

qplot(participants.visit, binwidth=5, geom="histogram", fill=I("grey"), col=I("black")) +
  weartals_theme + theme(text = element_text(size=20)) + xlab("lab visits") + ylab("wearables users")+
  scale_x_continuous(minor_breaks = seq(0, 150, 5))

png('plots/hist.png',width = 1200, height = 800,res=120)
h1 = hist(participants.visit,plot = FALSE)
h2 = hist(participants.wear,plot = FALSE,breaks = h1$breaks)
df = data.frame(breaks = h1$breaks[2:length(h1$breaks)], lab = h1$counts, wearables = h2$counts)
ggplot(data.frame(visits = participants.visit), aes(visits)) + 
  geom_histogram(aes(fill = "r", color="r"), data = data.frame(visits = participants.visit), alpha = 0.4, size=0.2) + 
  geom_histogram(aes(fill = "b", color="b"), data = data.frame(visits = participants.wear), alpha = 0.4, size=0.2) +
  weartals_theme + theme(text = element_text(size=20)) + xlab("visits") + ylab("users")+
  scale_x_continuous(minor_breaks = seq(0, 150, 5)) + 
  scale_fill_manual(name = 'type', 
                    values = c('r'='red','b'='blue'), labels = c('r'='lab','b'='wearable')) + 
  scale_color_manual(name = 'type', 
                  values = c('r'='red','b'='blue'), labels = c('r'='lab','b'='wearable'))
dev.off()

# GLU vs vitals
png('plots/glu-pulse.png',width = 1200, height = 800,res=120)
ggplot(labs.vitals.top, aes(Pulse, GLU, group = iPOP_ID, colour = iPOP_ID)) + 
  weartals_theme + theme(text = element_text(size=20)) +
  geom_point(size=3) + geom_smooth(method="lm", fill=NA)
dev.off()

png('plots/glu-temp.png',width = 1200, height = 800,res=120)
ggplot(labs.vitals.top, aes(Temp, GLU, group = iPOP_ID, colour = iPOP_ID)) + 
  weartals_theme + theme(text = element_text(size=20)) +
  geom_jitter(size=3) + geom_smooth(method="lm", fill=NA)
dev.off()

# PLT vs vitals
png('plots/plt-pulse.png',width = 1200, height = 800,res=120)
ggplot(labs.vitals.top, aes(Pulse, PLT, group = iPOP_ID, colour = iPOP_ID)) + 
  weartals_theme + theme(text = element_text(size=20)) +
  geom_point(size=3) + geom_smooth(method="lm", fill=NA)
dev.off()

png('plots/plt-temp.png',width = 1200, height = 800,res=120)
ggplot(labs.vitals.top, aes(Temp, PLT, group = iPOP_ID, colour = iPOP_ID)) + 
  weartals_theme + theme(text = element_text(size=20)) +
  geom_jitter(size=3) + geom_smooth(method="lm", fill=NA)
dev.off()

# GLU vs wear
png('plots/glu-hr.png',width = 1200, height = 800,res=120)
ggplot(labs.wear.top, aes(hr_mean, GLU, group = iPOP_ID, colour = iPOP_ID)) + 
  weartals_theme + theme(text = element_text(size=20)) + xlab("Heart rate (wearable)") + 
  geom_point(size=3) + geom_smooth(method="lm", fill=NA)
dev.off()

png('plots/glu-st.png',width = 1200, height = 800,res=120)
ggplot(labs.wear.top, aes(st_mean, GLU, group = iPOP_ID, colour = iPOP_ID)) + 
  weartals_theme + theme(text = element_text(size=20)) + xlab("Skin temp (wearable)") +
  geom_jitter(size=3) + geom_smooth(method="lm", fill=NA)
dev.off()

# PLT vs vitals
png('plots/plt-hr.png',width = 1200, height = 800,res=120)
ggplot(labs.wear.top, aes(hr_mean, PLT, group = iPOP_ID, colour = iPOP_ID)) + 
  weartals_theme + theme(text = element_text(size=20)) + xlab("Heart rate (wearable)") +
  geom_point(size=3) + geom_smooth(method="lm", fill=NA)
dev.off()

png('plots/plt-st.png',width = 1200, height = 800,res=120)
ggplot(labs.wear.top, aes(st_mean, PLT, group = iPOP_ID, colour = iPOP_ID)) + 
  weartals_theme + theme(text = element_text(size=20)) + xlab("Skin temp (wearable)") +
  geom_jitter(size=3) + geom_smooth(method="lm", fill=NA)
dev.off()

# Consistency of wearables
png('plots/hr-pulse.png',width = 1200, height = 800,res=120)
ggplot(wear.vitals[wear.vitals$iPOP_ID %in% participants.w,], aes(Pulse, rhr_mean, group = iPOP_ID, colour = iPOP_ID)) + 
  weartals_theme + theme(text = element_text(size=20)) + ylab("HR (wearable)") + xlab("Pulse") +
  geom_jitter(size=3) + geom_smooth(method="lm", fill=NA)
dev.off()

png('plots/st-temp.png',width = 1200, height = 800,res=120)
ggplot(wear.vitals[wear.vitals$iPOP_ID %in% participants.w,], aes(Temp, st_mean, group = iPOP_ID, colour = iPOP_ID)) + 
  weartals_theme + theme(text = element_text(size=20)) + ylab("Skin temp (wearable)") + xlab("Temp") +
  geom_jitter(size=3) + geom_smooth(method="lm", fill=NA)
dev.off()
