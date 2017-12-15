library(MASS)
library("reshape2")
library("ggthemes")
library("ggplot2")
library(dplyr)
library(reshape2)
library(randomForest)
library("lme4")
library(lme4)
library("MuMIn")
library("sjPlot")

# Path to the directory with data
dir = "./SECURE_data/"
source("load-data.R") # loads the data
source("ggplot-theme.R") # just to make things look nice

#######################
## Individual models ##
#######################
# Mixed-effect model
m1.lme4 = lmer(GLU ~ 1 + rhr_mean + st_mean + (rhr_mean|iPOP_ID),
               data = labs.wear.full)
plot(m1.lme4)
MuMIn::r.squaredGLMM(m1.lme4) # random effect explaines a lot

## Check how individual means perform
test.name = "NEUT"
model = lm(paste(test.name,"~ iPOP_ID"), data=labs.wear.full,na.action = na.omit)
png(paste(test.name,"individual.png",sep="-"))
plot(model$fitted.values, labs.wear.full[[test.name]][!is.na(labs.wear.full[[test.name]])],
     xlab=paste("Predicted",test.name),ylab=paste("True",test.name))
dev.off()
cor(model$fitted.values, labs.wear.full[[test.name]][!is.na(labs.wear.full[[test.name]])])

# Only ppl with some matching wearables
labs.wear = labs.wear.full[!is.na(labs.wear.full$lowAct_gsr_kurtosis),]


#######################
# Some individual plots
#######################
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
