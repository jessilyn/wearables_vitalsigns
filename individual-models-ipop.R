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

source("ggplot-theme.R") # just to make things look nice

####################################
## Figure 4A: Mixed effect-models ##
####################################
print("Building 4A plots")
# Mixed-effect model

# TODO: Are these the right tables?
m1.lme4 = lmer(GLU ~ 1 + rhr_mean + st_mean + (rhr_mean|iPOP_ID),
               data = wear)
MuMIn::r.squaredGLMM(m1.lme4) # random effect explaines a lot

## True vs predicted plot
test.name = "GLU"
png(paste("plots/",test.name,"-individual.png",sep=""))
plot(model$fitted.values, wear[[test.name]][!is.na(wear[[test.name]])],
     xlab=paste("Predicted",test.name),ylab=paste("True",test.name))
dev.off()
cor(model$fitted.values, wear[[test.name]][!is.na(wear[[test.name]])])



################################
## Figure 4B: Individual fits ##
################################
print("Building 4B plots")
# Only ppl with some matching wearables
labs.wear = wear[!is.na(wear$lowAct_gsr_kurtosis),] # with full obs
top.names = names(sort(-table(labs.wear$iPOP_ID)))[1:3] # 3ppl with largest numbers of obs
labs.wear.top = labs.wear[labs.wear$iPOP_ID %in% top.names,]

# GLU vs vitals
png('plots/glu-pulse.png',width = 1200, height = 800,res=120)
print(ggplot(labs.wear.top, aes(Pulse, GLU, group = iPOP_ID, colour = iPOP_ID)) + 
  weartals_theme + theme(text = element_text(size=20)) +
  geom_point(size=3) + geom_smooth(method="lm", fill=NA))
dev.off()

png('plots/glu-temp.png',width = 1200, height = 800,res=120)
print(ggplot(labs.wear.top, aes(Temp, GLU, group = iPOP_ID, colour = iPOP_ID)) + 
  weartals_theme + theme(text = element_text(size=20)) +
  geom_jitter(size=3) + geom_smooth(method="lm", fill=NA))
dev.off()

# PLT vs vitals
png('plots/plt-pulse.png',width = 1200, height = 800,res=120)
print(ggplot(labs.wear.top, aes(Pulse, PLT, group = iPOP_ID, colour = iPOP_ID)) + 
  weartals_theme + theme(text = element_text(size=20)) +
  geom_point(size=3) + geom_smooth(method="lm", fill=NA))
dev.off()

png('plots/plt-temp.png',width = 1200, height = 800,res=120)
print(ggplot(labs.wear.top, aes(Temp, PLT, group = iPOP_ID, colour = iPOP_ID)) + 
  weartals_theme + theme(text = element_text(size=20)) +
  geom_jitter(size=3) + geom_smooth(method="lm", fill=NA))
dev.off()

# GLU vs wear
png('plots/glu-hr.png',width = 1200, height = 800,res=120)
print(ggplot(labs.wear.top, aes(hr_mean, GLU, group = iPOP_ID, colour = iPOP_ID)) + 
  weartals_theme + theme(text = element_text(size=20)) + xlab("Heart rate (wearable)") + 
  geom_point(size=3) + geom_smooth(method="lm", fill=NA))
dev.off()

png('plots/glu-st.png',width = 1200, height = 800,res=120)
print(ggplot(labs.wear.top, aes(st_mean, GLU, group = iPOP_ID, colour = iPOP_ID)) + 
  weartals_theme + theme(text = element_text(size=20)) + xlab("Skin temp (wearable)") +
  geom_jitter(size=3) + geom_smooth(method="lm", fill=NA))
dev.off()

# PLT vs vitals
png('plots/plt-hr.png',width = 1200, height = 800,res=120)
print(ggplot(labs.wear.top, aes(hr_mean, PLT, group = iPOP_ID, colour = iPOP_ID)) + 
  weartals_theme + theme(text = element_text(size=20)) + xlab("Heart rate (wearable)") +
  geom_point(size=3) + geom_smooth(method="lm", fill=NA))
dev.off()

png('plots/plt-st.png',width = 1200, height = 800,res=120)
print(ggplot(labs.wear.top, aes(st_mean, PLT, group = iPOP_ID, colour = iPOP_ID)) + 
  weartals_theme + theme(text = element_text(size=20)) + xlab("Skin temp (wearable)") +
  geom_jitter(size=3) + geom_smooth(method="lm", fill=NA))
dev.off()

# Consistency of wearables
png('plots/hr-pulse.png',width = 1200, height = 800,res=120)
print(ggplot(labs.wear.top, aes(Pulse, rhr_mean, group = iPOP_ID, colour = iPOP_ID)) + 
  weartals_theme + theme(text = element_text(size=20)) + ylab("HR (wearable)") + xlab("Pulse") +
  geom_jitter(size=3) + geom_smooth(method="lm", fill=NA))
dev.off()

png('plots/st-temp.png',width = 1200, height = 800,res=120)
print(ggplot(labs.wear.top, aes(Temp, st_mean, group = iPOP_ID, colour = iPOP_ID)) + 
  weartals_theme + theme(text = element_text(size=20)) + ylab("Skin temp (wearable)") + xlab("Temp") +
  geom_jitter(size=3) + geom_smooth(method="lm", fill=NA))
dev.off()

print("Plots 4A-4B done")
