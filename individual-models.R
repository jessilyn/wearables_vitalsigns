source("population-models.R")

#######################
## Individual models ##
#######################
library("lme4")
m1.lme4 = lmer(GLU ~ 1 + rhr_mean + st_mean + (rhr_mean|iPOP_ID),
               data = labs.wear.full)
summary(m1.lme4)
plot(m1.lme4)
MuMIn::r.squaredGLMM(m1.lme4)

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
