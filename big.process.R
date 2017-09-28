#install.packages(c("data.table","psych","softImpute","ggfortify","ggplot2","ggthemes","sjPlot","lme4","MuMIn"), INSTALL_opts = c('--no-lock'))

library(data.table)
library(psych)
library(softImpute)
library(ggfortify)
library(ggplot2)

library("ggthemes")
source("ggplot-theme.R")

labs = fread("SECURE_data/big.labs.csv")
vitals = fread("SECURE_data/big.vitals.csv")

v = t(read.csv("top.csv",row.names = 1))
top = as.vector(v)
names(top) = colnames(v)
top = sort(top,decreasing = TRUE)[1:10]

data = labs[,3:53][,-4]
nms = colnames(labs[,3:53][,-4])
means = colMeans(data,na.rm = TRUE)
data = as.matrix(data)
data = t(t(data) - means)

sds = sqrt(colMeans(data**2,na.rm=TRUE))
data = t(t(data) / sds)

head(labs)

labs.vitals = merge(data.frame(labs), data.frame(vitals), by.x = c("ANON_ID","Clin_Result_Date"), by.y = c("ANON_ID","Clin_Result_Date") )
labs.vitals = labs.vitals[!is.na(labs.vitals$Pulse) & !is.na(labs.vitals$Pulse),]
patients = sort(table(labs.vitals$ANON_ID))

png('plots/big-hist-all.png',width = 1200, height = 800,res=120)
qplot(data.frame(count = patients), binwidth=5, geom="histogram", fill=I("grey"), col=I("black")) +
  weartals_theme + theme(text = element_text(size=20)) + xlab("visits") + ylab("users") +
  scale_x_continuous(minor_breaks = seq(0, 150, 5))
dev.off()

png('plots/big-hist-50pluss.png',width = 1200, height = 800,res=120)
qplot(patients[patients > 50], binwidth=5, geom="histogram", fill=I("grey"), col=I("black")) +
  weartals_theme + theme(text = element_text(size=20)) + xlab("visits") + ylab("users") +
  scale_x_continuous(minor_breaks = seq(0, 150, 5))
dev.off()


labs.vitals = labs.vitals[labs.vitals$Pulse > 50,]
labs.vitals = labs.vitals[labs.vitals$Temp > 96.5,]
patients = sort(table(labs.vitals[!is.na(labs.vitals$GLU_byMeter),]$ANON_ID))
top.patients.GLU = names(patients[patients > 20])

# GLU vs vitals
png('plots/big-glu-pulse.png',width = 1200, height = 800,res=120)
ggplot(labs.vitals[labs.vitals$ANON_ID %in% top.patients.GLU,], aes(Pulse, GLU_byMeter, group = ANON_ID, colour = ANON_ID)) + 
  weartals_theme + theme(text = element_text(size=20)) +
  geom_point(size=2) + geom_smooth(method="lm", fill=NA)
dev.off()

png('plots/big-glu-temp.png',width = 1200, height = 800,res=120)
ggplot(labs.vitals[labs.vitals$ANON_ID %in% top.patients.GLU,], aes(Temp, GLU_byMeter, group = ANON_ID, colour = ANON_ID)) + 
  weartals_theme + theme(text = element_text(size=20)) +
  geom_jitter(size=3) + geom_smooth(method="lm", fill=NA)
dev.off()

# PLT vs vitals
png('plots/big-plt-pulse.png',width = 1200, height = 800,res=120)
ggplot(labs.vitals[labs.vitals$ANON_ID %in% top.patients.GLU,], aes(Pulse, PLT, group = ANON_ID, colour = ANON_ID)) + 
  weartals_theme + theme(text = element_text(size=20)) +
  geom_point(size=3) + geom_smooth(method="lm", fill=NA)
dev.off()

png('plots/big-plt-temp.png',width = 1200, height = 800,res=120)
ggplot(labs.vitals[labs.vitals$ANON_ID %in% top.patients.GLU,], aes(Temp, PLT, group = ANON_ID, colour = ANON_ID)) + 
  weartals_theme + theme(text = element_text(size=20)) +
  geom_jitter(size=3) + geom_smooth(method="lm", fill=NA)
dev.off()

################
# Slope analysis
################
top.patients.GLU = names(patients[patients > 40])
data= labs.vitals[labs.vitals$ANON_ID %in% top.patients.GLU,]
sm = summary(lm(data$PLT ~ data$Pulse))

extract.slopes = function(pid, x, y)
{
  patients = unique(pid)
  pvals = c()
  slopes = c()
  rsqr = c()
  dfr = c()
  corr = c()
  for (patient in patients)
  {
    sm = summary(lm(y[pid %in% patient] ~ x[pid %in% patient],na.action = na.omit))
    f = sm$fstatistic
    p = pf(f[1],f[2],f[3],lower.tail=F)
    pvals = c(pvals, p)
    slopes = c(slopes, sm$coefficients[2])
    rsqr = c(rsqr, sm$r.squared)
    dfr = c(dfr, sm$fstatistic[3])
    ct = cor(y[pid %in% patient],x[pid %in% patient],use="complete.obs")
    corr = c(corr, ct)
  }
  data.frame(pvals = pvals, slopes = slopes, rsqr = rsqr, dfr = dfr, corr=corr)
}
slopes = extract.slopes(data$ANON_ID, data$Temp, data$PLT)
mean(slopes < 0.05)
boxplot(slopes)



#################
library("lme4")
library("sjPlot")

df = labs.vitals[labs.vitals$ANON_ID %in% top.patients.GLU,]

png('plots/glu-scatter.png',width = 1200, height = 800,res=120)
toplot = labs.vitals[,which(colnames(labs.vitals) %in% c("GLU_byMeter","Temp","Pulse") )]
toplot = na.omit(toplot)
plot(toplot)
dev.off()

png('plots/other-scatter.png',width = 1200, height = 800,res=120)
toplot = labs.vitals[,which(colnames(labs.vitals) %in% c(names(top),"Temp","Pulse") )[c(5:12)]]
toplot = na.omit(toplot)
plot(toplot)
dev.off()

png('plots/other-scatter.png',width = 1200, height = 800,res=120)
toplot = labs.vitals[,which(colnames(labs.vitals) %in% c(c("HSCRP","PLT","RDW"),"Temp","Pulse") )]
toplot = na.omit(toplot)
plot(toplot)
dev.off()

png('plots/other-scatter.png',width = 1200, height = 800,res=120)
toplot = labs.vitals[,which(colnames(labs.vitals) %in% c(c("NEUT","NEUTAB","LYM"),"Temp","Pulse")) ]
toplot = na.omit(toplot)
toplot = toplot[sample(nrow(toplot))[1:1000],]
plot(toplot)
dev.off()

m1.lm = lm(GLU_byMeter ~ 1 + Pulse,
               data = labs.vitals[labs.vitals$ANON_ID %in% top.patients.GLU,])

library("lme4")
m1.lme4 = lmer(GLU_fasting ~ Pulse + (Pulse|ANON_ID),
               data = labs.vitals[labs.vitals$ANON_ID %in% top.patients.GLU,])

mm.lym = lmer(LYM ~ Pulse + (Pulse|ANON_ID),
               data = labs.vitals)
mm.neut = lmer(NEUT ~ Pulse + (Pulse|ANON_ID),
               data = labs.vitals)

ggplot(labs.vitals, aes(x = NEUT, y = Pulse)) + weartals_theme + 
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1.5)
ggsave("plots/quadratic-neut.png")
ggplot(labs.vitals, aes(x = LYM, y = Pulse)) + weartals_theme + 
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1.5)
ggsave("plots/quadratic-lym.png")
ggplot(labs.vitals, aes(x = GLU_fasting, y = Pulse)) + weartals_theme + 
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1.5)
ggsave("plots/quadratic-glu.png")

coef(mm.lym)
coef(mm.neut)

cf = coef(mm.lym)


plot(m1.lme4)
MuMIn::r.squaredGLMM(m1.lme4)


sjp.setTheme(base = weartals_theme + theme(text = element_text(size=20)))
sjp.lmer(m1.lme4,type = "fe")
sjp.lmer(m1.lme4,type = "re")

#######

#slopes



# SOFT IMPUTE
# pca.labs = softImpute(data)
# PC = pca.labs$v
# colnames(PC) = c("PC1","PC2")
# PC = data.frame(PC)
# PC$test = nms
# PC$dist = PC$PC1**2 + PC$PC2**2
# PC$col = ifelse(nms %in% names(top),"analyzed","other")
# 
# ggplot(PC,aes(x=PC1,y=PC2))+
#   geom_point(aes(colour=col),size=3,alpha=0.5) +
#   geom_text(aes(label=ifelse(dist>0.008,as.character(test),'')),hjust=0.5, vjust=-0.5, size = 5) + 
#   weartals_theme + theme(text = element_text(size=20)) + 
#   ggtitle("Lab test components")
# 
# plot(pca.labs$v)
# df <- iris[c(1, 2, 3, 4)]
# autoplot()
