#install.packages(c("data.table","psych","softImpute","ggfortify","ggplot2","ggthemes","sjPlot","lme4","MuMIn"), INSTALL_opts = c('--no-lock'))

library(data.table)
library(psych)
library(softImpute)
library(ggfortify)
library(ggplot2)

library("ggthemes")
source("ggplot-theme.R")

# labs = fread("SECURE_data/big.labs.csv")
vitals = fread("SECURE_data/big.vitals.csv")

labs = fread("SECURE_data/20170905_Cleaned_joined_30k_labs_vitals.csv")

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

labs.vitals = merge(data.frame(labs), data.frame(vitals[,-c("Pulse","Temp")]), by.x = c("ANON_ID","Clin_Result_Date"), by.y = c("ANON_ID","Clin_Result_Date") )
labs.vitals = labs.vitals[!is.na(labs.vitals$Pulse) & !is.na(labs.vitals$Pulse),]
patients = sort(table(labs.vitals$ANON_ID))

png('plots/big-hist-all.png',width = 1200, height = 800,res=120)
qplot(data.frame(count = as.numeric(patients) ), binwidth=5, geom="histogram", fill=I("grey"), col=I("black")) +
  weartals_theme + theme(text = element_text(size=20)) + xlab("visits") + ylab("users") +
  scale_x_continuous(minor_breaks = seq(0, 150, 5))
dev.off()

png('plots/big-hist-50pluss.png',width = 1200, height = 800,res=120)
qplot(as.numeric(patients[patients > 50]), binwidth=5, geom="histogram", fill=I("grey"), col=I("black")) +
  weartals_theme + theme(text = element_text(size=20)) + xlab("visits") + ylab("users") +
  scale_x_continuous(minor_breaks = seq(0, 150, 5))
dev.off()


#labs.vitals = labs.vitals[labs.vitals$Pulse > 50,]
#labs.vitals = labs.vitals[labs.vitals$Temp > 96.5,]
patients = sort(table(labs.vitals[!is.na(labs.vitals$GLU_byMeter),]$ANON_ID))
top.patients.GLU = names(patients[patients > 20])

# GLU vs vitals
png('plots/big-glu-pulse.png',width = 1200, height = 800,res=120)
ggplot(labs.vitals[labs.vitals$ANON_ID %in% top.patients.GLU,], aes(Pulse, GLU_byMeter, group = ANON_ID, colour = ANON_ID)) + 
  weartals_theme + theme(text = element_text(size=20))+ theme(legend.position="none") +
  geom_point(size=2) + geom_smooth(method="lm", fill=NA)
dev.off()

png('plots/big-glu-temp.png',width = 1200, height = 800,res=120)
ggplot(labs.vitals[labs.vitals$ANON_ID %in% top.patients.GLU,], aes(Temp, GLU_byMeter, group = ANON_ID, colour = ANON_ID)) + 
  weartals_theme + theme(text = element_text(size=20))+ theme(legend.position="none") +
  geom_jitter(size=3) + geom_smooth(method="lm", fill=NA)
dev.off()

# PLT vs vitals
png('plots/big-plt-pulse.png',width = 1200, height = 800,res=120)
ggplot(labs.vitals[labs.vitals$ANON_ID %in% top.patients.GLU,], aes(Pulse, PLT, group = ANON_ID, colour = ANON_ID)) + 
  weartals_theme + theme(text = element_text(size=20))+ theme(legend.position="none") +
  geom_point(size=3) + geom_smooth(method="lm", fill=NA)
dev.off()

png('plots/big-plt-temp.png',width = 1200, height = 800,res=120)
ggplot(labs.vitals[labs.vitals$ANON_ID %in% top.patients.GLU,], aes(Temp, PLT, group = ANON_ID, colour = ANON_ID)) + 
  weartals_theme + theme(text = element_text(size=20))+ theme(legend.position="none") +
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
slopes = extract.slopes(data$ANON_ID, data$Temp, data$GLOB)
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

source("top-names.R")
top8 = top
plots = list()

plotlist = NULL
cols = 5
layout = NULL
numPlots = 10
layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                 ncol = cols, nrow = ceiling(numPlots/cols))
library("grid")
grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

nms = top8
nms[4] = "PROC."

vits = c("Pulse", "Temp")
weartals_theme = theme_bw() + theme(text = element_text(size=18), panel.border = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

library("grid")

## Univariate Mixed-effect
# !! Only patients with at least min_visits = 20
min_visits = 20
for (i in 1:length(1)){
  for (j in 1:1){
    vit = vits[j+1]
    clin = top8[i]
    #2*(i - 1) + j + 1
    matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
    
    patients = sort(table(labs.vitals[!is.na(labs.vitals[[clin]]),]$ANON_ID))
    labs.vitals.tmp = labs.vitals[labs.vitals$ANON_ID %in% names(patients[patients > min_visits]),]
    
    frm = paste0(clin," ~ ",vit," + (",vit,"|ANON_ID)")
    print(frm)
    mm = lmer(frm, data = labs.vitals.tmp)
    cf = coef(mm)
    qq = qplot(cf$ANON_ID[vit], geom="histogram")  + weartals_theme + xlab(paste0(nms[i]," ~ ",vit)) + ylab("count")
    print(qq, vp = viewport(layout.pos.row = matchidx$row,
                            layout.pos.col = matchidx$col))
  }
}

## Univariate Mixed-effect
# !! Only patients with at least min_visits = 20
min_visits = 50
for (i in 1:length(1)){
    clin = top8[i]
    #2*(i - 1) + j + 1
    matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
    
    patients = sort(table(labs.vitals[!is.na(labs.vitals[[clin]]),]$ANON_ID))
    labs.vitals.tmp = labs.vitals[labs.vitals$ANON_ID %in% names(patients[patients > min_visits]),]
    labs.vitals.tmp$ANON_ID = factor(labs.vitals.tmp$ANON_ID)
    
    nn = nrow(labs.vitals.tmp)
    smp = sample(nn)
    test = smp[(1+floor(nn*0.9)):nn]
    train = smp[1:floor(nn*0.9)]

    frm = paste0(clin," ~ Pulse + Temp + (Pulse + Temp|ANON_ID)")
    print(frm)
    mm = lmer(frm, data = labs.vitals.tmp[train,])
    cf = coef(mm)
    vit = "Pulse"
    qq = qplot(cf$ANON_ID[vit], geom="histogram")  + weartals_theme + xlab(paste0(nms[i]," ~ ",vit)) + ylab("count")
    print(qq, vp = viewport(layout.pos.row = matchidx$row,
                            layout.pos.col = matchidx$col))

    # Evaluate LR model
    frm = paste0(clin," ~ Pulse + Temp")
    m0 = lm(frm, labs.vitals.tmp[train,])
    pp = predict(m0, newdata = labs.vitals.tmp[test,])
    plot(pp, tt)
    print(cor(pp,tt,use = "na.or.complete"))
    
    # Evaluate MM model
    tt = labs.vitals.tmp[test,clin]
    pp = predict(mm, newdata = labs.vitals.tmp[test,])
    plot(pp, tt)
    print(cor(pp,tt,use = "na.or.complete"))
    
    # Evaluate LR model with ID
    frm = paste0(clin," ~ ANON_ID")
    m0 = lm(frm, labs.vitals.tmp[train,])
    pp = predict(m0, newdata = labs.vitals.tmp[test,])
    plot(pp, tt)
    print(cor(pp,tt,use = "na.or.complete"))
}



library(ggplot2)
library("lme4")

i = 1
vit = vits[j+1]
clin = top8[i]
#2*(i - 1) + j + 1
matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

patients = sort(table(labs.vitals[!is.na(labs.vitals[[clin]]),]$ANON_ID))
labs.vitals.tmp = labs.vitals[labs.vitals$ANON_ID %in% names(patients[patients > 20]),]

frm = paste0(clin," ~ ",vit," + (",vit,"|ANON_ID)")
print(frm)
mm = lmer(frm, data = labs.vitals.tmp)
cf = coef(mm)

quantile(cf$ANON_ID$Pulse)

cf$ANON_ID[cf$ANON_ID$Pulse < -0.4,]
dd = labs.vitals[labs.vitals$ANON_ID=="N-7881",c(vit,clin)]
ggplot(dd, aes_string(x = clin, y = vit)) + weartals_theme + 
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1.5)
ggsave("plots/neut-extreme-neg.png")

cf$ANON_ID[cf$ANON_ID$Pulse > 0.7,]
dd = labs.vitals[labs.vitals$ANON_ID=="D-6050",c(vit,clin)]
ggplot(dd, aes_string(x = clin, y = vit)) + weartals_theme + 
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1.5)
ggsave("plots/neut-extreme-pos.png")

toppat = names(sort(-table(labs.vitals$ANON_ID))[c(1:10)])

dd = labs.vitals[labs.vitals$ANON_ID %in% c("N-7881","D-6050",toppat) ,c("ANON_ID",vit,clin)]

if (!("ww" %in% ls()))
  ww = loess(paste0(vit," ~ ",clin), labs.vitals)
grid = seq(min(labs.vitals[[clin]], na.rm = T),max(labs.vitals[[clin]],na.rm = T),length.out = 100)
ff = approxfun(grid, predict(ww,grid))

ggplot(dd, aes_string(clin, vit, group = "ANON_ID", colour = "ANON_ID")) + 
  weartals_theme + theme(text = element_text(size=20)) +
#  geom_point(size=0) + 
  geom_smooth(method="lm", formula = y ~ x + I(x^2), size=1, fill=NA) +
  stat_function(fun = ff, size=0.7, color="black", linetype="dashed")
ggsave("plots/neut-slopes.png")

res = c()
for (iid in c(1:3)){
  vit = "Temp"
  clin = "NEUT"
  dd = labs.vitals[labs.vitals$ANON_ID %in% c(toppat[iid]) ,c("ANON_ID",vit,clin)]
  frm = paste0(clin," ~ 1")#,vit)
  mm = lm(frm, data = dd)
  mm
  
  print(nrow(dd))
  
  newres = c()
  for (nsmp in c( 3:20 * 7)){
    trial = c()
    for (r in 1:200){
      n = nrow(dd)
      dd0 = dd[sample(nsmp,replace = TRUE),]
      mm0 = lm(frm, data = dd0)
#      trial = rbind(trial, ((coef(mm0) - coef(mm))**2 / coef(mm)**2))
      trial = rbind(trial, coef(mm0)[1])
    }
    newres = rbind(newres, c(iid,nsmp,mean(trial),quantile(trial,c(0.025, 0.975))))
  }
  
  # newres[,3] = smooth.spline(newres[,3])$y
  # sdsm = smooth.spline(newres[,4])$y
  # newres[,4] = newres[,3] - sdsm
  # newres[,5] = newres[,3] + sdsm
  res = rbind(res, newres)
}
df = data.frame(Patient = toppat[res[,1]], obs = res[,2], mean = res[,3], sd_low = res[,4], sd_hi = res[,5])

ggplot(df, aes(obs, mean, group=Patient, colour=Patient))+
    geom_line() + weartals_theme + xlab("number of observations") + ylab(frm) + 
    geom_ribbon(aes(ymin=sd_low,ymax=sd_hi),alpha=0.3, colour = NA)
#    geom_line(data=predframe)+
#    geom_ribbon(data=predframe,aes(ymin=lwr,ymax=upr),alpha=0.3))
## Extreme examples

coef(mm.neut)


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
