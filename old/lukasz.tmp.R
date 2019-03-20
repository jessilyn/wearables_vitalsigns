#install.packages("ggfortify")
library(ggfortify)
labs.df = data.frame(labs[,colnames(labs) %in% c(colnames(labs)[1:2], allClin)])

wear.df = data.frame(wear)
clinwear = merge(labs.df, wear.df, by.x = c("iPOP_ID","Clin_Result_Date"), by.y = c("iPOP_ID","Clin_Result_Date") )

clinwear = na.omit(clinwear)
matched.labs = scale(clinwear[,3:(ncol(labs.df))])
matched.wear = scale(clinwear[,(ncol(labs.df) + 1):ncol(clinwear)])
matched.wear[,]

pc.wear = prcomp(matched.wear)
pc.wear$x[,1]

pc.labs = prcomp(matched.labs)
pc.labs$x[,1]

#cc = cancor(matched.labs, matched.wear)

pca = prcomp(labs.clin[,-c(1,2)])

## Liver
liver.cols = c("ALKP","BUN","ALT","TBIL","AST")
liver = matched.labs[,colnames(matched.labs) %in% liver.cols]
pc.liver = prcomp(liver)


plot(pc.wear$x[,1],pc.liver$x[,1])
cor(pc.wear$x[,1],liver[,3])**2

# Cardiometabolic
cardiometabolic.cols = c("A1C","ALB","GLU","UALB","CR","ALCRU","EGFR","CHOL","LDLHDL","HDL","CHOLHDL","NHDL","TGL","BMI","LDL","Pulse","BP")
cardiometabolic = matched.labs[,colnames(matched.labs) %in% cardiometabolic.cols]
pc.cardiometabolic = prcomp(cardiometabolic)

M = lm(pc.labs$x[,1] ~ matched.wear[,c("hr_mean","st_mean","gsr_mean","rhr_mean")])
summary(M)
M1 = stepAIC(M)
summary()
plot(pc.wear$x[,5],pc.cardiometabolic$x[,1])
cor(pc.wear$x[,1],pc.cardiometabolic$x[,1])

# Cardiometabolic
cardiometabolic.cols = c("A1C","ALB","GLU","UALB","CR","ALCRU","EGFR","CHOL","LDLHDL","HDL","CHOLHDL","NHDL","TGL","BMI","LDL","Pulse","BP")
cardiometabolic = matched.labs[,colnames(matched.labs) %in% cardiometabolic.cols]
pc.cardiometabolic = prcomp(cardiometabolic)

M = lm(pc.labs$x[,1] ~ matched.wear[,c("hr_mean","st_mean","gsr_mean","rhr_mean")])
summary(lm(matched.labs[,"GLU"] ~ matched.wear[,c("hr_mean")]))
summary(M)
M1 = stepAIC(M)
summary()
plot(pc.wear$x[,5],pc.cardiometabolic$x[,1])
cor(pc.wear$x[,1],pc.cardiometabolic$x[,1])
