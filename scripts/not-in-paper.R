################################
#  Currently not used in paper #
################################

# RUN iPOP CORRELATIONS between labs and vitals
options("scipen"=100, "digits"=4)
models=c(" ~ Pulse", # univariate with pulse only
         " ~ Temp",   # univariate with temp only
         " ~ Pulse + Temp", # bivariate with pulse + temp
         " ~ Pulse + I(Pulse^2)",
         " ~ Temp + I(Temp^2)" )

for (k in 1:length(models)){
  print(k)
  r<-matrix(ncol = length(unique(iPOPcorDf$iPOP_ID)),nrow = length(allClin),
            dimnames=list(
              c(allClin),
              c(unique(iPOPcorDf$iPOP_ID))))
  rsq.pred <-r; p<-r; fstat <-r; degfree <- r; numObs <-r;
  tmp=0
  for (i in allClin){ # for each of the 50 clinical lab tests
    tmp=tmp+1 # counter for index of allClin
    tmp2=0    # counter for index of iPOP_ID
    for (j in unique(iPOPcorDf$iPOP_ID)){ 
      tmp2=tmp2+1  # counter for index of iPOP_ID
      iPOPcorDf2 <- iPOPcorDf[!(iPOPcorDf$iPOP_ID %in% j),] # leave one person out
      df <- cbind(iPOPcorDf2[[i]], iPOPcorDf2[,c("Pulse", "Temp")])
      df <- na.omit(df)
      model<-lm(as.formula(paste0(i,models[k])),data=iPOPcorDf2)
      m <- summary(model) # quadratic univariate with pulse or temp only
      # r[tmp,tmp2]<-m$adj.r.squared # matrix of r-squared values for each left-one-out model
      # p[tmp,tmp2]<-1-pf(m$fstatistic[1],m$fstatistic[2],m$fstatistic[3]) # matrix of p-squared values for each left-one-out model
      numObs[tmp,tmp2]<-length(df$Pulse) # the number of each clinical lab test that has corresponding vital signs
      iPOPcorDf3 <- iPOPcorDf[(iPOPcorDf$iPOP_ID %in% j),] # test set (the one person that was left out)
      df3 <- cbind(iPOPcorDf3[[i]], iPOPcorDf3[,c("Pulse", "Temp")])
      df3 <- na.omit(df3)
      pred=predict(model, newdata=df3)# prediction on test person
      rsq.pred[tmp,tmp2] = 1 - (mean( (pred - df3[,1])**2 ) / var( (df[,1]) )) # test r.sq
    }
  }
  name.rsq <- paste("model.mean.rsq", k, sep = "")
  assign(name.rsq, data.frame(model = name.rsq, test = allClin, means = rowMeans(rsq.pred), sd =apply(rsq.pred, 1, sd))) 
}

rsq.plot<- as.data.frame(as.list((rbind(model.mean.rsq1, model.mean.rsq2, model.mean.rsq3, model.mean.rsq4, model.mean.rsq5))))
rsq.plot$model <- mapvalues(rsq.plot$model, from = c("model.mean.rsq1", "model.mean.rsq2", "model.mean.rsq3", "model.mean.rsq4", "model.mean.rsq5"), to = c("~ Pulse", "~ Temp", "~ Pulse + Temp", " ~ Pulse + I(Pulse^2)", " ~ Temp + I(Temp^2)"))

# plot how rsq changes with the different models, and add in error bars from sd.plot
ggplot(rsq.plot, aes(x=test, y=means, group=model, col=as.factor(rsq.plot$model))) +
  geom_point() +
  theme(axis.title=element_text(face="bold",size="12"),axis.text=element_text(size=12,face="bold"), panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 60, hjust = 1)) +
  xlab("Clinical Laboratory Test") + ylab("Cross-Validated R-squared (+/- SD)") +
  labs(linetype='Model')
guides(guide_legend(title="Model"))
#guides(fill=guide_legend(title="Model"))
scale_fill_discrete(name="Model")+
  geom_errorbar(aes(ymin=means-sd, ymax=means+sd), width=0.7,
                position=position_dodge(.7)) + ylim(0,1.5)

tot # total number of labs that have clin vitals measures corresponding to it
# num lab tests in iPOP dataset
tot <- 0; for (i in 7:56){
  tmp <- length(as.matrix(na.omit(wear[i]))); tot <- tot + tmp}; tot 

# num vital signs in iPOP dataset
tot <- 0; for (i in 7:56){tmp <- length(as.matrix(na.omit(wear[i]))); tot <- tot + tmp}; tot



#### END ####

## REVISION CODE ##

#Hematologic correlations

# PLT + GLOB + TP + HGB + HCT + RDW + MCH + MCV + RBC + MCHC
subsample <- corDf[sample(nrow(corDf), size = 10000, replace = FALSE),]
pairs(~ HGB + HCT + RBC,data=subsample, 
      main="CBC Correlations")
pairs(~ HGB + HCT + RBC + MONOAB + A1C + GLU_SerPlas + PLT + CL, data=subsample, 
      main="Top Lab Test Correlations")

blood = na.omit(corDf[,c("ANON_ID","Clin_Result_Date","HGB","HCT")])
model.blood <- lm(HGB ~ HCT, data = blood)
summary(model.blood)
#plot(model.blood)


colnames(icd)[colnames(icd)=="ICD_DATE"] <- "Clin_Result_Date"
colnames(cpt)[colnames(cpt)=="CPT_DATE"] <- "Clin_Result_Date"
icd$Clin_Result_Date<- as.character(icd$Clin_Result_Date)
cpt$Clin_Result_Date<- as.character(cpt$Clin_Result_Date)

blood$residuals <- model.blood$residuals ## Add the residuals to the data.frame
o <- order(blood$residuals^2, decreasing=T) ## Reorder to put largest first
top.residuals <- blood[o[1:10],]
top.residuals$Clin_Result_Date <- as.Date(top.residuals$Clin_Result_Date)
top.residuals$Clin_Result_Date<- as.character(top.residuals$Clin_Result_Date)
hgb.hct.divergence <- merge(top.residuals, icd, by = c("ANON_ID", "Clin_Result_Date"))

plot(HGB ~ HCT, data = blood)
abline(model.blood, col = "red")
points(HGB ~ HCT, data = top.residuals, col="red", pch=19)