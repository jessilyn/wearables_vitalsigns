library(ggplot2)
library(dplyr)
path <- "/Users/jessilyndunn/Documents/Dunn_Lab/Research/Papers_under_review/framework_paper/manuscript_drafts/SUBMISSION/REVISION/Nat_Med_RESUBMISSION/final_paper_code/"
setwd(paste0(path, "Fig6A/fifth_run_successful_lassos"))

weartals_theme = theme_classic() + theme(text = element_text(size=18), panel.border = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

top.models <- c("HCT", "HGB", "RBC", "MONOAB", "GLU",  "CL", "A1C", "PLT")

load("all.res.AllData.Rda") # as in population-models-final.R
all.df = all.res %>% group_by(model, test) %>%
  summarise(mean=mean(rve), sd=sd(rve), pval=mean(ve<1e-10))  %>%
  arrange(model,desc(mean))
all.df$test = factor(all.df$test, levels = unique(all.df$test))
All <- as.data.frame(all.df)
All.top <- All[All$test %in% top.models,]
All.rf <- All.top[All.top$model %in% "wear_nopers_rf",]
All.rf[,1]="All"
write.csv(All, paste0(path, "Fig6A/20200601_All_data.csv"))


load("all.res.oneDay.Rda") # as in population-models-final.R
oneDay.df = all.res %>% group_by(model, test) %>%
  summarise(mean=mean(rve), sd=sd(rve), pval=mean(ve<1e-10))  %>%
  arrange(model,desc(mean))
oneDay.df$test = factor(oneDay.df$test, levels = unique(oneDay.df$test))
oneDay <- as.data.frame(oneDay.df)
oneDay.top <- oneDay[oneDay$test %in% top.models,]
oneDay.rf <- oneDay.top[oneDay.top$model %in% "wear_nopers_rf",]
oneDay.rf[,1]="oneDay"
write.csv(oneDay, paste0(path, "Fig6A/20200601_oneDay_data.csv"))


load("all.res.oneMonth.Rda") # as in population-models-final.R
oneDay.df = all.res %>% group_by(model, test) %>%
  summarise(mean=mean(rve), sd=sd(rve), pval=mean(ve<1e-10))  %>%
  arrange(model,desc(mean))
oneDay.df$test = factor(oneDay.df$test, levels = unique(oneDay.df$test))
oneMonth <- as.data.frame(oneDay.df)
oneMonth.top <- oneMonth[oneMonth$test %in% top.models,]
oneMonth.rf <- oneMonth.top[oneMonth.top$model %in% "wear_nopers_rf",]
oneMonth.rf[,1]="oneMonth"
write.csv(oneMonth, paste0(path, "Fig6A/20200601_oneMonth_data.csv"))


load("all.res.oneWeek.Rda") # as in population-models-final.R
oneDay.df = all.res %>% group_by(model, test) %>%
  summarise(mean=mean(rve), sd=sd(rve), pval=mean(ve<1e-10))  %>%
  arrange(model,desc(mean))
oneDay.df$test = factor(oneDay.df$test, levels = unique(oneDay.df$test))
oneWeek <- as.data.frame(oneDay.df)
oneWeek.top <- oneWeek[oneWeek$test %in% top.models,]
oneWeek.rf <- oneWeek.top[oneWeek.top$model %in% "wear_nopers_rf",]
oneWeek.rf[,1]="oneWeek"
write.csv(oneWeek, paste0(path, "Fig6A/20200601_oneWeek_data.csv"))

load("all.res.threeDay.Rda") # as in population-models-final.R
oneDay.df = all.res %>% group_by(model, test) %>%
  summarise(mean=mean(rve), sd=sd(rve), pval=mean(ve<1e-10))  %>%
  arrange(model,desc(mean))
oneDay.df$test = factor(oneDay.df$test, levels = unique(oneDay.df$test))
threeDay <- as.data.frame(oneDay.df)
threeDay.top <- threeDay[threeDay$test %in% top.models,]
threeDay.rf <- threeDay.top[threeDay.top$model %in% "wear_nopers_rf",]
threeDay.rf[,1]="threeDay"
write.csv(threeDay, paste0(path, "Fig6A/20200601_threeDay_data.csv"))


to.plot<-rbind(oneDay.rf, threeDay.rf, oneWeek.rf, oneMonth.rf, All.rf)
to.plot$model<-factor(to.plot$model, levels = c("oneDay", "threeDay", "oneWeek", "oneMonth", "All", ordered = TRUE))

##cant get this working
colorsBlue <- c("#F7FBFF", "#C6DBEF", "#4292C6", "#08519C", "#08306B")
p<-ggplot(to.plot, mapping= aes(x=test,y=mean,color= colorsBlue)) +
  geom_point(size=3, position=position_dodge(width=0.2)) +
  geom_errorbar(aes(ymin=mean - to.plot$sd, ymax=mean + to.plot$sd), width=.8, position=position_dodge(width=0.2)) +
  weartals_theme+
  ylim(-0.1, 0.65)+
  scale_color_manual(labels = c("1 Day", "3 Day", "1 Week", "1 Month", "All Data")) +
  theme(legend.title = element_blank())+
  labs(x = NULL, y =expression(sqrt("Variance explained")))



##this version works but without a legend
colorsBlue<- rep(c("#F7FBFF", "#C6DBEF", "#4292C6", "#08519C", "#08306B"),8)
p<-ggplot(to.plot, mapping= aes(x=test,y=mean,color= colorsBlue)) +
  geom_point(size=3, position=position_dodge(width=0.2),color= colorsBlue) +
  geom_errorbar(aes(ymin=mean - to.plot$sd, ymax=mean + to.plot$sd), width=.8, position=position_dodge(width=0.2),color= colorsBlue) +
  weartals_theme+
  ylim(-0.1, 0.65)+
  scale_color_manual(values=c("#F7FBFF", "#C6DBEF", "#4292C6", "#08519C", "#08306B"), labels = c("1 Day", "3 Day", "1 Week", "1 Month", "All Data"))+
  theme(legend.title = element_blank())+
  labs(x = NULL, y =expression(sqrt("Variance explained")))




ggsave(paste0("Fig6A.png"),p,width=6,height=4)

## manuscript p7 RF and Lasso one Day model comparisons
wilcox.test(oneDay[oneDay$model %in% "wear_nopers_lm_lasso",]$mean, 
            oneDay[oneDay$model %in% "wear_nopers_rf",]$mean,
            alternative = c("two.sided"),
            mu = 0, paired = TRUE, exact = NULL, correct = TRUE,
            conf.int = FALSE, conf.level = 0.95)

wilcox.test(oneDay[oneDay$model %in% "clin_nopers_rf",]$mean, 
            oneDay[oneDay$model %in% "clin_nopers_lm",]$mean,
            alternative = c("two.sided"),
            mu = 0, paired = TRUE, exact = NULL, correct = TRUE,
            conf.int = FALSE, conf.level = 0.95)

wilcox.test(oneDay[oneDay$model %in% "wear_nopers_rf",]$mean, 
            oneDay[oneDay$model %in% "clin_nopers_lm",]$mean,
            alternative = c("two.sided"),
            mu = 0, paired = TRUE, exact = NULL, correct = TRUE,
            conf.int = FALSE, conf.level = 0.95)


mean(oneDay[oneDay$model %in% "wear_nopers_lm_lasso",]$mean)
mean(oneDay[oneDay$model %in% "wear_nopers_rf",]$mean)
mean(oneDay[oneDay$model %in% "clin_nopers_rf",]$mean)
mean(oneDay[oneDay$model %in% "clin_nopers_lm",]$mean)

top.rf.wvs.tests <- oneDay[oneDay$model %in% "wear_nopers_rf" & oneDay$mean > 0,]$test
top.lasso.wvs.tests <- oneDay[oneDay$model %in% "wear_nopers_lm_lasso" & oneDay$mean > 0,]$test
top.rf.cvs.tests <- oneDay[oneDay$model %in% "clin_nopers_rf" & oneDay$mean > 0,]$test
top.lm.cvs.tests <- oneDay[oneDay$model %in% "clin_nopers_lm" & oneDay$mean > 0,]$test

top.rf.wvs.results <- oneDay[oneDay$model %in% "wear_nopers_rf" & oneDay$mean > 0,]
top.lasso.wvs.results <- oneDay[oneDay$model %in% "wear_nopers_lm_lasso" & oneDay$mean > 0,]
top.rf.cvs.results <- oneDay[oneDay$model %in% "clin_nopers_rf" & oneDay$mean > 0,]
top.lm.cvs.results <- oneDay[oneDay$model %in% "clin_nopers_lm" & oneDay$mean > 0,]
rbind(top.rf.wvs.results, top.rf.cvs.results,  top.lm.cvs.results, top.lasso.wvs.results )
