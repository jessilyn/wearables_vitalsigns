###
# 2018-05-16 
#
# to plot the differences in pct_dev or corr_coef using different timespans of data leading up to the clinic visit

library(reshape2)
timespans <-c("AllData",
              "Monthprior",
              "TwoWeekPrior",
              "Weekprior",
              "ThreeDayPrior",
              "Dayprior" )
for (i in 1:length(timespans)){
nm <- timespans[i]
assign(nm[[i]], read.csv(paste0("/Users/jessilyn/Desktop/framework_timecourse/with_restingbugfix_demog_pctdev/scg_runs/", 
                        "20180507_pct_var_", timespans[i], ".csv"),
                 header=TRUE,sep=',',stringsAsFactors=FALSE))
}

vitals <- list()
lasso <- list()
rf <- list()
for (i in 1:(length(timespans))){
  vitals[[i]] <- eval(parse(text = paste(timespans[i],"$vitals",sep=""))) 
  lasso[[i]] <- eval(parse(text = paste(timespans[i],"$lasso",sep=""))) 
  rf[[i]] <- eval(parse(text = paste(timespans[i],"$rf",sep=""))) 
}

models <- c("vitals", "lasso", "rf")
for (model in models){
df <- data.frame(matrix(unlist(eval(parse(text = model))), nrow=length(eval(parse(text = model))[[1]]), byrow=F))
df[is.na(df)] <- 0 
df <- cbind(Dayprior$test, df)
colnames(df)<-c("test", timespans)
melt.df = melt(df, id = "test")
nm <- paste0("melt.df.",model)
assign(nm, melt.df)
}

# reorder
melt.df.rf$test = factor(melt.df.rf$test, levels = as.factor(melt.df.rf$test[order(-melt.df.rf$value)]))
melt.df.lasso$test = factor(melt.df.lasso$test, levels = as.factor(melt.df.lasso$test[order(-melt.df.lasso$value)]))
melt.df.vitals$test = factor(melt.df.vitals$test, levels = as.factor(melt.df.vitals$test[order(-melt.df.vitals$value)]))

weartals_theme = theme_bw() + theme(text = element_text(size=12), panel.border = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
for (model in models){
  df <- paste0("melt.df.",model)
  print(ggplot(eval(parse(text = df)), aes(x = test, y = value, group = variable, colour = variable)) + 
  geom_point() +
  weartals_theme + 
  labs(x = "Lab tests",y = expression(paste("Sqrt of % Variance Explained"))) +
  ggtitle(model))
}