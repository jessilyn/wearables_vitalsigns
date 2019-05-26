detach("package:plyr", unload=TRUE,force = TRUE)
library(ggplot2)
library(dplyr)

weartals_theme = theme_classic() + theme(text = element_text(size=18), panel.border = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

# save(all.res,file="all.res.Rda")

load("20190525-fig2d.Rda") # as in population-models-final.R

df = data.frame(all.res) %>% group_by(model, test) %>%
  summarise(mean=mean(rve), mean.ve=mean(ve), sd=sd(rve))  %>%
  arrange(model,desc(mean))
df$test = factor(df$test, levels = unique(df$test))

df$min = df$mean - df$sd
df$min[df$min < 0] = 0
df$max = df$mean + df$sd
p = ggplot(df, mapping= aes(x=test,y=mean,color=model)) + 
  geom_errorbar(aes(ymin=min, ymax=max), width=.2) +
  geom_point(size=3) +
  weartals_theme + 
  labs(x = NULL, y ="RPVE")
p
ggsave(paste0("population.png"),p,width=14,height=5)

## Get p-values
# Before running the code below
# * run bootstrap.experiment.2d experiment with randomized = TRUE 1000 times
# * summarize all the results into null.res (as in the all.res)
# * null.res is saved in fig2d-null.Rda together with all.res
# Belowe
# * use empirical distribution under the null 
# * p-value = likelihood of our result under the null
df = data.frame(df)
pval = c()
for (i in 1:nrow(df))
{
  pval = c(pval,mean((null.res[(as.character(null.res$test)==df[i,"test"]) & (as.character(null.res$model)==df[i,"model"]),])$ve > df$mean.ve[i]))
}
df$pval = pval
p.adjust(df$pval,method ="bonferroni")
p.adjust(df$pval,method ="fdr")
