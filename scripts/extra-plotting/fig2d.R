library(ggplot2)
library(dplyr)

weartals_theme = theme_classic() + theme(text = element_text(size=18), panel.border = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

# save(all.res,file="all.res.Rda")
load("all.res.Rda") # as in population-models-final.R

df = all.res %>% group_by(model, test) %>%
  summarise(mean=mean(rve), sd=sd(rve), pval=mean(ve<1e-10))  %>%
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


library("dplyr")
data("mtcars")
mtcars %>%
  group_by(cyl) %>%
  summarize(mean = mean(disp))
