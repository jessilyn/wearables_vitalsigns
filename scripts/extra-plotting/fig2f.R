load("res.cca.Rda")
df.res = data.frame(res.cca) %>%
  gather(group, value, Electrolytes:Hematologic) %>%
  group_by(group) %>%
  summarise(mean=mean(value), sd=sd(value)) %>%
  arrange(desc(mean))
df.res = data.frame(df.res)
df.res$group = factor(df.res$group, levels = df.res$group)

p=ggplot(df.res, aes(x=group, y=mean)) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2) +
  theme(legend.title = element_blank()) +
  geom_point(size=4.5) +
  weartals_theme + 
  ylim(0,0.5) +
  labs(x = NULL, y ="Correlation Coefficient")
p
geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.5)
ggsave(paste0("Figure2F.png"),p,width=8,height=6)
