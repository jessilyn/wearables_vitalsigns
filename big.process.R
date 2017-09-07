labs = fread("SECURE_data/big.labs.csv")
vitals = fread("SECURE_data/big.vitals.csv")

library(softImpute)
library(ggfortify)
library(ggplot2)

library("ggthemes")
source("ggplot-theme.R")

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

pca.labs = softImpute(data)
PC = pca.labs$v
colnames(PC) = c("PC1","PC2")
PC = data.frame(PC)
PC$test = nms
PC$dist = PC$PC1**2 + PC$PC2**2
PC$col = ifelse(nms %in% names(top),"analyzed","other")
                
ggplot(PC,aes(x=PC1,y=PC2))+
  geom_point(aes(colour=col),size=3,alpha=0.5) +
  geom_text(aes(label=ifelse(dist>0.008,as.character(test),'')),hjust=0.5, vjust=-0.5, size = 5) + 
  weartals_theme + theme(text = element_text(size=20)) + 
  ggtitle("Lab test components")

plot(pca.labs$v)
df <- iris[c(1, 2, 3, 4)]
autoplot()
