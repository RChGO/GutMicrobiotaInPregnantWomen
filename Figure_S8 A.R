rm(list = ls())

library(ggplot2)
library(reshape2)
library(Rmisc)


setwd("E:\\project\\urine\\final\\final.and.final")

alpha <- as.data.frame(read.csv('file:///E:/project/urine/new/hclust-barplot/data/alpha.txt',head=T,row.names = 1,sep='\t'))
UMP <- as.data.frame(read.csv('file:///E:/project/urine/new/hclust-barplot/data/UMP.map.txt',head=T,row.names = 1,sep='\t'))

alpha <- alpha[rownames(UMP),]
alpha$UMP <- UMP$data.cluster

##  四个alpha多样性  Shannon’s diversity index  ,  observed OTUs  ,  Faith’s phylogenetic diversity   ,  Pielou’s evenness


ggdata <- as.data.frame(cbind(alpha$observed_otus))
ggdata$UMP <- alpha$UMP
colnames(ggdata) <- c("alpha","UMP")

p1 <- ggplot(ggdata, aes(x = UMP, y = alpha, fill = as.factor(UMP))) +
  geom_boxplot() +
  labs(title = "observed OTUs", y = "observed OTUs", fill = "UMP") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(
      angle = 70,
      hjust = 1,
      vjust = 1
    ),
    panel.background = element_rect(fill = "transparent", colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.background = element_rect(fill = "transparent", colour = NA)
  )
p <- list(p1,p2,p3,p4)
pdf(file = "alpha.diversity.pdf",useDingbats = F,width = 30,height = 5)
multiplot(plotlist = p[1:4], cols = 4) 
dev.off()




