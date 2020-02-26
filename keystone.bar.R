
setwd('E:\\project\\urine\\new3\\network')

library(vegan)
library(ade4)
library(ggplot2)
library(reshape2)
library(cowplot)

bardata <- as.data.frame(read.csv('file:///E:/project/urine/new3/network/keystone.bar.txt',sep='\t',stringsAsFactors = F,head=T,row.names = 1))

genus <- factor(rownames(bardata),levels = as.vector(rownames(bardata)))

pdf(file = "keystone.bar.pdf",useDingbats = F,width = 10,height = 5)
ggplot(bardata,aes(x=genus,y=betweenness)) + 
  geom_bar(stat="identity",width = 0.7) +
  # ylim(0, max(compare.data$avg) * 1.7)+
  # geom_errorbar(aes(ymin=avg, ymax=avg+sd), width=.2,
  #               position=position_dodge(.9)) +
  theme(
    axis.text.x = element_text(angle = 70, hjust = 1,vjust = 1),
    panel.background = element_rect(fill = "transparent", colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.background = element_rect(fill = "transparent", colour = NA)    
  )

dev.off()




