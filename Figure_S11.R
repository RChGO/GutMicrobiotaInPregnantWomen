
rm(list = ls())
setwd('E:\\project\\urine\\new3\\pathway.LB.FOR.highlow')

library(vegan)
library(ade4)
library(ggplot2)
library(reshape2)
library(cowplot)

# L6 <- as.matrix(read.csv('file:///E:/project/urine/new/hclust-barplot/data/newL610000.txt',sep='\t',stringsAsFactors = F,head=T,row.names = 1))
group <- as.data.frame(read.table("file:///E:/project/urine/new/hclust-barplot/data/map.age.txt",sep = "\t",row.names = 1,header = T))
# 
#
##将功能表乘以百分比
high.LD <- as.data.frame(read.table("file:///E:/CRTdata/old/high.kegg.module.LD.prof",sep = "\t",row.names = 1,header = T))
low.LD <- as.data.frame(read.table("file:///E:/CRTdata/old/low.kegg.module.LD.prof",sep = "\t",row.names = 1,header = T))


high.LD <- high.LD[,rownames(group)]
low.LD <- low.LD[,rownames(group)]

##将功能表乘以百分比
high.LB <- as.data.frame(read.table("file:///E:/CRTdata/old/high.kegg.pathway.LB.prof",sep = "\t",row.names = 1,header = T))
low.LB <- as.data.frame(read.table("file:///E:/CRTdata/old/low.kegg.pathway.LB.prof",sep = "\t",row.names = 1,header = T))
pathway <- as.data.frame(read.table("file:///E:/project/urine/new3/data/pathwayLB.txt",sep = "\t",header = T))

# high.LB <- high.LB[,colnames(out)]
# low.LB <- low.LB[,colnames(out)]

high.LB <- high.LB[rownames(high.LB) %in% pathway$LB,]
low.LB <- low.LB[rownames(low.LB) %in% pathway$LB,]

high.LB <- high.LB[rownames(high.LB) %in% rownames(low.LB),]
low.LB <- low.LB[rownames(low.LB) %in% rownames(high.LB),]

# for (i in 1:ncol(out)) {
#   high.LB[,i] <- high.LB[,i]*as.numeric(out[1,i])
#   low.LB[,i] <- low.LB[,i]*as.numeric(out[2,i])
# }

high.gdata <- melt(t(high.LB))
colnames(high.gdata) <- c("group","LB","value")

low.gdata <- melt(t(low.LB))
colnames(low.gdata) <- c("group","LB","value")

high.gdata$LB <- factor(high.gdata$LB,levels = as.vector(unique(pathway$LB[pathway$LB %in% high.gdata$LB])))
low.gdata$LB <- factor(low.gdata$LB,levels = as.vector(unique(pathway$LB[pathway$LB %in% low.gdata$LB])))


head(high.gdata)
head(low.gdata)


high.gdata$group <- "high"
low.gdata$group <- "low"

gdata <- rbind(high.gdata,low.gdata)

pdf(file = "final.highlow.LB.pdf",useDingbats = F,width = 15)

ggplot(gdata,aes(x=LB,y=value,fill=group)) + 
  geom_boxplot( position=position_dodge(0.9),width = 0.7) +
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


write.table(gdata,file = "highlow.LB.table.txt",sep = "\t")

##算pvalue
pvalue <- matrix(0,nrow=nrow(high.LB),ncol=1)
rownames(pvalue) <- rownames(high.LB)
colnames(pvalue) <- "pvalue"
for (i in 1:nrow(high.LB)) {
  
  pvalue[i,1] <- wilcox.test(as.numeric(high.LB[i,]),as.numeric(low.LB[i,]))$p.value
  
}

write.table(pvalue,file = "final.pvalue.txt",sep = "\t")

