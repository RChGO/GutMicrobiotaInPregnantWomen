
rm(list = ls())
setwd('E:\\project\\urine\\new3\\compare.figS3')

library(vegan)
library(ade4)
library(ggplot2)
library(reshape2)
library(cowplot)

group <- as.data.frame(read.table("file:///E:/project/urine/new/hclust-barplot/data/map.age.txt",sep = "\t",row.names = 1,header = T))

##将功能表乘以百分比
high.LB <- as.data.frame(read.table("file:///E:/project/urine/new3/compare.figS3/all.kegg.pathway.LB.prof",sep = "\t",row.names = 1,header = T))
low.LB <- as.data.frame(read.table("file:///E:/project/urine/new3/compare.figS3/gut.kegg.pathway.LB.prof",sep = "\t",row.names = 1,header = T))
pathway <- as.data.frame(read.table("file:///E:/project/urine/new3/data/pathwayLB.txt",sep = "\t",header = T))


high.LB <- high.LB[rownames(high.LB) %in% pathway$LB,]
low.LB <- low.LB[rownames(low.LB) %in% pathway$LB,]

high.LB <- high.LB[rownames(high.LB) %in% rownames(low.LB),]
low.LB <- low.LB[rownames(low.LB) %in% rownames(high.LB),]

high.gdata <- melt(t(high.LB))
colnames(high.gdata) <- c("group","LB","value")

low.gdata <- melt(t(low.LB))
colnames(low.gdata) <- c("group","LB","value")

high.gdata$LB <- factor(high.gdata$LB,levels = as.vector(unique(pathway$LB[pathway$LB %in% high.gdata$LB])))
low.gdata$LB <- factor(low.gdata$LB,levels = as.vector(unique(pathway$LB[pathway$LB %in% low.gdata$LB])))


head(high.gdata)
head(low.gdata)


high.gdata$group <- "urine"
low.gdata$group <- "gut"

gdata <- rbind(high.gdata,low.gdata)

pdf(file = "final.compare.gut.LB.pdf",useDingbats = F,width = 15)

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

write.table(pvalue,file = "gut.LB.pvalue.txt",sep = "\t")



low.LB <- low.LB[rownames(high.LB),]

allpvalue<- function(prof1,prof2,rownameT) {
  prof1 <- t(prof1)
  prof2 <- t(prof2)
  p <- matrix(0,nrow = ncol(prof1),ncol=1)
  colnames(p) <- "p-value"
  if(rownameT==T){rownames(p) <- colnames(prof1)}
  
  for(i in 1:ncol(prof1)){
    
      p[i] <- wilcox.test(prof1[,i],prof2[,i])$p.value
   
  }
  return(p)
}

out <- allpvalue(high.LB,low.LB,T)

write.table(out,file = "pathway.LB.pvalue.txt",sep = "\t")







