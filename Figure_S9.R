rm(list = ls())

library(ggplot2)
library(reshape2)
library(Rmisc)
library(vegan)
library(ade4)


setwd("E:\\project\\urine\\final\\final.and.final")

LD <- as.data.frame(read.csv('file:///E:/project/urine/new/hclust-barplot/compare.data/urine.kegg.module.LD.prof',head=T,row.names = 1,sep='\t'))
LC <- as.data.frame(read.csv('file:///E:/project/urine/new/hclust-barplot/compare.data/urine.kegg.module.LC.prof',head=T,row.names = 1,sep='\t'))
UMP <- as.data.frame(read.csv('file:///E:/project/urine/new/hclust-barplot/data/UMP.map.txt',head=T,row.names = 1,sep='\t'))
module.lc <- as.data.frame(read.csv('file:///C:/Users/Tc/Desktop/module.lc.txt',head=T,sep='\t'))

LD <- as.data.frame(t(LD[,rownames(UMP)]))
LC <- as.data.frame(t(LC[,rownames(UMP)]))


LC$UMP <- UMP$data.cluster




####  dbRDA

data.dist=vegdist(LD,method="bray")

ord <- capscale(LD~UMP$data.cluster,distance = 'bray')
#ord <- dbrda(LD~UMP$data.cluster,distance = 'bray')
ords <- scores(ord,choices=c("CAP1","CAP2"))
or <- sqrt(ords$species[,1]^2+ords$species[,2]^2)
ords$species <- ords$species[head(order(or,decreasing = T),10),]
ords$species <- ords$species[order(or,decreasing = T),]
ords$species <- head(ords$species,10)
pdf(file = "LD.UMP.pdf",useDingbats = F)

s.class(ords$sites,as.factor(UMP$data.cluster),col=2:9)
points(ords$species[,1]/3,ords$species[,2]/3,col=1)
text(ords$species[,1]/3,ords$species[,2]/3+0.03,rownames(ords$species),cex=0.7)

dev.off()


#### UMP compare LC

LC.ggdata <- melt(LC)


LC.ggdata$variable <- factor(LC.ggdata$variable,levels = as.vector(module.lc$LC[module.lc$LC %in% colnames(LC)]))

pdf(file = "UMP.LC.pdf",useDingbats = F,width = 15)
ggplot(LC.ggdata,aes(x=variable,y=value,fill=UMP)) + 
  geom_bar(stat="identity", position=position_dodge(0.9),width = 0.7) +
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
# +
# stat_compare_means(comparisons = "*")+
#   stat_compare_means(method = "anova",label.x = 1.7,label.y = 0.075)
dev.off()







