
# diversity(data, index="shannon"

rm(list=ls())

library(vegan)


KO <- as.data.frame(read.csv('file:///E:/project/urine/new3/data/all.kegg.profile.txt',head=T,row.names = 1,sep='\t'))

low.l6 <- as.data.frame(read.csv('file:///E:/project/urine/new3/data/low.genus.txt',head=T,row.names = 1,sep='\t'))
# UMP <- as.data.frame(read.csv('file:///E:/project/urine/new/hclust-barplot/data/UMP.map.txt',head=T,row.names = 1,sep='\t'))


KO.shannon <- as.data.frame(diversity(t(KO), index="shannon"))

colnames(KO.shannon) <- "KO.shannon"


low.l6 <- low.l6[,rownames(KO.shannon)]

low.sum <- colSums(low.l6)

KO.shannon$low.sum <- low.sum



##  四个alpha多样性  Shannon’s diversity index  ,  observed OTUs  ,  Faith’s phylogenetic diversity   ,  Pielou’s evenness
p <-
  ggplot(KO.shannon, aes(x = low.sum, y = KO.shannon)) + geom_point() +
  annotate("text",label=paste("pvalue=",cor.test(KO.shannon$KO.shannon,KO.shannon$low.sum)$p.value,"  ","r=",cor.test(KO.shannon$KO.shannon,KO.shannon$low.sum)$estimate) ,x=0.75,y=6.2)+
  geom_smooth(method = "glm", level = 0) +
  labs(title = "KO Shannon’s diversity index", y = "KO Shannon’s diversity index", x = "Relative Abundance") +
  expand_limits(y=c(6,9))+
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

setwd("E:\\project\\urine\\new3\\lowgenus.KOshannon")

pdf(file = "KO.shannon.lowgenus.pdf",useDingbats = F)
p
dev.off()