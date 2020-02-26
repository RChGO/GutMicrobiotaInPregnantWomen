rm(list = ls())

library(ggplot2)
library(reshape2)
library(Rmisc)

setwd("E:\\project\\urine\\addnew")

alpha <- as.data.frame(read.csv('file:///E:/project/urine/new/hclust-barplot/data/alpha.txt',head=T,row.names = 1,sep='\t'))
genus.UMP <- as.data.frame(read.csv('file:///E:/project/urine/new/hclust-barplot/data/genus.UMP.txt',head=T,row.names = 1,sep='\t'))
high.l6 <- as.data.frame(read.csv('file:///E:/project/urine/new/hclust-barplot/data/high.genus.txt',head=T,row.names = 1,sep='\t'))
UMP <- as.data.frame(read.csv('file:///E:/project/urine/new/hclust-barplot/data/UMP.map.txt',head=T,row.names = 1,sep='\t'))

alpha <- alpha[rownames(UMP),]
alpha$UMP <- UMP$data.cluster
high.l6 <- as.data.frame(t(high.l6))
alpha <- alpha[rownames(high.l6),]
alpha$genus <- 0

## 1 Prevotella   2	Escherichia-Shigella   3	Gardnerella   4	Streptococcus   5	Veillonella   6	Lactobacillus

alpha$genus[alpha$UMP=="UMP1"] <- high.l6$Prevotella[alpha$UMP=="UMP1"]
alpha$genus[alpha$UMP=="UMP2"] <- high.l6$`Escherichia-Shigella`[alpha$UMP=="UMP2"]
alpha$genus[alpha$UMP=="UMP3"] <- high.l6$Gardnerella[alpha$UMP=="UMP3"]
alpha$genus[alpha$UMP=="UMP4"] <- high.l6$Streptococcus[alpha$UMP=="UMP4"]
alpha$genus[alpha$UMP=="UMP5"] <- high.l6$Veillonella[alpha$UMP=="UMP5"]
alpha$genus[alpha$UMP=="UMP6"] <- high.l6$Lactobacillus[alpha$UMP=="UMP6"]
alpha <- alpha[!(alpha$UMP=="UMP7"),]



##  四个alpha多样性  Shannon’s diversity index  ,  observed OTUs  ,  Faith’s phylogenetic diversity   ,  Pielou’s evenness
p1 <-
  ggplot(alpha, aes(x = genus, y = shannon, color = UMP)) + geom_point() +
  geom_smooth(method = "glm", level = 0) +
  labs(title = "Shannon’s diversity index", y = "Shannon’s diversity index", x = "Relative Abundance" , fill = "UMP") +
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

pdf(file = "alpha.genus.UMP.pdf",useDingbats = F,width = 30,height = 5)
multiplot(plotlist = p[1:4], cols = 4) 
dev.off()

UMP6 <- matrix(0,ncol=2,nrow=4)
colnames(UMP6) <- c("pvalue","r")
rownames(UMP6) <- c("observed_otus","shannon","faith_pd","pielou_e")


t1 <- cor.test(alpha$observed_otus[alpha$UMP=="UMP6"],alpha$genus[alpha$UMP=="UMP6"],method = "spearman")
t2 <- cor.test(alpha$shannon[alpha$UMP=="UMP6"],alpha$genus[alpha$UMP=="UMP6"],method = "spearman")
t3 <- cor.test(alpha$faith_pd[alpha$UMP=="UMP6"],alpha$genus[alpha$UMP=="UMP6"],method = "spearman")
t4 <- cor.test(alpha$pielou_e[alpha$UMP=="UMP6"],alpha$genus[alpha$UMP=="UMP6"],method = "spearman")

UMP6[1,1] <- t1$p.value
UMP6[1,2] <- t1$estimate
UMP6[2,1] <- t2$p.value
UMP6[2,2] <- t2$estimate
UMP6[3,1] <- t3$p.value
UMP6[3,2] <- t3$estimate
UMP6[4,1] <- t4$p.value
UMP6[4,2] <- t4$estimate

write.table(UMP6,file = "UMP6.txt",sep = "\t")

