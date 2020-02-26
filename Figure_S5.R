setwd("E:/project/urine/final/cluster/Figure S11")

data <- read.csv('barL6.txt',sep='\t',row.names = 1)
map <- as.data.frame(read.csv('hclust-mapping.txt',sep='\t',row.names = 1))

L6.m <- rowMeans(data) 
data <- data[head(order(L6.m,decreasing = T)),rownames(map)]
rownames(data) <- c("Prevotella","Streptococcus","Lactobacillus","Gardnerella","Escherichia-Shigella","Veillonella")

pdf("UMP1 boxplot.pdf",useDingbats=F)
data1 <- data[,map$data.cluster==1]
boxplot(t(data1),col=2:7,main="UMP1 six genera in the urine microbiota",las=2,cex.axis=0.54)

dev.off()

pdf("UMP2 boxplot.pdf",useDingbats=F)
data2 <- data[,map$data.cluster==2]
boxplot(t(data2),col=2:7,main="UMP2 six genera in the urine microbiota",las=2,cex.axis=0.54)
dev.off()

pdf("UMP3 boxplot.pdf",useDingbats=F)
data3 <- data[,map$data.cluster==3]
boxplot(t(data3),col=2:7,main="UMP3 six genera in the urine microbiota",las=2,cex.axis=0.54)
dev.off()

pdf("UMP4 boxplot.pdf",useDingbats=F)
data4 <- data[,map$data.cluster==4]
boxplot(t(data4),col=2:7,main="UMP4 six genera in the urine microbiota",las=2,cex.axis=0.54)
dev.off()

pdf("UMP5 boxplot.pdf",useDingbats=F)
data5 <- data[,map$data.cluster==5]
boxplot(t(data5),col=2:7,main="UMP5 six genera in the urine microbiota",las=2,cex.axis=0.54)
dev.off()

pdf("UMP6 boxplot.pdf",useDingbats=F)
data6 <- data[,map$data.cluster==6]
boxplot(t(data6),col=2:7,main="UMP6 six genera in the urine microbiota",las=2,cex.axis=0.54)
dev.off()

pdf("UMP7 boxplot.pdf",useDingbats=F)
data7 <- data[,map$data.cluster==7]
boxplot(t(data7),col=2:7,main="UMP7 six genera in the urine microbiota",las=2,cex.axis=0.54)
dev.off()








