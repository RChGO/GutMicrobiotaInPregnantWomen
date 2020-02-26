#####################################################################################################################################
###先用hclust(dist(x),method="Ward.D2")聚类，再用barplot画图
#####################################################################################################################################
rm(list = ls())

##JSD--genus  clutter
library("vegan")
library("clusterSim")
library("ade4")
library("rgl")
library("grid")
library("ggplot2")
library("ggtree")
library("ggdendro")
library("ggpubr")
library("reshape2")
library("RColorBrewer")

#################################################################################################################################
####   function   ####
#################################################################################################################################

JSD<- function(x,y) sqrt(0.5 * KLD(x, (x+y)/2) + 0.5 * KLD(y, (x+y)/2))
KLD <- function(x,y) sum(x * log(x/y))

dist.JSD <- function(inMatrix, pseudocount=0.000001, ...) {
  KLD <- function(x,y) sum(x *log(x/y))
  JSD<- function(x,y) sqrt(0.5 * KLD(x, (x+y)/2) + 0.5 * KLD(y, (x+y)/2))
  matrixColSize <- length(colnames(inMatrix))
  matrixRowSize <- length(rownames(inMatrix))
  colnames <- colnames(inMatrix)
  resultsMatrix <- matrix(0, matrixColSize, matrixColSize)
  
  inMatrix = apply(inMatrix,1:2,function(x) ifelse (x==0,pseudocount,x))
  
  for(i in 1:matrixColSize) {
    for(j in 1:matrixColSize) { 
      resultsMatrix[i,j]=JSD(as.vector(inMatrix[,i]),
                             as.vector(inMatrix[,j]))
    }
  }
  colnames -> colnames(resultsMatrix) -> rownames(resultsMatrix)
  as.dist(resultsMatrix)->resultsMatrix
  attr(resultsMatrix, "method") <- "dist"
  return(resultsMatrix) 
}

#################################################################################################################################
####   数据整理（  百分之一、千分之一、千分之五  //  JSD / bray）   ####
#################################################################################################################################

setwd("E:/project/urine/new/hclust-barplot")

####   data--genus（  百分之一、千分之一、千分之五  ）   ####
data <- read.table(file = "bar100L6.txt",sep = "\t",header = T,row.names = 1)  ## bar100L6是已经筛选了百分之一以上的菌，含量在百分之一以下的为others
data <- read.table(file = "bar5.1000L6.txt",sep = "\t",header = T,row.names = 1)  ## bar5.1000L6.txt是已经筛选了千分之五以上的菌，含量在千分之五以下的为others
data <- read.table(file = "barL6.txt",sep = "\t",header = T,row.names = 1)  ## barL6.txt是已经筛选了千分之一以上的菌，含量在千分之一以下的为others

dim(data)

####  JSD  data.dist（ JSD / bray ）   ####
data.dist <- dist.JSD(data)
out.hclust <- hclust(as.dist(data.dist),method="ward.D2")  ##聚类分析

####  bray  data.dist（ JSD / bray ）   ####
data.dist=vegdist(t(data),method="bray")
out.hclust <- hclust(data.dist,method="ward.D2")  ##聚类分析

#################################################################################################################################
####   cluter--score   ####
#################################################################################################################################

nclusters=NULL
silhouetteData <- NULL
for (k in 1:20) { 
  if (k==1) {
    nclusters[k]=NA 
    silhouetteData[k]=NA
  } else {
    data.cluster_cutree = cutree(out.hclust, k)
    silhouetteData[k] = mean(silhouette(data.cluster_cutree, data.dist)[,3])
    nclusters[k]=index.G1(t(data),data.cluster_cutree,  d = data.dist,
                          centrotypes = "medoids")
  }
}

pdf("hclust_JSD_silhouette1000.pdf",useDingbats=F)
plot(silhouetteData,type = "h", xlab="k clusters", ylab="silhouette index")
dev.off()
pdf("hclust_JSD_CH1000.pdf",useDingbats=F)
plot(nclusters, type="h", xlab="k clusters", ylab="CH index")
dev.off()

#################################################################################################################################
####   barplot   ####
#################################################################################################################################

pdf("hclust.pdf",useDingbats=F,width=15)
ggdendrogram(out.hclust,labels = FALSE,leaf_labels = FALSE)+theme_dendro()
dev.off()

newdata <- as.matrix(data[,out.hclust$order])

data.cluster = cutree(out.hclust, k=7)


pdf("bar.hclust.pdf",useDingbats=F,width=15)

newdatagg <- melt(data.frame(genus=row.names(newdata),newdata))
newdatagg$genus<-factor(newdatagg$genus,levels = unique(newdatagg$genus) )
mycolor = c(brewer.pal(12,'Paired'),"#66C2A5","#FC8D62","#E5C494",'#E5D8BD',brewer.pal(8,'Accent'),brewer.pal(9,'Set1'),
            brewer.pal(12,'Set3'),brewer.pal(8,'Dark2'),brewer.pal(8,'Set2'),
            brewer.pal(9,'Pastel1'),brewer.pal(8,'Pastel2'))
  ggplot(newdatagg,aes(variable,value))+
  geom_bar(aes(fill=genus),position = 'fill',stat = 'identity',width = 1,alpha=1)+
  labs(title="", x="sample", y="Relative Abundance")+
  scale_fill_manual(values = mycolor )+
  theme_classic()+
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
  )+
  theme(legend.position="bottom",legend.key.size=unit(0.5,'cm'))#左边left,右边 right, 底部bottom,上部：top
  
dev.off()
#ggarrange( p1 , p2 , ncol = 1,nrow =2,heights = c(1,2),hjust=c(-10,-0.5) )

#################################################################################################################################
####   cluter--PCOA   ####
#################################################################################################################################

data.cluster = cutree(out.hclust, k=7)

obs.pcoa=dudi.pco(data.dist, scannf=F, nf=7)
pdf("cluster_pcoa1.pdf",useDingbats=F)
s.class(obs.pcoa$li, fac=as.factor(data.cluster), grid=F,col=1:10)
dev.off()
pdf("cluster_pcoa2.pdf",useDingbats=F)
s.class(obs.pcoa$li, fac=as.factor(data.cluster), grid=F, cell=0, cstar=0, col=1:10)
dev.off()





