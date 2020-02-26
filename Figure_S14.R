#################################################################################################################################
####   cluter-kegg-PCOA   ####
#################################################################################################################################

####   data--kegg（  module LD、KO ）   ####
setwd("E:\\project\\CRC\\analyse.2019.11.19\\result\\pcoa")
library("vegan")
library(ade4)

kegg <- as.data.frame(read.csv(file = "file:///E:/project/urine/paper/Rscript20200219TC/data/newL610000.txt",sep = "\t",header = T,row.names = 1))  
 
map <- as.data.frame(read.csv(file = "file:///E:/project/urine/paper/Rscript20200219TC/data/UMP.map.txt",sep = "\t",header = T,row.names = 1))  
map <- map[colnames(kegg),]


####   PCOA   ####

####   distance--bray   ####
kegg.dist=vegdist(t(kegg),method="bray")
####   PICTRUE--PCOA   ####
obs.pcoa=dudi.pco(kegg.dist, scannf=F, nf=7)
pdf("KO_PCOA.pdf",useDingbats=F)
s.class(obs.pcoa$li, fac=as.factor(map$Region), grid=F,col=2:10,csta = 0)
dev.off()
# pdf("cluster_KO_pcoa2.pdf",useDingbats=F)
# s.class(obs.pcoa$li, fac=as.factor(map$Region), grid=F, cell=0, cstar=0, col=2:10)
# dev.off()


