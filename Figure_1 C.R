rm(list = ls())

setwd("E:\\project\\urine\\new\\hclust-barplot\\compare.data")

library(reshape2)
library(RColorBrewer)
library(ggplot2)
library(cowplot)
#####################################################################################################################################
######    function
#####################################################################################################################################

species<-function(ii){
  ii<-as.character(ii)
  #ii[intersect(grep('.*[|,.;]s__.*[|,.;]t__..*',ii),grep('[|,.;]t__$',ii,invert=T))]<-as.character(lapply(ii[intersect(grep('.*[|,.;]s__.*[|,.;]t__..*',ii),grep('[|,.;]t__$',ii,invert=T))],function(x){gsub('.*[|,.;]t','t',x)}))
  #ii[intersect(grep('.*[|,.;]g__.*[|,.;]s__..*',ii),grep('s__[|,.;]t__',ii,invert=T))]<-as.character(lapply(ii[intersect(grep('.*[|,.;]g__.*[|,.;]s__..*',ii),grep('s__[|,.;]t__',ii,invert=T))],function(x){gsub('.*[|,.;]s','s',x)}))
  #ii[intersect(grep('.*[|,.;]f__.*[|,.;]g__..*',ii),grep('g__[|,.;]s__',ii,invert=T))]<-as.character(lapply(ii[intersect(grep('.*[|,.;]f__.*[|,.;]g__..*',ii),grep('g__[|,.;]s__',ii,invert=T))],function(x){gsub('.*[|,.;]g','g',x)}))
  ii[intersect(grep('.*[|,.;]o__.*[|,.;]f__..*',ii),grep('f__[|,.;]g__',ii,invert=T))]<-as.character(lapply(ii[intersect(grep('.*[|,.;]o__.*[|,.;]f__..*',ii),grep('f__[|,.;]g__',ii,invert=T))],function(x){gsub('.*[|,.;]f','f',x)}))
  ii[intersect(grep('.*[|,.;]c__.*[|,.;]o__..*',ii),grep('o__[|,.;]f__',ii,invert=T))]<-as.character(lapply(ii[intersect(grep('.*[|,.;]c__.*[|,.;]o__..*',ii),grep('o__[|,.;]f__',ii,invert=T))],function(x){gsub('.*[|,.;]o','o',x)}))
  ii[intersect(grep('.*[|,.;]p__.*[|,.;]c__..*',ii),grep('c__[|,.;]o__',ii,invert=T))]<-as.character(lapply(ii[intersect(grep('.*[|,.;]p__.*[|,.;]c__..*',ii),grep('c__[|,.;]o__',ii,invert=T))],function(x){gsub('.*[|,.;]c','c',x)}))
  ii[intersect(grep('k__.*[|,.;]p__..*',ii),grep('k__[|,.;]p__',ii,invert=T))]<-as.character(lapply(ii[intersect(grep('k__.*[|,.;]p__..*',ii),grep('k__[|,.;]p__',ii,invert=T))],function(x){gsub('.*[|,.;]p','p',x)}))
  return(ii)
}

urine.l6 <- as.data.frame(read.csv('file:///E:/project/urine/new/hclust-barplot/compare.data/urine.L6.prof',skip = 1,head=T,row.names = 1,sep='\t'))

gut.l6 <- as.data.frame(read.csv('file:///E:/project/urine/new/hclust-barplot/compare.data/gut.L6.prof',skip = 1,head=T,row.names = 1,sep='\t'))

####均一化
urine.l6 <- urine.l6/10000
gut.l6 <- gut.l6/1600

b <- rownames(urine.l6)[rownames(urine.l6) %in% rownames(gut.l6)]   ###  213
d <- rownames(urine.l6)[rowMeans(urine.l6)>1/100][rownames(urine.l6)[rowMeans(urine.l6)>1/100] %in% rownames(gut.l6)[rowMeans(gut.l6)>1/100]]    ###  8

##基于含量在1/100以上，urine(5,16)与gut(8,24)共有的菌进行分析

urine.l6 <- urine.l6[rowMeans(urine.l6)>1/100,]
gut.l6 <- gut.l6[rowMeans(gut.l6)>1/100,]

rownames(urine.l6)
rownames(gut.l6)
gut.l6 <- gut.l6[-20,]
write.table(gut.l6,file = "gut.l6.txt",sep = "\t")
write.table(urine.l6,file = "urine.l6.txt",sep = "\t")

urine.l6 <- as.data.frame(read.csv('file:///E:/project/urine/new/hclust-barplot/compare.data/urine.l6100.txt',head=T,row.names = 1,sep='\t'))
gut.l6 <- as.data.frame(read.csv('file:///E:/project/urine/new/hclust-barplot/compare.data/gut.l6100.txt',head=T,row.names = 1,sep='\t'))

urine.l6 <- as.data.frame(t(urine.l6))
gut.l6 <- as.data.frame(t(gut.l6))

urine.l6.gdata <- melt(urine.l6)
colnames(urine.l6.gdata) <- c("Genus","value")

gut.l6.gdata <- melt(gut.l6)
colnames(gut.l6.gdata) <- c("Genus","value")

urine.l6.gdata$GG <- species(urine.l6.gdata$Genus)
urine.l6.gdata$name <- gsub('.*;p__','',urine.l6.gdata$Genus)
urine.l6.gdata$name <- gsub(';.*','',urine.l6.gdata$name)

urine.l6.gdata$GG <- factor(urine.l6.gdata$GG,levels=as.vector(unique(urine.l6.gdata$GG)))




gut.l6.gdata$GG <- species(gut.l6.gdata$Genus)
gut.l6.gdata$name <- gsub('.*;p__','',gut.l6.gdata$Genus)
gut.l6.gdata$name <- gsub(';.*','',gut.l6.gdata$name)

gut.l6.gdata$GG <- factor(gut.l6.gdata$GG,levels=as.vector(unique(gut.l6.gdata$GG)))


#####画图

#Color = data.frame(GG=sort(union(unique(urine.l6.gdata$name),unique(urine.l6.gdata$name))),Color=brewer.pal(4,'Paired'),stringsAsFactors = F)

p1 <- ggplot(data = urine.l6.gdata , aes(x = GG,y = value)) +
  geom_boxplot(aes(fill = name),size = 0.2,outlier.size = 0.2,alpha = 0.8) +
  #coord_flip() +
  labs(title = "", x = "", y = "Relative Abundance") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(family = "Helvetica",face = "bold",colour = "black",size = 10),
    axis.text = element_text(family = "Helvetica",colour = "black",size = 6,angle = 60,hjust = 1),
    axis.line.y = element_blank(),
    strip.text.y = element_text(family = "Helvetica",colour = "black",angle = 180,vjust = 0.5,hjust = 1,size = 10),
    strip.background = element_rect(size = 0.3,colour = 'black'),
    panel.border = element_rect(fill = NA,colour = "black",size = 0.3),
    legend.title = element_text(family = "Helvetica",colour = "black",size = 10),
    legend.text = element_text(family = "Helvetica",colour = "black",size = 8)
  ) +
  guides(fill = guide_legend("", ncol = 1), color = F)    #可修改legend的title，排列情况

p2 <- ggplot(data = gut.l6.gdata , aes(x = GG,y = value)) +
  geom_boxplot(aes(fill = name),size = 0.2,outlier.size = 0.2,alpha = 0.8) +
  #coord_flip() +
  labs(title = "", x = "", y = "Relative Abundance") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(family = "Helvetica",face = "bold",colour = "black",size = 10),
    axis.text = element_text(family = "Helvetica",colour = "black",size = 6,angle = 60,hjust = 1),
    axis.line.y = element_blank(),
    strip.text.y = element_text(family = "Helvetica",colour = "black",angle = 180,vjust = 0.5,hjust = 1,size = 10),
    strip.background = element_rect(size = 0.3,colour = 'black'),
    panel.border = element_rect(fill = NA,colour = "black",size = 0.3),
    legend.title = element_text(family = "Helvetica",colour = "black",size = 10),
    legend.text = element_text(family = "Helvetica",colour = "black",size = 8)
  ) +
  guides(fill = guide_legend("", ncol = 1), color = F)    #可修改legend的title，排列情况

pdf(file = "new.campare.genus.pdf",useDingbats = F,width = 7,height = 10)
plot_grid(p1,p2,nrow=2,ncol=1)
dev.off()











