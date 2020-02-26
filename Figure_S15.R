setwd("E:\\project\\urine\\addnew")


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


L2 <- as.data.frame(read.csv("file:///E:/project/urine/new/hclust-barplot/data/urine.L2.prof",skip=1,,head=T,row.names=1,sep="\t"))
map <- as.data.frame(read.csv("file:///E:/project/urine/new/hclust-barplot/data/map.age.txt",head=T,sep="\t",row.names=1))
library(ggplot2)
library(dplyr)##用来计算标签位置
library(Rmisc)
library(cowplot)
library(reshape2)
L2 <- L2/10000
L2 <- L2[,rownames(map)]
rownames(L2) <- species(rownames(L2))
L2.M <- as.data.frame(rowMeans(L2[1:5,map$Gender=="M"]))
L2.F <- as.data.frame(rowMeans(L2[1:5,map$Gender=="F"]))

colnames(L2.M) <- "M"
colnames(L2.F) <- "F"
L2.M$phylum<-factor(rownames(L2.M) ,levels = as.character(rownames(L2.M)))
L2.F$phylum<-factor(rownames(L2.F) ,levels = as.character(rownames(L2.F)))

p1 = ggplot(L2.M,aes(x="",y=M,fill=phylum))+
  geom_bar(stat="identity",width=1,position = position_stack(reverse =T))+
  coord_polar(theta="y",direction=1)+
  scale_fill_brewer(palette ="Set3",direction = 1)+
  labs(x="",y="",fill="Type")+
  ggtitle(label ="M",subtitle=NULL)+
  geom_text(aes(x=1.2,label=paste(round(M * 100, 2), "%")),position = position_stack(reverse =T,vjust=0.6),size=4)+
  theme(plot.title = element_text(hjust = 0.5))



p2 = ggplot(L2.F,aes(x="",y=F,fill=phylum))+
  geom_bar(stat="identity",width=1,position = position_stack(reverse =T))+
  coord_polar(theta="y",direction=1)+
  scale_fill_brewer(palette ="Set3",direction = 1)+
  labs(x="",y="",fill="Type")+
  ggtitle(label ="F",subtitle=NULL)+
  geom_text(aes(x=1.2,label=paste(round(F * 100, 2), "%")),position = position_stack(reverse =T,vjust=0.6),size=4)+
  theme(plot.title = element_text(hjust = 0.5))



##multiplot(p_pie1,p_pie2,p_pie3,p_pie4,p_pie5,p_pie6,p_pie7,p_pie8, cols = 4) 
pdf("phylum.fm.pie.pdf",useDingbats=F,width = 18)
plot_grid(p1,p2,nrow=1,ncol=2)
dev.off()

L2 <- as.data.frame(t(L2))


L2.gdata <- as.data.frame(L2$p__Fusobacteria)
colnames(L2.gdata) <- "Fusobacteria"
L2.gdata$group <- map$Gender
L2.gdata$age <- map$Age

# L2.gdata <- melt(L2.gdata)
# colnames(L2.gdata) <- c("phylum","group","age")

pdf(file = "p_Fusobacteria.age.pdf",useDingbats = F)
ggplot(L2.gdata,aes(x=age,y=Fusobacteria,color=group))+geom_point()+geom_smooth()
dev.off()



