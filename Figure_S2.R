setwd("E:/project/urine/final/core phyla&genera")

library(reshape2)
library(RColorBrewer)
library(ggplot2)
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
D<-read.csv('L2.prof',sep='\t',row.names = 1)
row.names(D)<-species(row.names(D))
#D<-read.csv('../Project/180323PY16SV4/2018.180323PY16SV4/2018.Mar26.180323PY16SV4/analysis/7.function/CloseRef.module.LC.prof',
#            sep='\t',row.names = 1)
#D<-D[,-3]
#D <- apply(D, 2, function(x){x/sum(x)}) 
#D<-D[,1:30]
r_o<-rowMeans(D)
D<-D[order(r_o,decreasing = T),]
dd<-as.data.frame(D[1:4,])
a<-colSums(D[1:4,])
dd['Others',]<- 1-a
D<-dd

d<-as.data.frame(t(D))
d<-t(d[do.call(order,-d),])

D<-melt(data.frame(phylum=row.names(d),d),id='phylum')
D$phylum <- factor(D$phylum,levels = row.names(d))
D$variable <- factor(D$variable,levels = colnames(d))

pdf("Figure S1 barplot.pdf",useDingbats=F,width=15)
mycolor = c(brewer.pal(12,'Paired'),"#66C2A5","#FC8D62","#E5C494",'#E5D8BD',brewer.pal(8,'Accent'),brewer.pal(9,'Set1'),
            brewer.pal(12,'Set3'),brewer.pal(8,'Dark2'),brewer.pal(8,'Set2'),
            brewer.pal(9,'Pastel1'),brewer.pal(8,'Pastel2'))
ggplot(D,aes(variable,value))+
  geom_bar(aes(fill=phylum),position = 'stack',stat = 'identity',width = 1,alpha=0.9)+
  labs(title="", x="", y="Relative Abundance")+
  scale_fill_manual(values = mycolor )+
  theme_classic()+
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
  )
dev.off()
