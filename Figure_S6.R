setwd("E:\\project\\urine\\new\\hclust-barplot\\hcluster.count.barplot")

library(reshape2)
library(RColorBrewer)
library(ggplot2)
#p<-c()
ff = 6
#inp=c("g__Prevotella","g__Escherichia-Shigella","g__Gardnerella","g__Streptococcus","g__Veillonella","g__Lactobacillus")[ff]

D <- read.table(file = "barL6.txt",sep = "\t",row.names = 1,header = T)
map <- read.table(file = "hclust-mapping1.txt",sep = "\t",row.names = 1,header = T)
inp <- unique(map[,c(4,5)])
inp = inp[-nrow(inp),]
inp$main.genus <- as.character(inp$main.genus)

map<-map[which(map$data.cluster!=7),]
D <- as.data.frame(t(D[1:6,row.names(map)]))

m <- map$data.cluster
m[map$data.cluster!=inp$data.cluster[ff]] <-'others'

m <- factor(m,c(as.character(ff),'others'))

fig1<-ggplot()+
  geom_histogram(aes(log2(D[,inp$main.genus[ff]]),fill=m),alpha=0.5)
f=ggplot_build(fig1)
dat1 <- f$data[[1]]

fig2<-ggplot()+
  geom_density(aes(log2(D[,inp$main.genus[ff]])),position='stack')
f=ggplot_build(fig2)
dat2 <- f$data[[1]]

l=unique(dat$fill)
dat1$group[dat1$fill=='#00BFC4']='others'
dat1$group[dat1$fill=='#F8766D']=as.character(ff)

cutoff<-max(dat1$count)/max(dat2$density)*0.8

p[[inp$main.genus[ff]]]<-ggplot(dat1)+
  geom_bar(aes(x=x,y=count,fill=group),position = 'stack',stat = 'identity',alpha=0.8)+
  geom_line(data=dat2,aes(x=x,y=density*cutoff),color='steelblue')+
  labs(title='',x=paste('log2(',inp$main.genus[ff],')'),y='Count')+
  guides(fill=guide_legend(title = 'Group'))+
  scale_y_continuous(
    sec.axis = sec_axis( ~./cutoff, #对次坐标轴刻度标签的二次映射（极差范围指定真实极差即可）  
                         name = "Dentity")           #次坐标名
  )+
  theme_classic()+
  theme(
    plot.title = element_text(hjust = 0.5,size = 10),
    panel.border = element_rect(
      color = 'black',
      fill=NA
    ),
    axis.title = element_text(size=8),
    axis.text = element_text(size = 6),
    axis.line = element_blank()
  )
pdf(paste('E:/Users/Desktop/TIANC/',inp$main.genus[ff],'.pdf',sep=''),width = 4.5,height = 3)
p[[inp$main.genus[ff]]]
dev.off()
#cowplot::plot_grid(plotlist = p)
