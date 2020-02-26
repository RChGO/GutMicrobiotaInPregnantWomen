rm(list = ls())
setwd("E:\\project\\urine\\new\\hclust-barplot\\line.Gender")
library(gcookbook)
library(ggplot2)

genus.avg <- as.data.frame(read.table("file:///E:/project/urine/new/hclust-barplot/line.Gender/barL6.txt",head=T,sep="\t",row.names=1))
alpha <- as.data.frame(read.table("file:///E:/project/urine/new/hclust-barplot/line.Gender/alpha.txt",head=T,sep="\t",row.names=1))
map <- as.data.frame(read.table("file:///E:/project/urine/new/hclust-barplot/adonis/map.age.txt",head=T,sep="\t",row.names=1))

genus.avg <- as.data.frame(t(genus.avg[1:6,rownames(map)]))
genus.avg$age <- map$Age
genus.avg$gender <- map$Gender

fig1 <- ggplot(genus.avg,aes(x=age , y=g__Prevotella,color=gender))+geom_point() + geom_smooth()
fig2 <- ggplot(genus.avg,aes(x=age , y=g__Streptococcus,color=gender))+geom_point() + geom_smooth()
fig3 <- ggplot(genus.avg,aes(x=age , y=g__Lactobacillus,color=gender))+geom_point() + geom_smooth()
fig4 <- ggplot(genus.avg,aes(x=age , y=g__Gardnerella,color=gender))+geom_point() + geom_smooth()
fig5 <- ggplot(genus.avg,aes(x=age , y=`g__Escherichia-Shigella`,color=gender))+geom_point() + geom_smooth()
fig6 <- ggplot(genus.avg,aes(x=age , y=g__Veillonella,color=gender))+geom_point() + geom_smooth()

pdf("Prevotella.line.pdf",useDingbats=F)
fig1
dev.off()
pdf("Streptococcus.line.pdf",useDingbats=F)
fig2
dev.off()
pdf("Lactobacillus.line.pdf",useDingbats=F)
fig3
dev.off()
pdf("Gardnerella.line.pdf",useDingbats=F)
fig4
dev.off()
pdf("Escherichia.line.pdf",useDingbats=F)
fig5
dev.off()
pdf("Veillonella.line.pdf",useDingbats=F)
fig6
dev.off()


alpha <- as.data.frame(alpha[rownames(map),])
alpha$age <- map$Age
alpha$gender <- map$Gender

fig7 <- ggplot(alpha,aes(x=age , y=observed_otus,color=gender))+geom_point() + geom_smooth()
fig8 <- ggplot(alpha,aes(x=age , y=shannon,color=gender))+geom_point() + geom_smooth()
fig9 <- ggplot(alpha,aes(x=age , y=faith_pd,color=gender))+geom_point() + geom_smooth()
fig10 <- ggplot(alpha,aes(x=age , y=pielou_e,color=gender))+geom_point() + geom_smooth()

pdf("observed_otus.line.pdf",useDingbats=F)
fig7
dev.off()
pdf("shannon.line.pdf",useDingbats=F)
fig8
dev.off()
pdf("faith_pd.line.pdf",useDingbats=F)
fig9
dev.off()
pdf("pielou_e.line.pdf",useDingbats=F)
fig10
dev.off()




