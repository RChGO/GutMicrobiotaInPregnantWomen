getwd()
setwd("E:\\project\\urine\\addnew\\hrid\\allgenus.forT.predgender")
rm(list=ls())

library(randomForest)
library('pROC')

L6 <- as.matrix(read.csv('file:///E:/project/urine/new/hclust-barplot/data/newL610000.txt',sep='\t',stringsAsFactors = F,head=T,row.names = 1))

map <- as.data.frame(read.csv('file:///E:/project/urine/addnew/second/newallmapping.txt',sep='\t',stringsAsFactors = F,head=T,row.names = 1))

map <- map[,2:4]

map.T1 <- map[map$Time=="T1",-2]
map.T2 <- map[map$Time=="T2",-2]
map.T3 <- map[map$Time=="T3",-2]
map.T4 <- map[map$Time=="T4",-2]

L6.T1 <- as.data.frame(t(L6[,rownames(map.T1)]))
L6.T2 <- as.data.frame(t(L6[,rownames(map.T2)]))
L6.T3 <- as.data.frame(t(L6[,rownames(map.T3)]))
L6.T4 <- as.data.frame(t(L6[,rownames(map.T4)]))

#########  huan
prof <- L6.T4
prof$Group <- map.T4$Gender


set.seed(1)
sub <- sample(1:nrow(prof),round(nrow(prof)*4/5))

train_Group <- as.factor(prof[sub,ncol(prof)])
test_Group <- as.factor(prof[-sub,ncol(prof)])
train_data <- prof[sub,-ncol(prof)] #取4/5的数据做训练集
test_data <- prof[-sub,-ncol(prof)]


#reading data


#选择ntree值，nree指定随机森林所包含的决策树数目，默认是500

set.seed(1)
ntree_fit <- randomForest(train_Group~.,data=train_data,ntree=10000)
pdf("T4.gender.ErrorRate.pdf")
plot(ntree_fit)
dev.off()

pdf("T4.gender.ImportanceFeatures.pdf")
varImpPlot(ntree_fit)
dev.off()

pred <- as.data.frame(predict(ntree_fit,train_data,type = "prob"))
a<-roc(as.character(train_Group),pred[,2])

pred <- as.data.frame(predict(ntree_fit,test_data,type = "prob"))
pred$predict <- NA
pred$predict[pred[,1]>pred[,2]] <- "F"
pred$predict[pred[,1]<pred[,2]] <- "M"
pred$real <- test_Group

write.table(pred,file="T4.pred.gender.txt",sep="\t")

b<-roc(as.character(test_Group),pred[,2])



pdf("T4.gender.ROC.pdf")
plot.roc(a, reuse.auc=TRUE,col="blue")
plot.roc(b,add=T, reuse.auc=TRUE,col="blue",partial.auc=c(1, 0.8),print.auc = T,print.auc.cex=2,print.auc.col='Black',
         axes=TRUE,legacy.axes=T,grid = c(0.2,0.2),grid.col = 'grey')
dev.off()


