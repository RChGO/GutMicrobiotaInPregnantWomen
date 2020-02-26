getwd()
setwd("E:\\project\\urine\\addnew\\thrid\\allgenus.forT.predgender")
rm(list=ls())

library(randomForest)
library('pROC')

L6 <- as.matrix(read.csv('file:///E:/project/urine/new/hclust-barplot/data/newL610000.txt',sep='\t',stringsAsFactors = F,head=T,row.names = 1))

map <- as.data.frame(read.csv('file:///E:/project/urine/addnew/second/newallmapping.txt',sep='\t',stringsAsFactors = F,head=T,row.names = 1))

map <- map[,2]


#########  huan
prof <- as.data.frame(t(L6))
prof$Group <- map


set.seed(1)
sub <- sample(1:nrow(prof),round(nrow(prof)*4/5))

train_Group <- as.factor(prof[sub,ncol(prof)])
test_Group <- as.factor(prof[-sub,ncol(prof)])
train_data <- prof[sub,-ncol(prof)] #取4/5的数据做训练集
test_data <- prof[-sub,-ncol(prof)]


#reading data
colnames(train_data ) <- make.names(colnames(train_data ))
colnames(test_data) <- make.names(colnames(test_data))

#选择ntree值，nree指定随机森林所包含的决策树数目，默认是500

set.seed(1)
ntree_fit <- randomForest(train_Group~.,data=train_data,ntree=1000)
pdf("gender.ErrorRate.pdf")
plot(ntree_fit)
dev.off()

pdf("gender.ImportanceFeatures.pdf")
varImpPlot(ntree_fit)
dev.off()

pred <- as.data.frame(predict(ntree_fit,train_data,type = "prob"))
a<-roc(as.character(train_Group),pred[,2])

pred <- as.data.frame(predict(ntree_fit,test_data,type = "prob"))
pred$predict <- NA
pred$predict[pred[,1]>pred[,2]] <- "F"
pred$predict[pred[,1]<pred[,2]] <- "M"
pred$real <- test_Group

write.table(pred,file="pred.gender.txt",sep="\t")

b<-roc(as.character(test_Group),pred[,2])



pdf("gender.ROC.pdf")
plot.roc(b, reuse.auc=TRUE,col="blue",partial.auc=c(1, 0.8),print.auc = T,print.auc.cex=2,print.auc.col='Black',
         axes=TRUE,legacy.axes=T,grid = c(0.2,0.2),grid.col = 'grey')
dev.off()


