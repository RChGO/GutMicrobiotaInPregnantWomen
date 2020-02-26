getwd()
setwd("E:\\project\\urine\\addnew\\thrid\\selectegenus.forsig.predgender")
rm(list=ls())

library(randomForest)
library('pROC')

L6 <- as.matrix(read.csv('file:///E:/project/urine/new/hclust-barplot/data/newL610000.txt',sep='\t',stringsAsFactors = F,head=T,row.names = 1))

map <- as.data.frame(read.csv('file:///E:/project/urine/addnew/second/newallmapping.txt',sep='\t',stringsAsFactors = F,head=T,row.names = 1))

sig <- as.data.frame(read.csv('file:///E:/project/urine/addnew/thrid/selectegenus.forsig.predgender/genus.importance.txt',sep='\t',stringsAsFactors = F,head=T,row.names = 1))
rownames(sig) <- make.names(rownames(sig))


#########  
prof <- as.data.frame(t(L6))
prof$Group <- map$Gender


set.seed(1)
sub <- sample(1:nrow(prof),round(nrow(prof)*4/5))

train_Group <- as.factor(prof[sub,ncol(prof)])
test_Group <- as.factor(prof[-sub,ncol(prof)])
train <- prof[sub,-ncol(prof)] #取4/5的数据做训练集
test <- prof[-sub,-ncol(prof)]

##挑选genus根据improtance

colnames(train) <- make.names(colnames(train))
colnames(test) <- make.names(colnames(test))


out <- matrix(0,nrow = nrow(sig),ncol = 1)
for (i in 1:nrow(sig)) {
  
  train_data <- as.data.frame(train[,rownames(sig)[1:i]])
  test_data <- as.data.frame(test[,rownames(sig)[1:i]])
  colnames(train_data) <- rownames(sig)[1:i]
  colnames(test_data) <- rownames(sig)[1:i]
  ntree_fit <- randomForest(train_Group~.,data=train_data,ntree=100)
  pred <- as.data.frame(predict(ntree_fit,test_data,type = "prob"))
  a<-roc(as.character(test_Group),pred[,2])
  out[i,1] <- a$auc
  
}

##观察后得出前40个菌  AUC最优

train_data <- as.data.frame(train[,rownames(sig)[1:40]])
test_data <- as.data.frame(test[,rownames(sig)[1:40]])
colnames(train_data) <- rownames(sig)[1:40]
colnames(test_data) <- rownames(sig)[1:40]

#reading data


#选择ntree值，nree指定随机森林所包含的决策树数目，默认是500

set.seed(3)
ntree_fit <- randomForest(train_Group~.,data=train_data,ntree=10000)
pdf("sig.gender.ErrorRate.pdf")
plot(ntree_fit)
dev.off()

pdf("sig.gender.ImportanceFeatures.pdf")
varImpPlot(ntree_fit)
dev.off()

pred <- as.data.frame(predict(ntree_fit,train_data,type = "prob"))
a<-roc(as.character(train_Group),pred[,2])

pred <- as.data.frame(predict(ntree_fit,test_data,type = "prob"))
pred$predict <- NA
pred$predict[pred[,1]>pred[,2]] <- "F"
pred$predict[pred[,1]<pred[,2]] <- "M"
pred$real <- test_Group

write.table(pred,file="sig.pred.gender.txt",sep="\t")

b<-roc(as.character(test_Group),pred[,2])

pdf("sig.gender.ROC.pdf")
plot.roc(b, reuse.auc=TRUE,col="blue",partial.auc=c(1, 0.8),print.auc = T,print.auc.cex=2,print.auc.col='Black',
         axes=TRUE,legacy.axes=T,grid = c(0.2,0.2),grid.col = 'grey')
dev.off()


