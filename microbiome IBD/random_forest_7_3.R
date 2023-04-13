#test
setwd("C:/Users/dingw/OneDrive/Desktop/mydata")
rm(list=ls())
library(readxl)
library(tidyverse)
library(randomForest)
library(caret)
library(pROC)

myinput<-'g.xls'
#myinput<-'top50.g.xls.txt'
df<-read.delim(myinput,header = T,row.names = 1)
rownames(df)<-gsub('-','_',rownames(df))
rownames(df)<-make.names(rownames(df))
library(randomForest)
df1<-as.data.frame(t(df))
df1$sample<-rownames(df1)
groups<-read.delim('metadata.txt',header = T)[,1:2]
groups$sample<-gsub(' ','',groups$sample)
#groups<-read.delim('group.txt',header = T)
colnames(groups)<-c('sample','group')
groups$group<-factor(groups$group,levels = c(groups$group[!duplicated(groups$group)]))
df2<-merge(groups,df1,by='sample')

data2<-df2[,-1]
set.seed(123)



trainid <- createDataPartition(y = data2$group, p = 0.7)

traindata <- data2[trainid$Resample1, ]
colnames(traindata)[1]
testdata <- data2[-trainid$Resample1, ]

#
set.seed(13)
NCV = 7

NCV3 <- replicate(
  NCV, 
  rfcv(trainx = traindata[,-1],
       trainy = traindata$group,
       step = 0.8,
       # nodesize = 5,
       cv.fold = 5), 
  simplify=FALSE
)

error_NCV3 <- sapply(NCV3, "[[", "error.cv") %>%
  as.data.frame() %>%
  rownames_to_column("VN") %>%
  mutate(VN = as.numeric(VN)) %>%
  pivot_longer(cols = -1) %>%
  group_by(VN) %>%
  summarise(error.mean = mean(value),
            error.mean.sd = sd(value)/sqrt(NCV))

minerror <- which.min(error_NCV3$error.mean)
minerror_1se <- sum(error_NCV3[minerror, 2:3])
VN <- min(error_NCV3$VN[error_NCV3$error.mean < minerror_1se])
VN###imporance number
library(ggplot2)
ggplot(data=error_NCV3,aes(x=VN,y=error.mean))+
  geom_point()+
  geom_line(aes(group=1))+
  geom_errorbar(aes(ymin=error.mean-error.mean.sd,ymax=error.mean+error.mean.sd), width=0.3,size=0.5)+
  geom_vline(xintercept = VN,linetype=2,color='red',show.legend = T)+
  annotate(geom='text',x=VN+1,y=min(error_NCV3$error.mean),
           label=paste0('VN=',VN),color='red',size=5)+
  labs(x="Number of features",y='OOB error')+
  
  theme_bw()+
  theme(
    panel.grid = element_blank(),
    #axis.text.x  = element_text(size=10, angle=50,hjust=1,colour="black", face = "bold"),
    axis.text.x  = element_text(size=10,colour="black",angle = 0,hjust=0.5),
    axis.title.x = element_text( face = "bold"),
    axis.text.y = element_text(size=10, colour="black"),
    axis.title.y = element_text( size = 12, face = "bold"),
    legend.text=element_text(size=8))
ggsave('number_feature_oob_error.pdf',width = 7,height = 4)

#
set.seed(13)
inirf <- randomForest(
  x = traindata[, -1],
  y = traindata$group,
  mtry = 5,
  # nodesize = 5,
  importance = T
)




VX <- importance(inirf) %>%
  as.data.frame() %>%
  arrange(-MeanDecreaseAccuracy) %>%
  rownames_to_column("X") %>%
  head(n = VN) %>%
  pull(X)
VX  ###new vx


rf_importance<-as.data.frame(importance(inirf))
write.table(cbind(tax=rownames(rf_importance),rf_importance),'all.randomForest.importance.txt',row.names = F,sep = '\t',quote = F)
#
rf_importance<-rf_importance[VX,]
library(reshape2)
library(ggplot2)

rf_importance2<-rf_importance%>%select(MeanDecreaseAccuracy,MeanDecreaseGini)
rf_importance2$type<-rownames(rf_importance2)

rf_importance2_1<-rf_importance2%>%arrange(MeanDecreaseAccuracy)
rf_importance2_1$type<-factor(rf_importance2_1$type,levels = rf_importance2_1$type)
a<-ggplot(data=rf_importance2_1,aes(x=type,y=MeanDecreaseAccuracy))+
  geom_bar(stat='identity')+
  scale_y_continuous(expand    =    expansion(mult = c(0,0.1),  add=c(0,0.1)))+
  coord_flip()+
  labs(x=NULL)+
  theme_classic()


rf_importance2_2<-rf_importance2%>%arrange(MeanDecreaseGini)
rf_importance2_2$type<-factor(rf_importance2_2$type,levels = rf_importance2_2$type)
b<-ggplot(data=rf_importance2_2,aes(x=type,y=MeanDecreaseGini))+
  geom_bar(stat='identity')+
  scale_y_continuous(expand    =    expansion(mult = c(0,0.1),  add=c(0,0.1)))+
  coord_flip()+
  labs(x=NULL)+
  theme_classic()
library(patchwork)
a+b
ggsave(paste0(myinput,'.randomForest.importance.pdf'),width = 10,height = 5)


#
set.seed(13)

vxrf <- randomForest(
  x = traindata[, VX],
  y = traindata$group,
  mtry = 5,
  # nodesize = 5,
  importance = T
)
vxrf
sink('random_forest_model.txt')
vxrf
sink(NULL)

pdf('step53.randomForest.importance.pdf',width = 9,height = 7)
varImpPlot(vxrf, type = 1, main = paste0(length(VX), " variables importance"))
dev.off()
#
# 预测训练集
trainpredprob <- predict(vxrf, newdata = traindata, type = "prob")

pdf('train_predict.pdf',width = 5,height = 5)
boxplot(trainpredprob, col = c("red", "green"))
dev.off()

#
# 训练集ROC
trainroc <- roc(response = traindata$group,
                predictor = trainpredprob[, 2],
                percent = T,
                ci = T)

# 训练集ROC曲线
library(showtext)
showtext_auto(enable = T)
pdf('train_roc.pdf',width = 7,height = 4)
plot(trainroc, 
     grid = T, 
     legacy.axes = F,
     print.auc = TRUE, 
     print.auc.x = 40, 
     print.auc.y = 5)
trainci <- ci(trainroc, of="se", boot.n=500)
plot(trainci, type = "shape", col=rgb(0, 255, 0, 120, maxColorValue=255))
plot(trainci, type = "bars", col="red")
dev.off()

#
# 约登法则
bestp <- trainroc$thresholds[
  which.max(trainroc$sensitivities + trainroc$specificities - 1)
]
bestp


#
# 训练集预测分类
trainpredlab <- as.factor(
  ifelse(trainpredprob[, 2] > bestp, "case", "control")
)
# 训练集混淆矩阵
sink('train_confution_matrix.txt')
confusionMatrix(data = trainpredlab, 
                reference = traindata$group, 
                positive = "case",
                mode = "everything") 

sink(NULL)

#
# 测试集预测概率
testpredprob <- predict(vxrf, newdata = testdata, type = "prob")

pdf('test_predict.pdf',width = 5,height = 5)
boxplot(testpredprob, col = c("red", "green"))
dev.off()

#
# 测试集预测分类
testpredlab <- as.factor(
  ifelse(testpredprob[, 2] > bestp, "case", "control")
)
# 测试集混淆矩阵
sink('test_confution_matrix.txt')
confusionMatrix(data = testpredlab, # 预测类别
                reference = testdata$group, # 实际类别
                positive = "case",
                mode = "everything") 

sink(NULL)
#
# 测试集ROC
testroc <- roc(response = testdata$group,
               predictor = testpredprob[, 2],
               percent = T,
               ci = T)
# 测试集ROC曲线
pdf('test_roc.pdf',width = 7,height = 4)
plot(testroc, 
     grid = T, 
     legacy.axes = F,
     print.auc = TRUE, 
     print.auc.x = 40, 
     print.auc.y = 5)
testci <- ci(testroc, of="se", boot.n=500)
plot(testci, type = "shape", col=rgb(0, 255, 0, 120, maxColorValue=255))
plot(testci, type = "bars", col="red")
dev.off()

