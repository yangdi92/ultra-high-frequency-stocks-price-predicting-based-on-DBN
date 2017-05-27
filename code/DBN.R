
### 深度学习程序#####
library(darch)
# normfuc ： 用来将数据规范化的函数
 source('normfuc.R')
# 数据预处理，即将数据规范化
# 注意：此处只用了9月30号10:00 以前的数据构造输入和输出
inputs <- x[1:4725,]
outputs <-as.matrix((y[1:4725,]),1,byrow=T)
inputs1<-normfuc(inputs)
outputs1<-normfuc(outputs)

# 构造深度网络结构
darch <- newDArch(c(4,6,1),batchSize=500)
# Pre-Train the darch
darch <- preTrainDArch(darch,inputs1,maxEpoch=300)
# Prepare the layers for backpropagation training for
# backpropagation training the layer functions must be
# set to the unit functions which calculates the also
# derivatives of the function result.
layers <- getLayers(darch)
layers[[length(layers)]][[2]]<-linearUnitDerivative
for(i in length(layers)-1:1){
  layers[[i]][[2]] <- sigmoidUnitDerivative
}
setLayers(darch) <- layers
rm(layers)
# Setting and running the Fine-Tune function
setFineTuneFunction(darch) <- backpropagation
darch <- fineTuneDArch(darch,inputs1,outputs1,maxEpoch=500,isClass=F)
darch <- getExecuteFunction(darch)(darch,inputs1)
outputs3 <- getExecOutputs(darch)
yy<-outputs3[[2]]
# 将拟合的价格yy还原
y1<-yy*(max(y[1:4725,])-min(y[1:4725,]))+min(y[1:4725,])
MPAE0<-mean(abs(y1-y[1:4725])/y[1:4725])
par(cex.main=0.8)
plot(1:4725,y[1:4725],type='l',ylab='stock price',xlab='n')
lines(1:length(y1),y1,lty=2,col=2)
legend('topleft',legend=c('Measured','Fitted'),lty=1:2,col=1:2,cex=0.7,bty='n')
title('Comparing measured to fitted')
grid(12)
# 滚动预测
 pre.y<-NULL
 pre.x<-normfuc(x[2:4726,])
 for(i in 1:10){
     darch.r<-runDArch(darch, pre.x)
     outs.p <- getExecOutputs(darch.r)
     pre.y[i]<-outs.p[[2]][length(outs.p[[2]])]
     up.r<-c(pre.x[4725,-1],pre.y[i])
     pre.x<-pre.x[-1,]
     pre.x<-rbind(pre.x,up.r)
       
 }
# 将预测价格还原
 pre.y1<-pre.y*(max(x[2:4726,])-min(x[2:4726,]))+min(x[2:4726,])
# 计算 MSE,RMSE,MPAE 
 MSE<-mean((y[4726:4755,1]-pre.y1)^2)
RMSE<-sqrt(MSE)
MPAE<- mean(abs(y[4726:4755,1]-pre.y1)/y[4726:4755,1])

