loadPackages<-function()##
{
 # if(!require(tidyverse)) install.packages("tidyverse")
   #library(tidyverse)
  library(purrr)
  if(!require(mRMRe)) install.packages("mRMRe")
  library(mRMRe)
  if(!require(caret)) install.packages("caret") 
  library(caret)
    library(data.table)
  library(doParallel)
  library(mice)
  # library(klaR)
}
constructModelList<-function()##构造模型列表
{
  modelColNames<-c('lasso','pls','knn','nb','C5.0','lm','rpart','M5','nnet','svmRadial','svmLinear','rf')
  modelRowNames<-c('switch','parametersGrid','Model')
  ##set the default control and param
  control<-list(FALSE,NULL,NULL)
  names(control)<-modelRowNames
  control<-list(control)
  ModelList<-rep(control,length(modelColNames))
  names(ModelList)<-modelColNames
  return(ModelList)
}
#######
rangeData<-function(inData) ##range标化，归为0-1 输入为datatable
{
  inData<-predict(preProcess(inData[,-1],method = 'range'),inData)
  return(inData)
}
centerAndScale<-function(inData)##将数据中心化与标准化 输入为datatable
{
  inData<-predict(preProcess(inData[,-1]),inData)
  return(inData)
}
#############NA deal
removeNARows<-function(inData)#删除空行 输入为datatable
{
  return(na.omit(inData))
}
removwNAcol<-function(inData)##删除空列 输入为datatable
{
  containTrue<-function(oneList)#检测某一个list是否包含
  {
    for(i in seq_along(oneList))
      if(isTRUE( oneList[i]))
        return(TRUE)
    return(FALSE)
  }
  colLength<-length(inData[1,])
  NAVector<-NULL
  NAPosition<-is.na(inData)
  for(i in seq(colLength))
  {
    if(containTrue(NAPosition[,i]))
      NAVector<-c(NAVector,i)
  }
  if(is.data.table(inData))
    inData<-inData[,-NAVector,with=FALSE]
  else
    inData<-inData[,-NAVector]
  return(inData)
}
implementsRf<-function(inData)#Random forest imputations (any)
{
  implementsfTemp<-mice(inData,m=1,method = 'rf')
  inData<-complete(implementsfTemp,1)
  return(inData)
}
implementsCart<-function(inData)#Classification and regression trees (any)
{
  implementsfTemp<-mice(inData,m=1,method = 'cart')
  inData<-complete(implementsfTemp,1)
  return(inData)
}
implementsFastSample<-function(inData)#Random sample from the observed values (any)
{
  implementsfTemp<-mice(inData,m=1,method = 'sample')
  inData<-complete(implementsfTemp,1)
  return(inData)
}
###########
deleteHighCorrleation<-function(inData,cutoffNum=0.90)##去除高相关变量中的一个,
{
  destrotCor<-cor(inData)
  destrotCorUpper<-upper.tri(destrotCor)
  highCorrleation<-NULL
  matrixLength<-length(destrotCorUpper[,1])
  for(i in seq(matrixLength))
    for(j in seq(matrixLength))
      if(!destrotCorUpper[i,j]) destrotCor[i,j]=0
  for(i in seq(matrixLength))
    for(j in seq(matrixLength))
      if(abs(destrotCor[i,j])>cutoffNum) highCorrleation<-c(highCorrleation,j)
  highCorrleation<-unique(highCorrleation)
  if(is.data.table(inData))
    inData<-inData[,-highCorrleation,with=FALSE]
  else
    inData<-inData[,-highCorrleation]
  return(inData)
}
deleteSdNearZero<-function(inData)#delete SD Near 0
{
  nzv <- nearZeroVar(inData)
  inData <- inData[, -nzv]
  return(inData)
}

# library(caret)
# library(data.table)
# we_123<-data.table(read.csv(inputPath))
# 
# #dataTest<-removwNAcol(we)
# we_123<-implementsFastSample(we_123)
# 
# we_123<-deleteSdNearZero(we_123)
# we_123<-deleteHighCorrleation(we_123)

#test<-centerAndScale(we)
#  dataTable<-dataTable[order[runif(97)],]
# dataTable <- dataTable[,-(57:62),with=FALSE]
# set.seed(557)
# indataTrain<-createDataPartition(dataTable$y,p=percentOfTrain,list = FALSE)
# dataTrain<-dataTable[indataTrain,]
# dataTest<-dataTable[-indataTrain,]
# dataTrain<-map_df(dataTrain,as.numeric)