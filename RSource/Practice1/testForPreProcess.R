
loadPackages<-function()##
{
 # if(!require(tidyverse)) install.packages("tidyverse")
   #library(tidyverse)elasticnet
  if(!require(ipred)) install.packages("ipred")
  library(ipred)
  if(!require(elasticnet)) install.packages("elasticnet")
  library(elasticnet)
  if(!require(xgboost)) install.packages("xgboost")
  library(xgboost)
  if(!require(gbm)) install.packages("gbm")
  library(gbm)
  if(!require(fastAdaboost)) install.packages("fastAdaboost")
  library(fastAdaboost)
  if(!require(glmnet)) install.packages("glmnet")
  library(glmnet)
  if(!require(FSelector)) install.packages("FSelector")
  library(FSelector)
  library(purrr)
  if(!require(mRMRe)) install.packages("mRMRe")
  library(mRMRe)
  if(!require(caret)) install.packages("caret") 
  library(caret)
  if(!require(mice)) install.packages("mice") 
  library(mice)
  if(!require(C50)) install.packages("C50") 
  library(C50)
  if(!require(e1071)) install.packages("e1071") 
  library(e1071)
  if(!require(kernlab)) install.packages("kernlab") 
  library(kernlab)
  if(!require(nnet)) install.packages("nnet") 
  library(nnet)
  if(!require(klaR)) install.packages("klaR") 
  library(klaR)
    library(data.table)
  library(doParallel)
  
  # library(klaR)
}
constructModelList<-function()##construct ModelList
{
  modelColNames<-c('xgbLinear','xgbTree','adaboost','gbm','glmnet','knn','nb','C5.0','nnet','svmRadial','svmLinear','rf')
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
rangeData<-function(inData) ##range 0-1 receive dataframe ,excluding y auto
{
  inData<-predict(preProcess(inData[,-1],method = 'range'),inData)
  return(inData)
}
centerAndScale<-function(inData)## center and scale  receive dataframe
{
  inData<-predict(preProcess(inData[,-1]),inData)
  return(inData)
}
#############NA deal
removeNARows<-function(inData)#delete NA row receive dataframe
{
  return(na.omit(inData))
}
removwNAcol<-function(inData)##delete NA col receive dataframe
{
  containTrue<-function(oneList)#
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
  implementsfTemp<-mice(inData,m=1,method = 'rf',seed =globalSeeds)
  inData<-complete(implementsfTemp,1)
  return(inData)
}
implementsCart<-function(inData)#Classification and regression trees (any)
{
  implementsfTemp<-mice(inData,m=1,method = 'cart',seed =globalSeeds )
  inData<-complete(implementsfTemp,1)
  return(inData)
}
implementsFastSample<-function(inData)#Random sample from the observed values (any)
{
  implementsfTemp<-mice(inData,m=1,method = 'sample',seed =globalSeeds)
  inData<-complete(implementsfTemp,1)
  return(inData)
}
###########delete  highCorrleation, if there an group of var, keep one of them;receive dataframe,
deleteHighCorrleation<-function(inData,cutoffNum=0.90)
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
