constructFeaturesList<-function()##构造模型列表
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