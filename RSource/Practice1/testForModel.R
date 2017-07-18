constructModelList<-function()##construct ModelList
{
  modelColNames<-c('xgbLinear','xgbTree','adaboost','gbm','glmnet','knn','nb','C5.0','nnet','svmRadial','svmLinear','rf')
  modelRowNames<-c('switch','parametersGrid','Model','FeatureMethods')
  ##set the default control and param
  control<-list(FALSE,NULL,NULL,NULL)
  names(control)<-modelRowNames
  control<-list(control)
  ModelList<-rep(control,length(modelColNames))
  names(ModelList)<-modelColNames
  return(ModelList)
}
fillModelList<-function(dataToBeDeal)
{
  
  for(j in c(1:length(globalFeatureList)))
  {
    if(globalFeatureList[[j]]$switch)
    {
      for(i in c(1:length(globalModelList)))
      {
        if(globalModelList[[i]]$switch)
        {
          set.seed(globalSeeds)
          
        cat(sprintf("way of feature's selection:%s,the model is building:%s\n ",names(globalFeatureList[j]),names(globalModelList[i])))
        try(tmpComparedModel<-train(x=as.data.frame(dataToBeDeal[,globalFeatureList[[j]]$var]),y= dataToBeDeal$y,
        method=names(globalModelList[i]),
        trControl=globalModelControl,
        tuneGrid=if(is.null(globalModelList[[i]]$parametersGrid)||globalSerach=="random") NULL else globalModelList[[i]]$parametersGrid,
        metric=globalModelEvalMetric,
        tuneLength = ifelse(globalModelControl$method == "none", 0,ifelse(globalSerach=="random",3,globalParamNum))#
        ))
        if(exists("tmpComparedModel")&&!is.null(tmpComparedModel))
        {
          if(!is.null(globalModelList[[i]]$Model))
          {
            if(globalModelEvalMetric=="Accuracy")
              if(max(globalModelList[[i]]$Model$results$Accuracy)<max(tmpComparedModel$results$Accuracy))
                      globalModelList[[i]]$Model<<-tmpComparedModel
            if(globalModelEvalMetric=="ROC")
              if(max(globalModelList[[i]]$Model$results$ROC)<max(tmpComparedModel$results$ROC))
                globalModelList[[i]]$Model<<-tmpComparedModel
          }
          else
          {
            globalModelList[[i]]$Model<<-tmpComparedModel
          }
          globalModelList[[i]]$FeatureMethods<<-names(globalFeatureList[j])
          cat(sprintf(" %s is done\n",names(globalModelList[i])))
        }
         else 
         {
           cat(sprintf("error of building %s using %s\n",names(globalModelList[i]),names(globalFeatureList[j])))
         }
        }
      }
    }
  }
}
 
fillModelControl<-function(methodsUsing="boot")#repeatedcv,none,cv
{
  globalModelControl<<-trainControl(method = methodsUsing)
  if(globalModelEvalMetric=="ROC") globalModelControl$summaryFunction<<-twoClassSummary()
  if(globalSerach=="random") globalModelControl$search<<-globalSerach
}