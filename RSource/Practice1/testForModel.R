fillModelList<-function(dealDataFrame)
{
  for(i in c(1:length(globalModelList)))
  {
    if(globalModelList[[i]]$switch)
    {
      for(j in c(1:length(globalFeatureList)))
      {
        if(globalFeatureList[[j]]$switch)
        {
              dealDataFrame<-dealDataFrame[,globalFeatureList[[j]]$var]
              if(is.null(globalModelControl))
              {
                tempTrainCtrl<-trainControl()
              }
              else
              {
                tempTrainCtrl<-globalModelControl##using the globalModelControl
              }
              if(is.null(globalModelList[[i]]$parametersGrid)||globalSerach=="random")
              {
                tempGrid<-NULL;
              } 
              else
              {
                tempGrid<-globalModelList[[i]]$parametersGrid;
              }
              set.seed(globalSeeds)
              tmpComparedModel<-train(y~.,data = dealDataFrame,
                                          method=names(globalModelList[i]),
                                          trControl=tempTrainCtrl,
                                          tuneGrid=tempGrid,metric=globalModelEvalMetric)
            if(!is.null(globalModelList[[i]]$Model)&&!is.null(tmpComparedModel))
            {
              if(globalModelEvalMetric=="Accuracy")
                if(max(globalModelList[[i]]$Model$results$Accuracy)<max(tmpComparedModel$results$Accuracy))
                        globalModelList[[i]]$Model<<-tmpComparedModel
              if(globalModelEvalMetric=="ROC")
                if(max(globalModelList[[i]]$Model$results$ROC)<max(tmpComparedModel$results$ROC))
                  globalModelList[[i]]$Model<<-tmpComparedModel
            }
          }
        }
    }
  }
}
 
fillModelControl<-function(methodsUsing="repeatedcv")#repeatedcv,none,cv
{
  globalModelControl<<-trainControl(method = methodsUsing)
  if(globalModelEvalMetric=="ROC") globalModelControl$summaryFunction<<-twoClassSummary()
  if(globalSerach=="random") Modelcontrol$search<<-globalSerach
}