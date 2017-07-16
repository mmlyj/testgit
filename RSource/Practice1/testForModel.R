fillModelList<-function(dataToBeDeal)
{
  
  for(j in c(1:length(globalFeatureList)))
  {
    if(globalFeatureList[[j]]$switch)
    {
      dealDataFrame<-dataToBeDeal[,c("y",globalFeatureList[[j]]$var)]
      for(i in c(1:length(globalModelList)))
      {
        if(globalModelList[[i]]$switch)
        {
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
          print(c(names(globalFeatureList[j]),names(globalModelList[i]),"dealing"))
        try(tmpComparedModel<-train(y~.,data = dealDataFrame,
                                      method=names(globalModelList[i]),
                                      trControl=tempTrainCtrl,
                                      tuneGrid=tempGrid,metric=globalModelEvalMetric))
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
            print(c(names(globalFeatureList[j]),names(globalModelList[i]),"done"))
        }
         else 
         {
           print(c(names(globalFeatureList[j]),names(globalModelList[i]),"error"))
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