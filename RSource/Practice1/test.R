accuracy<-0;
i<-1;
for (i in 1:length(globalModelList))
{
  if(globalModelList[[i]]$switch)
    if (max(globalModelList[[i]]$Model$results$Accuracy)>accuracy)
      accuracy<-max(globalModelList[[i]]$Model$results$Accuracy)
    
}