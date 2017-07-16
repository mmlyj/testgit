
constructFeaturesList<-function()##construct FeaturesList
{
  modelColNames<-c('svmLinearEvlsearch',
                   'adaboostsearch','rfeldaFuncs',
                   'rfetreebagFuncs','rferfFuncs','rfenbFuncs',
                   'ldaSBF','treebagSBF','nbSBF','rfSBF',
                   'lasso','C5.0','gbm','glmnet','xgbTree','xgbLinear')
  modelRowNames<-c('switch','var')
  ##set the default control and param
  control<-list(FALSE,NULL)
  names(control)<-modelRowNames
  control<-list(control)
  ModelList<-rep(control,length(modelColNames))
  names(ModelList)<-modelColNames
  return(ModelList)
}
hill.climbing.search.svmLinearEvl<-function(inData){
  dataTempSf<-inData;
  if(!is.factor( inData$y))
    dataTempSf$y<-factor(dataTempSf$y,levels=c(0,1),labels=c("NO", "Yes"))
  svmLinearEvl<- function(subset) {
    #k-fold cross validation
    k <- 5
    set.seed(globalSeeds)
    splits <- runif(nrow(dataTempSf))
    results = sapply(1:k, function(i) {
      test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
      train.idx <- !test.idx
      test <- dataTempSf[test.idx, , drop=FALSE]
      train <- dataTempSf[train.idx, , drop=FALSE]
      
      tree <- ksvm(as.simple.formula(subset, "y"), train,kernel="vanilladot")
      
      error.rate = sum(test$y != predict(tree, test[,subset])) / nrow(test)
      return(1 - error.rate)
    })
    
    return(mean(results))
  }
  set.seed(globalSeeds)
  subtmp <- hill.climbing.search(names(dataTempSf[,-1]), svmLinearEvl)
 # returnTmpVar<-list(tag=c("svmLinearEvlsearch"),subset=subsets)
  return(subtmp)
}
hill.climbing.search.adaboostEvl<-function(inData){
  dataTempSf<-inData;
  if(!is.factor( inData$y))
    dataTempSf$y<-factor(dataTempSf$y,levels=c(0,1),labels=c("NO", "Yes"))
  adaboostEvl<- function(subset) {
    k <- 5
    set.seed(globalSeeds)
    splits <- runif(nrow(dataTempSf))
    results = sapply(1:k, function(i) {
      test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
      train.idx <- !test.idx
      test <- dataTempSf[test.idx, , drop=FALSE]
      train <- dataTempSf[train.idx, , drop=FALSE]
      
      tree <- adaboost(as.simple.formula(subset, "y"), train,10)
      tmp<-predict(tree, test[,subset])
      ##made the two factor level the same
      tmp$class <- factor(tmp$class, levels=levels(test$y))
      error.rate = sum(test$y != tmp$class) / nrow(test)
      return(1 - error.rate)
    })
    return(mean(results))
  }
  set.seed(globalSeeds)
  subtmp<- hill.climbing.search(names(dataTempSf[,-1]), adaboostEvl)
 # returnTmpVar<-list(tag=c("adaboostsearch"),subset=subsets)
  return(subtmp)
}
rfeFuncs<-function(inData,funcs,tags)#ldaFuncs,treebagFuncs,rfFuncs,nbFuncs,https://topepo.github.io/caret/recursive-feature-elimination.html
{
  dataTempSf<-inData;
  if(!is.factor( inData$y))
    dataTempSf$y<-factor(dataTempSf$y,levels=c(0,1),labels=c("NO", "Yes"))
  set.seed(globalSeeds)
  ctrl <- rfeControl(functions = funcs , 
                     method = "cv",
                     repeats = 10
                     ,allowParallel=TRUE
                     
  )
  tmpModelrfe<- rfe(form=y~., data=dataTempSf,
                   rfeControl = ctrl)
  subtmp<-tmpModelrfe$optVariables
  #returnTmpVar<-list(tag=tags,subset=subtmp)
  return(subtmp)
}
sbfFuncs<-function(inData,funcs)##ldaSBF,treebagSBF,nbSBF,rfSBF,https://topepo.github.io/caret/feature-selection-using-univariate-filters.html
{
  dataTempSf<-inData;
  if(!is.factor( inData$y))
    dataTempSf$y<-factor(dataTempSf$y,levels=c(0,1),labels=c("NO", "Yes"))
  set.seed(globalSeeds)
  filterCtrl <- sbfControl( functions = funcs,method = "cv", repeats = 10)
  tmpSbfMOdel <- sbf(form=y~., data=dataTempSf,sbfControl = filterCtrl)
  subtmp<-tmpSbfMOdel$optVariables
 # returnTmpVar<-list(tag=tags,subset=subtmp)
  return(subtmp)
}
trainModelSf<-function(inData,funcs)#lasso,C5.0,gbm,glmnet,xgbTree,xgbLinear
{
  allowParellelHere<-TRUE
  dataTempSf<-inData;
  if(!is.factor( inData$y))
    dataTempSf$y<-factor(dataTempSf$y,levels=c(0,1),labels=c("NO", "Yes"))
  if(funcs %in% c("lasso")) 
    dataTempSf$y<-as.numeric(dataTempSf$y)
  if(funcs %in% c("xgbLinear","xgbTree")) ## xgb will parallel auto ,so turn off it here
  {
    allowParellelHere<-FALSE
  }  
  set.seed(globalSeeds)
  tmpModel<-train(y~.,data = dataTempSf,
        method=funcs,
        trControl=trainControl(method = "repeatedcv",number = 10,repeats = 5,allowParallel = allowParellelHere )    
  )
  subtmp<-predictors(tmpModel)
  #returnTmpVar<-list(tag=funcs,subset=subtmp)
  return(subtmp)
}
connectFeatureListAndFunc<-function(inputDataFrame,functionName)
{
  if(functionName %in% c('lasso','C5.0','gbm','glmnet','xgbTree','xgbLinear'))
    varTmpVar<-trainModelSf(inputDataFrame,functionName)
  if(functionName=='svmLinearEvlsearch')
    varTmpVar<-hill.climbing.search.svmLinearEvl(inputDataFrame)
  if(functionName=='adaboostsearch')
    varTmpVar<-hill.climbing.search.adaboostEvl(inputDataFrame)
  if(functionName %in% c('rfeldaFuncs','rfetreebagFuncs','rferfFuncs','rfenbFuncs'))
  {
    switch(functionName,
           'rfeldaFuncs'=varTmpVar<-rfeFuncs(inputDataFrame,ldaFuncs ),
           'rfetreebagFuncs'=varTmpVar<-rfeFuncs(inputDataFrame,treebagFuncs),
           'rferfFuncs'=varTmpVar<-rfeFuncs(inputDataFrame,rfFuncs),
           'rfenbFuncs'=varTmpVar<-rfeFuncs(inputDataFrame,nbFuncs)
           )
  }
  if(functionName %in% c('ldaSBF','treebagSBF','nbSBF','rfSBF'))
  {
    switch(functionName,
           'ldaSBF'=varTmpVar<-sbfFuncs(inputDataFrame,ldaSBF),
           'treebagSBF'=varTmpVar<-sbfFuncs(inputDataFrame,treebagSBF),
           'nbSBF'=varTmpVar<-sbfFuncs(inputDataFrame,nbSBF),
           'rfSBF'=varTmpVar<-sbfFuncs(inputDataFrame,rfSBF)
    )
  }
  return(varTmpVar)
}


fillFeatureList<-function(dealDataFrame)#fill the globalFeatureList using
{
  for(i in 1:length(globalFeatureList))
  {
    if(globalFeatureList[[i]]$switch==TRUE)
    { ##using try for stepping over error
      print(c(names(globalFeatureList[i]),"deaing"))
       try(globalFeatureList[[i]]$var<<-connectFeatureListAndFunc(dealDataFrame,names(globalFeatureList[i])))
      print(c(names(globalFeatureList[i]),"done"))
      #for closing the wrong way of selection
        if(is.null(globalFeatureList[[i]]$var)) globalFeatureList[[i]]$switch<<-FALSE
    }
  }
 
}


