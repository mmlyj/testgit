
# 'treebagSBF'=varTmpVar<-sbfFuncs(inputDataFrame,treebagSBF),
# 'nbSBF'=varTmpVar<-sbfFuncs(inputDataFrame,nbSBF),
# 'rfSBF'=varTmpVar<-sbfFuncs(inputDataFrame,rfSBF)
# TstFun <- function()
# {
#   
#   for( i in 1:100)
#   {  
#     x1 <- (rnorm(1000000,100,30))
#     
#     i <- i + 1
#   }
#   
# }
# 
# CTstFun <- cmpfun(TstFun)
# 
# system.time(TstFun())
# system.time(CTstFun()) 
#rm(list = ls())

#write_csv(set, "selected_feature.csv")
####################################
# loadPackages();
# globalModelControl<-NULL
# globalModelList<-constructModelList();
# inputPath<-'txz1.csv'
# numOfFeatures<-55
# percentOfTrain<-0.8
#    dataTable<-data.table(read.csv(inputPath))
#  #  dataTable<-dataTable[order[runif(97)],]
#    dataTable <- dataTable[,-(57:62),with=FALSE]
source("GlobalVar.R")

dataTable$y<-factor(dataTable$y,levels=c(0,1),labels=c("No", "Yes"))  
#   tempData<-mRMR.data(data = dataTrain)
#   ft <- mRMR.classic(data = tempData, target_indices = c(1),
#                      feature_count = numOfFeatures)
#   index<-as.vector(unlist(ft@filters))
#   te<-c(1,index)
#   train_sf <- as.data.table(dataTrain[,te])
#   #test_sf <- dataTest[,te,with=FALSE] ##data.table must set this using colselect
#   test_sf <- dataTest[,te] 
# # train_sf<-as.data.table(dataTrain)
# # test_sf<-dataTest
#    train_sf$y<-factor(train_sf$y,levels=c(0,1),labels=c("No", "Yes"))
#    test_sf$y<-factor(test_sf$y,levels=c(0,1),labels=c("No","Yes"))
#   globalModelControl<-trainControl(method = "repeatedcv",number = 10,repeats = 1,classProbs =TRUE )
#globalModelControl<-trainControl(method = 'LGOCV',number = 10,allowParallel = TRUE,classProbs = TRUE)
globalModelControl<-trainControl(method = 'LGOCV',allowParallel = TRUE,classProbs = TRUE,summaryFunction = twoClassSummary)
globalModelList$rpart$switch<-FALSE  
globalModelList$pls$switch<-FALSE
#  globalModelList$knn$control<-trainControl(method = 'cv',number = 10,classProbs =TRUE,summaryFunction = twoClassSummary,allowParallel = TRUE)
#  globalModelList$knn$control<-trainControl(method = 'cv',number = 10,allowParallel = TRUE)
globalModelList$knn$parametersGrid<-expand.grid(.k=c(1:20))
globalModelList$knn$switch<-FALSE

#  globalModelList$nb$control<-trainControl(method = 'cv',number = 10,allowParallel = TRUE)
globalModelList$nb$parametersGrid<-expand.grid(fL=c(1:3),usekernel=c(TRUE,FALSE),adjust=c(1:10))
globalModelList$nb$switch<-FALSE

globalModelList$C5.0$switch<-FALSE
#  globalModelList$C5.0$control<-trainControl(method = 'cv',number = 10,allowParallel = TRUE)
globalModelList$C5.0$parametersGrid<-expand.grid(trials=c(1:20),model='tree',winnow=FALSE)

globalModelList$rf$switch<-FALSE
#  globalModelList$rf$control<-trainControl(method = 'cv',number = 10,allowParallel = TRUE)
#  globalModelList$rf$control<-
globalModelList$rf$parametersGrid<-expand.grid(mtry=c(1:10))
globalModelList$gbm$switch<-TRUE
globalModelList$adaboost$switch<-FALSE
globalModelList$glmnet$switch<-FALSE
globalModelList$svmRadial$switch<-FALSE
globalModelList$svmRadial$parametersGrid<-expand.grid(sigma=c(0.27,0.28,0.29),C=c(0.1,0.2,0.3,0.4,0.8,1,1.5,2))
#  globalModelList$svmRadial$control<-trainControl(method = 'cv',number = 10,allowParallel = TRUE)

globalModelList$nnet$switch<-FALSE
#  globalModelList$nnet$control<-trainControl(method = 'cv',number = 10,allowParallel = TRUE)

globalModelList$svmLinear$switch<-TRUE
# globalModelList$svmLinear$control<-trainControl(method = 'cv',number = 10,allowParallel = TRUE)

system.time({ 
  for(i in c(1:length(globalModelList)))
  {
    if(globalModelList[[i]]$switch==TRUE)
    {
      if(is.null(globalModelControl))
      {
        tempTrainCtrl<-trainControl()
      }
      else
      {
        tempTrainCtrl<-globalModelControl
      }
      if(is.null(globalModelList[[i]]$parametersGrid))
      {
        tempGrid<-NULL;
      } 
      else
      {
        tempGrid<-globalModelList[[i]]$parametersGrid;
      }
      set.seed(globalSeeds)
      globalModelList[[i]]$Model<-train(y~.,data = dataTable,
                                        method=names(globalModelList[i]),
                                        trControl=tempTrainCtrl,
                                        tuneGrid=tempGrid,metric="ROC",
                                        preProcess = c("center","scale")
      )
    }
  }})
resamps <- resamples(list(#knn = globalModelList$knn$Model,
  # nb = globalModelList$nb$Model,
  #C5.0 = globalModelList$C5.0$Model,
  # rf=globalModelList$rf$Model,
  svmLinear=globalModelList$svmLinear$Model,
  svmRadial=globalModelList$svmRadial$Model,
  gbm=globalModelList$gbm$Model,
  adaboost=globalModelList$adaboost$Model,
  glmnet=globalModelList$glmnet$Model
  #nnet=globalModelList$nnet$Model,
  #pls=globalModelList$pls$Model
))
trellis.par.set(caretTheme())
dotplot(resamps, metric = "Accuracy")
stopCluster(cl)
#  warnings();
###########################################  before this,all code is  for init

# 并行计算方式
#system.time({
# for(i in c(1:length(globalModelList)))
#{
#   if(globalModelList[[i]]$switch==TRUE)
#  {
#    if(is.null(globalModelList[[i]]$control))
#   {
#     tempTrainCtrl<-trainControl()
#   }
#   else
#       {
#         tempTrainCtrl<-globalModelList[[i]]$control
#       }
#       if(is.null(globalModelList[[i]]$parametersGrid))
#       {
#         tempGrid<-NULL;
#       } 
#       else
#       {
#         tempGrid<-globalModelList[[i]]$parametersGrid;
#       }
#       set.seed(567)
#       globalModelList[[i]]$Model<-train(y~.,data = train_sf,
#                                   method=names(globalModelList[i]),
#                                   trControl=tempTrainCtrl,
#                                   tuneGrid=tempGrid,
#                                   preProcess = c("center","scale")
#       )
#     }
#   }
# })
#x <- foreach(x=1:10000,.combine='rbind')
#%dopar%
# model2<-train(y~.,data = train_sf,trControl=globalModelList[[2]]$control,method=names(globalModelList[2]),tuneGrid=grid,preProcess = c("center","scale"),metric = "ROC")})


#if(FALSE)
#{
#  globalModelList$knn$switch<-TRUE;
# globalModelList$knn$control<-trainControl();

#}
#globalModelList[1]


#foreach::foreach()
#globalModelList$lasso$control<-trainControl(method = 'cv',number = 25,selectionFunction = 'best');
########################
#if(knnMethdos==TRUE)
#{
#  ctrl_knn<-trainControl(method = 'cv',number = 25,selectionFunction = 'best')
#}

####################################
# 
#  library(ggplot2)
#  library(plotROC)
# # set.seed(2529)
# # D.ex <- rbinom(200, size = 1, prob = .5)
# # M1 <- rnorm(200, mean = D.ex, sd = .65)
# # M2 <- rnorm(200, mean = D.ex, sd = 1.5)
# D.ex<-test_sf[,1]
# M1<-predict(globalModelList$svmRadial$Model,test_sf[,-1],type = "prob")
# M2<-predict(globalModelList$rf$Model,test_sf[,-1],type = "prob")
#  test <- data.frame(D = D.ex,M1 = M1$Yes, M2 = M2$Yes, stringsAsFactors = FALSE)
#  longtest<-melt_roc(test,d="y",m=c("M1","M2"))
#  basicplot <- ggplot(longtest, aes(d = D, m = M,color =name)) + geom_roc()
#  basicplot
#  library(Daim)
# # md1.nb.pred = predict(md1.nb, test)
# # md1.nb.pred <- as.data.frame(md1.nb.pred)
#  test_sf <- as.data.frame(test_sf)
# 
#  pre<-predict(globalModelList$rf$Model,test_sf[,-1],type = "prob")
#  M3 <- roc(pre$Yes, test_sf$y, "Yes")
#  plot(M3,lwd = 3,col = c("violetred2"))
fillFeatureList2<-function(dealDataFrame)#fill the globalFeatureList using
{
  # for(i in 1:length(globalFeatureList))
  # {
  #   if(globalFeatureList[[i]]$switch==TRUE)
  #   {##using try for stepping over error
  #     try(globalFeatureList[[i]]$var<<-connectFeatureListAndFunc(dealDataFrame,names(globalFeatureList[i])))
  #     #for closing the wrong way of selection
  #     if(is.null(globalFeatureList[[i]]$var)) globalFeatureList[[i]]$switch<-FALSE
  #   }
  # }
  foreach(i=1:length(globalFeatureList),.combine = combine,.errorhandling=c('remove'),.packages = c("caret","FSelector")) %dopar% (globalFeatureList[[i]]$var<<-connectFeatureListAndFunc(dealDataFrame,names(globalFeatureList[i])))
}