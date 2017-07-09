rm(list = ls())

#write_csv(set, "selected_feature.csv")
####################################
loadPackages();
Modelcontrol<-NULL
ModelList<-constructModelList();
inputPath<-'txz1.csv'
numOfFeatures<-23
percentOfTrain<-0.8
   dataTable<-data.table(read.csv(inputPath))
 #  dataTable<-dataTable[order[runif(97)],]
   dataTable <- dataTable[,-(57:62),with=FALSE]
   set.seed(557)
   indataTrain<-createDataPartition(dataTable$y,p=percentOfTrain,list = FALSE)
   dataTrain<-dataTable[indataTrain,]
   dataTest<-dataTable[-indataTrain,]
   dataTrain<-map_df(dataTrain,as.numeric)
   
   tempData<-mRMR.data(data = dataTrain)
   ft <- mRMR.classic(data = tempData, target_indices = c(1),
                      feature_count = numOfFeatures)
   index<-as.vector(unlist(ft@filters))
   te<-c(1,index)
   train_sf <- as.data.table(dataTrain[,te])
   test_sf <- dataTest[,te,with=FALSE] ##data.table must set this using colselect
 # train_sf<-as.data.table(dataTrain)
 # test_sf<-dataTest
    train_sf$y<-factor(train_sf$y,levels=c(0,1),labels=c("No", "Yes"))
    test_sf$y<-factor(test_sf$y,levels=c(0,1),labels=c("No","Yes"))
 #   Modelcontrol<-trainControl(method = "repeatedcv",number = 10,repeats = 1,classProbs =TRUE )
   Modelcontrol<-trainControl(method = 'cv',number = 10,allowParallel = TRUE,classProbs = TRUE)
     ModelList$pls$switch<-TRUE
  #  ModelList$knn$control<-trainControl(method = 'cv',number = 10,classProbs =TRUE,summaryFunction = twoClassSummary,allowParallel = TRUE)
  #  ModelList$knn$control<-trainControl(method = 'cv',number = 10,allowParallel = TRUE)
     ModelList$knn$parametersGrid<-expand.grid(.k=c(1:20))
    ModelList$knn$switch<-TRUE
    
 #  ModelList$nb$control<-trainControl(method = 'cv',number = 10,allowParallel = TRUE)
  ModelList$nb$parametersGrid<-expand.grid(fL=c(1:3),usekernel=c(TRUE,FALSE),adjust=c(1:10))
   ModelList$nb$switch<-TRUE
  
  ModelList$C5.0$switch<-TRUE
#  ModelList$C5.0$control<-trainControl(method = 'cv',number = 10,allowParallel = TRUE)
 ModelList$C5.0$parametersGrid<-expand.grid(trials=c(1:20),model='tree',winnow=FALSE)
  
  ModelList$rf$switch<-TRUE
#  ModelList$rf$control<-trainControl(method = 'cv',number = 10,allowParallel = TRUE)
#  ModelList$rf$control<-
  ModelList$rf$parametersGrid<-expand.grid(mtry=c(1:10))
  
  ModelList$svmRadial$switch<-TRUE
  ModelList$svmRadial$parametersGrid<-expand.grid(sigma=c(0.27,0.28,0.29),C=c(0.1,0.2,0.3,0.4,0.8,1,1.5,2))
#  ModelList$svmRadial$control<-trainControl(method = 'cv',number = 10,allowParallel = TRUE)
  
  ModelList$nnet$switch<-TRUE
#  ModelList$nnet$control<-trainControl(method = 'cv',number = 10,allowParallel = TRUE)
  
  ModelList$svmLinear$switch<-TRUE
 # ModelList$svmLinear$control<-trainControl(method = 'cv',number = 10,allowParallel = TRUE)
  coreNum<-detectCores(logical = F)
  cl<- makeCluster(coreNum)
  registerDoParallel(cl)
  system.time({ 
    for(i in c(1:length(ModelList)))
    {
      if(ModelList[[i]]$switch==TRUE)
      {
        if(is.null(Modelcontrol))
        {
          tempTrainCtrl<-trainControl()
        }
        else
        {
          tempTrainCtrl<-Modelcontrol
        }
        if(is.null(ModelList[[i]]$parametersGrid))
         {
            tempGrid<-NULL;
        } 
        else
        {
            tempGrid<-ModelList[[i]]$parametersGrid;
        }
        set.seed(567)
        ModelList[[i]]$Model<-train(y~.,data = train_sf,
        method=names(ModelList[i]),
        trControl=tempTrainCtrl,
        tuneGrid=tempGrid,
        preProcess = c("center","scale")
        )
      }
    }})
 resamps <- resamples(list(knn = ModelList$knn$Model,
                           nb = ModelList$nb$Model,
                           C5.0 = ModelList$C5.0$Model,
                           rf=ModelList$rf$Model,
                           svmLinear=ModelList$svmLinear$Model,
                           svmRadial=ModelList$svmRadial$Model,
                           nnet=ModelList$nnet$Model,
                           pls=ModelList$pls$Model
                           ))
 trellis.par.set(caretTheme())
 dotplot(resamps, metric = "Accuracy")
 stopCluster(cl)
 #  warnings();
###########################################  before this,all code is  for init

# 并行计算方式
#system.time({
 # for(i in c(1:length(ModelList)))
  #{
 #   if(ModelList[[i]]$switch==TRUE)
  #  {
  #    if(is.null(ModelList[[i]]$control))
   #   {
   #     tempTrainCtrl<-trainControl()
   #   }
   #   else
#       {
#         tempTrainCtrl<-ModelList[[i]]$control
#       }
#       if(is.null(ModelList[[i]]$parametersGrid))
#       {
#         tempGrid<-NULL;
#       } 
#       else
#       {
#         tempGrid<-ModelList[[i]]$parametersGrid;
#       }
#       set.seed(567)
#       ModelList[[i]]$Model<-train(y~.,data = train_sf,
#                                   method=names(ModelList[i]),
#                                   trControl=tempTrainCtrl,
#                                   tuneGrid=tempGrid,
#                                   preProcess = c("center","scale")
#       )
#     }
#   }
# })
  #x <- foreach(x=1:10000,.combine='rbind')
 #%dopar%
 # model2<-train(y~.,data = train_sf,trControl=ModelList[[2]]$control,method=names(ModelList[2]),tuneGrid=grid,preProcess = c("center","scale"),metric = "ROC")})


#if(FALSE)
#{
#  ModelList$knn$switch<-TRUE;
 # ModelList$knn$control<-trainControl();
  
#}
#ModelList[1]


#foreach::foreach()
#ModelList$lasso$control<-trainControl(method = 'cv',number = 25,selectionFunction = 'best');
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
# M1<-predict(ModelList$svmRadial$Model,test_sf[,-1],type = "prob")
# M2<-predict(ModelList$rf$Model,test_sf[,-1],type = "prob")
#  test <- data.frame(D = D.ex,M1 = M1$Yes, M2 = M2$Yes, stringsAsFactors = FALSE)
#  longtest<-melt_roc(test,d="y",m=c("M1","M2"))
#  basicplot <- ggplot(longtest, aes(d = D, m = M,color =name)) + geom_roc()
#  basicplot
  library(Daim)
 # md1.nb.pred = predict(md1.nb, test)
 # md1.nb.pred <- as.data.frame(md1.nb.pred)
  test_sf <- as.data.frame(test_sf)

  pre<-predict(ModelList$rf$Model,test_sf[,-1],type = "prob")
  M3 <- roc(pre$Yes, test_sf$y, "Yes")
  plot(M3,lwd = 3,col = c("violetred2"))
  

