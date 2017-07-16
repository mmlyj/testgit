source("testForPreProcess.R")
source("testForSelectFeatures.R")
source("testForModel.R")
loadPackages();
coreNum<-detectCores(logical = F)
cl<- makeCluster(coreNum)
registerDoParallel(cl)
globalSeeds=123;##important var
globalModelControl<-NULL
globalModelList<-constructModelList();
globalFeatureList<-constructFeaturesList()
globalSerach<-"grid"##random
globalModelEvalMetric<-"Accuracy" ##ROC
globalInputPath<-'txz1.csv'
globalNumOfFeatures<-40
globalPercentOfTrain<-0.8
# globalDataTable<-data.table(read.csv(globalInputPath))
# globalDataTable<-centerAndScale(globalDataTable)
# globalDataTable<-implementsRf(globalDataTable)
# for (i in 1:length(globalFeatureList))
#   globalFeatureList[[i]]$switch<-TRUE
# for (i in 1:length(globalModelList))
#   globalModelList[[i]]$switch<-TRUE
# remove(i)
# globalDataTable$y<-factor(globalDataTable$y,levels=c(0,1),labels=c("NO", "Yes"))
# # globalFeatureList$svmLinearEvlsearch$switch<-FALSE
# # globalFeatureList$adaboostsearch$switch<-FALSE
# # globalFeatureList$lasso$switch<-FALSE
# # globalFeatureList$gbm$switch<-FALSE
# # globalFeatureList$xgbLinear$switch<-FALSE
# # globalFeatureList$xgbTree$switch<-FALSE
# system.time(fillFeatureList(globalDataTable))
# # globalModelList$xgbLinear$switch<-FALSE
# # globalModelList$xgbTree$switch<-FALSE
# system.time(fillModelList(globalDataTable))
#set.seed(globalSeeds)
# indataTrain<-createDataPartition(dataTable$y,p=percentOfTrain,list = FALSE)
# dataTrain<-dataTable[indataTrain,]
# dataTest<-dataTable[-indataTrain,]
# dataTrain<-map_df(dataTrain,as.numeric)
# FeatureList$ldaSBF$switch<-TRUE
# FeatureList$treebagSBF$switch<-TRUE
# FeatureList$nbSBF$switch<-TRUE
# FeatureList$rfSBF$switch<-TRUE
# #sbfFuncs(dataTrain,treebagSBF)
# system.time(fillFeatureList(dataTable))
#system.time(fillFeatureList2(dataTrain))
