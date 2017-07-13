source("testForPreProcess.R")
source("testForSelectFeatures.R")
source("testForModel.R")
coreNum<-detectCores(logical = F)
cl<- makeCluster(coreNum)
registerDoParallel(cl)
loadPackages();
globalSeeds=123;##important var
globalModelControl<-NULL
globalModelList<-constructModelList();
globalFeatureList<-constructFeaturesList()
globalSerach<-"grid"
globalModelEvalMetric<-"Accuracy"
globalInputPath<-'txz1.csv'
globalNumOfFeatures<-40
globalPercentOfTrain<-0.8
globalDataTable<-data.table(read.csv(globalInputPath))
globalDataTable<-centerAndScale(globalDataTable)
globalDataTable<-implementsRf(globalDataTable)
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
