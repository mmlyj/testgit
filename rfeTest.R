



library(caret)
library(data.table)
library(mice)
source("testForPreProcess.R")
inputPath<-'txz1.csv'
system.time({
dataTable<-data.table(read.csv(inputPath))
dataTable<-centerAndScale(dataTable)
dataTable<-implementsRf(dataTable)
dataTable$y<-factor(dataTable$y,levels=c(0,1),labels=c("No", "Yes"))
###########递归特征消除
ctrl <- rfeControl(functions = nbFuncs , 
                   method = "repeatedcv",
                   repeats = 5
                   ,allowParallel=TRUE
                   )
lmProfile <- rfe(dataTable[,-1], dataTable$y,
                sizes = 5:15,
                 rfeControl = ctrl)
})
######################遗传算法
ctrl <- gafsControl(functions =rfGA)
obj <- gafs(x = dataTable[,-1], 
            y = dataTable$y,
           # iters = 100,
            gafsControl = ctrl
            ## Now pass options to `train`
            )
######################模拟退火
sa_ctrl <- safsControl(functions = rfSA,
                       method = "repeatedcv",
                       repeats = 5,
                       improve = 50)

set.seed(10)
rf_sa <- safs(x = dataTable[,-1], y = dataTable$y,
              iters = 100,
              safsControl = sa_ctrl)
################单变量过滤
filterCtrl <- sbfControl( functions=rfSBF ,method = "repeatedcv", repeats = 5)
set.seed(10)
rfWithFilter <- sbf(x=dataTrain[,-1], y=dataTrain$y, sbfControl = filterCtrl)

dataTempSf<-dataTrain;
if(!is.factor( dataTempSf$y))
  dataTempSf$y<-factor(dataTempSf$y,levels=c(0,1),labels=c("NO", "Yes"))
set.seed(globalSeeds)
filterCtrl <- sbfControl( functions = treebagSBF,method = "cv", repeats = 5)
tmpSbfMOdel <- sbf(form=y~., data=dataTempSf,sbfControl = filterCtrl)