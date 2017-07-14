source("GlobalVar.R")
#data_x<-dataTable[,-1]
#data_y<-dataTable[,1]
dataTable$y<-factor(dataTable$y,levels=c(0,1),labels=c("0", "1"))
y<-dataTable[,1]
x<-dataTable[,-1]

glmModel<-glmnet(x=as.matrix( dataTable[,-1]),y=dataTable[,1],family="binomial")
preGlm<-predict(object=glmModel,newx=as.matrix(dataTable[,-1]),type="class")
preGlm
adaboostModel<-adaboost(y~.,dataTable[1:70,],10)
predadaboost<-predict(adaboostModel,dataTable[71:97,])

gbmModel<-gbm(y~. ,data = dataTable,distribution='bernoulli')

pre<-predict(gbmModel,dataTable[,-1],n.trees = 100,type = "class")
pre
svmModel<-ksvm(y~.,x,kernel="vanilladot",prob.model=TRUE)
presvm<-pred(svmModel,x)
presvm<-predict(svmModel,dataTable[,-1],type = "probabilities")
presvm
svmModel<-ksvm(y~.,dataTable[,-1],kernel="vanilladot",prob.model=TRUE)
presvm<-predict(svmModel,dataTable[,-1])
presvm
rfModel<-randomForest(y~.,dataTable,importance=TRUE)
prerf<-predict(rfModel,dataTable[,-1],type="prob")
prerf

indataTrain<-createDataPartition(dataTable$y,p=percentOfTrain,list = FALSE)
dataTrain<-dataTable[indataTrain,]
dataTest<-dataTable[-indataTrain,]

rfInSF<-function(subset) {
  #k-fold cross validation
  k <- 5
  splits <- runif(nrow(dataTempSF))
  results = sapply(1:k, function(i) {
    test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
    test <- dataTempSF[test.idx,]
    train <- dataTempSF[-test.idx,]
    tree <- randomForest(, train)
    error.rate = sum(test$Species != predict(tree, test)) / nrow(test)
    return(1 - error.rate)
  })
  return(mean(results))
}
glmnetInSF<-function(subset) {
  #k-fold cross validation
  k <- 5
  splits <- runif(nrow(dataTempSF))
  results = sapply(1:k, function(i) {
    test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
    train.idx <- !test.idx
    test <- dataTempSF[test.idx, , drop=FALSE]
    train <- dataTempSF[train.idx, , drop=FALSE]
    tree <- randomForest(y~., train)
    error.rate = sum(test$y != predict(tree, test)) / nrow(test)
    return(1 - error.rate)
  })
  return(mean(results))
}
svmlinearInSF<-function(subset) {
  #k-fold cross validation
  k <- 5
  splits <- runif(nrow(iris))
  results = sapply(1:k, function(i) {
    test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
    train.idx <- !test.idx
    test <- iris[test.idx, , drop=FALSE]
    train <- iris[train.idx, , drop=FALSE]
    tree <- randomForest(y~., train)
    error.rate = sum(test$Species != predict(tree, test)) / nrow(test)
    return(1 - error.rate)
  })
  return(mean(results))
}
adaboostInSF<-function(subset) {
  #k-fold cross validation
  k <- 5
  splits <- runif(nrow(iris))
  results = sapply(1:k, function(i) {
    test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
    train.idx <- !test.idx
    test <- iris[test.idx, , drop=FALSE]
    train <- iris[train.idx, , drop=FALSE]
    tree <- randomForest(y~., train)
    error.rate = sum(test$Species != predict(tree, test)) / nrow(test)
    return(1 - error.rate)
  })
  return(mean(results))
}