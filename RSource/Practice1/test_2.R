library(rpart)
source("GlobalVar.R")

dataTempSf<-dataTable;
dataTempSf$y<-factor(dataTempSf$y,levels=c(0,1),labels=c("NO", "Yes"))##y一定要因子化
#data(iris)
svmLinearEvl<- function(subset) {
  #k-fold cross validation
  k <- 5
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
###############
adaboostEvl<- function(subset) {
  #k-fold cross validation
  k <- 5
  splits <- runif(nrow(dataTempSf))
  results = sapply(1:k, function(i) {
    test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
    train.idx <- !test.idx
    test <- dataTempSf[test.idx, , drop=FALSE]
    train <- dataTempSf[train.idx, , drop=FALSE]
   
    tree <- adaboost(as.simple.formula(subset, "y"), train,10)
    tmp<-predict(tree, test[,subset])
   # tmp$class<-factor(tmp$class,levels=c(0,1),labels=c("NO", "Yes"))
    tmp$class <- factor(tmp$class, levels=levels(test$y))
    error.rate = sum(test$y != tmp$class) / nrow(test)
    return(1 - error.rate)
  })
   return(mean(results))
}
glmnetEvl<- function(subset) {
  #k-fold cross validation
  k <- 5
  splits <- runif(nrow(dataTempSf))
  results = sapply(1:k, function(i) {
    test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
    train.idx <- !test.idx
    test <- dataTempSf[test.idx, , drop=FALSE]
    train <- dataTempSf[train.idx, , drop=FALSE]
    
    tree <- adaboost(as.simple.formula(subset, "y"), train,10)
    tmp<-predict(tree, test[,subset])
   
    tmp$class <- factor(tmp$class, levels=levels(test$y))
    error.rate = sum(test$y != tmp$class) / nrow(test)
    return(1 - error.rate)
  })
  print(subset)
  print(mean(results))
  return(mean(results))
}

subset <- hill.climbing.search(names(dataTempSf[,-1]), glmnetEvl)

















resamps <- resamples(list(knn = globalModelList$knn$Model,
   nb = globalModelList$nb$Model,
  C5.0 = globalModelList$C5.0$Model,
   rf=globalModelList$rf$Model,
  svmLinear=globalModelList$svmLinear$Model,
  svmRadial=globalModelList$svmRadial$Model,
  gbm=globalModelList$gbm$Model,
  adaboost=globalModelList$adaboost$Model,
  glmnet=globalModelList$glmnet$Model,
  nnet=globalModelList$nnet$Model,
  pls=globalModelList$pls$Model
))
trellis.par.set(caretTheme())
dotplot(resamps, metric = "Accuracy")