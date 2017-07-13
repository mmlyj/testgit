library(rpart)
source("GlobalVar.R")
dataTempSf<-dataTable;
#data(iris)
evaluator <- function(subset) {
  #k-fold cross validation
  k <- 2
  splits <- runif(nrow(dataTempSf))
  results = sapply(1:k, function(i) {
    test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
    train.idx <- !test.idx
    test <- dataTempSf[test.idx, , drop=FALSE]
    train <- dataTempSf[train.idx, , drop=FALSE]
    tree <- rpart(as.simple.formula(subset, "y"), train)
    error.rate = sum(test$y != predict(tree, test[,-1])) / nrow(test)
    return(1 - error.rate)
  })
  print(subset)
  print(mean(results))
  return(mean(results))
}
subset <- hill.climbing.search(names(dataTempSf[,-1]), evaluator)
f <- as.simple.formula(subset, "y")
print(f)