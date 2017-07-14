func <- function(x) {
  n = 1
  raw <- x
  while (x > 1) {
    x <- ifelse(x%%2==0,x/2,3*x+1)
    n = n + 1
  }
  return(c(raw,n))
}
x<-detectCores(logical = F)
library(foreach)
# 非并行计算方式，类似于sapply函数的功能
system.time({
x <- foreach(x=1:10000,.combine='rbind') %do% func(x)})

library(doParallel)
cl <- makeCluster(8)
registerDoParallel(cl)
# 并行计算方式
system.time({
x <- foreach(x=1:10000,.combine='rbind') %dopar% func(x)})
stopCluster(cl)

library(randomForest)

cl <- makeCluster(4)
registerDoParallel(cl)
rf <- foreach(ntree=rep(25000, 4), 
              .combine=combine,
              .packages='randomForest') %dopar%
  randomForest(Species~., data=iris, ntree=ntree)
stopCluster(cl)

library(parallel)
# 用system.time来返回计算所需时间
system.time({
  x <- 1:1e6
  cl <- makeCluster(2)  # 初始化四核心集群
  results <- parLapply(cl,x,func) # lapply的并行版本
  res.df <- do.call('rbind',results) # 整合结果
  stopCluster(cl) # 关闭集群
})
# 找到最大的步数对应的数字
res.df[which.max(res.df[,2]),1]

