train_sf$y<-factor(train_sf$y,levels=c(0,1),labels=c("No", "Yes"))
test_sf$y<-factor(test_sf$y,levels=c(0,1),labels=c("No","Yes"))

ModelList$knn$control<-trainControl(method = 'cv',number = 10,classProbs =TRUE,summaryFunction = twoClassSummary,allowParallel = TRUE)
grid<-expand.grid(.k=c(5:20))
system.time(
model1<-train(y~.,data = train_sf,trControl=ModelList[[2]]$control,method=names(ModelList[2]),tuneGrid=grid,preProcess = c("center","scale"),metric = "ROC")
)