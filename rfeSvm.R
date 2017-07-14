svmRfe<-list(summary = defaultSummary,
               fit = function(x, y, first, last, ...){
                 library(kernlab)
                 
                remove(y)
               # View(y)
               # 
               #  names(tmp[,1])<-c("col")
                # kernlab::ksvm(x=x,y=y,kernel="vanilladot",prob.model=TRUE)
               kernlab::ksvm(y~.,data=x,kernel="vanilladot",prob.model=TRUE,...)
               # View(x[,1:2])
               },
               pred = function(object, x)
             {
                
               tmp <- predict(object, x)
               if (is.factor(object@fitted)) {
                 out <- cbind(data.frame(pred = tmp), as.data.frame(predict(object, x, type = "probabilities")))
               }
               else out <- tmp
         
               out
               },
               rank = function(object, x, y) {
                # View(x[,1:2])
                 #if(is.factor(y)) View(x[,1])
                 vimp <- filterVarImp(x[,-1],y)
                 
                 if (is.factor(y)) {
                   if (all(levels(y) %in% colnames(vimp))) {
                     avImp <- apply(vimp[, levels(y), drop = TRUE], 1, 
                                    mean)
                     vimp$Overall <- avImp
                   }
                 }
                 vimp <- vimp[order(vimp$Overall, decreasing = TRUE), , drop = FALSE]
                 vimp$var <- rownames(vimp)
                
                 vimp
               },
               selectSize = pickSizeBest,
               selectVar = pickVars)
rfRFE <-  list(summary = defaultSummary,
               fit = function(x, y, first, last, ...){
                 library(randomForest)
                 randomForest(x, y, importance = first, ...)
               },
               pred = function(object, x){
                 tmp <- predict(object, x)
                 if (is.factor(object@y)) {
                   out <- cbind(data.frame(pred = tmp), as.data.frame(predict(object, 
                                                                              x, type = "prob")))
                 }
                 else out <- tmp
                 out
               },
               rank = function(object, x, y) {
                 vimp <- filterVarImp(x,y)
                 if (is.factor(y)) {
                   if (all(levels(y) %in% colnames(vimp))) {
                     avImp <- apply(vimp[, levels(y), drop = TRUE], 1, 
                                    mean)
                     vimp$Overall <- avImp
                   }
                 }
                 vimp <- vimp[order(vimp$Overall, decreasing = TRUE), , drop = FALSE]
                 vimp$var <- rownames(vimp)
                 vimp
               },
               selectSize = pickSizeBest,
               selectVar = pickVars)
gbmSvm<-list(summary = defaultSummary,
             fit = function(x, y, first, last, ...){
               library(gbm)
               
               kernlab::ksvm(y~.,data=x,kernel="vanilladot",prob.model=TRUE,...)
               # View(x[,1:2])
             },
             pred = function(object, x)
             {
               
               tmp <- predict(object, x)
               if (is.factor(object@fitted)) {
                 out <- cbind(data.frame(pred = tmp), as.data.frame(predict(object, x, type = "probabilities")))
               }
               else out <- tmp
               
               out
             },
             rank = function(object, x, y) {
               # View(x[,1:2])
               #if(is.factor(y)) View(x[,1])
               vimp <- filterVarImp(x[,-1],y)
               
               if (is.factor(y)) {
                 if (all(levels(y) %in% colnames(vimp))) {
                   avImp <- apply(vimp[, levels(y), drop = TRUE], 1, 
                                  mean)
                   vimp$Overall <- avImp
                 }
               }
               vimp <- vimp[order(vimp$Overall, decreasing = TRUE), , drop = FALSE]
               vimp$var <- rownames(vimp)
               
               vimp
             },
             selectSize = pickSizeBest,
             selectVar = pickVars)
