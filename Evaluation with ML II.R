rm(list=ls())

#Age, BMI, BSA, Height, Weight, Preferred Term, AE related (Y/N) and SAE related (Y/N)

#read data
d1<-read.csv("pretreated.csv", header=TRUE, sep=",", stringsAsFactors = FALSE) 
dim(d1) #1633 x 1315

excl_col<-c("COLITIS","HYPOTHYROIDISM","PNEUMONIA","HEPATITIS")
d1<-d1[,setdiff(colnames(d1),excl_col)]
dim(d1) #1633 x 1311

d2<-na.omit(d1[,c(1:9,1310)])
dim(d2) #1161 x10 
d3<-merge(d2, d1[,10:1311], by="Subject_ID")
dim(d3)  #1161 x 1311
table(d3$immflag) #1014/147

d3[,c(3:6,1311)]<-lapply(d3[,c(3:6,1311)],factor)
d3[,c(11:1310)]<-lapply(d3[,c(11:1310)],function(a){
  ifelse(is.na(a),0,ifelse(a=="GRADE 1",1,ifelse(a=="GRADE 2",2,ifelse(a=="GRADE 3",3,ifelse(a=="GRADE 4",4,5)))))
})

d3<-d3[,apply(d3[,c(11:1310)],2,sum)!=0]  #keep only the AE is recorded
dim(d3) #1161 x 1169
d3<-d3[apply(d3[,c(11:1168)],1,sum)!=0,]
dim(d3) #1153 x 1169
#write.csv(d3, file="pretreated2.csv",row.names = FALSE)
d4<-d3[,-1] #remove ID
colnames(d4)[10:1167]<-paste("A",seq(10:1167),sep="")

###################################################################
#     CARET
###################################################################
library(caret)

#This model here includes Recursive Feature Elimination first
set.seed(222)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
system.time(results <- rfe(d4[,-1168], d4[,1168], sizes=c(5, 10, 50, 100, 200), rfeControl=control))

nzv <- nearZeroVar(d4[,-1168])
filteredDescr <- d4[, -nzv]
dim(filteredDescr)

#Append the important ones from RFE
confirmed<-predictors(results)
filteredDescr<-d4[,c(confirmed,setdiff(colnames(filteredDescr),confirmed))]
dim(filteredDescr)

#Append the important ones from Boruta
#confirmed<-c("A70", "A182", "A218", "A292", "A412", "A428", "A494", "A703", "A915", "A923", "A1047", "A1144")
#filteredDescr<-cbind(filteredDescr,d4[,confirmed])

set.seed(222)
testidx <- createDataPartition(filteredDescr$immflag, p = .3)[[1]]
training <- filteredDescr[-testidx,]
testing <-filteredDescr[testidx,] 

## Determine the predictor names
predictors <- names(training)[names(training) != c("immflag")]
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
ctrl <- trainControl(method = "cv", 
                     number = 10, 
                     classProbs = TRUE,
                     returnResamp = "all",
                     summaryFunction = fiveStats,
                     verboseIter = TRUE)

#grid <- expand.grid(.alpha=c(0,1),.lambda=seq(0,0.05,by=0.01)) #glmnet
#grid <- expand.grid(.size=c(1,5,10,15,20),.decay=c(0.01,0.001,0.1)) #nnet
#grid <- expand.grid(.C=10^seq(4,-2,length=5),.sigma=10^seq(1,-4,length=5))#SVM
#grid <- expand.grid(.interaction.depth = seq(1, 10, by = 2), 
#                    .n.trees = seq(20, 120, by = 20), 
#                    .shrinkage = c(0.01, 0.01, 0.1), .n.minobsinnode=10) #gbm
#grid <- expand.grid(k = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) 

set.seed(222)
up_train <- upSample(x = training[,predictors],
                     y = training$immflag,
                     ## keep the class variable name the same:
                     yname = "immflag")
table(up_train$immflag)

set.seed(222)
down_train<- downSample(x = training[,predictors],
                        y = training$immflag,
                        ## keep the class variable name the same:
                        yname = "immflag")
table(down_train$immflag)

library(DMwR)
set.seed(222)
smote_train <- SMOTE(immflag ~ ., data  = training)
table(smote_train$immflag)

###############################################################################
set.seed(222)
orig_fit <- train(data.matrix(training[,predictors]), training$immflag,
                  #method = 'glmnet',
                  #method = 'nnet',
                  method = 'rpart',
                  #method = "svmRadial",
                  #method = 'nb',
                  #method ='gbm',
                  #method = 'rf',
                  #method ="knn",
                  metric = "ROC",
                  #tuneGrid = grid, 
                  trControl = ctrl)
varImp(orig_fit)
#summary(orig_fit) #gbm
plot(orig_fit, metric="ROC")
confusionMatrix(predict(orig_fit, newdata=data.matrix(testing[, predictors])),testing$immflag)

set.seed(222)
down_outside <- train(data.matrix(down_train[,predictors]), down_train$immflag,
                      #method='glmnet',
                      #method='nnet',
                      method = 'rpart',
                      #method='nb',
                      #method='rf',
                      #method = "svmRadial",
                      #method = 'knn',
                      #method='gbm',
                      metric = "ROC",
                      #tuneGrid = grid,
                      trControl = ctrl)

varImp(down_outside)
plot(down_outside, metric="ROC")
confusionMatrix(predict(down_outside, newdata=data.matrix(testing[, predictors])),testing$immflag)

set.seed(222)
up_outside <- train(data.matrix(up_train[,predictors]), up_train$immflag,
                    #method = 'glmnet',
                    #method = 'nnet',
                    method = 'rpart',
                    #method = 'svmRadial',
                    #method = 'nb',
                    #method = 'knn',
                    #method = 'rf',
                    #method = 'gbm',
                    metric = "ROC",
                    #tuneGrid = grid, #glmnet and nnet use only
                    trControl = ctrl)

varImp(up_outside)
plot(up_outside, metric="ROC")
confusionMatrix(predict(up_outside, newdata=data.matrix(testing[, predictors])),testing$immflag)

set.seed(222)
smote_outside <- train(data.matrix(smote_train[,predictors]), smote_train$immflag,
                       #method='glmnet',
                       #method='nnet',
                       method = 'rpart',
                       #method ='svmRadial',
                       #method='nb',
                       #method='knn',
                       #method='gbm',
                       #method='rf',
                       metric = "ROC",
                       #tuneGrid = grid,
                       trControl = ctrl)

varImp(smote_outside)
plot(smote_outside, metric="ROC")
confusionMatrix(predict(smote_outside, newdata=data.matrix(testing[, predictors])),testing$immflag)

outside_models <- list(original = orig_fit,
                       down = down_outside,
                       up = up_outside,
                       SMOTE = smote_outside
)

outside_resampling <- resamples(outside_models)
summary(outside_resampling, metric = "ROC")

#Real test check compared to resampling results
test_roc <- function(model, data) {
  library(pROC)
  roc_obj <- roc(data$immflag,
                 predict(model, data.matrix(data[,predictors]), type = "prob")[,1])
  ci(roc_obj)
}

outside_test <- lapply(outside_models, test_roc, data = testing)
outside_test <- lapply(outside_test, as.vector)
outside_test <- do.call("rbind", outside_test)
colnames(outside_test) <- c("lower", "ROC", "upper")
outside_test <- as.data.frame(outside_test)
outside_test


# Plot ROC curve for prediction of the independent validation set
roc.curve <- plot.roc(testing$immflag, predict(orig_fit, data.matrix(testing[,predictors]), type = "prob")[,1], percent=TRUE, ci=TRUE)
roc2<-roc(testing$immflag, predict(orig_fit, data.matrix(testing[,predictors]), type = "prob")[,1])
plot(roc2, legacy.axes = TRUE)
plot(roc2, print.thres="best", print.thres.best.method="closest.topleft")
result.coords<-coords(roc2, "best", best.method = "closest.topleft",ret=c("threshold", "accuracy"))
coords(roc2, 0.5, ret="accuracy") #Find accuracy if using 50% probability as cut-off

plot(roc2, print.thres = c(.5), type = "S",
     print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)",
     print.thres.cex = .8, 
     legacy.axes = TRUE)

# Make prediction using the best top-left cutoff.
result.predicted.label <- factor(
  ifelse(predict(orig_fit, data.matrix(testing[,predictors]), type = "prob")[,1] > result.coords[1], 
         "Yes", "No"))
table(result.predicted.label,testing$immflag)

