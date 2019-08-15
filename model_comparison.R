suppressMessages(library(tidyverse))
suppressMessages(library(stringr))
suppressMessages(library(caret))
suppressMessages(library(doMC))
suppressMessages(library(pROC))
registerDoMC(cores=1)


randomForest <- function(training, testing,settings){
  rfFit <- train(subclass ~ sp+wp+wnp+snp+ds+dm+dl+ss+sm+sl,
                 data = training,
                 metric="ROC",
                 method = "rf",
                 trControl = settings)
  result <- prediction_and_result(rfFit, testing)
  return(result)
}

knn <- function(training, testing_data,settings){
  knnFit <- train(subclass ~ sp+wp+wnp+snp+ds+dm+dl+ss+sm+sl, 
                  data = training, 
                  method = "knn", 
                  trControl = settings, 
                  preProcess = c("center","scale"), 
                  tuneLength = 20)
  result <- prediction_and_result(knnFit, testing_data)
  return(result)
}

logisticRegression <- function(training, testing_data,settings){
  logicRFit <- train(subclass ~ sp+wp+wnp+snp+ds+dm+dl+ss+sm+sl, 
                     method='glm', 
                     trControl = settings,
                     preProcess=c('scale', 'center'), 
                     data=training, 
                     family=binomial(link='logit'))
  result <- prediction_and_result(logicRFit, testing_data)
  return(result)
}

naiveBayes <- function(training, testing_data,settings){
  naiveBayesFit <- train(subclass ~ sp+wp+wnp+snp+ds+dm+dl+ss+sm+sl, 
                         method='nb', 
                         trControl = settings,
                         preProcess=c('scale', 'center'), 
                         data=training)
  result <- prediction_and_result(naiveBayesFit, testing_data)
  return(result)
}

suportVectorMachine <- function(training, testing_data,settings){
  svmFit <- train(subclass ~ sp+wp+wnp+snp+ds+dm+dl+ss+sm+sl, 
                  method='svmLinear', 
                  trControl = settings,
                  preProcess=c('scale', 'center'), 
                  data=training, 
                  family=binomial(link='logit'))
  result <- prediction_and_result(svmFit, testing_data)
  return(result)
}

prediction_and_result <- function(model_fit,testing_data){
  predProb <- predict(model_fit, testing_data, type = 'prob')
  pred_result <- ifelse(predProb$botnet >= 0.5, 'botnet', 'normal')
  pred_result <- as.factor(pred_result)
  cm <- confusionMatrix(pred_result,testing_data$subclass)
  result <- cm$byClass
  result_roc <- roc(testing_data$subclass,predProb[,"botnet"], levels = c('normal','botnet'))
  
  return(c(result[7],result[9],result[10],result[11],'AUC'= auc(result_roc)))
}

#Loading data
feature_vectors_cleaned <- read.csv('/home/jguerra/datasets/stratosphere/balanced_feature_vector_cleaned.txt', stringsAsFactors = F, sep = '|')
feature_vectors_cleaned$class <- as.factor(feature_vectors_cleaned$class)
feature_vectors_cleaned$subclass <- feature_vectors_cleaned$class

#Creating training and testing
set.seed(212)
trainIndex <- createDataPartition(feature_vectors_cleaned$class, p=0.70, list=FALSE)
data_training <- feature_vectors_cleaned[ trainIndex,]
data_testing <- feature_vectors_cleaned[-trainIndex,]

#data_train = data_train %>% filter(length>5)
train <- upSample(x = data_training,  y = data_training$class, yname="subclass")

training <- train[,-c(11,16)]
testing <- data_testing[,-c(11)]

#model Setting
ctrl_fast <- trainControl(method="cv", 
                          repeats=2,
                          number=10, 
                          summaryFunction=twoClassSummary,
                          verboseIter=T,
                          classProbs=TRUE,
                          allowParallel = FALSE)

lr_result <- logisticRegression(training = training, testing_data = testing, settings = ctrl_fast)
knn_result <- knn(training = training, testing_data = testing, settings = ctrl_fast)
svm_result <- suportVectorMachine(training = training, testing_data = testing, settings = ctrl_fast)
nb_result <- naiveBayes(training = training, testing_data = testing, settings = ctrl_fast)
rf_result <- randomForest(training = training, testing = testing, settings = ctrl_fast)

#model_fit
#predProb <- predict(model_fit, testing, type = 'prob')
#pred_result <- ifelse(predProb$botnet >= 0.5, 'botnet', 'normal')
#pred_result <- as.factor(pred_result)
#cm <- confusionMatrix(pred_result,testing_data$subclass)
