#Libraries
suppressMessages(library(tidyverse))
suppressMessages(library(stringr))
suppressMessages(library(caret))
suppressMessages(library(doMC))
suppressMessages(library(factoextra))
suppressMessages(library(cluster))
suppressMessages(library(NbClust))
registerDoMC(cores=4)

#Loading data
feature_vectors_cleaned <- read.csv('/home/jguerra/datasets/stratosphere/balanced_feature_vector_cleaned.txt', stringsAsFactors = F, sep = '|')
feature_vectors_cleaned$class <- as.factor(feature_vectors_cleaned$class)
feature_vectors_cleaned$subclass <- feature_vectors_cleaned$class

#Create training set and testset
set.seed(212)
trainIndex <- createDataPartition(feature_vectors_cleaned$class, p=0.70, list=FALSE)
data_training <- feature_vectors_cleaned[ trainIndex,]
data_testing <- feature_vectors_cleaned[-trainIndex,]
train <- upSample(x = data_training,  y = data_training$class, yname="subclass")
training <- train[,-c(11,16)]
testing <- data_testing[,-c(11)]

cold_start_data_only_rf <- function(training.sampled,testing,settings){
  size_training <- nrow(training.sampled)
  split_size_training = size_training / 200
  testing_result = data.frame(numeric(nrow(testing)))
  
  count_random <- foreach(i=1:split_size_training) %dopar% {
    200 * i
  }
  metric <- numeric(split_size_training)
  metric_t <- numeric(split_size_training)
  for(i in c(1:44)){
    count <- 200 * i
    aux_training_set <- training.sampled[c(1:count), ]#training[sample(size_training, count), ]
    new_rfFit <- train(subclass ~ sp+wp+wnp+snp+ds+dm+dl+ss+sm+sl,
                       data = aux_training_set,
                       metric="ROC",
                       method = "rf",
                       trControl = settings)
    #Testing predict
    predsrfprobs <- predict(new_rfFit,testing,type='prob')
    predsrf <- ifelse(predsrfprobs$botnet >=0.5,'botnet','normal')
    predsrf <- as.factor(predsrf)
    cm <- confusionMatrix(predsrf,testing$subclass)
    metric[i] <- cm$byClass['F1']
    
  }
  output <- do.call(rbind, Map(data.frame, data_count=count_random, metric=metric))
  list_result <- list('output' = output, 'testing_result' = testing_result)
}

#Caret Setting
ctrl_fast <- trainControl(method="cv", 
                          repeats=2,
                          number=10, 
                          summaryFunction=twoClassSummary,
                          verboseIter=T,
                          classProbs=TRUE,
                          allowParallel = TRUE)


set.seed(225)
size_training <- nrow(training)
rf_training.sampled_5 <- training[sample(size_training, size_training), ]
rf_result_5 <- cold_start_data_only_rf(rf_training.sampled_5, testing, settings = ctrl_fast)
rf_data.result <- data.frame(rf_result_5$output$data_count)
x <- c('count_of_data')
for(i in c(1:30)){
  current_seed <- 226 + i
  set.seed(current_seed)
  rf_training.sampled_current <- training[sample(size_training, size_training), ]
  rf_result_current <- cold_start_data_only_rf(rf_training.sampled_current, testing, settings = ctrl_fast)
  
  rf_data.result <- cbind(rf_data.result,rf_result_current$output$metric)
  x[i+1] <- paste('iteration_',toString(i),sep = "")
  print(paste('Iteration', i, sep = ' '))
}
names(rf_data.result) <- x
write.table(rf_data.result,file="balanced_random_forest_30_iterations_f1_testing2.txt",sep="|", row.names = F)


