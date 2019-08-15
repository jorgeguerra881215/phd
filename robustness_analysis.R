suppressMessages(library(tidyverse))
suppressMessages(library(stringr))
suppressMessages(library(caret))
suppressMessages(library(doMC))
registerDoMC(cores=1)

#Usefull function 

generate_data_noisy <- function(dataset, porcent){
  list_aux <- sample(nrow(dataset) ,porcent)
  noisy_data_sample <- dataset[list_aux,]
  no_noisy_data_sample <- dataset[-list_aux,]
  
  noisy_data_sample_b <- noisy_data_sample %>% filter(subclass == 'botnet')
  noisy_data_sample_n <- noisy_data_sample %>% filter(subclass == 'normal')
  
  noisy_data_sample_b$subclass <- as.character(noisy_data_sample_b$subclass)
  noisy_data_sample_b$subclass[noisy_data_sample_b$subclass == 'botnet'] <- 'normal'
  noisy_data_sample_b$subclass <- as.factor(noisy_data_sample_b$subclass)
  
  noisy_data_sample_n$subclass <- as.character(noisy_data_sample_n$subclass)
  noisy_data_sample_n$subclass[noisy_data_sample_n$subclass == 'normal'] <- 'botnet'
  noisy_data_sample_n$subclass <- as.factor(noisy_data_sample_n$subclass)
  
  noisy_data <- rbind(noisy_data_sample_b, noisy_data_sample_n)
  training_noisy <- rbind(no_noisy_data_sample,noisy_data)
  training_noisy <- training_noisy[sample(nrow(training_noisy),nrow(training_noisy)),]
  return(training_noisy)
}

get_ELA_measure <- function(A0, Ax){
  RLA <- abs(A0 - Ax) / A0
  FA0 <- (1 - A0) / A0
  ELA <- RLA + FA0
  return(ELA)
}

randomForest_performace <- function(training_data, testing_data,settings){
  rfFit <- train(subclass ~ sp+wp+wnp+snp+ds+dm+dl+ss+sm+sl,
                 data = training_data,
                 metric="ROC",
                 method = "rf",
                 trControl = settings)
  predsrfprobs <- predict(rfFit,testing_data,type='prob')
  predsrf <- ifelse(predsrfprobs$botnet >=0.5,'botnet','normal')
  predsrf <- as.factor(predsrf)
  cm <- confusionMatrix(predsrf,testing_data$subclass)
  result <- cm$byClass
  return(result)
}

#######################################################################################################

#Loading data
feature_vectors_cleaned <- read.csv('/home/jguerra/datasets/stratosphere/balanced_feature_vector_cleaned.txt', stringsAsFactors = F, sep = '|')
feature_vectors_cleaned$class <- as.factor(feature_vectors_cleaned$class)
feature_vectors_cleaned$subclass <- feature_vectors_cleaned$class
#feature_vectors_cleaned

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


#Main
set.seed(331)
partitions <- seq(2,90,2)

balanced_accuracy <- randomForest_performace(training,testing,ctrl_fast)
total <- nrow(training)
result_rf_accuracy <- data.frame('count_of_data' = partitions)
result_ela_measure <- data.frame('count_of_data' = partitions)
write.table(result_rf_accuracy,file="/home/jguerra/datasets/stratosphere/balanced_test_result_rf_accuracy_2.txt",sep="|", row.names = F)
write.table(result_ela_measure,file="/home/jguerra/datasets/stratosphere/balanced_test_result_ela_measure_2.txt",sep="|", row.names = F)
for (j in c(1:30)){
  write(paste('ITERATION',j,sep=' '),file="/home/jguerra/datasets/stratosphere/test2.txt",append = TRUE)
  rf_measures_result <- c()
  ela_measures_result <- c()
  #Calculando RF performance con variaciones en el porciento de ruido en training
  for(i in partitions){
    write(paste('--- sub-iteration:',i,sep=' '),file="/home/jguerra/datasets/stratosphere/test2.txt",append = TRUE)
    porcent <- (i*total) / 100
    training_noisy <- generate_data_noisy(training,porcent)
    balanced_accuracy_aux <- randomForest_performace(training_noisy, testing, ctrl_fast)
    rf_measures_result[i] <- balanced_accuracy_aux
    #Calculando ELA measure
    balanced_accuracy_noisy <- balanced_accuracy_aux
    ela_measures_result[i] <- get_ELA_measure(A0 = balanced_accuracy['Balanced Accuracy'], Ax = balanced_accuracy_noisy)
  }
  
  #Quedandome con las posiciones pares que son las que tienen los valores
  ela_measures_result <- ela_measures_result[partitions]
  rf_measures_result <- rf_measures_result[partitions]
  
  colum_name <- paste('iteration_',toString(j),sep = "")
  result_rf_accuracy <- cbind(result_rf_accuracy, colum_name = rf_measures_result)
  result_ela_measure <- cbind(result_ela_measure, colum_name = ela_measures_result)
  
  write.table(result_rf_accuracy,file="/home/jguerra/datasets/stratosphere/balanced_test_result_rf_accuracy_2.txt",sep="|", row.names = F)
  write.table(result_ela_measure,file="/home/jguerra/datasets/stratosphere/balanced_test_result_ela_measure_2.txt",sep="|", row.names = F)
}

#result_ela_measure
#result_rf_accuracy