library(tidyverse)
library(caret)
library(doMC)
library(lsa)
registerDoMC(cores=1)


cosine_similarity <- function(vector, matrix){
  v1 <- as.numeric(vector)
  matrix <- t(as.matrix(matrix)) #Creamos la matriz y la rotamos con la funcion t()
  return(cosine(v1,matrix))
}

prediction_by_similarity <- function(train_element, prediction_element,number_element){
  numeric_train_element <- train_element[,1:10] #Getting characteristic numeric vector
  similarity_result <- cosine_similarity(prediction_element[1:10],numeric_train_element)
  train_element$similarity_result <- similarity_result
  train_element_order_by_similarity <- train_element[order(train_element$similarity_result, decreasing = T),]
  train_element_order_by_similarity <- train_element_order_by_similarity[1:number_element,]
  aux <- train_element_order_by_similarity %>% group_by(class) %>% summarise(n=n()) %>% arrange(desc(n))
  result <- aux[1,1]
  return(result)
}

#Loading data balanced dataset
feature_vectors_cleaned <- read.csv('/home/jguerra/datasets/stratosphere/balanced_feature_vector_cleaned.txt', stringsAsFactors = F, sep = '|')
feature_vectors_cleaned$class <- as.factor(feature_vectors_cleaned$class)
feature_vectors_cleaned$subclass <- feature_vectors_cleaned$class

#Creating training and testing
set.seed(212)
trainIndex <- createDataPartition(feature_vectors_cleaned$subclass, p=0.70, list=FALSE)
data_training <- feature_vectors_cleaned[ trainIndex,]
data_testing <- feature_vectors_cleaned[-trainIndex,]

#data_train = data_train %>% filter(length>5)
train <- upSample(x = data_training,  y = data_training$subclass, yname="subclass")

training <- train[,-c(11,16)]
testing <- data_testing[,-c(11)]


cs_data.result <- data.frame(110)#(rf_result_5$output$data_count) = 110 por eso esta comentado
write.table(cs_data.result,file="/home/jguerra/datasets/stratosphere/cosine_similarity_result_ctu19.txt",sep="|", row.names = F)
for(i in c(1:30)){
  current_seed <- 226 #+ i
  set.seed(current_seed)
  write(paste('ITERATION',i,sep=' '),file="/home/jguerra/datasets/stratosphere/test.txt",append = TRUE)
  size_training <- nrow(training)
  cs_training.sampled_current <- training[sample(size_training, size_training), ]
  split_size_training = size_training / 200
  metric <- numeric(split_size_training)
  for(j in 1:44){
    write(paste('--sub iteration',j,sep=' '),file="/home/jguerra/datasets/stratosphere/test.txt",append = TRUE)
    count <- 200 * j
    aux_training_set <- cs_training.sampled_current[c(1:count), ]
    result <- c()
    for(k in 1:nrow(testing)){
      prediction_vector <- testing[k,]
      prediction_vector <- as.vector(as.matrix(prediction_vector))
      output_result <- prediction_by_similarity(aux_training_set,prediction_vector,101)
      result[k] <- output_result
    }
    vector_result <- unlist(result)
    cm <- confusionMatrix(vector_result,testing$class)
    metric[j] <- cm$byClass['F1']
  }
  cs_data.result <- cbind(cs_data.result, metric)
  write.table(cs_data.result,file="/home/jguerra/datasets/stratosphere/cosine_similarity_result_ctu19.txt",sep="|", row.names = F)
}
#write.table(cs_data.result,file="/home/jguerra/datasets/stratosphere/cosine_similarity_result_ctu19.txt",sep="|", row.names = F)
