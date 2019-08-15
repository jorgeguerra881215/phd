suppressMessages(library(tidyverse))
suppressMessages(library(stringr))
suppressMessages(library(caret))
suppressMessages(library(doMC))
registerDoMC(cores=2)

#Main Functions
ilab_sampled_mixed <- function(data_prediction){
  #mezclo los elementos del training y les agrego una columna con la prediccion
  #separo los elementos por clases predecidas (< 0.5) para armar los clusters
  #por cada una de las clases filtro los elementos por los puertos mas importantes (25, 80, 443)
  not_uncertaining <- data_prediction %>% filter(botnet > 0.6 | botnet < 0.4)
  list_of_ports <- c('25','80', '443', '53')
  final_result <- data.frame(matrix(ncol = ncol(data_prediction),nrow = 0))
  names(final_result) <- names(data_prediction)
  for(p in list_of_ports){
    #filtrando por puerto y por clases(bot y normal)
    aux_data <- not_uncertaining %>% filter(port == p)
    aux_data <- aux_data %>% mutate(prediction_class = ifelse(botnet > 0.5, 'botnet', 'normal'))
    only_bot_aux_data <- aux_data %>% filter(prediction_class == 'botnet')
    only_normal_aux_data <- aux_data %>% filter(prediction_class == 'normal')
    mixed_data <- aux_data
    
    #Botnet space
    if(only_bot_aux_data > 50){
      #Clusterizando
      kmean_result <- kmeans(only_bot_aux_data[,1:10], 3, nstart = 25)
      #claculando las distancia hacia el centroide
      aux_result <- sqrt(rowSums(only_bot_aux_data[,1:10] - fitted(kmean_result)) ^ 2)
      #agragando los resultados de las distancias
      only_bot_aux_data <- cbind(only_bot_aux_data, 'centroid_distance'=aux_result)
      #ordenando el dataframe por las distancias
      new_data <- only_bot_aux_data[order(only_bot_aux_data$centroid_distance),]
      #seleccionando los elementos
      final_result <- rbind(final_result, new_data[1:50,])
      final_result <- rbind(final_result, new_data[(nrow(new_data)-49):nrow(new_data),])
    }
    else{
      final_result <- rbind(final_result, only_bot_aux_data)
    }
    
    
    if(only_normal_aux_data > 50){
      #Normal Space
      kmean_result <- kmeans(only_normal_aux_data[,1:10], 3, nstart = 25)
      #claculando las distancia hacia el centroide
      aux_result <- sqrt(rowSums(only_normal_aux_data[,1:10] - fitted(kmean_result)) ^ 2)
      #agragando los resultados de las distancias
      only_normal_aux_data <- cbind(only_normal_aux_data, 'centroid_distance'=aux_result)
      #ordenando el dataframe por las distancias
      new_data <- only_normal_aux_data[order(only_normal_aux_data$centroid_distance),]
      #seleccionando los elementos
      final_result <- rbind(final_result, new_data[1:50,])
      final_result <- rbind(final_result, new_data[(nrow(new_data)-49):nrow(new_data),])
    }
    else{
      final_result <- rbind(final_result, only_normal_aux_data)
    }
    
  }
  final_result
}

cold_start_data_ilab_using_lr <- function(training.sampled,testing,settings){
  size_training <- nrow(training.sampled)
  split_size_training = size_training / 200
  testing_result = data.frame(numeric(nrow(testing)))
  
  count_random <- foreach(i=1:split_size_training) %dopar% {
    200 * i
  }
  metric <- numeric(split_size_training)
  metric_t <- numeric(split_size_training)
  aux_training_set <- training.sampled[c(1:200), ]
  #metric <- foreach(i=1:split_size_training) %do% {
  for(i in c(1:44)){
    write(paste('--- sub-iteration:',i,sep=' '),file="/home/jguerra/datasets/stratosphere/test.txt",append = TRUE)
    new_lrFit <- train(subclass ~ sp+wp+wnp+snp+ds+dm+dl+ss+sm+sl,
                       data = aux_training_set,
                       metric="ROC",
                       method = "glm",
                       trControl = settings,
                       family=binomial(link='logit'))
    
    #Testing predict
    predslrprobs <- predict(new_lrFit,testing,type='prob')
    predslr <- ifelse(predslrprobs$botnet >=0.5,'botnet','normal')
    predslr <- as.factor(predslr)
    cm <- confusionMatrix(predslr,testing$subclass)
    metric[i] <- cm$byClass['F1']
    
    
    #Trainning predict
    predslrprobs_t <- predict(new_lrFit,training.sampled,type='prob')
    data_prediction <- cbind(training.sampled,predslrprobs_t)
    new_index <- ilab_sampled_mixed(data_prediction)
    aux_training_set <- rbind(aux_training_set,new_index[1:14])
    
  }
  output <- do.call(rbind, Map(data.frame, data_count=count_random, metric=metric))
  #output_t <- do.call(rbind, Map(data.frame, data_count=count_random, metric=metric_t))
  #list_result <- list('output' = output, 'output_t' = output_t, 'testing_result' = testing_result)
  list_result <- list('output' = output, 'testing_result' = testing_result)
}
############################################################################################################################

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
                          allowParallel = TRUE)

# Main
set.seed(225)
size_training <- nrow(training)
lr_training.sampled_5 <- training[sample(size_training, size_training), ]
lr_result_5 <- cold_start_data_ilab_using_lr(lr_training.sampled_5, testing, settings = ctrl_fast)

lr_data.result <- data.frame(lr_result_5$output$data_count)
#rf_data.result_t <- data.frame(rf_result_5$output$data_count)
#foreach(i=1:30) %dopar%{
#for(i in c(1:30)){
  write(paste('ITERATION',i,sep=' '),file="/home/jguerra/datasets/stratosphere/test.txt",append = TRUE)
  current_seed <- 226 + i
  set.seed(current_seed)
  #size_training <- nrow(training)
  lr_training.sampled_current <- training[sample(size_training, size_training), ]
  lr_result_current <- cold_start_data_ilab_using_lr(lr_training.sampled_current, testing, settings = ctrl_fast)
  
  lr_data.result <- cbind(lr_data.result,lr_result_current$output$metric)
  #rf_data.result_t <- cbind(rf_data.result_t,rf_result_current$output_t$metric)
#}

x <- c('count_of_data')
for(i in c(1:30)){
  x[i+1] <- paste('iteration_',toString(i),sep = "")
}
x
names(lr_data.result) <- x
#names(rf_data.result_t) <- x
lr_data.result
#rf_data.result_t 

write.table(lr_data.result,file="/home/jguerra/datasets/stratosphere/test_script_result.txt",sep="|", row.names = F)
#write.table(rf_data.result_t,file="random_forest_30_iterations_f1_training.txt",sep="|", row.names = F)
