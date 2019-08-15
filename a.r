#!/usr/bin/Rscript
updateData <- function(){
#suppressMessages(library(tidyverse))
#Load data
study_dataset <- read.csv('/home/jguerra/datasets/acm_riskID_study/ctu13_riskID_dataset.txt', stringsAsFactors = F, sep = '|')

study_result_1 <- try(read.csv('/home/jguerra/public_html/riskID_only_ordered_list/server/urank_logs.txt', stringsAsFactors = F, sep = ',', row.names = NULL, header = FALSE))
if("try-error" %in% class(study_result_1)) study_result_1 = data.frame(session_id = character(),user_name = character(), user_token = character(), date_time = character(), action = character(), connection_id = character(), app_version = character())
names(study_result_1) <- c('session_id', 'user_name', 'user_token', 'date_time', 'action','connection_id', 'app_version')

study_result_2 <- try(read.csv('/home/jguerra/public_html/riskID_only_prediction/server/urank_logs.txt', stringsAsFactors = F, sep = ',', row.names = NULL, header = FALSE))
if("try-error" %in% class(study_result_2)) study_result_2 = data.frame(session_id = character(),user_name = character(), user_token = character(), date_time = character(), action = character(), connection_id = character(), app_version = character())
names(study_result_2) <- c('session_id', 'user_name', 'user_token', 'date_time', 'action','connection_id', 'app_version')

study_result_3 <- try(read.csv('/home/jguerra/public_html/riskID_simple/server/urank_logs.txt', stringsAsFactors = F, sep = ',', row.names = NULL, header = FALSE))
if("try-error" %in% class(study_result_3)) study_result_3 = data.frame(session_id = character(),user_name = character(), user_token = character(), date_time = character(), action = character(), connection_id = character(), app_version = character())
names(study_result_3) <- c('session_id', 'user_name', 'user_token', 'date_time', 'action','connection_id', 'app_version')

study_result_4 <- try(read.csv('/home/jguerra/public_html/riskID/server/urank_logs.txt', stringsAsFactors = F, sep = ',', row.names = NULL, header = FALSE))
if("try-error" %in% class(study_result_4)) study_result_4 = data.frame(session_id = character(),user_name = character(), user_token = character(), date_time = character(), action = character(), connection_id = character(), app_version = character())
names(study_result_4) <- c('session_id', 'user_name', 'user_token', 'date_time', 'action','connection_id', 'app_version')

study_result <- rbind(study_result_1, study_result_2)
study_result <- rbind(study_result, study_result_3)
study_result <- rbind(study_result, study_result_4)

#Results
bel_botnet <- study_result %>% filter(action == 'Label Botnet')
label_normal <- study_result %>% filter(action == 'Label Normal')
label_action <- bind_rows(label_botnet, label_normal)
label_action <- label_action[,c('connection_id','action','user_name')]
names(label_action) <- c('id','action','user_name')
label_action_result = merge(x = label_action, y = study_dataset[,c(1,2)], by = "id", all.x = TRUE)
label_action_result_final = label_action_result %>% mutate(result = ifelse((action == 'Label Botnet' & class == 'botnet') | (action == 'Label Normal' & class == 'normal'),1,0))

result <- label_action_result_final %>% group_by(user_name) %>% dplyr::summarise(n=n(), correct_label = sum(result), incorrect_label = n - correct_label) %>% arrange(desc(correct_label))
#Output
write.table(result,file="live_labeling_result.txt",sep="|", row.names = F)

}

updateData()
