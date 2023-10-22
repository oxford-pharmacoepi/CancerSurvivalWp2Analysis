library(here)
library(dplyr)
library(tidyr)
library(stringr)

# printing numbers with 1 decimal place and commas 
nice.num<-function(x) {
  trimws(format(round(x,1),
                big.mark=",", nsmall = 1, digits=1, scientific=FALSE))}
# printing numbers with 2 decimal place and commas 
nice.num2<-function(x) {
  trimws(format(round(x,2),
                big.mark=",", nsmall = 2, digits=2, scientific=FALSE))}
# printing numbers with 3 decimal place and commas 
nice.num3<-function(x) {
  trimws(format(round(x,3),
                big.mark=",", nsmall = 3, digits=3, scientific=FALSE))}
# for counts- without decimal place
nice.num.count<-function(x) {
  trimws(format(x,
                big.mark=",", nsmall = 0, digits=1, scientific=FALSE))}



#### Load and extract data -----
#data
study_results <- readRDS(here("data","Results.rds"))
# extract each element from the list to put results into r environment
list2env(study_results,globalenv())

# filter results for just survival results
survival_km <- survival_estimates %>% 
  filter(Method == "Kaplan-Meier")
surv_prob_km <- survival_probabilities %>% 
  filter(Method == "Kaplan-Meier")
med_surv_km <- median_survival_results %>% 
  filter(Method == "Kaplan-Meier")

# tableone_summary_breast <- tableone_summary %>% 
#   filter(group_level == "Breastcancer" & strata_name == "Overall")
# 
# reformat_table_one <- function(result_imm){
#   
#   reformatted_table1 <- data.frame(x = NA, y= NA)
#   n1 <- table_one_imm %>% dplyr::filter(variable == "Number subjects") %>% dplyr::pull(estimate)
#   
#   # variables assembled by min/max etc
#   cat_var <- table_one_imm %>% dplyr::filter(estimate_type == "min") %>% dplyr::select(variable) %>% dplyr::distinct() %>% dplyr::pull(variable)
#   
#   for (i in (1:length(cat_var))){
#     reformatted_table1 <- rbind(reformatted_table1, 
#                                 data.frame(x = paste0(cat_var[[i]], ", median (IQR)"), 
#                                            y = paste0(table_one_imm %>% dplyr::filter(variable == cat_var[[i]]) %>% dplyr::filter(estimate_type == "median") %>% dplyr::pull(estimate),
#                                                       " (",
#                                                       table_one_imm %>% dplyr::filter(variable == cat_var[[i]]) %>% dplyr::filter(estimate_type == "q25") %>% dplyr::pull(estimate),
#                                                       "-",
#                                                       table_one_imm %>% dplyr::filter(variable == cat_var[[i]]) %>% dplyr::filter(estimate_type == "q75") %>% dplyr::pull(estimate),
#                                                       ")"))
#     )
#   }
#   
#   # age group variables
#   age_var <- table_one_imm %>% 
#     dplyr::filter(variable == "Age group") %>% 
#     dplyr::select(variable_level) %>%
#     dplyr::distinct() %>%
#     dplyr::pull()
#   
#   for (i in (1:length(age_var))){
#     reformatted_table1 <- rbind(reformatted_table1, data.frame(x = paste0("Age Group, ", age_var[[i]], ", n(%)"),
#                                                                y = paste0(table_one_imm %>% dplyr::filter(variable_level == age_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
#                                                                           " (",
#                                                                           round(as.numeric(table_one_imm %>% dplyr::filter(variable_level == age_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
#                                                                           ")")) )
#   }
#   
#   
#   #condition variables
#   condition_var <- table_one_imm %>% 
#     dplyr::filter(variable == "Conditions flag -inf to 0 days") %>%
#     dplyr::filter(!variable_level == "Malignant neoplastic disease") %>%
#     dplyr::select(variable_level) %>%
#     dplyr::distinct() %>%
#     dplyr::pull(variable_level)
#   
#   for (i in (1:length(condition_var))){
#     reformatted_table1 <- rbind(reformatted_table1, data.frame(x = paste0(condition_var[[i]], ", n(%)"),
#                                                                y = paste0(table_one_imm %>% dplyr::filter(variable_level == condition_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
#                                                                           " (",
#                                                                           round(as.numeric(table_one_imm %>% dplyr::filter(variable_level == condition_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
#                                                                           ")")))
#   }
#   
#   #medication variables
#   medication_var <- table_one_imm %>% 
#     dplyr::filter(stringr::str_detect(variable, 'Medications flag -365 to 0 days')) %>%
#     #dplyr::filter(!variable == "Ref medications antineoplastic agents flag m365 to 0") %>%
#     dplyr::select(variable_level) %>%
#     dplyr::distinct() %>%
#     dplyr::pull(variable_level)
#   
#   for (i in (1:length(medication_var))){
#     reformatted_table1 <- rbind(reformatted_table1, data.frame(x = paste0(medication_var[[i]], ", n(%)"),
#                                                                y = paste0(table_one_imm %>% dplyr::filter(variable_level == medication_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
#                                                                           " (",
#                                                                           round(as.numeric(table_one_imm %>% dplyr::filter(variable_level == medication_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
#                                                                           ")")))
#   }
#   reformatted_table1 <- reformatted_table1 %>% dplyr::distinct()
#   
#   ###rename columns
#   colnames(reformatted_table1) <- c(
#     "Characteristic",
#     paste0("Overall (n = ", as.integer(n1), ")")
#   )
#   return(reformatted_table1)
# }
# 
# testy <- reformat_table_one(tableone_summary_breast)
