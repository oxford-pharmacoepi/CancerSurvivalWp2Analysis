# Functions for analysis -----

# get the risk table
RiskSetCount <- function(timeindex, survivaltime) {
  atrisk <- NULL
  for (t in timeindex)
    atrisk <- c(atrisk, sum(survivaltime >= t))
  return(atrisk)
}

# hazard function over time extraction ----
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time,hazard,lower.ci,upper.ci))
}

# measuring time in minutes using tictoc package
toc_min <- function(tic,toc,msg="") {
  mins <- round((((toc-tic)/60)),2)
  outmsg <- paste0(mins, " minutes elapsed")
}

nice.num1<-function(x) {
  base::trimws(format(round(x,1),
                      big.mark=",", nsmall = 1, digits=1, scientific=FALSE))}

nice.num2<-function(x) {
  base::trimws(format(round(x,2),
                      big.mark=",", nsmall = 2, digits=2, scientific=FALSE))}


reformat_table_one <- function(table_one_summary){
  
  reformatted_table1 <- data.frame(x = character(),  y = character())
  
  n1 <- table_one_summary %>% dplyr::filter(variable == "Number subjects") %>% dplyr::pull(estimate)
  
  reformatted_table1 <- dplyr::bind_rows(reformatted_table1,
                                         data.frame(x = paste0("n"),
                                                    y = paste0(n1)))
  
  # variables assembled by min/max etc
  cat_var <- table_one_summary %>% dplyr::filter(estimate_type == "min") %>% dplyr::select(variable) %>% dplyr::distinct() %>% dplyr::pull(variable)
  
  for (i in (1:length(cat_var))){
    reformatted_table1 <- dplyr::bind_rows(reformatted_table1,
                                           data.frame(x = paste0(cat_var[[i]], ": median (IQR)"),
                                                      y = paste0(table_one_summary %>% dplyr::filter(variable == cat_var[[i]]) %>% dplyr::filter(estimate_type == "median") %>% dplyr::pull(estimate),
                                                                 " (",
                                                                 table_one_summary %>% dplyr::filter(variable == cat_var[[i]]) %>% dplyr::filter(estimate_type == "q25") %>% dplyr::pull(estimate),
                                                                 " - ",
                                                                 table_one_summary %>% dplyr::filter(variable == cat_var[[i]]) %>% dplyr::filter(estimate_type == "q75") %>% dplyr::pull(estimate),
                                                                 ")"))
    )
  }
  
  # sex group variables
  sex_var <- table_one_summary %>%
    dplyr::filter(variable == "Sex") %>%
    dplyr::select(variable_level) %>%
    dplyr::distinct() %>%
    dplyr::pull()
  
  for (i in (1:length(sex_var))){
    reformatted_table1 <- dplyr::bind_rows(reformatted_table1, data.frame(x = paste0("Sex: ", sex_var[[i]], " n (%)"),
                                                                          y = paste0(table_one_summary %>% dplyr::filter(variable_level == sex_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                                     " (",
                                                                                     round(as.numeric(table_one_summary %>% dplyr::filter(variable_level == sex_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                                     ")")) )
  }
  
  # age group variables
  age_var <- table_one_summary %>%
    dplyr::filter(variable == "Age group") %>%
    dplyr::select(variable_level) %>%
    dplyr::distinct() %>%
    dplyr::pull()
  
  for (i in (1:length(age_var))){
    reformatted_table1 <- dplyr::bind_rows(reformatted_table1, data.frame(x = paste0("Age Group: ", age_var[[i]], " n (%)"),
                                                                          y = paste0(table_one_summary %>% dplyr::filter(variable_level == age_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                                     " (",
                                                                                     round(as.numeric(table_one_summary %>% dplyr::filter(variable_level == age_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                                     ")")) )
  }
  
  
  #condition variables
  condition_var <- table_one_summary %>%
    dplyr::filter(stringr::str_detect(variable, 'Conditions flag -inf')) %>%
    dplyr::select(variable_level) %>%
    dplyr::distinct() %>%
    dplyr::pull(variable_level)
  
  if(length(condition_var) != 0) {
    for (i in (1:length(condition_var))){
      reformatted_table1 <- dplyr::bind_rows(reformatted_table1, data.frame(x = paste0(condition_var[[i]], " n (%)"),
                                                                            y = paste0(table_one_summary %>% dplyr::filter(variable_level == condition_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                                       " (",
                                                                                       round(as.numeric(table_one_summary %>% dplyr::filter(variable_level == condition_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                                       ")")))
    }
  }
  
  #obesity variables
  obesity_var <- table_one_summary %>%
    dplyr::filter(stringr::str_detect(variable, 'Obesity flag -inf')) %>%
    dplyr::select(variable_level) %>%
    dplyr::distinct() %>%
    dplyr::pull(variable_level)
  
  if(length(obesity_var) != 0) {
    for (i in (1:length(obesity_var))){
      reformatted_table1 <- dplyr::bind_rows(reformatted_table1, data.frame(x = paste0(obesity_var[[i]], " n (%)"),
                                                                            y = paste0(table_one_summary %>% dplyr::filter(variable_level == obesity_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                                       " (",
                                                                                       round(as.numeric(table_one_summary %>% dplyr::filter(variable_level == obesity_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                                       ")")))
    }
  }
  
  #medication variables
  medication_var <- table_one_summary %>%
    dplyr::filter(stringr::str_detect(variable, 'Medications flag -365')) %>%
    dplyr::select(variable_level) %>%
    dplyr::distinct() %>%
    dplyr::pull(variable_level)
  
  
  if(length(medication_var) != 0) {
    for (i in (1:length(medication_var))){
      reformatted_table1 <- dplyr::bind_rows(reformatted_table1, data.frame(x = paste0(medication_var[[i]], " n (%)"),
                                                                            y = paste0(table_one_summary %>% dplyr::filter(variable_level == medication_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                                       " (",
                                                                                       round(as.numeric(table_one_summary %>% dplyr::filter(variable_level == medication_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                                       ")")))
    } 
  }
  
  #cancer outcomes  
  outcome_var <- table_one_summary %>%
    dplyr::filter(stringr::str_detect(variable, 'Outcome flag 0 to 0')) %>%
    dplyr::select(variable_level) %>%
    dplyr::distinct() %>%
    dplyr::pull(variable_level)
  
  
  if(length(outcome_var) != 0) {
    for (i in (1:length(outcome_var))){
      reformatted_table1 <- dplyr::bind_rows(reformatted_table1, data.frame(x = paste0(outcome_var[[i]], " n (%)"),
                                                                            y = paste0(table_one_summary %>% dplyr::filter(variable_level == outcome_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                                       " (",
                                                                                       round(as.numeric(table_one_summary %>% dplyr::filter(variable_level == outcome_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                                       ")"))) 
      
    } 
  }
  
  
  reformatted_table1 <- reformatted_table1 %>% dplyr::distinct()
  
  ###rename columns
  colnames(reformatted_table1) <- c("Description", "Value") 
  
  return(reformatted_table1)
  
}
