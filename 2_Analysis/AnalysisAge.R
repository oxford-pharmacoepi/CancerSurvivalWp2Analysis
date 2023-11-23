#################################################
# AGE ANALYSIS
#################################################

# km survival, risk table, median survival, hazard over time from the observed data for each cancer ----
tic("KM analysis for age population")
info(logger, 'KM analysis for age population START')

# capture output in list
observedkm_age <- list()
observedmedianKM_age <- list()
observedhazotKM_age <- list()
observedrisktableKM_age <- list()

# loop to carry out for each cancer
for(j in 1:nrow(cancer_cohorts)) {
  
  #subset the data by cancer type
  data <- Pop %>%
    dplyr::filter(cohort_definition_id == cancer_cohorts$cohort_definition_id[j])
  
  #age levels
  agelevels <- data %>%
    dplyr::group_by(age_gr) %>% dplyr::summarise(count = n())
  
    #carry out km estimate
    observedkm_age[[j]] <- survival::survfit(Surv(time_years, status) ~ age_gr, data=data) %>%
      tidy() %>%
      dplyr::rename(Age = strata) %>%
      dplyr::mutate(Method = "Kaplan-Meier", Cancer = cancer_cohorts$cohort_name[j], 
             Age = stringr::str_replace(Age, "age_gr=", ""),
             Sex = "Both") 
    
    # reduce the number of rows due to size of km outputs (doesnt effect results)
    # 18 to 39
    if(nrow(observedkm_age[[j]][observedkm_age[[j]]$Age == "18 to 39",]) > 4000){
      observedkm_1839 <- observedkm_age[[j]][observedkm_age[[j]]$Age == "18 to 39",] %>%
        dplyr::filter(row_number() %% 5 == 1)
    } else if (nrow(observedkm_age[[j]][observedkm_age[[j]]$Age == "18 to 39",]) > 2000 &
               nrow(observedkm_age[[j]][observedkm_age[[j]]$Age == "18 to 39",]) < 4000){
      observedkm_1839 <- observedkm_age[[j]][observedkm_age[[j]]$Age == "18 to 39",] %>%
        dplyr::filter(row_number() %% 3 == 1)
    } else if (nrow(observedkm_age[[j]][observedkm_age[[j]]$Age == "18 to 39",]) > 1000 &
               nrow(observedkm_age[[j]][observedkm_age[[j]]$Age == "18 to 39",]) < 2000){
      observedkm_1839 <- observedkm_age[[j]][observedkm_age[[j]]$Age == "18 to 39",] %>%
        dplyr::filter(row_number() %% 2 == 1)
    } else {
      observedkm_1839 <- observedkm_age[[j]][observedkm_age[[j]]$Age == "18 to 39",]
    }

    # 40 to 49
    if(nrow(observedkm_age[[j]][observedkm_age[[j]]$Age == "40 to 49",]) > 4000){
      observedkm_4049 <- observedkm_age[[j]][observedkm_age[[j]]$Age == "40 to 49",] %>%
        dplyr::filter(row_number() %% 5 == 1)
    } else if (nrow(observedkm_age[[j]][observedkm_age[[j]]$Age == "40 to 49",]) > 2000 &
               nrow(observedkm_age[[j]][observedkm_age[[j]]$Age == "40 to 49",]) < 4000){
      observedkm_4049 <- observedkm_age[[j]][observedkm_age[[j]]$Age == "40 to 49",] %>%
        dplyr::filter(row_number() %% 3 == 1)
    } else if (nrow(observedkm_age[[j]][observedkm_age[[j]]$Age == "40 to 49",]) > 1000 &
               nrow(observedkm_age[[j]][observedkm_age[[j]]$Age == "40 to 49",]) < 2000){
      observedkm_4049 <- observedkm_age[[j]][observedkm_age[[j]]$Age == "40 to 49",] %>%
        dplyr::filter(row_number() %% 2 == 1)
    } else {
      observedkm_4049 <- observedkm_age[[j]][observedkm_age[[j]]$Age == "40 to 49",]
    }   
    
    
    # 50 to 59
    if(nrow(observedkm_age[[j]][observedkm_age[[j]]$Age == "50 to 59",]) > 4000){
      observedkm_5059 <- observedkm_age[[j]][observedkm_age[[j]]$Age == "50 to 59",] %>%
        dplyr::filter(row_number() %% 5 == 1)
    } else if (nrow(observedkm_age[[j]][observedkm_age[[j]]$Age == "50 to 59",]) > 2000 &
               nrow(observedkm_age[[j]][observedkm_age[[j]]$Age == "50 to 59",]) < 4000){
      observedkm_5059 <- observedkm_age[[j]][observedkm_age[[j]]$Age == "50 to 59",] %>%
        dplyr::filter(row_number() %% 3 == 1)
    } else if (nrow(observedkm_age[[j]][observedkm_age[[j]]$Age == "50 to 59",]) > 1000 &
               nrow(observedkm_age[[j]][observedkm_age[[j]]$Age == "50 to 59",]) < 2000){
      observedkm_5059 <- observedkm_age[[j]][observedkm_age[[j]]$Age == "50 to 59",] %>%
        dplyr::filter(row_number() %% 2 == 1)
    } else {
      observedkm_5059 <- observedkm_age[[j]][observedkm_age[[j]]$Age == "50 to 59",]
    }   
    
    # 60 to 69
    if(nrow(observedkm_age[[j]][observedkm_age[[j]]$Age == "60 to 69",]) > 4000){
      observedkm_6069 <- observedkm_age[[j]][observedkm_age[[j]]$Age == "60 to 69",] %>%
        dplyr::filter(row_number() %% 5 == 1)
    } else if (nrow(observedkm_age[[j]][observedkm_age[[j]]$Age == "60 to 69",]) > 2000 &
               nrow(observedkm_age[[j]][observedkm_age[[j]]$Age == "60 to 69",]) < 4000){
      observedkm_6069 <- observedkm_age[[j]][observedkm_age[[j]]$Age == "60 to 69",] %>%
        dplyr::filter(row_number() %% 3 == 1)
    } else if (nrow(observedkm_age[[j]][observedkm_age[[j]]$Age == "60 to 69",]) > 1000 &
               nrow(observedkm_age[[j]][observedkm_age[[j]]$Age == "60 to 69",]) < 2000){
      observedkm_6069 <- observedkm_age[[j]][observedkm_age[[j]]$Age == "60 to 69",] %>%
        dplyr::filter(row_number() %% 2 == 1)
    } else {
      observedkm_6069 <- observedkm_age[[j]][observedkm_age[[j]]$Age == "60 to 69",]
    }    
    
    # 70 to 79
    if(nrow(observedkm_age[[j]][observedkm_age[[j]]$Age == "70 to 79",]) > 4000){
      observedkm_7079 <- observedkm_age[[j]][observedkm_age[[j]]$Age == "70 to 79",] %>%
        dplyr::filter(row_number() %% 5 == 1)
    } else if (nrow(observedkm_age[[j]][observedkm_age[[j]]$Age == "70 to 79",]) > 2000 &
               nrow(observedkm_age[[j]][observedkm_age[[j]]$Age == "70 to 79",]) < 4000){
      observedkm_7079 <- observedkm_age[[j]][observedkm_age[[j]]$Age == "70 to 79",] %>%
        dplyr::filter(row_number() %% 3 == 1)
    } else if (nrow(observedkm_age[[j]][observedkm_age[[j]]$Age == "70 to 79",]) > 1000 &
               nrow(observedkm_age[[j]][observedkm_age[[j]]$Age == "70 to 79",]) < 2000){
      observedkm_7079 <- observedkm_age[[j]][observedkm_age[[j]]$Age == "70 to 79",] %>%
        dplyr::filter(row_number() %% 2 == 1)
    } else {
      observedkm_7079 <- observedkm_age[[j]][observedkm_age[[j]]$Age == "70 to 79",]
    }       
    
    # 80+
    if(nrow(observedkm_age[[j]][observedkm_age[[j]]$Age == "80 +",]) > 4000){
      observedkm_80 <- observedkm_age[[j]][observedkm_age[[j]]$Age == "80 +",] %>%
        dplyr::filter(row_number() %% 5 == 1)
    } else if (nrow(observedkm_age[[j]][observedkm_age[[j]]$Age == "80 +",]) > 2000 &
               nrow(observedkm_age[[j]][observedkm_age[[j]]$Age == "80 +",]) < 4000){
      observedkm_80 <- observedkm_age[[j]][observedkm_age[[j]]$Age == "80 +",] %>%
        dplyr::filter(row_number() %% 3 == 1)
    } else if (nrow(observedkm_age[[j]][observedkm_age[[j]]$Age == "80 +",]) > 1000 &
               nrow(observedkm_age[[j]][observedkm_age[[j]]$Age == "80 +",]) < 2000){
      observedkm_80 <- observedkm_age[[j]][observedkm_age[[j]]$Age == "80 +",] %>%
        dplyr::filter(row_number() %% 2 == 1)
    } else {
      observedkm_80 <- observedkm_age[[j]][observedkm_age[[j]]$Age == "80 +",]
    }   

    observedkm_age[[j]] <- bind_rows(
      observedkm_1839,
      observedkm_4049,
      observedkm_5059,
      observedkm_6069,
      observedkm_7079,
      observedkm_80)    
        
    print(paste0("KM for observed data ", Sys.time()," for ", cancer_cohorts$cohort_name[j] , " completed")) 
    
    # get risk table for specific times for each age group then combine again ---
    grid <- seq(0,floor(max(data$time_years)),by=0.5) # get the number of years
    grid <-  grid[(stringr::str_detect(grid, "[1-9]\\.5", negate = TRUE )) & (stringr::str_detect(grid, "10.5", negate = TRUE )) &
                    (stringr::str_detect(grid, "20.5", negate = TRUE )) & (stringr::str_detect(grid, "30.5", negate = TRUE ))] # remove all the half years apart from the first half year
    sprob <- survival::survfit(Surv(time_years, status) ~ age_gr, data=data) %>% 
      summary(times = grid, extend = TRUE)
    cols <- lapply(c(2:16) , function(x) sprob[x])
    
    kmage <- do.call(data.frame, cols) %>%
      dplyr::select(c(n.risk, n.event, n.censor, strata)) %>% 
      dplyr::mutate(strata = stringr::str_replace(strata, "age_gr=", ""))
    
    
    # risk tables for different age groups
    kmagegp <- list()
    
    for(k in 1: length(table(kmage$strata))) {
      
      kmagegp[[k]] <- kmage %>%
        dplyr::filter(strata == names(table(kmage$strata)[k])) %>%
        dplyr::select(!c(strata)) %>%
        t() %>%
        as_tibble() %>%
        `colnames<-`(grid) %>%
        dplyr::mutate(Method = "Kaplan-Meier",
               Cancer = cancer_cohorts$cohort_name[j],
               Age = names(table(kmage$strata)[k]) ,
               Sex = "Both" ,
               details = c("n.risk", "n.event", "n.censor")) %>%
        dplyr::relocate(details) 
      
    }
    
    # bind results for age groups
    observedrisktableKM_age[[j]] <- bind_rows(kmagegp)
    
    print(paste0("Extract risk table ", Sys.time()," for ",cancer_cohorts$cohort_name[j], " completed"))
    
    
    # KM median survival ---
    modelKM <- survival::survfit(Surv(time_years, status) ~ age_gr, data=data) %>%
      summary()
    
    medianKM <- modelKM$table %>%
      as.data.frame() %>%
      tibble::rownames_to_column() %>%  
      dplyr::rename(Age = rowname, n = records, se =`se(rmean)`) %>% 
      dplyr::mutate(rmean = round(rmean, 4),
             median = round(median, 4),
            Age = stringr::str_replace(Age, "age_gr=", ""),
            `0.95LCL` = round(`0.95LCL`, 4),
            `0.95UCL` = round(`0.95UCL`, 4),
            "rmean in years (SE)"= ifelse(!is.na(rmean),
                                          paste0(paste0(nice.num2(rmean)), " (",
                                                 paste0(nice.num2(se)), ")"),
                                          NA),
            "Median Survival in Years (95% CI)"= ifelse(!is.na(median),
                                                        paste0(paste0(nice.num2(median)), " (",
                                                               paste0(nice.num2(`0.95LCL`)),"-",
                                                               paste0(nice.num2(`0.95UCL`)), ")"),
                                                        NA)) %>% 
      dplyr::select(-c(`0.95LCL`,`0.95UCL`, n.max, n.start)) %>% 
      dplyr::mutate(n  = replace(n, n ==  0 , NA),
             events = replace(events, events ==  0 , NA)) %>%
      dplyr::mutate(n  = replace(n, n <=  10 , "<10"),
             events  = replace(events, events <=  10 , "<10"))  %>%
      dplyr::mutate(n  = replace_na(n, "0"),
             events  = replace_na(events, "0")) %>% 
      dplyr::mutate(n = as.character(n),
             events = as.character(events))

    print(paste0("Median survival from KM from observed data ", Sys.time()," for ", cancer_cohorts$cohort_name[j] , " completed"))
    
    # survival probabilities ----
    surprobsKM <- do.call(data.frame, cols) %>%
      dplyr::select(c(time, surv, lower, upper, strata)) %>% 
      dplyr::rename(Age = strata) %>% 
      dplyr::filter(time == 1 | time == 5 | time == 10 ) %>% 
      dplyr::mutate(Age = stringr::str_replace(Age, "age_gr=", "") ) %>% 
      dplyr::mutate(surv = round((surv*100),4),
             lower = round((lower*100),4),
             upper = round((upper*100),4),
             "Survival Rate % (95% CI)"= ifelse(!is.na(surv),
                                                paste0(paste0(nice.num1(surv)), " (",
                                                       paste0(nice.num1(lower)),"-",
                                                       paste0(nice.num1(upper)), ")"),
                                                NA)) %>% 
      dplyr::select(-c(lower, upper)) %>% 
      tidyr::pivot_wider(names_from = time, 
                  values_from = c(`Survival Rate % (95% CI)`, surv),
                  names_prefix = " year ",
                  names_sep = "")
    
    # Extract rmean at 10 years
    model_rm <- survival::survfit(Surv(time_years, status) ~ age_gr, data=data)
    rmean10 <- survival:::survmean(model_rm, rmean=c(10))$matrix %>% 
      as.data.frame() %>% 
      tibble::rownames_to_column() %>%  
      dplyr::select(rmean, `se(rmean)`, rowname) %>% 
      dplyr::rename(rmean10yr = rmean, se10yr =`se(rmean)`, Age = rowname) %>% 
      dplyr::mutate(Age = stringr::str_replace(Age, "age_gr=", "") ,
             "rmean 10yrs in years (SE)"= ifelse(!is.na(rmean10yr),
                                                 paste0(paste0(nice.num2(rmean10yr)), " (",
                                                        paste0(nice.num2(se10yr)), ")"),
                                                 NA))
    
    observedmedianKM_age[[j]] <- dplyr::inner_join(medianKM, rmean10, by = "Age") %>% 
    dplyr::inner_join(surprobsKM, by = "Age")
    observedmedianKM_age[[j]] <- observedmedianKM_age[[j]] %>% 
      dplyr::mutate(Method = "Kaplan-Meier", 
             Cancer = cancer_cohorts$cohort_name[j] ,
             Sex = "Both" )
    
    # Extract rmean at 5 years
    rmean5 <- survival:::survmean(model_rm, rmean=c(5))$matrix %>% 
      as.data.frame() %>% 
      tibble::rownames_to_column() %>%  
      dplyr::select(rmean, `se(rmean)`, rowname) %>% 
      dplyr::rename(rmean5yr = rmean, se5yr =`se(rmean)`, Age = rowname) %>% 
      dplyr::mutate(Age = stringr::str_replace(Age, "age_gr=", "") ,
                    "rmean 5yrs in years (SE)"= ifelse(!is.na(rmean5yr),
                                                        paste0(paste0(nice.num2(rmean5yr)), " (",
                                                               paste0(nice.num2(se5yr)), ")"),
                                                        NA))
    
    observedmedianKM_age[[j]] <- dplyr::inner_join(medianKM, rmean5, by = "Age") %>% 
    dplyr::inner_join(rmean10, by = "Age") %>% 
    dplyr::inner_join(surprobsKM, by = "Age")
    
    observedmedianKM_age[[j]] <- observedmedianKM_age[[j]] %>% 
      dplyr::mutate(Method = "Kaplan-Meier", 
                    Cancer = cancer_cohorts$cohort_name[j] ,
                    Sex = "Both" )
    
    rm(surprobsKM,medianKM,rmean10, rmean5,model_rm,modelKM)
    
    print(paste0("Survival for 1, 5 and 10 years from observed data ", Sys.time()," for ", cancer_cohorts$cohort_name[j] , " completed"))
    
    # hazard over time ---
    # paper https://arxiv.org/pdf/1509.03253.pdf states bshazard good package
    # this can fall over with small sample numbers therefore trycatch is in place which tries to perform it and if errors
    # removes age group sequentially.
    print(paste0("Trying Hazard over time results for all age groups ", Sys.time()," for ", cancer_cohorts$cohort_name[j] ))
    
    tryCatch(
      {
        modelhot <- dplyr::group_by(data, age_gr) %>% 
          do(as.data.frame(bshazard(Surv(time_years, status)~1, data=., verbose=FALSE))) %>% 
          ungroup %>%
          dplyr::mutate(Method = "Kaplan-Meier", Cancer = cancer_cohorts$cohort_name[j], Sex = "Both") %>% 
          dplyr::rename(Age = age_gr) },
      error = function(e) {
        cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , "trying again removing small sample numbers", "\n")
        info(logger, paste0(" First model not carried out due to low sample numbers for ",cancer_cohorts$cohort_name[j], " start removing age groups and repeat", e))},
      warning = function(w){info(logger, paste0(cancer_cohorts$cohort_name[j], ": ", w))}
    )
    
    # if model is successful with no removal of age groups if not remove 18-29 age group
    if (exists("modelhot") == TRUE) {
      
      observedhazotKM_age[[j]] <- modelhot
      
      #print out progress               
      print(paste0("Hazard over time results ", Sys.time()," for ",cancer_cohorts$cohort_name[j], " age strat completed"))
      # if model falls over remove first age group 
      
    } else {
      
      print(paste0("Trying Hazard over time results again removing 18-39 year old age group ", Sys.time()," for ", cancer_cohorts$cohort_name[j]))
      
      data <- data %>% 
        dplyr::filter(age_gr != "18 to 39") 
    
    tryCatch(
      {
        modelhot <- dplyr::group_by(data, age_gr) %>% 
          do(as.data.frame(bshazard(Surv(time_years, status)~1, data=., verbose=FALSE))) %>% 
          ungroup %>%
          dplyr::mutate(Method = "Kaplan-Meier", Cancer = cancer_cohorts$cohort_name[j], Sex = "Both") %>% 
          dplyr::rename(Age = age_gr)
        
      },
      error = function(e) {
        cat("An error occurred: ")
        cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , " trying again removing small sample numbers", "\n")
        info(logger, paste0(" after removal of 18-39 age group:  Second attempt not carried out due to low sample numbers for ", cancer_cohorts$cohort_name[j], e))} ,
      warning = function(w){info(logger, paste0(cancer_cohorts$cohort_name[j], ": ", w))}
    )
      
    }
    
    # if model successful after removal of 18-39 age group if not remove 40-49 age group
    if (exists("modelhot") == TRUE) {
      
      observedhazotKM_age[[j]] <- modelhot 

      #print out progress               
      print(paste0("Hazard over time results ", Sys.time()," for ",cancer_cohorts$cohort_name[j], " age strat completed"))
      
      # if model falls over remove second age group
    } else {
      
      print(paste0("Trying Hazard over time results again removing 40-49 year old age group ", Sys.time()," for ",cancer_cohorts$cohort_name[j]))
      
      data <- data %>% 
        dplyr::filter(age_gr != "18 to 39") %>% 
        dplyr::filter(age_gr != "40 to 49")
      
    tryCatch(
      {
        modelhot <- dplyr::group_by(data,age_gr) %>% 
          do(as.data.frame(bshazard(Surv(time_years, status)~1, data=., verbose=FALSE))) %>% 
          ungroup %>%
          dplyr::mutate(Method = "Kaplan-Meier", Cancer = cancer_cohorts$cohort_name[j], Sex = "Both") %>% 
          dplyr::rename(Age = age_gr)
        
      },
      error = function(e) {
        cat("An error occurred: ")
        cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , " trying again removing small sample numbers", "\n")
        info(logger, paste0(" after removal of 40-49 age group: attempt not carried out due to low sample numbers for ",cancer_cohorts$cohort_name[j], e))},
      warning = function(w){info(logger, paste0(cancer_cohorts$cohort_name[j], ": ", w))}
    ) 
      }
    
    
    if (exists("modelhot") == TRUE) {
      
      observedhazotKM_age[[j]] <- modelhot
      
      #print out progress               
      print(paste0("Hazard over time results ", Sys.time()," for ",cancer_cohorts$cohort_name[j], " age strat completed"))
      
    }  else  {
      
      print(paste0("Trying Hazard over time results again removing 50-59 year old age group ", Sys.time()," for ",cancer_cohorts$cohort_name[j]))
      
      data <- data %>% 
        dplyr::filter(age_gr != "18 to 39") %>% 
        dplyr::filter(age_gr != "40 to 49") %>% 
        dplyr::filter(age_gr != "50 to 59")
      
    tryCatch(
      {
        modelhot <- dplyr::group_by(data,age_gr) %>% 
          do(as.data.frame(bshazard(Surv(time_years, status)~1, data=., verbose=FALSE))) %>% 
          ungroup %>%
          dplyr::mutate(Method = "Kaplan-Meier", Cancer = cancer_cohorts$cohort_name[j], Sex = "Both") %>% 
          dplyr::rename(Age = age_gr)
        
      },
      error = function(e) {
        cat("An error occurred: ")
        cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , " trying again removing small sample numbers", "\n")
        info(logger, paste0("after removal of 50-59 age group: attempt not carried out due to low sample numbers for ",cancer_cohorts$cohort_name[j], e))} ,
      warning = function(w){info(logger, paste0(cancer_cohorts$cohort_name[j], ": ", w))}
    ) }
    
    
    if (exists("modelhot") == TRUE) {
      
      observedhazotKM_age[[j]] <- modelhot
      
      #print out progress               
      print(paste0("Hazard over time results ", Sys.time()," for ",cancer_cohorts$cohort_name[j], " age strat completed"))
      
    }  else  {
      
      print(paste0("Trying Hazard over time results again removing 60-69 year old age group ", Sys.time()," for ",cancer_cohorts$cohort_name[j]))
      
      data <- data %>% 
        dplyr::filter(age_gr != "18 to 39") %>% 
        dplyr::filter(age_gr != "40 to 49") %>% 
        dplyr::filter(age_gr != "50 to 59") %>% 
        dplyr::filter(age_gr != "60 to 69")
    
    
    tryCatch(
      {
        modelhot <- dplyr::group_by(data,age_gr) %>% 
          do(as.data.frame(bshazard(Surv(time_years, status)~1, data=., verbose=FALSE))) %>% 
          ungroup %>%
          dplyr::mutate(Method = "Kaplan-Meier", Cancer = cancer_cohorts$cohort_name[j], Sex = "Both") %>% 
          dplyr::rename(Age = age_gr)
      },
      error = function(e) {
        cat("An error occurred: ")
        cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , " trying again removing small sample numbers", "\n")
        info(logger, paste0("after removal of 60-69 age group:  attempt not carried out due to low sample numbers for ",cancer_cohorts$cohort_name[j], e)) },
      warning = function(w){info(logger, paste0(cancer_cohorts$cohort_name[j], ": ", w))}
    )
    }    
    
    if (exists("modelhot") == TRUE) {
      
      observedhazotKM_age[[j]] <- modelhot 
      
      #print out progress               
      print(paste0("Hazard over time results ", Sys.time()," for ", cancer_cohorts$cohort_name[j] , " age strat completed"))
      
    }  else  {
      
      print(paste0("Trying Hazard over time results again removing 70-79 year old age group ", Sys.time()," for ", cancer_cohorts$cohort_name[j]))
      
      data <- data %>% 
        dplyr::filter(age_gr != "18 to 39") %>% 
        dplyr::filter(age_gr != "40 to 49") %>% 
        dplyr::filter(age_gr != "50 to 59") %>% 
        dplyr::filter(age_gr != "60 to 69") %>% 
        dplyr::filter(age_gr != "70 to 79")
    
    tryCatch(
      {
        modelhot <- dplyr::group_by(data,age_gr) %>% 
          do(as.data.frame(bshazard(Surv(time_years, status)~1, data=., verbose=FALSE))) %>% 
          ungroup %>%
          dplyr::mutate(Method = "Kaplan-Meier", Cancer = cancer_cohorts$cohort_name[j], Sex = "Both") %>% 
          dplyr::rename(Age = age_gr)
      },
      error = function(e) {
        cat("An error occurred: ")
        cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , " trying again removing small sample numbers", "\n")
        info(logger, paste0("after removal of 70-79 age group: attempt not carried out due to low sample numbers for ", cancer_cohorts$cohort_name[j], e))},
      warning = function(w){info(logger, paste0(cancer_cohorts$cohort_name[j], ": ", w))}
    ) 
      }
    
    if (exists("modelhot") == TRUE) {
      
      observedhazotKM_age[[j]] <- modelhot
      
      #print out progress               
      print(paste0("Hazard over time results ", Sys.time()," for ",cancer_cohorts$cohort_name[j], " age strat completed"))

    } else {
      
      print(paste0("hazard over time age stratification not carried due to low sample numbers in all age groups ", Sys.time()," for ",cancer_cohorts$cohort_name[j]))
      info(logger, paste0("hazard over time age stratification not carried due to low sample numbers in all age groups ", Sys.time()," for ",cancer_cohorts$cohort_name[j]))
      
    }
    
    if (exists("modelhot") == TRUE) {

    # 18 to 39
    if(nrow(observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "18 to 39",]) > 4000){
      observedhazotkm_1839 <- observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "18 to 39",] %>%
        dplyr::filter(row_number() %% 5 == 1)
    } else if (nrow(observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "18 to 39",]) > 2000 &
               nrow(observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "18 to 39",]) < 4000){
      observedhazotkm_1839 <- observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "18 to 39",] %>%
        dplyr::filter(row_number() %% 3 == 1)
    } else if (nrow(observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "18 to 39",]) > 2000 &
               nrow(observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "18 to 39",]) < 1000){
      observedhazotkm_1839 <- observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "18 to 39",] %>%
        dplyr::filter(row_number() %% 2 == 1)
    } else {
      observedhazotkm_1839 <- observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "18 to 39",]
    }
    
    
    # 40 to 49
    if(nrow(observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "40 to 49",]) > 4000){
      observedhazotkm_4049 <- observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "40 to 49",] %>%
        dplyr::filter(row_number() %% 5 == 1)
    } else if (nrow(observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "40 to 49",]) > 2000 &
               nrow(observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "40 to 49",]) < 4000){
      observedhazotkm_4049 <- observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "40 to 49",] %>%
        dplyr::filter(row_number() %% 3 == 1)
    } else if (nrow(observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "40 to 49",]) > 1000 &
               nrow(observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "40 to 49",]) < 2000){
      observedhazotkm_4049 <- observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "40 to 49",] %>%
        dplyr::filter(row_number() %% 2 == 1)
    } else {
      observedhazotkm_4049 <- observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "40 to 49",]
    }   
    
    
    # 50 to 59
    if(nrow(observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "50 to 59",]) > 4000){
      observedhazotkm_5059 <- observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "50 to 59",] %>%
        dplyr::filter(row_number() %% 5 == 1)
    } else if (nrow(observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "50 to 59",]) > 2000 &
               nrow(observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "50 to 59",]) < 4000){
      observedhazotkm_5059 <- observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "50 to 59",] %>%
        dplyr::filter(row_number() %% 3 == 1)
    } else if (nrow(observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "50 to 59",]) > 1000 &
               nrow(observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "50 to 59",]) < 2000){
      observedhazotkm_5059 <- observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "50 to 59",] %>%
        dplyr::filter(row_number() %% 2 == 1)
    } else {
      observedhazotkm_5059 <- observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "50 to 59",]
    }   
    
    # 60 to 69
    if(nrow(observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "60 to 69",]) > 4000){
      observedhazotkm_6069 <- observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "60 to 69",] %>%
        dplyr::filter(row_number() %% 5 == 1)
    } else if (nrow(observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "60 to 69",]) > 2000 &
               nrow(observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "60 to 69",]) < 4000){
      observedhazotkm_6069 <- observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "60 to 69",] %>%
        dplyr::filter(row_number() %% 3 == 1)
    } else if (nrow(observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "60 to 69",]) > 1000 &
               nrow(observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "60 to 69",]) < 2000){
      observedhazotkm_6069 <- observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "60 to 69",] %>%
        dplyr::filter(row_number() %% 2 == 1)
    } else {
      observedhazotkm_6069 <- observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "60 to 69",]
    }    
    
    # 70 to 79
    if(nrow(observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "70 to 79",]) > 4000){
      observedhazotkm_7079 <- observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "70 to 79",] %>%
        dplyr::filter(row_number() %% 5 == 1)
    } else if (nrow(observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "70 to 79",]) > 2000 &
               nrow(observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "70 to 79",]) < 4000){
      observedhazotkm_7079 <- observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "70 to 79",] %>%
        dplyr::filter(row_number() %% 3 == 1)
    } else if (nrow(observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "70 to 79",]) > 1000 &
               nrow(observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "70 to 79",]) < 4000){
      observedhazotkm_7079 <- observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "70 to 79",] %>%
        dplyr::filter(row_number() %% 2 == 1)
    } else {
      observedhazotkm_7079 <- observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "70 to 79",]
    }       
    
    # 80+
    if(nrow(observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "80 +",]) > 4000){
      observedhazotkm_80 <- observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "80 +",] %>%
        dplyr::filter(row_number() %% 5 == 1)
    } else if (nrow(observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "80 +",]) > 2000 &
               nrow(observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "80 +",]) < 4000){
      observedhazotkm_80 <- observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "80 +",] %>%
        dplyr::filter(row_number() %% 3 == 1)
    } else if (nrow(observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "80 +",]) > 1000 &
               nrow(observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "80 +",]) < 2000){
      observedhazotkm_80 <- observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "80 +",] %>%
        dplyr::filter(row_number() %% 2 == 1)
    } else {
      observedhazotkm_80 <- observedhazotKM_age[[j]][observedhazotKM_age[[j]]$Age == "80 +",]
    }   
    
    observedhazotKM_age[[j]] <- bind_rows(
      observedhazotkm_1839,
      observedhazotkm_4049,
      observedhazotkm_5059,
      observedhazotkm_6069,
      observedhazotkm_7079,
      observedhazotkm_80)  
    
    rm(modelhot)
    }
    
}

# take the results from a list (one element for each cancer) and put into dataframe for KM survival
observedkmcombined_age <- dplyr::bind_rows(observedkm_age) %>%
  dplyr::rename(est = estimate ,ucl = conf.high, lcl = conf.low )  %>%
  dplyr::mutate(Stratification = "Age", Adjustment = "None")

medkmcombined_age <- dplyr::bind_rows(observedmedianKM_age) %>%
  dplyr::mutate(Stratification = "Age", Adjustment = "None")

hotkmcombined_age <- dplyr::bind_rows(observedhazotKM_age) %>%
  dplyr::rename(est = hazard, ucl = upper.ci, lcl = lower.ci ) %>%
  dplyr::mutate(Stratification = "Age", Adjustment = "None")

#generate the risk table and remove entries < 10 patients
risktableskm_age <- dplyr::bind_rows(observedrisktableKM_age) %>% 
  dplyr::filter(details != "n.censor") %>% 
  dplyr::mutate(Stratification = "Age", Adjustment = "None") %>% 
  dplyr::mutate(across(everything(), ~replace(., .==  0 , NA))) %>%
  dplyr::mutate(across(where(is.numeric), ~replace(., .<  10 , "<10"))) %>% 
  dplyr::mutate(across(everything(), as.character)) %>%
  replace(is.na(.), "0")

#################################################################################
# duplicate results for shiny dashboard
observedkmcombined_ageA <- dplyr::bind_rows(observedkm_age) %>%
  dplyr::rename(est = estimate ,ucl = conf.high, lcl = conf.low )  %>%
  dplyr::mutate(Stratification = "None", Adjustment = "Age")

medkmcombined_ageA <- dplyr::bind_rows(observedmedianKM_age) %>%
  dplyr::mutate(Stratification = "None", Adjustment = "Age")

hotkmcombined_ageA <- dplyr::bind_rows(observedhazotKM_age) %>%
  dplyr::rename(est = hazard, ucl = upper.ci, lcl = lower.ci ) %>%
  dplyr::mutate(Stratification = "None", Adjustment = "Age")

#generate the risk table and remove entries < 10 patients
risktableskm_ageA <- dplyr::bind_rows(observedrisktableKM_age) %>% 
  dplyr::filter(details != "n.censor") %>% 
  dplyr::mutate(Stratification = "None", Adjustment = "Age") %>% 
  dplyr::mutate(across(everything(), ~replace(., .==  0 , NA))) %>%
  dplyr::mutate(across(where(is.numeric), ~replace(., .<  10 , "<10"))) %>% 
  dplyr::mutate(across(everything(), as.character)) %>%
  replace(is.na(.), "0")

toc(func.toc=toc_min)

info(logger, 'KM analysis for age stratification COMPLETE')

#########################################################
# Extrapolation analysis for age adjustment ------
#########################################################

tic("Extrapolation analysis for age adjustment")

info(logger, 'Extrapolation analysis for age adjustment START')

# Initiate lists to store output within loop ---- 
extrapolations_age <- list() # extrapolation over time
gof_haz_age <- list() # goodness of fit
hazot_age <- list() # hazard over time
parameters_age <- list() # parameters from each model
pred_median_mean_age <- list() # extract the predicted median and RMST and surv prob at 1 5 and 10 years from extrapolation methods

for(j in 1:nrow(cancer_cohorts)) {

  # set up temp tables
  extrap_results_temp <- list() 
  gof_results_temp <- list()
  hazot_results_temp <- list() 
  parameters_results_temp <- list()
  pred_median_mean_results_temp <- list()
  
  #subset the data by cancer type
  data <- Pop %>%
    dplyr::filter(cohort_definition_id == cancer_cohorts$cohort_definition_id[j])
  
  #create a reference level in this case we are using 70-79 (for most cancers highest numbers of people diagnosed)
  data$age_gr <- as.factor(data$age_gr)
  data$age_gr <-  relevel(data$age_gr, "70 to 79")
  
  agelevels <- data %>%
    dplyr::group_by(age_gr) %>% dplyr::summarise(count = n())
  
    #carry out extrapolation for each cancer
    for(i in 1:length(extrapolations)) {   # Head of for-loop
      
      if(extrapolations[i] == "spline1") {
        
        # 1knotspline
        tryCatch(
          model <- flexsurv::flexsurvspline(formula=Surv(time_years,status-1) ~ age_gr ,data=data,k = 1, scale = "hazard"),
          error = function(e){
            cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
          warning = function(w){
            cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        )
        
        if (exists("model") == TRUE) {
          
          tryCatch({
          
          #extrapolation
          extrap_results_temp[[i]] <- model %>%
            summary(t=t/365, tidy = TRUE) %>%
            dplyr::mutate(Method = extrapolations_formatted[i],
                   Cancer = cancer_cohorts$cohort_name[j], 
                   Sex = "Both" ) %>% 
            dplyr::rename(Age = age_gr)
          
          #get the goodness of fit for each model
          gof_results_temp[[i]] <- model %>%
            broom::glance() %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                   Cancer = cancer_cohorts$cohort_name[j], 
                   Sex = "Both" ) %>% 
            dplyr::slice(rep(1:n(), each = nrow(agelevels))) %>% 
            dplyr::mutate(Age = levels(as.factor(data$age_gr)))
          
          #grab the parameters and knots from the model
          coefs.p <- model[["coefficients"]] %>%
            tibble::enframe() %>%
            tidyr::pivot_wider(names_from = name, values_from = value) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Sex = "Both" ) 
          
          knots.p <- model[["knots"]] %>%
            setNames(., c("SplineLowerB", "SplineInternal1" , "SplineUpperB")) %>%
            tibble::enframe() %>%
            tidyr::pivot_wider(names_from = name, values_from = value)
          
          parameters_results_temp[[i]] <- bind_cols(coefs.p,  knots.p ) %>% 
            dplyr::slice(rep(1:n(), each = nrow(agelevels))) %>% 
            dplyr::mutate(Age = levels(as.factor(data$age_gr)))
          
          # hazard over time
          hazot_results_temp[[i]] <- model %>%
            summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                   Cancer = cancer_cohorts$cohort_name[j], 
                   Sex = "Both" ) %>% 
            dplyr::rename(Age = age_gr)
          
          # median and mean predicted survival
          pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
            dplyr::rename(median = est) %>% 
            dplyr::mutate(median = round(median, 4),
                   lcl = round(lcl, 4),
                   ucl = round(ucl, 4),
                   "Median Survival in Years (95% CI)"= ifelse(!is.na(median),
                                                               paste0(paste0(nice.num2(median)), " (",
                                                                      paste0(nice.num2(lcl)),"-",
                                                                      paste0(nice.num2(ucl)), ")"),
                                                               NA)) %>% 
            dplyr::select(-c(lcl, ucl))
          
          pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
            dplyr::rename(rmean = est) %>% 
            dplyr::mutate(rmean = round(rmean, 4),
                   se = round(se, 4),
                   time = round(time,4) ,
                   "rmean in years (SE)"= ifelse(!is.na(rmean),
                                                 paste0(paste0(nice.num2(rmean)), " (",
                                                        paste0(nice.num2(se)), ")"),
                                                 NA)) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>%
            dplyr::rename(rmean5yr = est) %>%
            dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                          se = round(se, 4),
                          time = round(time,4) ,
                          "rmean 5yrs in years (SE)"= ifelse(!is.na(rmean5yr),
                                                              paste0(paste0(nice.num2(rmean5yr)), " (",
                                                                     paste0(nice.num2(se)), ")"),
                                                              NA)) %>%
            dplyr::rename(se5yr = se) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>%
            dplyr::rename(rmean10yr = est) %>%
            dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                   se = round(se, 4),
                   time = round(time,4) ,
                   "rmean 10yrs in years (SE)"= ifelse(!is.na(rmean10yr),
                                                       paste0(paste0(nice.num2(rmean10yr)), " (",
                                                              paste0(nice.num2(se)), ")"),
                                                       NA)) %>%
            dplyr::rename(se10yr = se) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
            dplyr::mutate(est = round((est*100),4),
                   lcl = round((lcl*100),4),
                   ucl = round((ucl*100),4),
                   "Survival Rate % (95% CI)"= ifelse(!is.na(est),
                                                      paste0(paste0(nice.num1(est)), " (",
                                                             paste0(nice.num1(lcl)),"-",
                                                             paste0(nice.num1(ucl)), ")"),
                                                      NA)) %>% 
            dplyr::rename("surv" = est) %>% 
            dplyr::select(-c(lcl, ucl)) %>% 
            tidyr::pivot_wider(names_from = time, 
                        values_from = c(`Survival Rate % (95% CI)`, surv),
                        names_prefix = " year ",
                        names_sep = "")
          
          pred_median_mean_results_temp[[i]] <- dplyr::inner_join(pr_mean, pr_mean5, by = "age_gr" ) %>% 
            dplyr::inner_join(pr_mean10, by = "age_gr" ) %>% 
            dplyr::inner_join(pr_median, by = "age_gr" ) %>% 
            dplyr::inner_join(pr_survival_prob, by = "age_gr")
          
          pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
            dplyr::mutate(Method = extrapolations_formatted[i], 
                   Cancer = cancer_cohorts$cohort_name[j], 
                   Sex = "Both" ) %>% 
            dplyr::rename(Age = age_gr)

          rm(model, pr_mean, pr_median, pr_survival_prob, pr_mean10, pr_mean5)
          
          #print out progress               
          print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cancer_cohorts$cohort_name[j], " completed"))

          }, 
          
          error = function(e) {
            cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for age model", "\n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
          
          warning = function(w) {
            cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for age model", "\n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
          
          )
          
        } 
          
 
      } else if(extrapolations[i] == "spline2") {
        # 2knotspline
        
        tryCatch(
          model <- flexsurv::flexsurvspline(formula=Surv(time_years,status-1) ~ age_gr , data=data , k = 2, scale = "hazard"),
          error = function(e){
            cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
          warning = function(w){
            cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        )
        
        if (exists("model") == TRUE) {
          
          
          tryCatch({
          
          extrap_results_temp[[i]] <- model %>%
            summary(t=t/365, tidy = TRUE) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                   Cancer = cancer_cohorts$cohort_name[j], 
                   Sex = "Both") %>% 
            dplyr::rename(Age = age_gr)
          
          #get the goodness of fit for each model
          gof_results_temp[[i]] <- model %>%
            broom::glance() %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                   Cancer = cancer_cohorts$cohort_name[j],
                   Sex = "Both" ) %>% 
            dplyr::slice(rep(1:n(), each = nrow(agelevels))) %>% 
            dplyr::mutate(Age = levels(as.factor(data$age_gr)))
          
          #extract parameters
          #grab the parameters and knots from the model
          coefs.p <- model[["coefficients"]] %>%
            tibble::enframe() %>%
            tidyr::pivot_wider(names_from = name, values_from = value) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                   Cancer = cancer_cohorts$cohort_name[j], 
                   Sex = "Both") 
          
          knots.p <- model[["knots"]] %>%
            setNames(., c("SplineLowerB", "SplineInternal1" , "SplineInternal2" ,"SplineUpperB")) %>%
            tibble::enframe() %>%
            tidyr::pivot_wider(names_from = name, values_from = value)
          
          parameters_results_temp[[i]] <- bind_cols(coefs.p,  knots.p ) %>% 
            dplyr::slice(rep(1:n(), each = nrow(agelevels))) %>% 
            dplyr::mutate(Age = levels(as.factor(data$age_gr)))
          
          # hazard over time
          hazot_results_temp[[i]] <- model %>%
            summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                   Cancer = cancer_cohorts$cohort_name[j], 
                   Sex = "Both") %>% 
            dplyr::rename(Age = age_gr)
          
          # median and mean predicted survival
          pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
            dplyr::rename(median = est) %>% 
            dplyr::mutate(median = round(median, 4),
                   lcl = round(lcl, 4),
                   ucl = round(ucl, 4),
                   "Median Survival in Years (95% CI)"= ifelse(!is.na(median),
                                                               paste0(paste0(nice.num2(median)), " (",
                                                                      paste0(nice.num2(lcl)),"-",
                                                                      paste0(nice.num2(ucl)), ")"),
                                                               NA)) %>% 
            dplyr::select(-c(lcl, ucl))
          
          pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
            dplyr::rename(rmean = est) %>% 
            dplyr::mutate(rmean = round(rmean, 4),
                   se = round(se, 4),
                   time = round(time,4) ,
                   "rmean in years (SE)"= ifelse(!is.na(rmean),
                                                 paste0(paste0(nice.num2(rmean)), " (",
                                                        paste0(nice.num2(se)), ")"),
                                                 NA)) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>%
            dplyr::rename(rmean5yr = est) %>%
            dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                          se = round(se, 4),
                          time = round(time,4) ,
                          "rmean 5yrs in years (SE)"= ifelse(!is.na(rmean5yr),
                                                             paste0(paste0(nice.num2(rmean5yr)), " (",
                                                                    paste0(nice.num2(se)), ")"),
                                                             NA)) %>%
            dplyr::rename(se5yr = se) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>%
            dplyr::rename(rmean10yr = est) %>%
            dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                          se = round(se, 4),
                          time = round(time,4) ,
                          "rmean 10yrs in years (SE)"= ifelse(!is.na(rmean10yr),
                                                              paste0(paste0(nice.num2(rmean10yr)), " (",
                                                                     paste0(nice.num2(se)), ")"),
                                                              NA)) %>%
            dplyr::rename(se10yr = se) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
            dplyr::mutate(est = round((est*100),4),
                          lcl = round((lcl*100),4),
                          ucl = round((ucl*100),4),
                          "Survival Rate % (95% CI)"= ifelse(!is.na(est),
                                                             paste0(paste0(nice.num1(est)), " (",
                                                                    paste0(nice.num1(lcl)),"-",
                                                                    paste0(nice.num1(ucl)), ")"),
                                                             NA)) %>% 
            dplyr::rename("surv" = est) %>% 
            dplyr::select(-c(lcl, ucl)) %>% 
            tidyr::pivot_wider(names_from = time, 
                               values_from = c(`Survival Rate % (95% CI)`, surv),
                               names_prefix = " year ",
                               names_sep = "")
          
          pred_median_mean_results_temp[[i]] <- dplyr::inner_join(pr_mean, pr_mean5, by = "age_gr" ) %>% 
            dplyr::inner_join(pr_mean10, by = "age_gr" ) %>% 
            dplyr::inner_join(pr_median, by = "age_gr" ) %>% 
            dplyr::inner_join(pr_survival_prob, by = "age_gr")
          
          pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j], 
                          Sex = "Both" ) %>% 
            dplyr::rename(Age = age_gr)
          
          rm(model, pr_mean, pr_median, pr_survival_prob, pr_mean10, pr_mean5)
          
          #print out progress               
          print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cancer_cohorts$cohort_name[j], " completed"))
          
          }, 
          
          error = function(e) {
            cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for age model", "\n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
          
          warning = function(w) {
            cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for age model", "\n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
          
          )
          
        } 
          
        
      } else if(extrapolations[i] == "spline3") {
        # 3knotspline
        
        tryCatch(
          model <- flexsurv::flexsurvspline(formula=Surv(time_years,status-1) ~ age_gr, data=data, k = 3, scale = "hazard"),
          error = function(e){
            cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
          warning = function(w){
            cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        )
        
        if (exists("model") == TRUE) {
          
          tryCatch({
          
          extrap_results_temp[[i]] <- model %>%
            summary(t=t/365, tidy = TRUE) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                   Cancer = cancer_cohorts$cohort_name[j],
                   Sex = "Both")%>% 
            dplyr::rename(Age = age_gr)
          
          #get the goodness of fit for each model
          gof_results_temp[[i]] <- model %>%
            broom::glance() %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                   Cancer = cancer_cohorts$cohort_name[j], 
                   Sex = "Both" ) %>% 
            dplyr::slice(rep(1:n(), each = nrow(agelevels))) %>% 
            dplyr::mutate(Age = levels(as.factor(data$age_gr)))
          
          #extract parameters
          #grab the parameters and knots from the model
          coefs.p <- model[["coefficients"]] %>%
            tibble::enframe() %>%
            tidyr::pivot_wider(names_from = name, values_from = value) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                   Cancer = cancer_cohorts$cohort_name[j], 
                   Sex = "Both") 
          
          knots.p <- model[["knots"]] %>%
            setNames(., c("SplineLowerB", "SplineInternal1" , "SplineInternal2", "SplineInternal3" ,"SplineUpperB")) %>%
            tibble::enframe() %>%
            tidyr::pivot_wider(names_from = name, values_from = value)
          
          parameters_results_temp[[i]] <- bind_cols(coefs.p,  knots.p ) %>% 
            dplyr::slice(rep(1:n(), each = nrow(agelevels))) %>% 
            dplyr::mutate(Age = levels(as.factor(data$age_gr)))
          
          # hazard over time
          hazot_results_temp[[i]] <- model %>%
            summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                   Cancer = cancer_cohorts$cohort_name[j], 
                   Sex = "Both") %>% 
            dplyr::rename(Age = age_gr)
          
          # median and mean predicted survival
          pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
            dplyr::rename(median = est) %>% 
            dplyr::mutate(median = round(median, 4),
                   lcl = round(lcl, 4),
                   ucl = round(ucl, 4),
                   "Median Survival in Years (95% CI)"= ifelse(!is.na(median),
                                                               paste0(paste0(nice.num2(median)), " (",
                                                                      paste0(nice.num2(lcl)),"-",
                                                                      paste0(nice.num2(ucl)), ")"),
                                                               NA)) %>% 
            dplyr::select(-c(lcl, ucl))
          
          pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
            dplyr::rename(rmean = est) %>% 
            dplyr::mutate(rmean = round(rmean, 4),
                   se = round(se, 4),
                   time = round(time,4) ,
                   "rmean in years (SE)"= ifelse(!is.na(rmean),
                                                 paste0(paste0(nice.num2(rmean)), " (",
                                                        paste0(nice.num2(se)), ")"),
                                                 NA)) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>%
            dplyr::rename(rmean5yr = est) %>%
            dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                          se = round(se, 4),
                          time = round(time,4) ,
                          "rmean 5yrs in years (SE)"= ifelse(!is.na(rmean5yr),
                                                             paste0(paste0(nice.num2(rmean5yr)), " (",
                                                                    paste0(nice.num2(se)), ")"),
                                                             NA)) %>%
            dplyr::rename(se5yr = se) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>%
            dplyr::rename(rmean10yr = est) %>%
            dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                          se = round(se, 4),
                          time = round(time,4) ,
                          "rmean 10yrs in years (SE)"= ifelse(!is.na(rmean10yr),
                                                              paste0(paste0(nice.num2(rmean10yr)), " (",
                                                                     paste0(nice.num2(se)), ")"),
                                                              NA)) %>%
            dplyr::rename(se10yr = se) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
            dplyr::mutate(est = round((est*100),4),
                          lcl = round((lcl*100),4),
                          ucl = round((ucl*100),4),
                          "Survival Rate % (95% CI)"= ifelse(!is.na(est),
                                                             paste0(paste0(nice.num1(est)), " (",
                                                                    paste0(nice.num1(lcl)),"-",
                                                                    paste0(nice.num1(ucl)), ")"),
                                                             NA)) %>% 
            dplyr::rename("surv" = est) %>% 
            dplyr::select(-c(lcl, ucl)) %>% 
            tidyr::pivot_wider(names_from = time, 
                               values_from = c(`Survival Rate % (95% CI)`, surv),
                               names_prefix = " year ",
                               names_sep = "")
          
          pred_median_mean_results_temp[[i]] <- dplyr::inner_join(pr_mean, pr_mean5, by = "age_gr" ) %>% 
            dplyr::inner_join(pr_mean10, by = "age_gr" ) %>% 
            dplyr::inner_join(pr_median, by = "age_gr" ) %>% 
            dplyr::inner_join(pr_survival_prob, by = "age_gr")
          
          pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j], 
                          Sex = "Both" ) %>% 
            dplyr::rename(Age = age_gr)
          
          rm(model, pr_mean, pr_median, pr_survival_prob, pr_mean10, pr_mean5)
          
          #print out progress               
          print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cancer_cohorts$cohort_name[j], " completed"))
          
          }, 
          
          error = function(e) {
            cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for age model", "\n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
          
          warning = function(w) {
            cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for age model", "\n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
          
          )
          
        } 

        
      } else if(extrapolations[i] == "spline5") {
        # 5knotspline
        
        tryCatch(
          model <- flexsurv::flexsurvspline(formula=Surv(time_years,status-1) ~ age_gr , data=data, k = 5, scale = "hazard"),
          error = function(e){
            cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
          warning = function(w){
            cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        )
        
        if (exists("model") == TRUE) {
          
          
          tryCatch({
          
          extrap_results_temp[[i]] <- model %>%
            summary(t=t/365, tidy = TRUE) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                   Cancer = cancer_cohorts$cohort_name[j], 
                   Sex = "Both") %>% 
            dplyr::rename(Age = age_gr)
          
          #get the goodness of fit for each model
          gof_results_temp[[i]] <- model %>%
            broom::glance() %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                   Cancer = cancer_cohorts$cohort_name[j], 
                   Sex = "Both" ) %>% 
            dplyr::slice(rep(1:n(), each = nrow(agelevels))) %>% 
            dplyr::mutate(Age = levels(as.factor(data$age_gr)))
          
          coefs.p <- model[["coefficients"]] %>%
            tibble::enframe() %>%
            tidyr::pivot_wider(names_from = name, values_from = value) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                   Cancer = cancer_cohorts$cohort_name[j], 
                   Sex = "Both") 
          
          knots.p <- model[["knots"]] %>%
            setNames(., c("SplineLowerB", "SplineInternal1" , "SplineInternal2", "SplineInternal3" ,
                          "SplineInternal4" ,"SplineInternal5" , "SplineUpperB")) %>%
            tibble::enframe() %>%
            tidyr::pivot_wider(names_from = name, values_from = value)
          
          parameters_results_temp[[i]] <- bind_cols(coefs.p,  knots.p ) %>% 
            dplyr::slice(rep(1:n(), each = nrow(agelevels))) %>% 
            dplyr::mutate(Age = levels(as.factor(data$age_gr)))
          
          # hazard over time
          hazot_results_temp[[i]] <- model %>%
            summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                   Cancer = cancer_cohorts$cohort_name[j], 
                   Sex = "Both") %>% 
            dplyr::rename(Age = age_gr)
          
          # median and mean predicted survival
          pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
            dplyr::rename(median = est) %>% 
            dplyr::mutate(median = round(median, 4),
                   lcl = round(lcl, 4),
                   ucl = round(ucl, 4),
                   "Median Survival in Years (95% CI)"= ifelse(!is.na(median),
                                                               paste0(paste0(nice.num2(median)), " (",
                                                                      paste0(nice.num2(lcl)),"-",
                                                                      paste0(nice.num2(ucl)), ")"),
                                                               NA)) %>% 
            dplyr::select(-c(lcl, ucl))
          
          pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
            dplyr::rename(rmean = est) %>% 
            dplyr::mutate(rmean = round(rmean, 4),
                   se = round(se, 4),
                   time = round(time,4) ,
                   "rmean in years (SE)"= ifelse(!is.na(rmean),
                                                 paste0(paste0(nice.num2(rmean)), " (",
                                                        paste0(nice.num2(se)), ")"),
                                                 NA)) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>%
            dplyr::rename(rmean5yr = est) %>%
            dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                          se = round(se, 4),
                          time = round(time,4) ,
                          "rmean 5yrs in years (SE)"= ifelse(!is.na(rmean5yr),
                                                             paste0(paste0(nice.num2(rmean5yr)), " (",
                                                                    paste0(nice.num2(se)), ")"),
                                                             NA)) %>%
            dplyr::rename(se5yr = se) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>%
            dplyr::rename(rmean10yr = est) %>%
            dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                          se = round(se, 4),
                          time = round(time,4) ,
                          "rmean 10yrs in years (SE)"= ifelse(!is.na(rmean10yr),
                                                              paste0(paste0(nice.num2(rmean10yr)), " (",
                                                                     paste0(nice.num2(se)), ")"),
                                                              NA)) %>%
            dplyr::rename(se10yr = se) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
            dplyr::mutate(est = round((est*100),4),
                          lcl = round((lcl*100),4),
                          ucl = round((ucl*100),4),
                          "Survival Rate % (95% CI)"= ifelse(!is.na(est),
                                                             paste0(paste0(nice.num1(est)), " (",
                                                                    paste0(nice.num1(lcl)),"-",
                                                                    paste0(nice.num1(ucl)), ")"),
                                                             NA)) %>% 
            dplyr::rename("surv" = est) %>% 
            dplyr::select(-c(lcl, ucl)) %>% 
            tidyr::pivot_wider(names_from = time, 
                               values_from = c(`Survival Rate % (95% CI)`, surv),
                               names_prefix = " year ",
                               names_sep = "")
          
          pred_median_mean_results_temp[[i]] <- dplyr::inner_join(pr_mean, pr_mean5, by = "age_gr" ) %>% 
            dplyr::inner_join(pr_mean10, by = "age_gr" ) %>% 
            dplyr::inner_join(pr_median, by = "age_gr" ) %>% 
            dplyr::inner_join(pr_survival_prob, by = "age_gr")
          
          pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j], 
                          Sex = "Both" ) %>% 
            dplyr::rename(Age = age_gr)
          
          rm(model, pr_mean, pr_median, pr_survival_prob, pr_mean10, pr_mean5)
          
          #print out progress               
          print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cancer_cohorts$cohort_name[j], " completed"))
          
          }, 
          
          error = function(e) {
            cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for age model", "\n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
          
          warning = function(w) {
            cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for age model", "\n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
          
          )
          
        }
          
        
      } else {
        
        #carry out models for different parametric methods survival
        tryCatch(
          model <- flexsurv::flexsurvreg(Surv(time_years, status) ~ age_gr, data=data, dist=extrapolations[i]),
          error = function(e){
            cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
          warning = function(w){
            cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        )
        
        if (exists("model") == TRUE) {
          
          tryCatch({

            # extrapolations
            extrap_results_temp[[i]] <- model %>%
              summary(t=t/365, tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                     Cancer = cancer_cohorts$cohort_name[j],
                     Sex = "Both") %>% 
              dplyr::rename(Age = age_gr)
            
            #get the goodness of fit for each model
            gof_results_temp[[i]] <- model %>%
              broom::glance() %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                     Cancer = cancer_cohorts$cohort_name[j], 
                     Sex = "Both" ) %>% 
              dplyr::slice(rep(1:n(), each = nrow(agelevels))) %>% 
              dplyr::mutate(Age = levels(as.factor(data$age_gr)))
            
            #grab the parameters from the model
            parameters_results_temp[[i]] <- model[["coefficients"]] %>%
              tibble::enframe() %>%
              tidyr::pivot_wider(names_from = name, values_from = value) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                     Cancer = cancer_cohorts$cohort_name[j], 
                     Sex = "Both" ) %>% 
              dplyr::slice(rep(1:n(), each = nrow(agelevels))) %>% 
              dplyr::mutate(Age = levels(as.factor(data$age_gr)))
            
            #extract the hazard function over time
            hazot_results_temp[[i]] <- model %>%
              summary(t=(t + 1)/365, type = "hazard",tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                     Cancer = cancer_cohorts$cohort_name[j], 
                     Sex = "Both" ) %>% 
              dplyr::rename(Age = age_gr)
            
            # median and mean predicted survival
            pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
              dplyr::rename(median = est) %>% 
              dplyr::mutate(median = round(median, 4),
                     lcl = round(lcl, 4),
                     ucl = round(ucl, 4),
                     "Median Survival in Years (95% CI)"= ifelse(!is.na(median),
                                                                 paste0(paste0(nice.num2(median)), " (",
                                                                        paste0(nice.num2(lcl)),"-",
                                                                        paste0(nice.num2(ucl)), ")"),
                                                                 NA)) %>% 
              dplyr::select(-c(lcl, ucl))
            
            pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean = est) %>% 
              dplyr::mutate(rmean = round(rmean, 4),
                     se = round(se, 4),
                     time = round(time,4) ,
                     "rmean in years (SE)"= ifelse(!is.na(rmean),
                                                   paste0(paste0(nice.num2(rmean)), " (",
                                                          paste0(nice.num2(se)), ")"),
                                                   NA)) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>%
              dplyr::rename(rmean5yr = est) %>%
              dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                            se = round(se, 4),
                            time = round(time,4) ,
                            "rmean 5yrs in years (SE)"= ifelse(!is.na(rmean5yr),
                                                               paste0(paste0(nice.num2(rmean5yr)), " (",
                                                                      paste0(nice.num2(se)), ")"),
                                                               NA)) %>%
              dplyr::rename(se5yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>%
              dplyr::rename(rmean10yr = est) %>%
              dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                            se = round(se, 4),
                            time = round(time,4) ,
                            "rmean 10yrs in years (SE)"= ifelse(!is.na(rmean10yr),
                                                                paste0(paste0(nice.num2(rmean10yr)), " (",
                                                                       paste0(nice.num2(se)), ")"),
                                                                NA)) %>%
              dplyr::rename(se10yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
              dplyr::mutate(est = round((est*100),4),
                            lcl = round((lcl*100),4),
                            ucl = round((ucl*100),4),
                            "Survival Rate % (95% CI)"= ifelse(!is.na(est),
                                                               paste0(paste0(nice.num1(est)), " (",
                                                                      paste0(nice.num1(lcl)),"-",
                                                                      paste0(nice.num1(ucl)), ")"),
                                                               NA)) %>% 
              dplyr::rename("surv" = est) %>% 
              dplyr::select(-c(lcl, ucl)) %>% 
              tidyr::pivot_wider(names_from = time, 
                                 values_from = c(`Survival Rate % (95% CI)`, surv),
                                 names_prefix = " year ",
                                 names_sep = "")
            
            pred_median_mean_results_temp[[i]] <- dplyr::inner_join(pr_mean, pr_mean5, by = "age_gr" ) %>% 
              dplyr::inner_join(pr_mean10, by = "age_gr" ) %>% 
              dplyr::inner_join(pr_median, by = "age_gr" ) %>% 
              dplyr::inner_join(pr_survival_prob, by = "age_gr")
            
            pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j], 
                            Sex = "Both" ) %>% 
              dplyr::rename(Age = age_gr)
            
            rm(model, pr_mean, pr_median, pr_survival_prob, pr_mean10, pr_mean5)
            
            #print out progress               
            print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cancer_cohorts$cohort_name[j], " completed"))
            
            
          }, 
          
          error = function(e) {
            cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for age model", "\n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
          
          warning = function(w) {
            cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for age model", "\n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}

          )
          
        }
        
        
      }
      
      
    }
    
    extrapolatedcombined <- dplyr::bind_rows(extrap_results_temp)
    gofcombined <- dplyr::bind_rows(gof_results_temp)
    hotcombined <- dplyr::bind_rows(hazot_results_temp)   
    parcombined <- dplyr::bind_rows(parameters_results_temp)
    medcombined <- dplyr::bind_rows(pred_median_mean_results_temp)
    
    extrapolations_age[[j]] <- extrapolatedcombined
    gof_haz_age[[j]] <- gofcombined
    hazot_age[[j]] <- hotcombined
    parameters_age[[j]] <-  parcombined
    pred_median_mean_age[[j]] <- medcombined
    
    #print out progress               
    print(paste0(cancer_cohorts$cohort_name[j]," Extrapolation Analysis Completed ", Sys.time()))

  } 
  
# Merge results together from each cancer and extrapolation into a dataframe ---
extrapolatedfinalage <- dplyr::bind_rows(extrapolations_age) %>%
  dplyr::mutate(Stratification = "None", Adjustment = "Age")
goffinalage <- dplyr::bind_rows(gof_haz_age) %>%
  dplyr::mutate(Stratification = "None", Adjustment = "Age")
hazardotfinalage <- dplyr::bind_rows(hazot_age) %>%
  dplyr::mutate(Stratification = "None", Adjustment = "Age")
parametersfinalage <- dplyr::bind_rows(parameters_age)  %>%
  dplyr::mutate(Stratification = "None", Adjustment = "Age") %>% 
  dplyr::relocate(shape, .after = Age) %>% 
  dplyr::relocate(rate, .after = Age) 
predmedmeanfinalage <- dplyr::bind_rows(pred_median_mean_age)  %>%
  dplyr::mutate(Stratification = "None", Adjustment = "Age")

toc(func.toc=toc_min)

info(logger, 'Extrapolation analysis for age adjustment COMPLETE')


########################################
# AGE STRATIFICATION EXTRAPOLATION
#######################################

tic("extrapolation analysis with age stratification")
info(logger, 'extrapolation analysis for age stratification START')

extrapolations_ageS <- list()
gof_haz_ageS <- list()
hazot_ageS <- list()
parameters_ageS <- list()
pred_median_mean_ageS <- list()

for(j in 1:nrow(cancer_cohorts)) { 
  
  #temp results
  extrap_results_temp <- list() 
  gof_results_temp <- list() 
  hazot_results_temp <- list()
  parameters_results_temp <- list() 
  pred_median_mean_results_temp <- list()
  
  #for each age 
  extrap_age <- list()
  gof_age <- list()
  hot_age <- list()   
  par_age <- list()
  med_age <- list()   
  
  #subset the data by cancer type
  data <- Pop %>%
    dplyr::filter(cohort_definition_id == cancer_cohorts$cohort_definition_id[j])
  
  agelevels <- data %>%
    dplyr::group_by(age_gr) %>% dplyr::summarise(count = n()) %>% tally()
  
  agevalues <- data %>%
    dplyr::group_by(age_gr) %>% dplyr::summarise(count = n())
  
    for (agel in 1:nrow(agevalues)) {
      
      data_age <- data %>% 
        dplyr::filter(age_gr == agevalues$age_gr[agel])
      
      #split per gender then run extrapolations
      print(paste0("extrapolations for stratification"," ", Sys.time()," for " , cancer_cohorts$cohort_name[j] ," ", agevalues$age_gr[agel] ," started"))
      
      #carry out extrapolation for each cancer
      for(i in 1:length(extrapolations)) {   # Head of for-loop
        
        if(extrapolations[i] == "spline1") {
          
          # 1knotspline
          tryCatch(
            model <- flexsurv::flexsurvspline	(formula=Surv(time_years,status-1) ~ 1 ,data=data_age ,k = 1, scale = "hazard"),
            error = function(e){
              cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
            warning = function(w){
              cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
          )
          
          if (exists("model") == TRUE) {
            
            tryCatch({
              
            #extrapolation
            extrap_results_temp[[i]] <- model %>%
              summary(t=t/365, tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                     Cancer = cancer_cohorts$cohort_name[j],
                     Sex = "Both",
                     Age = names(table(data_age$age_gr)))
            
            #get the goodness of fit for each model
            gof_results_temp[[i]] <- model %>%
              broom::glance() %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                     Cancer = cancer_cohorts$cohort_name[j], 
                     Sex = "Both",
                     Age = names(table(data_age$age_gr)))
            
            #grab the parameters and knots from the model
            coefs.p <- model[["coefficients"]] %>%
              tibble::enframe() %>%
              tidyr::pivot_wider(names_from = name, values_from = value) %>%
              dplyr::mutate(Method = extrapolations_formatted[i],
                     Cancer = cancer_cohorts$cohort_name[j], 
                     Sex = "Both" ) 
            
            knots.p <- model[["knots"]] %>%
              setNames(., c("SplineLowerB", "SplineInternal1" , "SplineUpperB")) %>%
              tibble::enframe() %>%
              tidyr::pivot_wider(names_from = name, values_from = value)
            
            parameters_results_temp[[i]] <- bind_cols(coefs.p,  knots.p ) %>% 
              dplyr::mutate(Age = names(table(data_age$age_gr)))
            
            # hazard over time
            hazot_results_temp[[i]] <- model %>%
              summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                     Cancer = cancer_cohorts$cohort_name[j],
                     Sex = "Both" ,
                     Age = names(table(data_age$age_gr)))
            
            # median and mean survival predictions from extrapolation
            pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
              dplyr::rename(median = est) %>% 
              dplyr::mutate(median = round(median, 4),
                     lcl = round(lcl, 4),
                     ucl = round(ucl, 4),
                     "Median Survival in Years (95% CI)"= ifelse(!is.na(median),
                                                                 paste0(paste0(nice.num2(median)), " (",
                                                                        paste0(nice.num2(lcl)),"-",
                                                                        paste0(nice.num2(ucl)), ")"),
                                                                 NA)) %>% 
              dplyr::select(-c(lcl, ucl))
            
            pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean = est) %>% 
              dplyr::mutate(rmean = round(rmean, 4),
                     se = round(se, 4),
                     time = round(time,4) ,
                     "rmean in years (SE)"= ifelse(!is.na(rmean),
                                                   paste0(paste0(nice.num2(rmean)), " (",
                                                          paste0(nice.num2(se)), ")"),
                                                   NA)) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean5yr = est) %>% 
              dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                            se = round(se, 4),
                            time = round(time,4) ,
                            "rmean 5yrs in years (SE)"= ifelse(!is.na(rmean5yr),
                                                                paste0(paste0(nice.num2(rmean5yr)), " (",
                                                                       paste0(nice.num2(se)), ")"),
                                                                NA)) %>% 
              dplyr::rename(se5yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean10yr = est) %>% 
              dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                     se = round(se, 4),
                     time = round(time,4) ,
                     "rmean 10yrs in years (SE)"= ifelse(!is.na(rmean10yr),
                                                         paste0(paste0(nice.num2(rmean10yr)), " (",
                                                                paste0(nice.num2(se)), ")"),
                                                         NA)) %>% 
              dplyr::rename(se10yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            # survival predicted probabilities from extrapolations
            pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
              dplyr::mutate(est = round((est*100),4),
                     lcl = round((lcl*100),4),
                     ucl = round((ucl*100),4),
                     "Survival Rate % (95% CI)"= ifelse(!is.na(est),
                                                        paste0(paste0(nice.num1(est)), " (",
                                                               paste0(nice.num1(lcl)),"-",
                                                               paste0(nice.num1(ucl)), ")"),
                                                        NA)) %>% 
              dplyr::rename("surv" = est) %>% 
              dplyr::select(-c(lcl, ucl)) %>% 
              tidyr::pivot_wider(names_from = time, 
                          values_from = c(`Survival Rate % (95% CI)`, surv),
                          names_prefix = " year ",
                          names_sep = "")
            
            pred_median_mean_results_temp[[i]] <- bind_cols(pr_mean,pr_mean5, pr_mean10, pr_median, pr_survival_prob )
            pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
              dplyr::mutate(Method = extrapolations_formatted[i], 
                     Cancer = cancer_cohorts$cohort_name[j], 
                     Sex = "Both", 
                     Age = names(table(data_age$age_gr)))
            
            rm(model,pr_survival_prob, pr_mean, pr_median, pr_mean10, pr_mean5 )
            
            #print out progress               
            print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " , cancer_cohorts$cohort_name[j] , " completed"))
          
            
            }, 
            
            error = function(e) {
              cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for age model", "\n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
            
            warning = function(w) {
              cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for age model", "\n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
            
            )
          
          }
          
        } else if(extrapolations[i] == "spline2") {
          # 2knotspline
          
          tryCatch(
            error = function(e){
              cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
            warning = function(w){
              cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
          )
          
          if (exists("model") == TRUE) {
            
            tryCatch({
            
            extrap_results_temp[[i]] <- model %>%
              summary(t=t/365, tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                     Cancer = cancer_cohorts$cohort_name[j], 
                     Sex = "Both",
                     Age = names(table(data_age$age_gr)))
            
            #get the goodness of fit for each model
            gof_results_temp[[i]] <- model %>%
              broom::glance() %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                     Cancer = cancer_cohorts$cohort_name[j], 
                     Sex = "Both",
                     Age = names(table(data_age$age_gr)))
            
            #extract parameters
            #grab the parameters and knots from the model
            coefs.p <- model[["coefficients"]] %>%
              tibble::enframe() %>%
              tidyr::pivot_wider(names_from = name, values_from = value) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                     Cancer = cancer_cohorts$cohort_name[j], 
                     Sex = "Both") 
            
            knots.p <- model[["knots"]] %>%
              setNames(., c("SplineLowerB", "SplineInternal1" , "SplineInternal2" ,"SplineUpperB")) %>%
              tibble::enframe() %>%
              tidyr::pivot_wider(names_from = name, values_from = value)
            
            parameters_results_temp[[i]] <- bind_cols(coefs.p,  knots.p ) %>% 
              dplyr::mutate(Age = names(table(data_age$age_gr)))
            
            # hazard over time
            hazot_results_temp[[i]] <- model %>%
              summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                     Cancer = cancer_cohorts$cohort_name[j], 
                     Sex = "Both",
                     Age = names(table(data_age$age_gr)))
            
            # median and mean survival predictions from extrapolation
            pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
              dplyr::rename(median = est) %>% 
              dplyr::mutate(median = round(median, 4),
                     lcl = round(lcl, 4),
                     ucl = round(ucl, 4),
                     "Median Survival in Years (95% CI)"= ifelse(!is.na(median),
                                                                 paste0(paste0(nice.num2(median)), " (",
                                                                        paste0(nice.num2(lcl)),"-",
                                                                        paste0(nice.num2(ucl)), ")"),
                                                                 NA)) %>% 
              dplyr::select(-c(lcl, ucl))
            
            pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean = est) %>% 
              dplyr::mutate(rmean = round(rmean, 4),
                     se = round(se, 4),
                     time = round(time,4) ,
                     "rmean in years (SE)"= ifelse(!is.na(rmean),
                                                   paste0(paste0(nice.num2(rmean)), " (",
                                                          paste0(nice.num2(se)), ")"),
                                                   NA)) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean5yr = est) %>% 
              dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                            se = round(se, 4),
                            time = round(time,4) ,
                            "rmean 5yrs in years (SE)"= ifelse(!is.na(rmean5yr),
                                                               paste0(paste0(nice.num2(rmean5yr)), " (",
                                                                      paste0(nice.num2(se)), ")"),
                                                               NA)) %>% 
              dplyr::rename(se5yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean10yr = est) %>% 
              dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                            se = round(se, 4),
                            time = round(time,4) ,
                            "rmean 10yrs in years (SE)"= ifelse(!is.na(rmean10yr),
                                                                paste0(paste0(nice.num2(rmean10yr)), " (",
                                                                       paste0(nice.num2(se)), ")"),
                                                                NA)) %>% 
              dplyr::rename(se10yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            # survival predicted probabilities from extrapolations
            pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
              dplyr::mutate(est = round((est*100),4),
                            lcl = round((lcl*100),4),
                            ucl = round((ucl*100),4),
                            "Survival Rate % (95% CI)"= ifelse(!is.na(est),
                                                               paste0(paste0(nice.num1(est)), " (",
                                                                      paste0(nice.num1(lcl)),"-",
                                                                      paste0(nice.num1(ucl)), ")"),
                                                               NA)) %>% 
              dplyr::rename("surv" = est) %>% 
              dplyr::select(-c(lcl, ucl)) %>% 
              tidyr::pivot_wider(names_from = time, 
                                 values_from = c(`Survival Rate % (95% CI)`, surv),
                                 names_prefix = " year ",
                                 names_sep = "")
            
            pred_median_mean_results_temp[[i]] <- bind_cols(pr_mean,pr_mean5, pr_mean10, pr_median, pr_survival_prob )
            pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j], 
                            Sex = "Both", 
                            Age = names(table(data_age$age_gr)))
            
            rm(model,pr_survival_prob, pr_mean, pr_median, pr_mean10, pr_mean5 )
            
            #print out progress               
            print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " , cancer_cohorts$cohort_name[j] , " completed"))
            
            

            }, 
            
            error = function(e) {
              cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for age model", "\n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
            
            warning = function(w) {
              cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for age model", "\n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
            
            )
            
          }
          
        } else if(extrapolations[i] == "spline3") {
          # 3knotspline
          
          tryCatch(
            model <- flexsurv::flexsurvspline	(formula=Surv(time_years,status-1) ~ 1, data=data_age, k = 3, scale = "hazard"),
            error = function(e){
              cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
            warning = function(w){
              cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
          )
          
          if (exists("model") == TRUE) {
            
            tryCatch({
            
            extrap_results_temp[[i]] <- model %>%
              summary(t=t/365, tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i],
                     Cancer = cancer_cohorts$cohort_name[j],
                     Sex = "Both",
                     Age = names(table(data_age$age_gr)))
            
            #get the goodness of fit for each model
            gof_results_temp[[i]] <- model %>%
              broom::glance() %>%
              dplyr::mutate(Method = extrapolations_formatted[i],
                     Cancer = cancer_cohorts$cohort_name[j], 
                     Sex = "Both" ,
                     Age = names(table(data_age$age_gr)))
            
            #extract parameters
            #grab the parameters and knots from the model
            coefs.p <- model[["coefficients"]] %>%
              tibble::enframe() %>%
              tidyr::pivot_wider(names_from = name, values_from = value) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                     Cancer = cancer_cohorts$cohort_name[j],
                     Sex = "Both") 
            
            knots.p <- model[["knots"]] %>%
              setNames(., c("SplineLowerB", "SplineInternal1" , "SplineInternal2", "SplineInternal3" ,"SplineUpperB")) %>%
              tibble::enframe() %>%
              tidyr::pivot_wider(names_from = name, values_from = value)
            
            parameters_results_temp[[i]] <- bind_cols(coefs.p,  knots.p ) %>% 
              dplyr::mutate(Age = names(table(data_age$age_gr)))
            
            # hazard over time
            hazot_results_temp[[i]] <- model %>%
              summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                     Cancer = cancer_cohorts$cohort_name[j], 
                     Sex = "Both",
                     Age = names(table(data_age$age_gr)))
            
            # median and mean survival predictions from extrapolation
            pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
              dplyr::rename(median = est) %>% 
              dplyr::mutate(median = round(median, 4),
                     lcl = round(lcl, 4),
                     ucl = round(ucl, 4),
                     "Median Survival in Years (95% CI)"= ifelse(!is.na(median),
                                                                 paste0(paste0(nice.num2(median)), " (",
                                                                        paste0(nice.num2(lcl)),"-",
                                                                        paste0(nice.num2(ucl)), ")"),
                                                                 NA)) %>% 
              dplyr::select(-c(lcl, ucl))
            
            pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean = est) %>% 
              dplyr::mutate(rmean = round(rmean, 4),
                     se = round(se, 4),
                     time = round(time,4) ,
                     "rmean in years (SE)"= ifelse(!is.na(rmean),
                                                   paste0(paste0(nice.num2(rmean)), " (",
                                                          paste0(nice.num2(se)), ")"),
                                                   NA)) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean5yr = est) %>% 
              dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                            se = round(se, 4),
                            time = round(time,4) ,
                            "rmean 5yrs in years (SE)"= ifelse(!is.na(rmean5yr),
                                                               paste0(paste0(nice.num2(rmean5yr)), " (",
                                                                      paste0(nice.num2(se)), ")"),
                                                               NA)) %>% 
              dplyr::rename(se5yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean10yr = est) %>% 
              dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                            se = round(se, 4),
                            time = round(time,4) ,
                            "rmean 10yrs in years (SE)"= ifelse(!is.na(rmean10yr),
                                                                paste0(paste0(nice.num2(rmean10yr)), " (",
                                                                       paste0(nice.num2(se)), ")"),
                                                                NA)) %>% 
              dplyr::rename(se10yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            # survival predicted probabilities from extrapolations
            pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
              dplyr::mutate(est = round((est*100),4),
                            lcl = round((lcl*100),4),
                            ucl = round((ucl*100),4),
                            "Survival Rate % (95% CI)"= ifelse(!is.na(est),
                                                               paste0(paste0(nice.num1(est)), " (",
                                                                      paste0(nice.num1(lcl)),"-",
                                                                      paste0(nice.num1(ucl)), ")"),
                                                               NA)) %>% 
              dplyr::rename("surv" = est) %>% 
              dplyr::select(-c(lcl, ucl)) %>% 
              tidyr::pivot_wider(names_from = time, 
                                 values_from = c(`Survival Rate % (95% CI)`, surv),
                                 names_prefix = " year ",
                                 names_sep = "")
            
            pred_median_mean_results_temp[[i]] <- bind_cols(pr_mean,pr_mean5, pr_mean10, pr_median, pr_survival_prob )
            pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j], 
                            Sex = "Both", 
                            Age = names(table(data_age$age_gr)))
            
            rm(model,pr_survival_prob, pr_mean, pr_median, pr_mean10, pr_mean5 )
            
            #print out progress               
            print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " , cancer_cohorts$cohort_name[j] , " completed"))
            
          
            }, 
            
            error = function(e) {
              cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for age model", "\n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
            
            warning = function(w) {
              cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for age model", "\n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
            
            )
            
          }
          
        } else if(extrapolations[i] == "spline5") {
          # 5knotspline
          
          tryCatch(
            model <- flexsurv::flexsurvspline	(formula=Surv(time_years,status-1) ~ 1 , data=data_age, k = 5, scale = "hazard"),
            error = function(e){
              cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
            warning = function(w){
              cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
          )
          
          if (exists("model") == TRUE) {
            
            tryCatch({
              
            extrap_results_temp[[i]] <- model %>%
              summary(t=t/365, tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                     Cancer = cancer_cohorts$cohort_name[j], 
                     Sex = "Both",
                     Age = names(table(data_age$age_gr)))
            
            #get the goodness of fit for each model
            gof_results_temp[[i]] <- model %>%
              broom::glance() %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                     Cancer = cancer_cohorts$cohort_name[j], 
                     Sex = "Both" ,
                     Age = names(table(data_age$age_gr)))
            
            coefs.p <- model[["coefficients"]] %>%
              tibble::enframe() %>%
              tidyr::pivot_wider(names_from = name, values_from = value) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                     Cancer = cancer_cohorts$cohort_name[j], 
                     Sex = "Both") 
            
            knots.p <- model[["knots"]] %>%
              setNames(., c("SplineLowerB", "SplineInternal1" , "SplineInternal2", "SplineInternal3" ,
                            "SplineInternal4" ,"SplineInternal5" , "SplineUpperB")) %>%
              tibble::enframe() %>%
              tidyr::pivot_wider(names_from = name, values_from = value)
            
            parameters_results_temp[[i]] <- bind_cols(coefs.p,  knots.p ) %>% 
              dplyr::mutate(Age = names(table(data_age$age_gr)))
            
            # hazard over time
            hazot_results_temp[[i]] <- model %>%
              summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                     Cancer = cancer_cohorts$cohort_name[j], 
                     Sex = "Both",
                     Age = names(table(data_age$age_gr)))
            
            # median and mean survival predictions from extrapolation
            pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
              dplyr::rename(median = est) %>% 
              dplyr::mutate(median = round(median, 4),
                     lcl = round(lcl, 4),
                     ucl = round(ucl, 4),
                     "Median Survival in Years (95% CI)"= ifelse(!is.na(median),
                                                                 paste0(paste0(nice.num2(median)), " (",
                                                                        paste0(nice.num2(lcl)),"-",
                                                                        paste0(nice.num2(ucl)), ")"),
                                                                 NA)) %>% 
              dplyr::select(-c(lcl, ucl))
            
            pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean = est) %>% 
              dplyr::mutate(rmean = round(rmean, 4),
                     se = round(se, 4),
                     time = round(time,4) ,
                     "rmean in years (SE)"= ifelse(!is.na(rmean),
                                                   paste0(paste0(nice.num2(rmean)), " (",
                                                          paste0(nice.num2(se)), ")"),
                                                   NA)) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean5yr = est) %>% 
              dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                            se = round(se, 4),
                            time = round(time,4) ,
                            "rmean 5yrs in years (SE)"= ifelse(!is.na(rmean5yr),
                                                               paste0(paste0(nice.num2(rmean5yr)), " (",
                                                                      paste0(nice.num2(se)), ")"),
                                                               NA)) %>% 
              dplyr::rename(se5yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean10yr = est) %>% 
              dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                            se = round(se, 4),
                            time = round(time,4) ,
                            "rmean 10yrs in years (SE)"= ifelse(!is.na(rmean10yr),
                                                                paste0(paste0(nice.num2(rmean10yr)), " (",
                                                                       paste0(nice.num2(se)), ")"),
                                                                NA)) %>% 
              dplyr::rename(se10yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            # survival predicted probabilities from extrapolations
            pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
              dplyr::mutate(est = round((est*100),4),
                            lcl = round((lcl*100),4),
                            ucl = round((ucl*100),4),
                            "Survival Rate % (95% CI)"= ifelse(!is.na(est),
                                                               paste0(paste0(nice.num1(est)), " (",
                                                                      paste0(nice.num1(lcl)),"-",
                                                                      paste0(nice.num1(ucl)), ")"),
                                                               NA)) %>% 
              dplyr::rename("surv" = est) %>% 
              dplyr::select(-c(lcl, ucl)) %>% 
              tidyr::pivot_wider(names_from = time, 
                                 values_from = c(`Survival Rate % (95% CI)`, surv),
                                 names_prefix = " year ",
                                 names_sep = "")
            
            pred_median_mean_results_temp[[i]] <- bind_cols(pr_mean,pr_mean5, pr_mean10, pr_median, pr_survival_prob )
            pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j], 
                            Sex = "Both", 
                            Age = names(table(data_age$age_gr)))
            
            rm(model,pr_survival_prob, pr_mean, pr_median, pr_mean10, pr_mean5 )
            
            #print out progress               
            print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " , cancer_cohorts$cohort_name[j] , " completed"))
            
            
            }, 
            
            error = function(e) {
              cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for age model", "\n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
            
            warning = function(w) {
              cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for age model", "\n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
            
            )
            
            
          }
          
        } else {
          
          #carry out models for different parametric methods survival
          tryCatch(
            model <- flexsurv::flexsurvreg(Surv(time_years, status) ~ 1, data=data_age, dist=extrapolations[i]),
            error = function(e){
              cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
            warning = function(w){
              cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
          )
          
          if (exists("model") == TRUE) {
            
            tryCatch({
            
            # extrapolations
            extrap_results_temp[[i]] <- model %>%
              summary(t=t/365, tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                     Cancer = cancer_cohorts$cohort_name[j], 
                     Sex = "Both",
                     Age = names(table(data_age$age_gr)))
            
            #get the goodness of fit for each model
            gof_results_temp[[i]] <- model %>%
              broom::glance() %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                     Cancer = cancer_cohorts$cohort_name[j], 
                     Sex = "Both" ,
                     Age = names(table(data_age$age_gr)))
            
            #grab the parameters from the model
            parameters_results_temp[[i]] <- model[["coefficients"]] %>%
              tibble::enframe() %>%
              tidyr::pivot_wider(names_from = name, values_from = value) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                     Cancer = cancer_cohorts$cohort_name[j], 
                     Sex = "Both" ,
                     Age = names(table(data_age$age_gr)))
            
            #extract the hazard function over time
            hazot_results_temp[[i]] <- model %>%
              summary(t=(t + 1)/365, type = "hazard",tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                     Cancer = cancer_cohorts$cohort_name[j], 
                     Sex = "Both" ,
                     Age = names(table(data_age$age_gr)))
            
            # median and mean survival predictions from extrapolation
            pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
              dplyr::rename(median = est) %>% 
              dplyr::mutate(median = round(median, 4),
                     lcl = round(lcl, 4),
                     ucl = round(ucl, 4),
                     "Median Survival in Years (95% CI)"= ifelse(!is.na(median),
                                                                 paste0(paste0(nice.num2(median)), " (",
                                                                        paste0(nice.num2(lcl)),"-",
                                                                        paste0(nice.num2(ucl)), ")"),
                                                                 NA)) %>% 
              dplyr::select(-c(lcl, ucl))
            
            pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean = est) %>% 
              dplyr::mutate(rmean = round(rmean, 4),
                     se = round(se, 4),
                     time = round(time,4) ,
                     "rmean in years (SE)"= ifelse(!is.na(rmean),
                                                   paste0(paste0(nice.num2(rmean)), " (",
                                                          paste0(nice.num2(se)), ")"),
                                                   NA)) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean5yr = est) %>% 
              dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                            se = round(se, 4),
                            time = round(time,4) ,
                            "rmean 5yrs in years (SE)"= ifelse(!is.na(rmean5yr),
                                                               paste0(paste0(nice.num2(rmean5yr)), " (",
                                                                      paste0(nice.num2(se)), ")"),
                                                               NA)) %>% 
              dplyr::rename(se5yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean10yr = est) %>% 
              dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                            se = round(se, 4),
                            time = round(time,4) ,
                            "rmean 10yrs in years (SE)"= ifelse(!is.na(rmean10yr),
                                                                paste0(paste0(nice.num2(rmean10yr)), " (",
                                                                       paste0(nice.num2(se)), ")"),
                                                                NA)) %>% 
              dplyr::rename(se10yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            # survival predicted probabilities from extrapolations
            pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
              dplyr::mutate(est = round((est*100),4),
                            lcl = round((lcl*100),4),
                            ucl = round((ucl*100),4),
                            "Survival Rate % (95% CI)"= ifelse(!is.na(est),
                                                               paste0(paste0(nice.num1(est)), " (",
                                                                      paste0(nice.num1(lcl)),"-",
                                                                      paste0(nice.num1(ucl)), ")"),
                                                               NA)) %>% 
              dplyr::rename("surv" = est) %>% 
              dplyr::select(-c(lcl, ucl)) %>% 
              tidyr::pivot_wider(names_from = time, 
                                 values_from = c(`Survival Rate % (95% CI)`, surv),
                                 names_prefix = " year ",
                                 names_sep = "")
            
            pred_median_mean_results_temp[[i]] <- bind_cols(pr_mean,pr_mean5, pr_mean10, pr_median, pr_survival_prob )
            pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j], 
                            Sex = "Both", 
                            Age = names(table(data_age$age_gr)))
            
            rm(model,pr_survival_prob, pr_mean, pr_median, pr_mean10, pr_mean5 )
            
            #print out progress               
            print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " , cancer_cohorts$cohort_name[j] , " completed"))
            
            
            }, 
            
            error = function(e) {
              cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for age model", "\n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
            
            warning = function(w) {
              cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for age model", "\n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
            
            )
          
          
          }
          
        }
        
      }
      
      #put results for age here
      extrap_age[[agel]] <- dplyr::bind_rows(extrap_results_temp)
      gof_age[[agel]] <- dplyr::bind_rows(gof_results_temp)
      hot_age[[agel]] <- dplyr::bind_rows(hazot_results_temp)   
      par_age[[agel]] <- dplyr::bind_rows(parameters_results_temp)
      med_age[[agel]] <- dplyr::bind_rows(pred_median_mean_results_temp)
      
      # clear the lists again ready for next iteration
      extrap_results_temp <- list() 
      gof_results_temp <- list() 
      hazot_results_temp <- list() 
      parameters_results_temp <- list() 
      pred_median_mean_results_temp <- list()
      
        
        
      }
    
    extrapolatedcombined <- dplyr::bind_rows(extrap_age)
    gofcombined <- dplyr::bind_rows(gof_age)
    hotcombined <- dplyr::bind_rows(hot_age)   
    parcombined <- dplyr::bind_rows(par_age)
    medcombined <- dplyr::bind_rows(med_age)
    
    extrapolations_ageS[[j]] <- extrapolatedcombined
    gof_haz_ageS[[j]] <- gofcombined
    hazot_ageS[[j]] <- hotcombined
    parameters_ageS[[j]] <-  parcombined
    pred_median_mean_ageS[[j]] <- medcombined
    
    #print out progress               
    print(paste0(cancer_cohorts$cohort_name[j]," Extrapolation Analysis Completed ", Sys.time()))
  
}

# Merge results together from each cancer and extrapolation into a dataframe ---
extrapolatedfinalageS <- dplyr::bind_rows(extrapolations_ageS) %>%
  dplyr::mutate(Stratification = "Age", Adjustment = "None")
goffinalageS <- dplyr::bind_rows(gof_haz_ageS) %>%
  dplyr::mutate(Stratification = "Age" , Adjustment = "None")
hazardotfinalageS <- dplyr::bind_rows(hazot_ageS) %>%
  dplyr::mutate(Stratification = "Age", Adjustment = "None")
parametersfinalageS <- dplyr::bind_rows(parameters_ageS)  %>%
  dplyr::mutate(Stratification = "Age", Adjustment = "None") %>%
  dplyr::relocate(shape, .after = Sex) %>%
  dplyr::relocate(rate, .after = Sex) %>%
  dplyr::mutate(rate = coalesce(rate, `1`)) %>%
  dplyr::select(!c(`1`))
predmedmeanfinalageS <- dplyr::bind_rows(pred_median_mean_ageS)  %>%
  dplyr::mutate(Stratification = "Age", Adjustment = "None")

toc(func.toc=toc_min)

info(logger, 'Extrapolation analysis for age stratification COMPLETE')