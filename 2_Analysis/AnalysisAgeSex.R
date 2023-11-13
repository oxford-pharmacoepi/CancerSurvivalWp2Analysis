########################################
# SEX*AGE KM STRATIFICATION
########################################

# km survival, risk table, median survival, survival at 1,5, and 10 years hazard over time from the observed data for each cancer ----
info(logger, 'KM analysis for age*sex stratification START')

# capture output in list
observedkm_age_sex <- list()
observedmedianKM_age_sex <- list()
observedhazotKM_age_sex <- list()
observedrisktableKM_age_sex <- list()

# loop to carry out for each cancer
for(j in 1:nrow(outcome_cohorts)) { 
  
  #subset the data by cancer type
  data <- Pop %>%
    dplyr::filter(cohort_definition_id == j) 
  
  #age levels
  agesexlevels <- data %>%
    dplyr::group_by(sex_age_gp) %>% dplyr::summarise(count = n())
  
  #determines if both sexes in the data
  sexlevels <- data %>%
    dplyr::group_by(sex) %>% dplyr::summarise(count = n()) %>% dplyr::tally()
  
  if(sexlevels == 2){
    
    # carry out km estimate --- 
    observedkm_age_sex[[j]] <- survival::survfit(Surv(time_years, status) ~ sex_age_gp, data=data) %>%
      tidy() %>%
      dplyr::mutate(Method = "Kaplan-Meier", 
             Cancer = outcome_cohorts$cohort_name[j], 
             strata = str_replace(strata, "sex_age_gp=", "")) %>%  
             tidyr::separate(col = "strata",
                      into = c("Age", "Sex"),
                      sep = "_",
                      remove = F) %>% 
      dplyr::rename(sex_age_gp = strata)
    
    
    # 18 to 39 female
    if(nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "18 to 39_Female",]) > 4000){
      observedkm_1839f <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "18 to 39_Female",] %>%
        dplyr::filter(row_number() %% 4 == 1)
    } else if (nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "18 to 39_Female",]) > 2000 &
               nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "18 to 39_Female",]) < 4000){
      observedkm_1839f <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "18 to 39_Female",] %>%
        dplyr::filter(row_number() %% 3 == 1)
    } else if (nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "18 to 39_Female",]) > 1000 &
               nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "18 to 39_Female",]) < 2000){
      observedkm_1839f <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "18 to 39_Female",] %>%
        dplyr::filter(row_number() %% 2 == 1)
    } else {
      observedkm_1839f <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "18 to 39_Female",]
    }
    
    # 18 to 39 male
    if(nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "18 to 39_Male",]) > 4000){
      observedkm_1839m <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "18 to 39_Male",] %>%
        dplyr::filter(row_number() %% 4 == 1)
    } else if (nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "18 to 39_Male",]) > 2000 &
               nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "18 to 39_Male",]) < 4000){
      observedkm_1839m <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "18 to 39_Male",] %>%
        dplyr::filter(row_number() %% 3 == 1)
    } else if (nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "18 to 39_Male",]) > 1000 &
               nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "18 to 39_Male",]) < 2000){
      observedkm_1839m <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "18 to 39_Male",] %>%
        dplyr::filter(row_number() %% 2 == 1)
    } else {
      observedkm_1839m <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "18 to 39_Male",]
    }
    
    
    # 40 to 49 female
    if(nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "40 to 49_Female",]) > 4000){
      observedkm_4049f <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "40 to 49_Female",] %>%
        dplyr::filter(row_number() %% 4 == 1)
    } else if (nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "40 to 49_Female",]) > 2000 &
               nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "40 to 49_Female",]) < 4000){
      observedkm_4049f <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "40 to 49_Female",] %>%
        dplyr::filter(row_number() %% 3 == 1)
    } else if (nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "40 to 49_Female",]) > 1000 &
               nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "40 to 49_Female",]) < 2000){
      observedkm_4049f <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "40 to 49_Female",] %>%
        dplyr::filter(row_number() %% 2 == 1)
    } else {
      observedkm_4049f <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "40 to 49_Female",]
    }
    
    # 40 to 49 male
    if(nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "40 to 49_Male",]) > 4000){
      observedkm_4049m <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "40 to 49_Male",] %>%
        dplyr::filter(row_number() %% 4 == 1)
    } else if (nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "40 to 49_Male",]) > 2000 &
               nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "40 to 49_Male",]) < 4000){
      observedkm_4049m <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "40 to 49_Male",] %>%
        dplyr::filter(row_number() %% 3 == 1)
    } else if (nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "40 to 49_Male",]) > 1000 &
               nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "40 to 49_Male",]) < 2000){
      observedkm_4049m <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "40 to 49_Male",] %>%
        dplyr::filter(row_number() %% 2 == 1)
    } else {
      observedkm_4049m <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "40 to 49_Male",]
    }
    
    
    # 50 to 59 female
    if(nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "50 to 59_Female",]) > 4000){
      observedkm_5059f <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "50 to 59_Female",] %>%
        dplyr::filter(row_number() %% 4 == 1)
    } else if (nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "50 to 59_Female",]) > 2000 &
               nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "50 to 59_Female",]) < 4000){
      observedkm_5059f <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "50 to 59_Female",] %>%
        dplyr::filter(row_number() %% 3 == 1)
    } else if (nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "50 to 59_Female",]) > 1000 &
               nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "50 to 59_Female",]) < 2000){
      observedkm_5059f <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "50 to 59_Female",] %>%
        dplyr::filter(row_number() %% 2 == 1)
    } else {
      observedkm_5059f <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "50 to 59_Female",]
    }
    
    # 50 to 59 male
    if(nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "50 to 59_Male",]) > 4000){
      observedkm_5059m <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "50 to 59_Male",] %>%
        dplyr::filter(row_number() %% 4 == 1)
    } else if (nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "50 to 59_Male",]) > 2000 &
               nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "50 to 59_Male",]) < 4000){
      observedkm_5059m <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "50 to 59_Male",] %>%
        dplyr::filter(row_number() %% 3 == 1)
    } else if (nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "50 to 59_Male",]) > 1000 &
               nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "50 to 59_Male",]) < 2000){
      observedkm_5059m <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "50 to 59_Male",] %>%
        dplyr::filter(row_number() %% 2 == 1)
    } else {
      observedkm_5059m <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "50 to 59_Male",]
    }
    
    
    
    # 60 to 69 female
    if(nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "60 to 69_Female",]) > 4000){
      observedkm_6069f <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "60 to 69_Female",] %>%
        dplyr::filter(row_number() %% 4 == 1)
    } else if (nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "60 to 69_Female",]) > 2000 &
               nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "60 to 69_Female",]) < 4000){
      observedkm_6069f <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "60 to 69_Female",] %>%
        dplyr::filter(row_number() %% 3 == 1)
    } else if (nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "60 to 69_Female",]) > 1000 &
               nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "60 to 69_Female",]) < 2000){
      observedkm_6069f <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "60 to 69_Female",] %>%
        dplyr::filter(row_number() %% 2 == 1)
    } else {
      observedkm_6069f <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "60 to 69_Female",]
    }
    
    # 60 to 69 male
    if(nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "60 to 69_Male",]) > 4000){
      observedkm_6069m <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "60 to 69_Male",] %>%
        dplyr::filter(row_number() %% 4 == 1)
    } else if (nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "60 to 69_Male",]) > 2000 &
               nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "60 to 69_Male",]) < 4000){
      observedkm_6069m <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "60 to 69_Male",] %>%
        dplyr::filter(row_number() %% 3 == 1)
    } else if (nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "60 to 69_Male",]) > 1000 &
               nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "60 to 69_Male",]) < 2000){
      observedkm_6069m <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "60 to 69_Male",] %>%
        dplyr::filter(row_number() %% 2 == 1)
    } else {
      observedkm_6069m <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "60 to 69_Male",]
    }
    
    
    # 70 to 79 female
    if(nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "70 to 79_Female",]) > 4000){
      observedkm_7079f <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "70 to 79_Female",] %>%
        dplyr::filter(row_number() %% 4 == 1)
    } else if (nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "70 to 79_Female",]) > 2000 &
               nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "70 to 79_Female",]) < 4000){
      observedkm_7079f <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "70 to 79_Female",] %>%
        dplyr::filter(row_number() %% 3 == 1)
    } else if (nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "70 to 79_Female",]) > 1000 &
               nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "70 to 79_Female",]) < 2000){
      observedkm_7079f <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "70 to 79_Female",] %>%
        dplyr::filter(row_number() %% 2 == 1)
    } else {
      observedkm_7079f <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "70 to 79_Female",]
    }
    
    # 70 to 79 male
    if(nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "70 to 79_Male",]) > 4000){
      observedkm_7079m <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "70 to 79_Male",] %>%
        dplyr::filter(row_number() %% 4 == 1)
    } else if (nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "70 to 79_Male",]) > 2000 &
               nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "70 to 79_Male",]) < 4000){
      observedkm_7079m <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "70 to 79_Male",] %>%
        dplyr::filter(row_number() %% 3 == 1)
    } else if (nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "70 to 79_Male",]) > 1000 &
               nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "70 to 79_Male",]) < 2000){
      observedkm_7079m <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "70 to 79_Male",] %>%
        dplyr::filter(row_number() %% 2 == 1)
    } else {
      observedkm_7079m <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "70 to 79_Male",]
    }
    
    
    
    # 80+ female
    if(nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "80 +_Female",]) > 4000){
      observedkm_80f <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "80 +_Female",] %>%
        dplyr::filter(row_number() %% 4 == 1)
    } else if (nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "80 +_Female",]) > 2000 &
               nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "80 +_Female",]) < 4000){
      observedkm_80f <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "80 +_Female",] %>%
        dplyr::filter(row_number() %% 3 == 1)
    } else if (nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "80 +_Female",]) > 1000 &
               nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "80 +_Female",]) < 2000){
      observedkm_80f <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "80 +_Female",] %>%
        dplyr::filter(row_number() %% 2 == 1)
    } else {
      observedkm_80f <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "80 +_Female",]
    }
    
    # 80+ male
    if(nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "80 +_Male",]) > 4000){
      observedkm_80m <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "80 +_Male",] %>%
        dplyr::filter(row_number() %% 4 == 1)
    } else if (nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "80 +_Male",]) > 2000 &
               nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "80 +_Male",]) < 4000){
      observedkm_80m <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "80 +_Male",] %>%
        dplyr::filter(row_number() %% 3 == 1)
    } else if (nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "80 +_Male",]) > 1000 &
               nrow(observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "80 +_Male",]) < 2000){
      observedkm_80m <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "80 +_Male",] %>%
        dplyr::filter(row_number() %% 2 == 1)
    } else {
      observedkm_80m <- observedkm_age_sex[[j]][observedkm_age_sex[[j]]$sex_age_gp == "80 +_Male",]
    }
    
    
    observedkm_age_sex[[j]] <- bind_rows(
      observedkm_1839f,
      observedkm_1839m,
      observedkm_4049f,
      observedkm_4049m,
      observedkm_5059f,
      observedkm_5059m,
      observedkm_6069f,
      observedkm_6069m,
      observedkm_7079f,
      observedkm_7079m,
      observedkm_80f,
      observedkm_80m) %>% 
    dplyr::select(!c(sex_age_gp))
    
    
    print(paste0("KM for observed data age*sex strat ", Sys.time()," for ",outcome_cohorts$cohort_name[j], " completed"))
    
    # hazard over time ---
    # paper https://arxiv.org/pdf/1509.03253.pdf states bshazard good package
    # this can fall over with small sample numbers therefore trycatch is in place which tries to perform it and if errors
    # removes age group sequentially.
    print(paste0("Trying Hazard over time results for all age*sex groups ", Sys.time()," for ",outcome_cohorts$cohort_name[j]))
    
    tryCatch( {
        modelhot <- dplyr::group_by(data, sex_age_gp) %>% 
          do(as.data.frame(bshazard(Surv(time_years, status)~1, data=., verbose=FALSE))) %>% 
          ungroup %>%
          dplyr::mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohort_name[j]) %>% 
          tidyr::separate(col = "sex_age_gp",
                   into = c("Age", "Sex"),
                   sep = "_" ,
                   remove = F) },
      error = function(e) {
        cat(conditionMessage(e), "for", outcome_cohorts$cohort_name[j] , "trying again removing small sample numbers", "\n")
        info(logger, paste0(" First model not carried out due to low sample numbers for ",outcome_cohorts$cohort_name[j], "start removing age groups and repeat", e))},
      warning = function(w){info(logger, paste0(outcome_cohorts$cohort_name[j], ": ", w))}
    )
    
    # if model is successful with no removal of age groups if not remove 18-29 age group
    if (exists("modelhot") == TRUE) {
      
      observedhazotKM_age_sex[[j]] <- modelhot
      
      #print out progress               
      print(paste0("Hazard over time results ", Sys.time()," for ",outcome_cohorts$cohort_name[j], " age*sex strat completed"))
      # if model falls over remove first age group 
      
    } else {
      
      print(paste0("Trying Hazard over time results again removing 18-39 year old female age group ", Sys.time()," for ",outcome_cohorts$cohort_name[j]))
      
      data <- data %>% 
        dplyr::filter(sex_age_gp != "18 to 39_Female") 
      
      tryCatch({
          modelhot <- dplyr::group_by(data, sex_age_gp) %>% 
            do(as.data.frame(bshazard(Surv(time_years, status)~1, data=., verbose=FALSE))) %>% 
            ungroup %>%
            dplyr::mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohort_name[j]) %>% 
            tidyr::separate(col = "sex_age_gp",
                     into = c("Age", "Sex"),
                     sep = "_" ,
                     remove = F)
          
        },
        error = function(e) {
          cat("An error occurred: ")
          cat(conditionMessage(e), "for", outcome_cohorts$cohort_name[j] , "trying again removing small sample numbers", "\n")
          info(logger, paste0(" after removal of 18-39 age female group:  Second attempt not carried out due to low sample numbers for ", outcome_cohorts$cohort_name[j], e))} ,
        warning = function(w){info(logger, paste0(outcome_cohorts$cohort_name[j], ": ", w))}
      )
      
    }
    
    # if model successful after removal of 18-39 female age group if not remove 18-39 male age group
    if (exists("modelhot") == TRUE) {
      
      observedhazotKM_age_sex[[j]] <- modelhot 
      
      #print out progress               
      print(paste0("Hazard over time results ", Sys.time()," for ",outcome_cohorts$cohort_name[j], " age*sex strat completed"))
      
      # if model falls over remove second age*sex group
    } else {
      
      print(paste0("Trying Hazard over time results again removing 18-39 male age group ", Sys.time()," for ",outcome_cohorts$cohort_name[j]))
      
      data <- data %>% 
        dplyr::filter(sex_age_gp != "18 to 39_Female") %>% 
        dplyr::filter(sex_age_gp != "18 to 39_Male")
      
      tryCatch( {
          modelhot <- dplyr::group_by(data, sex_age_gp) %>% 
            do(as.data.frame(bshazard(Surv(time_years, status)~1, data=., verbose=FALSE))) %>% 
            ungroup %>%
            dplyr::mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohort_name[j]) %>% 
            tidyr::separate(col = "sex_age_gp",
                     into = c("Age", "Sex"),
                     sep = "_" ,
                     remove = F)
          
        },
        error = function(e) {
          cat("An error occurred: ")
          cat(conditionMessage(e), "for", outcome_cohorts$cohort_name[j] , "trying again removing small sample numbers", "\n")
          info(logger, paste0(" after removal of 18-39 male age group: attempt not carried out due to low sample numbers for ",outcome_cohorts$cohort_name[j], e))},
        warning = function(w){info(logger, paste0(outcome_cohorts$cohort_name[j], ": ", w))}
      ) 
    }
    
    
    if (exists("modelhot") == TRUE) {
      
      observedhazotKM_age_sex[[j]] <- modelhot
      
      #print out progress               
      print(paste0("Hazard over time results ", Sys.time()," for ",outcome_cohorts$cohort_name[j], " age*sex strat completed"))
      
    } else {
      
      print(paste0("Trying Hazard over time results again removing 40-49 year old female age group ", Sys.time()," for ",outcome_cohorts$cohort_name[j]))
      
      data <- data %>% 
        dplyr::filter(sex_age_gp != "18 to 39_Female") %>% 
        dplyr::filter(sex_age_gp != "18 to 39_Male") %>% 
        dplyr::filter(sex_age_gp != "40 to 49_Female")
      
      tryCatch( {
          modelhot <- dplyr::group_by(data, sex_age_gp) %>% 
            do(as.data.frame(bshazard(Surv(time_years, status)~1, data=., verbose=FALSE))) %>% 
            ungroup %>%
            dplyr::mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohort_name[j]) %>% 
            tidyr::separate(col = "sex_age_gp",
                     into = c("Age", "Sex"),
                     sep = "_" ,
                     remove = F)
          
        },
        error = function(e) {
          cat("An error occurred: ")
          cat(conditionMessage(e), "for", outcome_cohorts$cohort_name[j] , "trying again removing small sample numbers", "\n")
          info(logger, paste0(" after removal of 40-49 age female group: attempt not carried out due to low sample numbers for ",outcome_cohorts$cohort_name[j], e))},
        warning = function(w){info(logger, paste0(outcome_cohorts$cohort_name[j], ": ", w))}
      ) 
    }
    

    if (exists("modelhot") == TRUE) {
      
      observedhazotKM_age_sex[[j]] <- modelhot
      
      #print out progress               
      print(paste0("Hazard over time results ", Sys.time()," for ",outcome_cohorts$cohort_name[j], " age*sex strat completed"))
      
    }  else  {
      
      print(paste0("Trying Hazard over time results again removing 40-49 age male group ", Sys.time()," for ",outcome_cohorts$cohort_name[j]))
      
      data <- data %>% 
        dplyr::filter(sex_age_gp != "18 to 39_Female") %>% 
        dplyr::filter(sex_age_gp != "18 to 39_Male") %>% 
        dplyr::filter(sex_age_gp != "40 to 49_Female") %>% 
        dplyr::filter(sex_age_gp != "40 to 49_Male")
      
      tryCatch( {
          modelhot <- dplyr::group_by(data, sex_age_gp) %>% 
            do(as.data.frame(bshazard(Surv(time_years, status)~1, data=., verbose=FALSE))) %>% 
            ungroup %>%
            dplyr::mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohort_name[j]) %>% 
            tidyr::separate(col = "sex_age_gp",
                     into = c("Age", "Sex"),
                     sep = "_" ,
                     remove = F)
          
          
        },
        error = function(e) {
          cat("An error occurred: ")
          cat(conditionMessage(e), "for", outcome_cohorts$cohort_name[j] , "trying again removing small sample numbers", "\n")
          info(logger, paste0("after removal of 40 to 49_Male: attempt not carried out due to low sample numbers for ",outcome_cohorts$cohort_name[j], e))} ,
        warning = function(w){info(logger, paste0(outcome_cohorts$cohort_name[j], ": ", w))}
      ) 
      
      }
    
    
    if (exists("modelhot") == TRUE) {
      
      observedhazotKM_age_sex[[j]] <- modelhot
      
      #print out progress               
      print(paste0("Hazard over time results ", Sys.time()," for ",outcome_cohorts$cohort_name[j], " age*sex strat completed"))
      
    }  else  {
      
      print(paste0("Trying Hazard over time results again removing 50-59 female year old age group ", Sys.time()," for ",outcome_cohorts$cohort_name[j]))
      
      data <- data %>% 
        dplyr::filter(sex_age_gp != "18 to 39_Female") %>% 
        dplyr::filter(sex_age_gp != "18 to 39_Male") %>% 
        dplyr::filter(sex_age_gp != "40 to 49_Female") %>% 
        dplyr::filter(sex_age_gp != "40 to 49_Male") %>% 
        dplyr::filter(sex_age_gp != "50 to 59_Female")
      
      
      tryCatch({
          modelhot <- dplyr::group_by(data, sex_age_gp) %>% 
            do(as.data.frame(bshazard(Surv(time_years, status)~1, data=., verbose=FALSE))) %>% 
            ungroup %>%
            dplyr::mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohort_name[j]) %>% 
            tidyr::separate(col = "sex_age_gp",
                     into = c("Age", "Sex"),
                     sep = "_" ,
                     remove = F)
          
        },
        error = function(e) {
          cat("An error occurred: ")
          cat(conditionMessage(e), "for", outcome_cohorts$cohort_name[j] , "trying again removing small sample numbers", "\n")
          info(logger, paste0("after removal of 50 to 59_female:  attempt not carried out due to low sample numbers for ",outcome_cohorts$cohort_name[j], e)) },
        warning = function(w){info(logger, paste0(outcome_cohorts$cohort_name[j], ": ", w))}
      )
    }    
    
    if (exists("modelhot") == TRUE) {
      
      observedhazotKM_age_sex[[j]] <- modelhot 
      
      #print out progress               
      print(paste0("Hazard over time results ", Sys.time()," for ",outcome_cohorts$cohort_name[j], " age*sex strat completed"))
      
    }  else  {
      
      print(paste0("Trying Hazard over time results again removing 50 to 59_male year old age group ", Sys.time()," for ",outcome_cohorts$cohort_name[j]))
      
      data <- data %>% 
        dplyr::filter(sex_age_gp != "18 to 39_Female") %>% 
        dplyr::filter(sex_age_gp != "18 to 39_Male") %>% 
        dplyr::filter(sex_age_gp != "40 to 49_Female") %>% 
        dplyr::filter(sex_age_gp != "40 to 49_Male") %>% 
        dplyr::filter(sex_age_gp != "50 to 59_Female") %>% 
        dplyr::filter(sex_age_gp != "50 to 59_Male")
      
      tryCatch({modelhot <- dplyr::group_by(data, sex_age_gp) %>% 
            do(as.data.frame(bshazard(Surv(time_years, status)~1, data=., verbose=FALSE))) %>% 
            ungroup %>%
            dplyr::mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohort_name[j]) %>% 
            tidyr::separate(col = "sex_age_gp",
                 into = c("Age", "Sex"),
                 sep = "_" ,
                 remove = F)
          
        },
        error = function(e) {
          cat("An error occurred: ")
          cat(conditionMessage(e), "for", outcome_cohorts$cohort_name[j] , "trying again removing small sample numbers", "\n")
          info(logger, paste0("after removal of 50 to 59_Male group: attempt not carried out due to low sample numbers for ", outcome_cohorts$cohort_name[j], e))},
        warning = function(w){info(logger, paste0(outcome_cohorts$cohort_name[j], ": ", w))}
      ) 
    }
    
    if (exists("modelhot") == TRUE) {
      
      observedhazotKM_age_sex[[j]] <- modelhot
      
      #print out progress               
      print(paste0("Hazard over time results ", Sys.time()," for ",outcome_cohorts$cohort_name[j], " age*sex strat completed"))
      
      
      # add more code here for 60/70
    } else {
      
      print(paste0("Trying Hazard over time results again removing 60 to 69 year old female age group ", Sys.time()," for ",outcome_cohorts$cohort_name[j]))
      
      data <- data %>% 
        dplyr::filter(sex_age_gp != "18 to 39_Female") %>% 
        dplyr::filter(sex_age_gp != "18 to 39_Male") %>% 
        dplyr::filter(sex_age_gp != "40 to 49_Female") %>% 
        dplyr::filter(sex_age_gp != "40 to 49_Male") %>% 
        dplyr::filter(sex_age_gp != "50 to 59_Female") %>% 
        dplyr::filter(sex_age_gp != "50 to 59_Male") %>% 
        dplyr::filter(sex_age_gp != "60 to 69_Female")
      
      tryCatch({
        modelhot <- dplyr::group_by(data, sex_age_gp) %>% 
          do(as.data.frame(bshazard(Surv(time_years, status)~1, data=., verbose=FALSE))) %>% 
          ungroup %>%
          dplyr::mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohort_name[j]) %>% 
          tidyr::separate(col = "sex_age_gp",
                   into = c("Age", "Sex"),
                   sep = "_" ,
                   remove = F)
        
      },
      error = function(e) {
        cat("An error occurred: ")
        cat(conditionMessage(e), "for", outcome_cohorts$cohort_name[j] , "trying again removing small sample numbers", "\n")
        info(logger, paste0(" after removal of 60 to 69_Female group:  Second attempt not carried out due to low sample numbers for ", outcome_cohorts$cohort_name[j], e))} ,
      warning = function(w){info(logger, paste0(outcome_cohorts$cohort_name[j], ": ", w))}
      )
      
    }
    
    # if model successful after removal of 60 to 69 female age group if not remove 60 to 69 male age group
    if (exists("modelhot") == TRUE) {
      
      observedhazotKM_age_sex[[j]] <- modelhot 
      
      #print out progress               
      print(paste0("Hazard over time results ", Sys.time()," for ",outcome_cohorts$cohort_name[j], " age*sex strat completed"))  
      
    } else {

      print(paste0("Trying Hazard over time results again removing 60 to 69 year old male age group ", Sys.time()," for ",outcome_cohorts$cohort_name[j]))
      
      data <- data %>% 
        dplyr::filter(sex_age_gp != "18 to 39_Female") %>% 
        dplyr::filter(sex_age_gp != "18 to 39_Male") %>% 
        dplyr::filter(sex_age_gp != "40 to 49_Female") %>% 
        dplyr::filter(sex_age_gp != "40 to 49_Male") %>% 
        dplyr::filter(sex_age_gp != "50 to 59_Female") %>% 
        dplyr::filter(sex_age_gp != "50 to 59_Male") %>% 
        dplyr::filter(sex_age_gp != "60 to 69_Female") %>% 
        dplyr::filter(sex_age_gp != "60 to 69_Male")
      
      tryCatch({ modelhot <- dplyr::group_by(data, sex_age_gp) %>% 
          do(as.data.frame(bshazard(Surv(time_years, status)~1, data=., verbose=FALSE))) %>% 
          ungroup %>%
          dplyr::mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohort_name[j]) %>% 
        tidyr::separate(col = "sex_age_gp",
                 into = c("Age", "Sex"),
                 sep = "_" ,
                 remove = F)
        
      },
      error = function(e) {
        cat("An error occurred: ")
        cat(conditionMessage(e), "for", outcome_cohorts$cohort_name[j] , "trying again removing small sample numbers", "\n")
        info(logger, paste0(" after removal of 60 to 69_male group:  attempt not carried out due to low sample numbers for ", outcome_cohorts$cohort_name[j], e))} ,
      warning = function(w){info(logger, paste0(outcome_cohorts$cohort_name[j], ": ", w))}
      )
      
    }
    
    # if model successful after removal of 60 to 69 male age group if not remove 70 to 79 female age group
    if (exists("modelhot") == TRUE) {
      
      observedhazotKM_age_sex[[j]] <- modelhot 
      
      #print out progress               
      print(paste0("Hazard over time results ", Sys.time()," for ",outcome_cohorts$cohort_name[j], " age * sex strat completed")) 
      
    } else {
      
      print(paste0("Trying Hazard over time results again removing 70 to 79 year old female age group ", Sys.time()," for ",outcome_cohorts$cohort_name[j]))
      
      data <- data %>% 
        dplyr::filter(sex_age_gp != "18 to 39_Female") %>% 
        dplyr::filter(sex_age_gp != "18 to 39_Male") %>% 
        dplyr::filter(sex_age_gp != "40 to 49_Female") %>% 
        dplyr::filter(sex_age_gp != "40 to 49_Male") %>% 
        dplyr::filter(sex_age_gp != "50 to 59_Female") %>% 
        dplyr::filter(sex_age_gp != "50 to 59_Male") %>% 
        dplyr::filter(sex_age_gp != "60 to 69_Female") %>% 
        dplyr::filter(sex_age_gp != "60 to 69_Male") %>% 
        dplyr::filter(sex_age_gp != "70 to 79_Female")
      
      tryCatch({modelhot <- dplyr::group_by(data, sex_age_gp) %>% 
          do(as.data.frame(bshazard(Surv(time_years, status)~1, data=., verbose=FALSE))) %>% 
          ungroup %>%
          dplyr::mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohort_name[j]) %>% 
        tidyr::separate(col = "sex_age_gp",
                 into = c("Age", "Sex"),
                 sep = "_" ,
                 remove = F)
        
      },
      error = function(e) {
        cat("An error occurred: ")
        cat(conditionMessage(e), "for", outcome_cohorts$cohort_name[j] , "trying again removing small sample numbers", "\n")
        info(logger, paste0(" after removal of 70 to 79_female group:  attempt not carried out due to low sample numbers for ", outcome_cohorts$cohort_name[j], e))} ,
      warning = function(w){info(logger, paste0(outcome_cohorts$cohort_name[j], ": ", w))}
      )
      
    }
    
    # if model successful after removal of 70 to 79 female age group if not remove 70 to 79 male age group
    if (exists("modelhot") == TRUE) {
      
      observedhazotKM_age_sex[[j]] <- modelhot      
      
    } else {   
      
    
      print(paste0("Trying Hazard over time results again removing 70 to 79 year old male age group ", Sys.time()," for ",outcome_cohorts$cohort_name[j]))
      
      data <- data %>% 
        dplyr::filter(sex_age_gp != "18 to 39_Female") %>% 
        dplyr::filter(sex_age_gp != "18 to 39_Male") %>% 
        dplyr::filter(sex_age_gp != "40 to 49_Female") %>% 
        dplyr::filter(sex_age_gp != "40 to 49_Male") %>% 
        dplyr::filter(sex_age_gp != "50 to 59_Female") %>% 
        dplyr::filter(sex_age_gp != "50 to 59_Male") %>% 
        dplyr::filter(sex_age_gp != "60 to 69_Female") %>% 
        dplyr::filter(sex_age_gp != "60 to 69_Male") %>% 
        dplyr::filter(sex_age_gp != "70 to 79_Female") %>% 
        dplyr::filter(sex_age_gp != "70 to 79_Male")
        
      
      tryCatch({
        modelhot <- dplyr::group_by(data, sex_age_gp) %>% 
          do(as.data.frame(bshazard(Surv(time_years, status)~1, data=., verbose=FALSE))) %>% 
          ungroup %>%
          dplyr::mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohort_name[j]) %>% 
          tidyr::separate(col = "sex_age_gp",
                   into = c("Age", "Sex"),
                   sep = "_" ,
                   remove = F)
        
      },
      error = function(e) {
        cat("An error occurred: ")
        cat(conditionMessage(e), "for", outcome_cohorts$cohort_name[j] , "trying again removing small sample numbers", "\n")
        info(logger, paste0(" after removal of 70 to 79_male group:  attempt not carried out due to low sample numbers for ", outcome_cohorts$cohort_name[j], e))} ,
      warning = function(w){info(logger, paste0(outcome_cohorts$cohort_name[j], ": ", w))}
      )
      
    }
    
    # if model successful after removal of 70 to 79 male age group if not analysis cant be run
    if (exists("modelhot") == TRUE) {
      
      observedhazotKM_age_sex[[j]] <- modelhot      
      
      
    } else {
      
      print(paste0("hazard over time age stratification not carried due to low sample numbers in all age groups ", Sys.time()," for ",outcome_cohorts$cohort_name[j]))
      info(logger, paste0("hazard over time age stratification not carried due to low sample numbers in all age groups ", Sys.time()," for ",outcome_cohorts$cohort_name[j]))
      
    }
    
    
    
    if (exists("modelhot") == TRUE) {
      
      #remove rows for plotting purposes
      # 18 to 39 female
      if(nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "18 to 39_Female",]) > 4000){
        observedhazotkm_1839f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "18 to 39_Female",] %>%
          dplyr::filter(row_number() %% 4 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "18 to 39_Female",]) > 2000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "18 to 39_Female",]) < 4000){
        observedhazotkm_1839f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "18 to 39_Female",] %>%
          dplyr::filter(row_number() %% 3 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "18 to 39_Female",]) > 1000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "18 to 39_Female",]) < 2000){
        observedhazotkm_1839f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "18 to 39_Female",] %>%
          dplyr::filter(row_number() %% 2 == 1)
      } else {
        observedhazotkm_1839f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "18 to 39_Female",]
      }
      
      # 18 to 39 male
      if(nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "18 to 39_Male",]) > 4000){
        observedhazotkm_1839m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "18 to 39_Male",] %>%
          dplyr::filter(row_number() %% 4 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "18 to 39_Male",]) > 2000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "18 to 39_Male",]) < 4000){
        observedhazotkm_1839m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "18 to 39_Male",] %>%
          dplyr::filter(row_number() %% 3 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "18 to 39_Male",]) > 1000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "18 to 39_Male",]) < 2000){
        observedhazotkm_1839m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "18 to 39_Male",] %>%
          dplyr::filter(row_number() %% 2 == 1)
      } else {
        observedhazotkm_1839m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "18 to 39_Male",]
      }
      

      # 40 to 49 female
      if(nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "40 to 49_Female",]) > 4000){
        observedhazotkm_4049f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "40 to 49_Female",] %>%
          dplyr::filter(row_number() %% 4 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "40 to 49_Female",]) > 2000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "40 to 49_Female",]) < 4000){
        observedhazotkm_4049f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "40 to 49_Female",] %>%
          dplyr::filter(row_number() %% 3 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "40 to 49_Female",]) > 1000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "40 to 49_Female",]) < 2000){
        observedhazotkm_4049f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "40 to 49_Female",] %>%
          dplyr::filter(row_number() %% 2 == 1)
      } else {
        observedhazotkm_4049f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "40 to 49_Female",]
      }
      
      # 40 to 49 male
      if(nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "40 to 49_Male",]) > 4000){
        observedhazotkm_4049m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "40 to 49_Male",] %>%
          dplyr::filter(row_number() %% 4 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "40 to 49_Male",]) > 2000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "40 to 49_Male",]) < 4000){
        observedhazotkm_4049m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "40 to 49_Male",] %>%
          dplyr::filter(row_number() %% 3 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "40 to 49_Male",]) > 1000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "40 to 49_Male",]) < 2000){
        observedhazotkm_4049m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "40 to 49_Male",] %>%
          dplyr::filter(row_number() %% 2 == 1)
      } else {
        observedhazotkm_4049m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "40 to 49_Male",]
      }


      # 50 to 59 female
      if(nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "50 to 59_Female",]) > 4000){
        observedhazotkm_5059f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "50 to 59_Female",] %>%
          dplyr::filter(row_number() %% 4 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "50 to 59_Female",]) > 2000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "50 to 59_Female",]) < 4000){
        observedhazotkm_5059f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "50 to 59_Female",] %>%
          dplyr::filter(row_number() %% 3 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "50 to 59_Female",]) > 1000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "50 to 59_Female",]) < 2000){
        observedhazotkm_5059f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "50 to 59_Female",] %>%
          dplyr::filter(row_number() %% 2 == 1)
      } else {
        observedhazotkm_5059f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "50 to 59_Female",]
      }
      
      # 50 to 59 male
      if(nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "50 to 59_Male",]) > 4000){
        observedhazotkm_5059m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "50 to 59_Male",] %>%
          dplyr::filter(row_number() %% 4 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "50 to 59_Male",]) > 2000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "50 to 59_Male",]) < 4000){
        observedhazotkm_5059m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "50 to 59_Male",] %>%
          dplyr::filter(row_number() %% 3 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "50 to 59_Male",]) > 1000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "50 to 59_Male",]) < 2000){
        observedhazotkm_5059m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "50 to 59_Male",] %>%
          dplyr::filter(row_number() %% 2 == 1)
      } else {
        observedhazotkm_5059m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "50 to 59_Male",]
      }
      
      

      # 60 to 69 female
      if(nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "60 to 69_Female",]) > 4000){
        observedhazotkm_6069f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "60 to 69_Female",] %>%
          dplyr::filter(row_number() %% 4 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "60 to 69_Female",]) > 2000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "60 to 69_Female",]) < 4000){
        observedhazotkm_6069f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "60 to 69_Female",] %>%
          dplyr::filter(row_number() %% 3 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "60 to 69_Female",]) > 1000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "60 to 69_Female",]) < 2000){
        observedhazotkm_6069f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "60 to 69_Female",] %>%
          dplyr::filter(row_number() %% 2 == 1)
      } else {
        observedhazotkm_6069f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "60 to 69_Female",]
      }
      
      # 60 to 69 male
      if(nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "60 to 69_Male",]) > 4000){
        observedhazotkm_6069m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "60 to 69_Male",] %>%
          dplyr::filter(row_number() %% 4 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "60 to 69_Male",]) > 2000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "60 to 69_Male",]) < 4000){
        observedhazotkm_6069m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "60 to 69_Male",] %>%
          dplyr::filter(row_number() %% 3 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "60 to 69_Male",]) > 1000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "60 to 69_Male",]) < 2000){
        observedhazotkm_6069m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "60 to 69_Male",] %>%
          dplyr::filter(row_number() %% 2 == 1)
      } else {
        observedhazotkm_6069m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "60 to 69_Male",]
      }
      

      # 70 to 79 female
      if(nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "70 to 79_Female",]) > 4000){
        observedhazotkm_7079f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "70 to 79_Female",] %>%
          dplyr::filter(row_number() %% 4 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "70 to 79_Female",]) > 2000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "70 to 79_Female",]) < 4000){
        observedhazotkm_7079f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "70 to 79_Female",] %>%
          dplyr::filter(row_number() %% 3 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "70 to 79_Female",]) > 1000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "70 to 79_Female",]) < 2000){
        observedhazotkm_7079f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "70 to 79_Female",] %>%
          dplyr::filter(row_number() %% 2 == 1)
      } else {
        observedhazotkm_7079f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "70 to 79_Female",]
      }
      
      # 70 to 79 male
      if(nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "70 to 79_Male",]) > 4000){
        observedhazotkm_7079m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "70 to 79_Male",] %>%
          dplyr::filter(row_number() %% 4 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "70 to 79_Male",]) > 2000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "70 to 79_Male",]) < 4000){
        observedhazotkm_7079m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "70 to 79_Male",] %>%
          dplyr::filter(row_number() %% 3 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "70 to 79_Male",]) > 1000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "70 to 79_Male",]) < 2000){
        observedhazotkm_7079m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "70 to 79_Male",] %>%
          dplyr::filter(row_number() %% 2 == 1)
      } else {
        observedhazotkm_7079m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "70 to 79_Male",]
      }

      

      # 80+ female
      if(nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "80 +_Female",]) > 4000){
        observedhazotkm_80f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "80 +_Female",] %>%
          dplyr::filter(row_number() %% 4 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "80 +_Female",]) > 2000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "80 +_Female",]) < 4000){
        observedhazotkm_80f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "80 +_Female",] %>%
          dplyr::filter(row_number() %% 3 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "80 +_Female",]) > 1000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "80 +_Female",]) < 2000){
        observedhazotkm_80f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "80 +_Female",] %>%
          dplyr::filter(row_number() %% 2 == 1)
      } else {
        observedhazotkm_80f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "80 +_Female",]
      }
      
      # 80+ male
      if(nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "80 +_Male",]) > 4000){
        observedhazotkm_80m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "80 +_Male",] %>%
          dplyr::filter(row_number() %% 4 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "80 +_Male",]) > 2000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "80 +_Male",]) < 4000){
        observedhazotkm_80m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "80 +_Male",] %>%
          dplyr::filter(row_number() %% 3 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "80 +_Male",]) > 1000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "80 +_Male",]) < 2000){
        observedhazotkm_80m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "80 +_Male",] %>%
          dplyr::filter(row_number() %% 2 == 1)
      } else {
        observedhazotkm_80m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "80 +_Male",]
      }


      observedhazotKM_age_sex[[j]] <- bind_rows(
        observedhazotkm_1839f,
        observedhazotkm_1839m,
        observedhazotkm_4049f,
        observedhazotkm_4049m,
        observedhazotkm_5059f,
        observedhazotkm_5059m,
        observedhazotkm_6069f,
        observedhazotkm_6069m,
        observedhazotkm_7079f,
        observedhazotkm_7079m,
        observedhazotkm_80f,
        observedhazotkm_80m) %>% 
        dplyr::select(-c(sex_age_gp))
      
      rm(modelhot)
    }
    

    print(paste0("Hazard over time results ", Sys.time()," for ",outcome_cohorts$cohort_name[j], " age*sex strat completed"))

    # risk table ----
    grid <- seq(0,floor(max(data$time_years)),by=0.5) # get the number of years
    grid <-  grid[(str_detect(grid, "[1-9]\\.5", negate = TRUE )) & (str_detect(grid, "10.5", negate = TRUE )) &
                    (str_detect(grid, "20.5", negate = TRUE )) & (str_detect(grid, "30.5", negate = TRUE ))] # remove all the half years apart from the first half year
    
    sprob <- survival::survfit(Surv(time_years, status) ~ sex_age_gp, data=data) %>% 
      summary(times = grid, extend = TRUE)
    cols <- lapply(c(2:16) , function(x) sprob[x])
    
    kmagesex <- do.call(data.frame, cols) %>%
      dplyr::select(c(n.risk, n.event, n.censor, strata)) %>% 
      dplyr::mutate(strata = str_replace(strata, "sex_age_gp=", ""))
    
    
    # risk tables for different age groups
    kmagesexgp <- list()
    
    for(k in 1: length(table(kmagesex$strata))) {
      
      kmagesexgp[[k]] <- kmagesex %>%
        dplyr::filter(strata == names(table(kmagesex$strata)[k])) %>%
        dplyr::select(!c(strata)) %>%
        t() %>%
        as_tibble() %>%
        `colnames<-`(grid) %>%
        dplyr::mutate(Method = "Kaplan-Meier",
               Cancer = outcome_cohorts$cohort_name[j],
               agesex = names(table(kmagesex$strata)[k]) ,
               details = c("n.risk", "n.event", "n.censor")) %>%
            tidyr::separate(col = "agesex",
                     into = c("Age", "Sex"),
                     sep = "_") %>% 
        
        relocate(details) 
      
    }
  
    # bind results for age*sex groups
    observedrisktableKM_age_sex[[j]] <- bind_rows(kmagesexgp)
    
    # KM median survival---
    modelKM <- survival::survfit(Surv(time_years, status) ~ sex_age_gp, data=data) %>%
      summary()
    
    medianKM <- modelKM$table %>%
      as.data.frame() %>%
      tibble::rownames_to_column() %>%  
      dplyr::rename(agesex = rowname, n = records, se =`se(rmean)`) %>% 
      dplyr::mutate(rmean = round(rmean, 4),
             median = round(median, 4),
             `0.95LCL` = round(`0.95LCL`, 4),
             `0.95UCL` = round(`0.95UCL`, 4),
             agesex = str_replace(agesex, "sex_age_gp=", ""),
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
    
    
    # Extract rmean at 10 years
    model_rm <- survival::survfit(Surv(time_years, status) ~ sex_age_gp, data=data)
    rmean10 <- survival:::survmean(model_rm, rmean=c(10))$matrix %>% 
      as.data.frame() %>% 
      tibble::rownames_to_column() %>%  
      dplyr::select(rmean, `se(rmean)`, rowname) %>% 
      dplyr::rename(rmean10yr = rmean, se10yr =`se(rmean)`, agesex = rowname) %>% 
      dplyr::mutate(agesex = str_replace(agesex, "sex_age_gp=", "") ,
             "rmean 10yrs in years (SE)"= ifelse(!is.na(rmean10yr),
                                                 paste0(paste0(nice.num2(rmean10yr)), " (",
                                                        paste0(nice.num2(se10yr)), ")"),
                                                 NA)) 
    
    print(paste0("Median survival from KM from observed data ", Sys.time()," for ",outcome_cohorts$cohort_name[j], " completed"))
    
    # survival probabilities ----
    surprobsKM <- do.call(data.frame, cols) %>%
      dplyr::select(c(time, surv, lower, upper, strata)) %>% 
      dplyr::filter(time == 1 | time == 5 | time == 10 ) %>% 
      dplyr::mutate(surv = round((surv*100),4),
             lower = round((lower*100),4),
             upper = round((upper*100),4),
             strata = str_replace(strata, "sex_age_gp=", ""),
             "Survival Rate % (95% CI)"= ifelse(!is.na(surv),
                                                paste0(paste0(nice.num1(surv)), " (",
                                                       paste0(nice.num1(lower)),"-",
                                                       paste0(nice.num1(upper)), ")"),
                                                NA)) %>% 
      dplyr::select(-c(lower, upper)) %>% 
      dplyr::rename(agesex = strata) %>% 
      tidyr::pivot_wider(names_from = time, 
                  values_from = c(`Survival Rate % (95% CI)`, surv),
                  names_prefix = " year ",
                  names_sep = "")
    
    observedmedianKM_age_sex[[j]] <- dplyr::inner_join(medianKM, rmean10, by = "agesex") %>% 
      dplyr::inner_join(surprobsKM, by = "agesex")

    observedmedianKM_age_sex[[j]] <- observedmedianKM_age_sex[[j]] %>% 
      dplyr::mutate(Method = "Kaplan-Meier", 
             Cancer = outcome_cohorts$cohort_name[j]  ) %>% 
    tidyr::separate(col = "agesex",
             into = c("Age", "Sex"),
             sep = "_",
             remove = T) 
    
    rm(surprobsKM,medianKM,rmean10,model_rm,modelKM)
    
    print(paste0("Survival for 1, 5 and 10 years from observed data ", Sys.time()," for ",outcome_cohorts$cohort_name[j], " completed"))

  } else {
    
print(paste0("age*sex stratification KM analysis not carried out for ", outcome_cohorts$cohort_name[j], " due to only 1 sex present age stratification will have results " , Sys.time()))

  }
  
}

# take the results from a list (one element for each cancer) and put into dataframe ----
observedkmcombined_age_sex <- dplyr::bind_rows(observedkm_age_sex) %>%
  dplyr::rename(est = estimate ,ucl = conf.high, lcl = conf.low ) %>%
  dplyr::mutate(Stratification = "agesex", Adjustment = "None")

medkmcombined_age_sex <- dplyr::bind_rows(observedmedianKM_age_sex) %>%
  dplyr::mutate(Stratification = "agesex", Adjustment = "None")

hotkmcombined_age_sex <- dplyr::bind_rows(observedhazotKM_age_sex) %>%
  dplyr::rename(est = hazard, ucl = upper.ci, lcl = lower.ci ) %>%
  dplyr::mutate(Stratification = "agesex", Adjustment = "None")

#generate the risk table 
risktableskm_age_sex <- dplyr::bind_rows(observedrisktableKM_age_sex) %>%
dplyr::filter(details != "n.censor") %>% 
  dplyr::mutate(Stratification = "agesex", Adjustment = "None") %>% 
  dplyr::mutate(across(everything(), ~replace(., .==  0 , NA))) %>%
  dplyr::mutate(across(where(is.numeric), ~replace(., .<  10 , "<10"))) %>% 
  dplyr::mutate(across(everything(), as.character)) %>%
  replace(is.na(.), "0")

toc(func.toc=toc_min)

info(logger, 'KM analysis for AGE*SEX COMPLETE')