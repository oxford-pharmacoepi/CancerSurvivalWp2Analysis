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
    filter(cohort_definition_id == j) 
  
  #age levels
  agesexlevels <- data %>%
    group_by(sex_age_gp) %>% summarise(count = n())
  
  #determines if both sexes in the data
  sexlevels <- data %>%
    group_by(sex) %>% summarise(count = n()) %>% tally()
  
  if(sexlevels == 2){
    
    # carry out km estimate --- 
    observedkm_age_sex[[j]] <- survfit (Surv(time_years, status) ~ sex_age_gp, data=data) %>%
      tidy() %>%
      mutate(Method = "Kaplan-Meier", 
             Cancer = outcome_cohorts$cohort_name[j], 
             strata = str_replace(strata, "sex_age_gp=", "")) %>%  
             separate(col = "strata",
                      into = c("Age", "Sex"),
                      sep = "_")
    
    print(paste0("KM for observed data age*sex strat ", Sys.time()," for ",outcome_cohorts$cohort_name[j], " completed"))
    
    # hazard over time ---
    # paper https://arxiv.org/pdf/1509.03253.pdf states bshazard good package
    # this can fall over with small sample numbers therefore trycatch is in place which tries to perform it and if errors
    # removes age group sequentially.
    print(paste0("Trying Hazard over time results for all age*sex groups ", Sys.time()," for ",outcome_cohorts$cohort_name[j]))
    
    tryCatch( {
        modelhot <- group_by(data, sex_age_gp) %>% 
          do(as.data.frame(bshazard(Surv(time_years, status)~1, data=., verbose=FALSE))) %>% 
          ungroup %>%
          mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohort_name[j]) %>% 
          separate(col = "sex_age_gp",
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
        filter(sex_age_gp != "18 to 39_Female") 
      
      tryCatch({
          modelhot <- group_by(data, sex_age_gp) %>% 
            do(as.data.frame(bshazard(Surv(time_years, status)~1, data=., verbose=FALSE))) %>% 
            ungroup %>%
            mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohort_name[j]) %>% 
            separate(col = "sex_age_gp",
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
        filter(sex_age_gp != "18 to 39_Female") %>% 
        filter(sex_age_gp != "18 to 39_Male")
      
      tryCatch( {
          modelhot <- group_by(data, sex_age_gp) %>% 
            do(as.data.frame(bshazard(Surv(time_years, status)~1, data=., verbose=FALSE))) %>% 
            ungroup %>%
            mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohort_name[j]) %>% 
            separate(col = "sex_age_gp",
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
        filter(sex_age_gp != "18 to 39_Female") %>% 
        filter(sex_age_gp != "18 to 39_Male") %>% 
        filter(sex_age_gp != "40 to 49_Female")
      
      tryCatch( {
          modelhot <- group_by(data, sex_age_gp) %>% 
            do(as.data.frame(bshazard(Surv(time_years, status)~1, data=., verbose=FALSE))) %>% 
            ungroup %>%
            mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohort_name[j]) %>% 
            separate(col = "sex_age_gp",
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
        filter(sex_age_gp != "18 to 39_Female") %>% 
        filter(sex_age_gp != "18 to 39_Male") %>% 
        filter(sex_age_gp != "40 to 49_Female") %>% 
        filter(sex_age_gp != "40 to 49_Male")
      
      tryCatch( {
          modelhot <- group_by(data, sex_age_gp) %>% 
            do(as.data.frame(bshazard(Surv(time_years, status)~1, data=., verbose=FALSE))) %>% 
            ungroup %>%
            mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohort_name[j]) %>% 
            separate(col = "sex_age_gp",
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
        filter(sex_age_gp != "18 to 39_Female") %>% 
        filter(sex_age_gp != "18 to 39_Male") %>% 
        filter(sex_age_gp != "40 to 49_Female") %>% 
        filter(sex_age_gp != "40 to 49_Male") %>% 
        filter(sex_age_gp != "50 to 59_Female")
      
      
      tryCatch({
          modelhot <- group_by(data, sex_age_gp) %>% 
            do(as.data.frame(bshazard(Surv(time_years, status)~1, data=., verbose=FALSE))) %>% 
            ungroup %>%
            mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohort_name[j]) %>% 
            separate(col = "sex_age_gp",
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
        filter(sex_age_gp != "18 to 39_Female") %>% 
        filter(sex_age_gp != "18 to 39_Male") %>% 
        filter(sex_age_gp != "40 to 49_Female") %>% 
        filter(sex_age_gp != "40 to 49_Male") %>% 
        filter(sex_age_gp != "50 to 59_Female") %>% 
        filter(sex_age_gp != "50 to 59_Male")
      
      tryCatch({modelhot <- group_by(data, sex_age_gp) %>% 
            do(as.data.frame(bshazard(Surv(time_years, status)~1, data=., verbose=FALSE))) %>% 
            ungroup %>%
            mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohort_name[j]) %>% 
            separate(col = "sex_age_gp",
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
        filter(sex_age_gp != "18 to 39_Female") %>% 
        filter(sex_age_gp != "18 to 39_Male") %>% 
        filter(sex_age_gp != "40 to 49_Female") %>% 
        filter(sex_age_gp != "40 to 49_Male") %>% 
        filter(sex_age_gp != "50 to 59_Female") %>% 
        filter(sex_age_gp != "50 to 59_Male") %>% 
        filter(sex_age_gp != "60 to 69_Female")
      
      tryCatch({
        modelhot <- group_by(data, sex_age_gp) %>% 
          do(as.data.frame(bshazard(Surv(time_years, status)~1, data=., verbose=FALSE))) %>% 
          ungroup %>%
          mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohort_name[j]) %>% 
          separate(col = "sex_age_gp",
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
        filter(sex_age_gp != "18 to 39_Female") %>% 
        filter(sex_age_gp != "18 to 39_Male") %>% 
        filter(sex_age_gp != "40 to 49_Female") %>% 
        filter(sex_age_gp != "40 to 49_Male") %>% 
        filter(sex_age_gp != "50 to 59_Female") %>% 
        filter(sex_age_gp != "50 to 59_Male") %>% 
        filter(sex_age_gp != "60 to 69_Female") %>% 
        filter(sex_age_gp != "60 to 69_Male")
      
      tryCatch({ modelhot <- group_by(data, sex_age_gp) %>% 
          do(as.data.frame(bshazard(Surv(time_years, status)~1, data=., verbose=FALSE))) %>% 
          ungroup %>%
          mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohort_name[j]) %>% 
        separate(col = "sex_age_gp",
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
      print(paste0("Hazard over time results ", Sys.time()," for ",outcome_cohorts$cohort_name[j], " age * strat completed")) 
      
    } else {
      
      print(paste0("Trying Hazard over time results again removing 70 to 79 year old female age group ", Sys.time()," for ",outcome_cohorts$cohort_name[j]))
      
      data <- data %>% 
        filter(sex_age_gp != "18 to 39_Female") %>% 
        filter(sex_age_gp != "18 to 39_Male") %>% 
        filter(sex_age_gp != "40 to 49_Female") %>% 
        filter(sex_age_gp != "40 to 49_Male") %>% 
        filter(sex_age_gp != "50 to 59_Female") %>% 
        filter(sex_age_gp != "50 to 59_Male") %>% 
        filter(sex_age_gp != "60 to 69_Female") %>% 
        filter(sex_age_gp != "60 to 69_Male") %>% 
        filter(sex_age_gp != "70 to 79_Female")
      
      tryCatch({modelhot <- group_by(data, sex_age_gp) %>% 
          do(as.data.frame(bshazard(Surv(time_years, status)~1, data=., verbose=FALSE))) %>% 
          ungroup %>%
          mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohort_name[j]) %>% 
        separate(col = "sex_age_gp",
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
        filter(sex_age_gp != "18 to 39_Female") %>% 
        filter(sex_age_gp != "18 to 39_Male") %>% 
        filter(sex_age_gp != "40 to 49_Female") %>% 
        filter(sex_age_gp != "40 to 49_Male") %>% 
        filter(sex_age_gp != "50 to 59_Female") %>% 
        filter(sex_age_gp != "50 to 59_Male") %>% 
        filter(sex_age_gp != "60 to 69_Female") %>% 
        filter(sex_age_gp != "60 to 69_Male") %>% 
        filter(sex_age_gp != "70 to 79_Female") %>% 
        filter(sex_age_gp != "70 to 79_Male")
        
      
      tryCatch({
        modelhot <- group_by(data, sex_age_gp) %>% 
          do(as.data.frame(bshazard(Surv(time_years, status)~1, data=., verbose=FALSE))) %>% 
          ungroup %>%
          mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohort_name[j]) %>% 
          separate(col = "sex_age_gp",
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
      if(nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "18 to 39_Female",]) > 6000){
        observedhazotkm_1839f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "18 to 39_Female",] %>%
          filter(row_number() %% 4 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "18 to 39_Female",]) > 3000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "18 to 39_Female",]) < 6000){
        observedhazotkm_1839f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "18 to 39_Female",] %>%
          filter(row_number() %% 3 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "18 to 39_Female",]) > 2000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "18 to 39_Female",]) < 3000){
        observedhazotkm_1839f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "18 to 39_Female",] %>%
          filter(row_number() %% 2 == 1)
      } else {
        observedhazotkm_1839f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "18 to 39_Female",]
      }
      
      # 18 to 39 male
      if(nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "18 to 39_Male",]) > 6000){
        observedhazotkm_1839m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "18 to 39_Male",] %>%
          filter(row_number() %% 4 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "18 to 39_Male",]) > 3000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "18 to 39_Male",]) < 6000){
        observedhazotkm_1839m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "18 to 39_Male",] %>%
          filter(row_number() %% 3 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "18 to 39_Male",]) > 2000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "18 to 39_Male",]) < 3000){
        observedhazotkm_1839m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "18 to 39_Male",] %>%
          filter(row_number() %% 2 == 1)
      } else {
        observedhazotkm_1839m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "18 to 39_Male",]
      }
      

      # 40 to 49 female
      if(nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "40 to 49_Female",]) > 6000){
        observedhazotkm_4049f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "40 to 49_Female",] %>%
          filter(row_number() %% 4 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "40 to 49_Female",]) > 3000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "40 to 49_Female",]) < 6000){
        observedhazotkm_4049f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "40 to 49_Female",] %>%
          filter(row_number() %% 3 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "40 to 49_Female",]) > 2000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "40 to 49_Female",]) < 3000){
        observedhazotkm_4049f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "40 to 49_Female",] %>%
          filter(row_number() %% 2 == 1)
      } else {
        observedhazotkm_4049f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "40 to 49_Female",]
      }
      
      # 40 to 49 male
      if(nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "40 to 49_Male",]) > 6000){
        observedhazotkm_4049m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "40 to 49_Male",] %>%
          filter(row_number() %% 4 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "40 to 49_Male",]) > 3000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "40 to 49_Male",]) < 6000){
        observedhazotkm_4049m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "40 to 49_Male",] %>%
          filter(row_number() %% 3 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "40 to 49_Male",]) > 2000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "40 to 49_Male",]) < 3000){
        observedhazotkm_4049m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "40 to 49_Male",] %>%
          filter(row_number() %% 2 == 1)
      } else {
        observedhazotkm_4049m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "40 to 49_Male",]
      }


      # 50 to 59 female
      if(nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "50 to 59_Female",]) > 6000){
        observedhazotkm_5059f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "50 to 59_Female",] %>%
          filter(row_number() %% 4 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "50 to 59_Female",]) > 3000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "50 to 59_Female",]) < 6000){
        observedhazotkm_5059f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "50 to 59_Female",] %>%
          filter(row_number() %% 3 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "50 to 59_Female",]) > 2000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "50 to 59_Female",]) < 3000){
        observedhazotkm_5059f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "50 to 59_Female",] %>%
          filter(row_number() %% 2 == 1)
      } else {
        observedhazotkm_5059f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "50 to 59_Female",]
      }
      
      # 50 to 59 male
      if(nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "50 to 59_Male",]) > 6000){
        observedhazotkm_5059m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "50 to 59_Male",] %>%
          filter(row_number() %% 4 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "50 to 59_Male",]) > 3000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "50 to 59_Male",]) < 6000){
        observedhazotkm_5059m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "50 to 59_Male",] %>%
          filter(row_number() %% 3 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "50 to 59_Male",]) > 2000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "50 to 59_Male",]) < 3000){
        observedhazotkm_5059m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "50 to 59_Male",] %>%
          filter(row_number() %% 2 == 1)
      } else {
        observedhazotkm_5059m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "50 to 59_Male",]
      }
      
      

      # 60 to 69 female
      if(nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "60 to 69_Female",]) > 6000){
        observedhazotkm_6069f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "60 to 69_Female",] %>%
          filter(row_number() %% 4 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "60 to 69_Female",]) > 3000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "60 to 69_Female",]) < 6000){
        observedhazotkm_6069f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "60 to 69_Female",] %>%
          filter(row_number() %% 3 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "60 to 69_Female",]) > 2000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "60 to 69_Female",]) < 3000){
        observedhazotkm_6069f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "60 to 69_Female",] %>%
          filter(row_number() %% 2 == 1)
      } else {
        observedhazotkm_6069f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "60 to 69_Female",]
      }
      
      # 60 to 69 male
      if(nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "60 to 69_Male",]) > 6000){
        observedhazotkm_6069m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "60 to 69_Male",] %>%
          filter(row_number() %% 4 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "60 to 69_Male",]) > 3000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "60 to 69_Male",]) < 6000){
        observedhazotkm_6069m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "60 to 69_Male",] %>%
          filter(row_number() %% 3 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "60 to 69_Male",]) > 2000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "60 to 69_Male",]) < 3000){
        observedhazotkm_6069m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "60 to 69_Male",] %>%
          filter(row_number() %% 2 == 1)
      } else {
        observedhazotkm_6069m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "60 to 69_Male",]
      }
      

      # 70 to 79 female
      if(nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "70 to 79_Female",]) > 6000){
        observedhazotkm_7079f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "70 to 79_Female",] %>%
          filter(row_number() %% 4 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "70 to 79_Female",]) > 3000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "70 to 79_Female",]) < 6000){
        observedhazotkm_7079f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "70 to 79_Female",] %>%
          filter(row_number() %% 3 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "70 to 79_Female",]) > 2000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "70 to 79_Female",]) < 3000){
        observedhazotkm_7079f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "70 to 79_Female",] %>%
          filter(row_number() %% 2 == 1)
      } else {
        observedhazotkm_7079f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "70 to 79_Female",]
      }
      
      # 70 to 79 male
      if(nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "70 to 79_Male",]) > 6000){
        observedhazotkm_7079m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "70 to 79_Male",] %>%
          filter(row_number() %% 4 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "70 to 79_Male",]) > 3000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "70 to 79_Male",]) < 6000){
        observedhazotkm_7079m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "70 to 79_Male",] %>%
          filter(row_number() %% 3 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "70 to 79_Male",]) > 2000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "70 to 79_Male",]) < 3000){
        observedhazotkm_7079m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "70 to 79_Male",] %>%
          filter(row_number() %% 2 == 1)
      } else {
        observedhazotkm_7079m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "70 to 79_Male",]
      }

      

      # 80+ female
      if(nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "80 +_Female",]) > 6000){
        observedhazotkm_80f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "80 +_Female",] %>%
          filter(row_number() %% 4 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "80 +_Female",]) > 3000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "80 +_Female",]) < 6000){
        observedhazotkm_80f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "80 +_Female",] %>%
          filter(row_number() %% 3 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "80 +_Female",]) > 2000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "80 +_Female",]) < 3000){
        observedhazotkm_80f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "80 +_Female",] %>%
          filter(row_number() %% 2 == 1)
      } else {
        observedhazotkm_80f <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "80 +_Female",]
      }
      
      # 80+ male
      if(nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "80 +_Male",]) > 6000){
        observedhazotkm_80m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "80 +_Male",] %>%
          filter(row_number() %% 4 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "80 +_Male",]) > 3000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "80 +_Male",]) < 6000){
        observedhazotkm_80m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "80 +_Male",] %>%
          filter(row_number() %% 3 == 1)
      } else if (nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "80 +_Male",]) > 2000 &
                 nrow(observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "80 +_Male",]) < 3000){
        observedhazotkm_80m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "80 +_Male",] %>%
          filter(row_number() %% 2 == 1)
      } else {
        observedhazotkm_80m <- observedhazotKM_age_sex[[j]][observedhazotKM_age_sex[[j]]$sex_age_gp == "80 +_Male",]
      }


      observedhazotKM_age[[j]] <- bind_rows(
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
        observedhazotkm_80m)  
      
      rm(modelhot)
    }
    

    print(paste0("Hazard over time results ", Sys.time()," for ",outcome_cohorts$cohort_name[j], " age*sex strat completed"))

    # risk table ----
    grid <- seq(0,floor(max(data$time_years)),by=0.5) # get the number of years
    grid <-  grid[(str_detect(grid, "[1-9]\\.5", negate = TRUE )) & (str_detect(grid, "10.5", negate = TRUE )) &
                    (str_detect(grid, "20.5", negate = TRUE )) & (str_detect(grid, "30.5", negate = TRUE ))] # remove all the half years apart from the first half year
    
    sprob <- survfit(Surv(time_years, status) ~ sex_age_gp, data=data) %>% 
      summary(times = grid, extend = TRUE)
    cols <- lapply(c(2:16) , function(x) sprob[x])
    
    kmagesex <- do.call(data.frame, cols) %>%
      select(c(n.risk, n.event, n.censor, strata)) %>% 
      mutate(strata = str_replace(strata, "sex_age_gp=", "")) %>% 
      separate(col = "strata",
               into = c("Age", "Sex"),
               sep = "_",
               remove = F)

    # risk tables for different age*sex groups
    kmagesexgp <- list()
    
    for(k in 1: length(table(kmagesex$strata))) {
      
      kmagesexgp[[k]] <- kmagesex %>%
      filter(strata == names(table(kmagesex$strata)[k])) %>%
      select(!c(Age, Sex, strata)) %>%
      t() %>%
      as_tibble() %>%
      `colnames<-`(grid) %>%
      mutate(Method = "Kaplan-Meier",
             Cancer = outcome_cohorts$cohort_name[j],
             agesex = names(table(kmagesex$strata)[k]) ,
             details = c("n.risk", "n.event", "n.censor")) %>%
      relocate(details) %>% 
        separate(col = "agesex",
                 into = c("Age", "Sex"),
                 sep = "_")
      
    }
    
    # bind results for age*sex groups
    observedrisktableKM_age_sex[[j]] <- bind_rows(kmagesexgp)
    
    # KM median survival---
    modelKM <- survfit(Surv(time_years, status) ~ sex_age_gp, data=data) %>%
      summary()
    
    medianKM <- modelKM$table %>%
      as.data.frame() %>%
      tibble::rownames_to_column() %>%  
      rename(agesex = rowname, n = records, se =`se(rmean)`) %>% 
      mutate(rmean = round(rmean, 4),
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
      separate(col = "agesex",
               into = c("Age", "Sex"),
               sep = "_",
               remove = F) %>% 
      select(-c(`0.95LCL`,`0.95UCL`, n.max, n.start, se)) %>% 
      mutate(n  = replace(n, n ==  0 , NA),
             events = replace(events, events ==  0 , NA)) %>%
      mutate(n  = replace(n, n <=  10 , "<10"),
             events  = replace(events, events <=  10 , "<10"))  %>%
      mutate(n  = replace_na(n, "0"),
             events  = replace_na(events, "0")) %>% 
      mutate(n = as.character(n),
             events = as.character(events))
    
    print(paste0("Median survival from KM from observed data ", Sys.time()," for ",outcome_cohorts$cohort_name[j], " completed"))
    
    # survival probabilities ----
    surprobsKM <- do.call(data.frame, cols) %>%
      select(c(time, surv, lower, upper, strata)) %>% 
      filter(time == 1 | time == 5 | time == 10 ) %>% 
      mutate(surv = round((surv*100),4),
             lower = round((lower*100),4),
             upper = round((upper*100),4),
             strata = str_replace(strata, "sex_age_gp=", ""),
             "Survival Rate % (95% CI)"= ifelse(!is.na(surv),
                                                paste0(paste0(nice.num1(surv)), " (",
                                                       paste0(nice.num1(lower)),"-",
                                                       paste0(nice.num1(upper)), ")"),
                                                NA)) %>% 
      select(-c(lower, upper)) %>% 
      separate(col = "strata",
               into = c("Age", "Sex"),
               sep = "_",
               remove = F) %>% 
      rename(agesex = strata) %>% 
      pivot_wider(names_from = time, 
                  values_from = c(`Survival Rate % (95% CI)`, surv),
                  names_prefix = " year ",
                  names_sep = "")
    
    observedmedianKM_age_sex[[j]] <- inner_join(medianKM, surprobsKM, by = c("agesex", "Age", "Sex"))
    observedmedianKM_age_sex[[j]] <- observedmedianKM_age_sex[[j]] %>% 
      mutate(Method = "Kaplan-Meier", 
             Cancer = outcome_cohorts$cohort_name[j]  )
    
    rm(surprobsKM, medianKM)
    
    print(paste0("Survival for 1, 5 and 10 years from observed data ", Sys.time()," for ",outcome_cohorts$cohort_name[j], " completed"))

  } else {
    
print(paste0("age*sex stratification KM analysis not carried out for ", outcome_cohorts$cohort_name[j], " due to only 1 sex present age stratification will have results " , Sys.time()))

  }
  
}

# take the results from a list (one element for each cancer) and put into dataframe ----
observedkmcombined_age_sex <- dplyr::bind_rows(observedkm_age_sex) %>%
  rename(est = estimate ,ucl = conf.high, lcl = conf.low ) %>%
  mutate(Stratification = "agesex", Adjustment = "None")

medkmcombined_age_sex <- dplyr::bind_rows(observedmedianKM_age_sex) %>%
  mutate(Stratification = "agesex", Adjustment = "None")

hotkmcombined_age_sex <- dplyr::bind_rows(observedhazotKM_age_sex) %>%
  rename(est = hazard, ucl = upper.ci, lcl = lower.ci, sexAge = sex_age_gp ) %>%
  mutate(Stratification = "agesex", Adjustment = "None")

#generate the risk table 
risktableskm_age_sex <- dplyr::bind_rows(observedrisktableKM_age_sex) %>%
filter(details != "n.censor") %>% 
  mutate(Stratification = "agesex", Adjustment = "None") %>% 
  mutate(across(everything(), ~replace(., .==  0 , NA))) %>%
  mutate(across(where(is.numeric), ~replace(., .<  10 , "<10"))) %>% 
  mutate(across(everything(), as.character)) %>%
  replace(is.na(.), "0")

toc(func.toc=toc_min)

info(logger, 'KM analysis for AGE*SEX COMPLETE')