#################################################
# SEX ANALYSIS
#################################################

# km survival, risk table, median survival, hazard over time from the observed data for each cancer ----
tic("KM analysis for sex population")
info(logger, 'KM analysis for sex population START')

# capture output in list
observedkm_sex <- list()
observedmedianKM_sex <- list()
observedhazotKM_sex <- list()
observedrisktableKM_sex <- list()

# loop to carry out for each cancer
for(j in 1:nrow(outcome_cohorts)) {
  
  #subset the data by cancer type
  data <- Pop %>%
    filter(cohort_definition_id == j)
  
  #creates a test that determines if both sexs in the data and runs if only two
  sexlevels <- data %>%
    group_by(sex) %>% summarise(count = n()) %>% tally()

  if(sexlevels == 2){
    
    #carry out km estimate
    observedkm_sex[[j]] <- survfit (Surv(time_years, status) ~ sex, data=data) %>%
      tidy() %>%
      rename(Sex = strata) %>%
      mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohort_name[j], Age = "All", Sex = str_replace(Sex, "sex=Male", "Male"), 
             Sex = str_replace(Sex,"sex=Female", "Female")) 
    
    # reduce the size of KM for plotting
    if(nrow(observedkm_sex[[j]][observedkm_sex[[j]]$Sex == "Female",]) > 4000){
      observedkm_female <- observedkm_sex[[j]][observedkm_sex[[j]]$Sex == "Female",] %>%
        filter(row_number() %% 4 == 1)
    } else if (nrow(observedkm_sex[[j]][observedkm_sex[[j]]$Sex == "Female",]) > 2000 &
               nrow(observedkm_sex[[j]][observedkm_sex[[j]]$Sex == "Female",]) < 4000){
      observedkm_female <- observedkm_sex[[j]][observedkm_sex[[j]]$Sex == "Female",] %>%
        filter(row_number() %% 3 == 1)
    } else if (nrow(observedkm_sex[[j]][observedkm_sex[[j]]$Sex == "Female",]) > 1000 &
               nrow(observedkm_sex[[j]][observedkm_sex[[j]]$Sex == "Female",]) < 2000){
      observedkm_female <- observedkm_sex[[j]][observedkm_sex[[j]]$Sex == "Female",] %>%
        filter(row_number() %% 2 == 1)
    } else {
      observedkm_female <- observedkm_sex[[j]][observedkm_sex[[j]]$Sex == "Female",]
    }

    if(nrow(observedkm_sex[[j]][observedkm_sex[[j]]$Sex == "Male",]) > 4000){
      observedkm_male <- observedkm_sex[[j]][observedkm_sex[[j]]$Sex == "Male",] %>%
        filter(row_number() %% 4 == 1)
    } else if (nrow(observedkm_sex[[j]][observedkm_sex[[j]]$Sex == "Male",]) > 2000 &
               nrow(observedkm_sex[[j]][observedkm_sex[[j]]$Sex == "Male",]) < 4000){
      observedkm_male <- observedkm_sex[[j]][observedkm_sex[[j]]$Sex == "Male",] %>%
        filter(row_number() %% 3 == 1)
    } else if (nrow(observedkm_sex[[j]][observedkm_sex[[j]]$Sex == "Male",]) > 1000 &
               nrow(observedkm_sex[[j]][observedkm_sex[[j]]$Sex == "Male",]) < 2000){
      observedkm_male <- observedkm_sex[[j]][observedkm_sex[[j]]$Sex == "Male",] %>%
        filter(row_number() %% 2 == 1)
    } else {
      observedkm_male <- observedkm_sex[[j]][observedkm_sex[[j]]$Sex == "Male",]
    }

    observedkm_sex[[j]] <- bind_rows(observedkm_female, observedkm_male)
    
      print(paste0("KM for observed data ", Sys.time()," for ",outcome_cohorts$cohort_name[j], " completed"))
        

    # get risk table for specific times for each sex then combine again ---
      grid <- seq(0,floor(max(data$time_years)),by=0.5) #get the number of years
      grid <-  grid[(str_detect(grid, "[1-9]\\.5", negate = TRUE )) & (str_detect(grid, "10.5", negate = TRUE )) &
                      (str_detect(grid, "20.5", negate = TRUE )) & (str_detect(grid, "30.5", negate = TRUE ))] # remove all the half years apart from the first half year
      sprob <- survfit(Surv(time_years, status) ~ sex, data=data) %>% 
        summary(times = grid, extend = TRUE)
      cols <- lapply(c(2:16) , function(x) sprob[x])
      
        kmsex <- do.call(data.frame, cols) %>%
        select(c(n.risk, n.event, n.censor, strata)) %>% 
        rename(Sex = strata) %>% 
        mutate(Sex = str_replace(Sex, "sex=Male", "Male"), Sex = str_replace(Sex,"sex=Female", "Female"))

      # risk table for females       
        kmsex_f <- kmsex %>% 
          filter(Sex == "Female") %>% 
          select(!c(Sex)) %>% 
          t() %>% 
          as_tibble() %>% 
          `colnames<-`(grid) %>%
          mutate(Method = "Kaplan-Meier", 
                 Cancer = outcome_cohorts$cohort_name[j],
                 Age = "All",
                 Sex = "Female" ,
                 details = c("n.risk", "n.event", "n.censor")) %>% 
          relocate(details)

       # risk table for males
        kmsex_m <- kmsex %>% 
          filter(Sex == "Male") %>% 
          select(!c(Sex)) %>% 
          t() %>% 
          as_tibble() %>% 
          `colnames<-`(grid) %>%
          mutate(Method = "Kaplan-Meier", 
                 Cancer = outcome_cohorts$cohort_name[j],
                 Age = "All",
                 Sex = "Male" ,
                 details = c("n.risk", "n.event", "n.censor")) %>% 
          relocate(details)       
        
        # bind results for both sexes
        observedrisktableKM_sex[[j]] <- bind_rows(kmsex_f, kmsex_m )
      
      print(paste0("Extract risk table ", Sys.time()," for ",outcome_cohorts$cohort_name[j], " completed"))
      
    # survival probabilities ----
    surprobsKM <- do.call(data.frame, cols) %>%
      select(c(time, surv, lower, upper, strata)) %>% 
      rename(Sex = strata) %>% 
      filter(time == 1 | time == 5 | time == 10 ) %>% 
      mutate(Sex = str_replace(Sex, "sex=Male", "Male"), 
             Sex = str_replace(Sex,"sex=Female", "Female")) %>% 
      mutate(surv = round((surv*100),4),
             lower = round((lower*100),4),
             upper = round((upper*100),4),
             "Survival Rate % (95% CI)"= ifelse(!is.na(surv),
                                                paste0(paste0(nice.num1(surv)), " (",
                                                       paste0(nice.num1(lower)),"-",
                                                       paste0(nice.num1(upper)), ")"),
                                                NA)) %>% 
      select(-c(lower, upper)) %>% 
      pivot_wider(names_from = time, 
                  values_from = c(`Survival Rate % (95% CI)`, surv),
                  names_prefix = " year ",
                  names_sep = "")
    
      # KM median survival ---
      modelKM <- survfit(Surv(time_years, status) ~ sex, data=data) %>%
        summary()
      
      medianKM <- modelKM$table %>%
        as.data.frame() %>%
        tibble::rownames_to_column() %>%  
        rename(Sex = rowname, n = records, se =`se(rmean)`) %>% 
        mutate(rmean = round(rmean, 4),
               median = round(median, 4),
               Sex = str_replace(Sex, "sex=Male", "Male"), 
               Sex = str_replace(Sex,"sex=Female", "Female"),
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
        select(-c(`0.95LCL`,`0.95UCL`, n.max, n.start, se)) %>% 
        mutate(n  = replace(n, n ==  0 , NA),
               events = replace(events, events ==  0 , NA)) %>%
        mutate(n  = replace(n, n <=  10 , "<10"),
               events  = replace(events, events <=  10 , "<10"))  %>%
        mutate(n  = replace_na(n, "0"),
               events  = replace_na(events, "0")) %>% 
        mutate(n = as.character(n),
               events = as.character(events))
      
      # Extract rmean at 10 years
      model_rm <- survfit(Surv(time_years, status) ~ sex, data=data)
      rmean10 <- survival:::survmean(model_rm, rmean=c(10))$matrix %>% 
        as.data.frame() %>% 
        tibble::rownames_to_column() %>%  
        select(rmean, `se(rmean)`, rowname) %>% 
        rename(rmean10yr = rmean, se10yr =`se(rmean)`, Sex = rowname) %>% 
        mutate(Sex = str_replace(Sex, "sex=Male", "Male"), 
               Sex = str_replace(Sex,"sex=Female", "Female"),
               "rmean 10yrs in years (SE)"= ifelse(!is.na(rmean10yr),
                                                   paste0(paste0(nice.num2(rmean10yr)), " (",
                                                          paste0(nice.num2(se10yr)), ")"),
                                                   NA))
      
      observedmedianKM_sex[[j]] <- inner_join(medianKM, rmean10, by = "Sex") %>% 
        inner_join(surprobsKM, by = "Sex")
      observedmedianKM_sex[[j]] <- observedmedianKM_sex[[j]] %>% 
        mutate(Method = "Kaplan-Meier", 
               Cancer = outcome_cohorts$cohort_name[j] ,
               Age = "All")
      
      rm(surprobsKM,medianKM,rmean10,model_rm,modelKM)
      
      print(paste0("Median and rmean survival from KM from observed data ", Sys.time()," for ",outcome_cohorts$cohort_name[j], " completed"))
  
    
    # hazard over time ---
    # paper https://arxiv.org/pdf/1509.03253.pdf states bshazard good package
    observedhazotKM_sex[[j]] <- group_by(data,sex) %>% 
      do(as.data.frame(bshazard(Surv(time_years, status)~1, data=., verbose=FALSE))) %>% 
      ungroup %>%
      mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohort_name[j], Age = "All") %>% 
      rename(Sex = sex) 
    
    # reduce the size of haz over time for plotting
    if(nrow(observedhazotKM_sex[[j]][observedhazotKM_sex[[j]]$Sex == "Female",]) > 4000){
      observedhazotKM_female <- observedhazotKM_sex[[j]][observedhazotKM_sex[[j]]$Sex == "Female",] %>%
        filter(row_number() %% 4 == 1)
    } else if (nrow(observedhazotKM_sex[[j]][observedhazotKM_sex[[j]]$Sex == "Female",]) > 2000 &
               nrow(observedhazotKM_sex[[j]][observedhazotKM_sex[[j]]$Sex == "Female",]) < 4000){
      observedhazotKM_female <- observedhazotKM_sex[[j]][observedhazotKM_sex[[j]]$Sex == "Female",] %>%
        filter(row_number() %% 3 == 1)
    } else if (nrow(observedhazotKM_sex[[j]][observedhazotKM_sex[[j]]$Sex == "Female",]) > 1000 &
               nrow(observedhazotKM_sex[[j]][observedhazotKM_sex[[j]]$Sex == "Female",]) < 2000){
      observedhazotKM_female <- observedhazotKM_sex[[j]][observedhazotKM_sex[[j]]$Sex == "Female",] %>%
        filter(row_number() %% 2 == 1)
    } else {
      observedhazotKM_female <- observedhazotKM_sex[[j]][observedhazotKM_sex[[j]]$Sex == "Female",]
    }
    
    if(nrow(observedhazotKM_sex[[j]][observedhazotKM_sex[[j]]$Sex == "Male",]) > 4000){
      observedhazotKM_male <- observedhazotKM_sex[[j]][observedhazotKM_sex[[j]]$Sex == "Male",] %>%
        filter(row_number() %% 4 == 1)
    } else if (nrow(observedhazotKM_sex[[j]][observedhazotKM_sex[[j]]$Sex == "Male",]) > 2000 &
               nrow(observedhazotKM_sex[[j]][observedhazotKM_sex[[j]]$Sex == "Male",]) < 4000){
      observedhazotKM_male <- observedhazotKM_sex[[j]][observedhazotKM_sex[[j]]$Sex == "Male",] %>%
        filter(row_number() %% 3 == 1)
    } else if (nrow(observedhazotKM_sex[[j]][observedhazotKM_sex[[j]]$Sex == "Male",]) > 1000 &
               nrow(observedhazotKM_sex[[j]][observedhazotKM_sex[[j]]$Sex == "Male",]) < 2000){
      observedhazotKM_male <- observedhazotKM_sex[[j]][observedhazotKM_sex[[j]]$Sex == "Male",] %>%
        filter(row_number() %% 2 == 1)
    } else {
      observedhazotKM_male <- observedhazotKM_sex[[j]][observedhazotKM_sex[[j]]$Sex == "Male",]
    }
    
    observedhazotKM_sex[[j]] <- bind_rows(observedhazotKM_female, observedhazotKM_male)
    
    
    print(paste0("Hazard over time results ", Sys.time()," for ",outcome_cohorts$cohort_name[j], "sex strat completed"))
    
    
  } else {
    
    print(paste0("sex stratification KM analysis not carried out for ", outcome_cohorts$cohort_name[j], " due to only 1 sex present " , Sys.time()))
    
  }
  
} # this closes the loop on the analysis containing both sexes 
  
# take the results from a list (one element for each cancer) and put into dataframe for KM survival
observedkmcombined_sex <- dplyr::bind_rows(observedkm_sex) %>%
  rename(est = estimate ,ucl = conf.high, lcl = conf.low )  %>%
  mutate(Stratification = "Sex", Adjustment = "None")

medkmcombined_sex <- dplyr::bind_rows(observedmedianKM_sex) %>%
  mutate(Stratification = "Sex", Adjustment = "None")

hotkmcombined_sex <- dplyr::bind_rows(observedhazotKM_sex) %>%
  rename(est = hazard, ucl = upper.ci, lcl = lower.ci ) %>%
  mutate(Stratification = "Sex", Adjustment = "None")

#generate the risk table and remove entries < 10 patients
risktableskm_sex <- dplyr::bind_rows(observedrisktableKM_sex) %>% 
  filter(details != "n.censor") %>% 
  mutate(Stratification = "Sex", Adjustment = "None") %>% 
  mutate(across(everything(), ~replace(., .==  0 , NA))) %>%
  mutate(across(everything(), ~replace(., .<=  10 , "<10"))) %>% 
  mutate(across(everything(), as.character)) %>%
  replace(is.na(.), "0")

###################################################################
# duplicate the results above so can be filtered in the dashboard ---
observedkmcombined_sexA <- dplyr::bind_rows(observedkm_sex) %>%
  rename(est = estimate ,ucl = conf.high, lcl = conf.low )  %>%
  mutate(Stratification = "None", Adjustment = "Sex")

medkmcombined_sexA <- dplyr::bind_rows(observedmedianKM_sex) %>%
  mutate(Stratification = "None", Adjustment = "Sex")

hotkmcombined_sexA <- dplyr::bind_rows(observedhazotKM_sex) %>%
  rename(est = hazard, ucl = upper.ci, lcl = lower.ci ) %>%
  mutate(Stratification = "None", Adjustment = "Sex")

#generate the risk table and remove entries < 10 patients
risktableskm_sexA <- dplyr::bind_rows(observedrisktableKM_sex) %>% 
  filter(details != "n.censor") %>% 
  mutate(Stratification = "None", Adjustment = "Sex") %>% 
  mutate(across(everything(), ~replace(., .==  0 , NA))) %>%
  mutate(across(everything(), ~replace(., .<=  10 , "<10"))) %>% 
  mutate(across(everything(), as.character)) %>%
  replace(is.na(.), "0")

toc(func.toc=toc_min)

info(logger, 'KM analysis for sex stratification COMPLETE')
  
#########################################################
# Extrapolation analysis for sex adjustment ------
#########################################################
tic("Extrapolation analysis for sex adjustment")

info(logger, 'Extrapolation analysis for sex adjustment START')

# Initiate lists to store output within loop ---- 
extrapolations_sex <- list() # extrapolation over time
gof_haz_sex <- list() # goodness of fit
hazot_sex <- list() # hazard over time 
parameters_sex <- list() # parameters from each model
pred_median_mean_sex <- list() # extract the predicted median and RMST, surv prob 1,5,10 from extrapolation methods

for(j in 1:nrow(outcome_cohorts)) { 

  #temp lists to store results
  extrap_results_temp <- list() 
  gof_results_temp <- list() 
  hazot_results_temp <- list() 
  parameters_results_temp <- list()
  pred_median_mean_results_temp <- list()
  
  #subset the data by cancer type
  data <- Pop %>%
    filter(cohort_definition_id == j) 
  
  sexlevels <- data %>%
    group_by(sex) %>% summarise(count = n()) %>% tally()
  
  # analysis wont run if only 1 sex present
  if(sexlevels == 2){
  
  #carry out extrapolation for each cancer
  for(i in 1:length(extrapolations)) {   # Head of for-loop
    
    if(extrapolations[i] == "spline1") {
      
      # 1knotspline
      tryCatch(
        model <- flexsurvspline(formula=Surv(time_years,status-1) ~ sex ,data=data,k = 1, scale = "hazard"),
        error = function(e){
          cat(conditionMessage(e), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "error not carried out \n")
          info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
        warning = function(w){
          cat(conditionMessage(w), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "warning problem with model \n")
          info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
      )
      
      if (exists("model") == TRUE) {
        
        tryCatch({
          
        #extrapolation
        extrap_results_temp[[i]] <- model %>%
          summary(t=t/365, tidy = TRUE) %>%
          mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohort_name[j], Age = "All" ) %>% 
          rename(Sex = sex)
        
        #get the goodness of fit for each model
        gof_results_temp[[i]] <- model %>%
          glance() %>%
          mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohort_name[j], Age = "All" ) %>% 
          slice(rep(1:n(), each = 2)) %>% 
          mutate(Sex = levels(as.factor(data$sex)))
        
        #grab the parameters and knots from the model
        coefs.p <- model[["coefficients"]] %>%
          enframe() %>%
          pivot_wider(names_from = name, values_from = value) %>%
          mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohort_name[j], Age = "All" ) 
        
        knots.p <- model[["knots"]] %>%
          setNames(., c("SplineLowerB", "SplineInternal1" , "SplineUpperB")) %>%
          enframe() %>%
          pivot_wider(names_from = name, values_from = value)
        
        parameters_results_temp[[i]] <- bind_cols(coefs.p,  knots.p ) %>% 
          slice(rep(1:n(), each = 2)) %>% 
          mutate(Sex = levels(as.factor(data$sex)))
        
        # hazard over time
        hazot_results_temp[[i]] <- model %>%
          summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
          mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohort_name[j], Age = "All" ) %>% 
          rename(Sex = sex)
        
        # median and mean predicted survival
        pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
          rename(median = est) %>% 
          mutate(median = round(median, 4),
                 lcl = round(lcl, 4),
                 ucl = round(ucl, 4),
                 "Median Survival in Years (95% CI)"= ifelse(!is.na(median),
                                                             paste0(paste0(nice.num2(median)), " (",
                                                                    paste0(nice.num2(lcl)),"-",
                                                                    paste0(nice.num2(ucl)), ")"),
                                                             NA)) %>% 
          select(-c(lcl, ucl))
        
        pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
          rename(rmean = est) %>% 
          mutate(rmean = round(rmean, 4),
                 se = round(se, 4),
                 time = round(time,4) ,
                 "rmean in years (SE)"= ifelse(!is.na(rmean),
                                               paste0(paste0(nice.num2(rmean)), " (",
                                                      paste0(nice.num2(se)), ")"),
                                               NA)) %>% 
          select(-c(lcl, ucl, se, time))
        
        pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>%
          rename(rmean10yrs = est) %>%
          mutate(rmean10yrs = round(rmean10yrs, 4),
                 se = round(se, 4),
                 time = round(time,4) ,
                 "rmean 10yrs in years (SE)"= ifelse(!is.na(rmean10yrs),
                                                     paste0(paste0(nice.num2(rmean10yrs)), " (",
                                                            paste0(nice.num2(se)), ")"),
                                                     NA)) %>%
          select(-c(lcl, ucl, se, time))
        
        pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
          mutate(est = round((est*100),4),
                 lcl = round((lcl*100),4),
                 ucl = round((ucl*100),4),
                 "Survival Rate % (95% CI)"= ifelse(!is.na(est),
                                                    paste0(paste0(nice.num1(est)), " (",
                                                           paste0(nice.num1(lcl)),"-",
                                                           paste0(nice.num1(ucl)), ")"),
                                                    NA)) %>% 
          rename("surv" = est) %>% 
          select(-c(lcl, ucl)) %>% 
          pivot_wider(names_from = time, 
                      values_from = c(`Survival Rate % (95% CI)`, surv),
                      names_prefix = " year ",
                      names_sep = "")
        
        pred_median_mean_results_temp[[i]] <- inner_join(pr_mean, pr_mean10, by = "sex" ) %>% 
          inner_join(pr_median, by = "sex" ) %>% 
          inner_join(pr_survival_prob, by = "sex")
        pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
          mutate(Method = extrapolations_formatted[i], 
                 Cancer = outcome_cohorts$cohort_name[j], 
                 Age = "All" ) %>% 
          rename(Sex = sex)
        
        rm(model, pr_mean, pr_median, pr_mean10, pr_survival_prob)
        
        #print out progress               
        print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohort_name[j], " completed"))
      
        }, 
        
        error = function(e) {
          cat(conditionMessage(e), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "model not carried out for sex model", "\n")
          info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
        
        warning = function(w) {
          cat(conditionMessage(w), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "potential problem with model for sex model", "\n")
          info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        
        )
      
      }
      
    } else if(extrapolations[i] == "spline2") {
      # 2knotspline
      
      tryCatch(
        model <- flexsurvspline(formula=Surv(time_years,status-1) ~ sex , data=data , k = 2, scale = "hazard"),
        error = function(e){
          cat(conditionMessage(e), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "error not carried out \n")
          info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
        warning = function(w){
          cat(conditionMessage(w), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "warning problem with model \n")
          info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
      )
      
      if (exists("model") == TRUE) {
        
        tryCatch({
        
        extrap_results_temp[[i]] <- model %>%
          summary(t=t/365, tidy = TRUE) %>%
          mutate(Method = extrapolations_formatted[i],
                 Cancer = outcome_cohorts$cohort_name[j], 
                 Age = "All")%>% 
          rename(Sex = sex)
        
        #get the goodness of fit for each model
        gof_results_temp[[i]] <- model %>%
          glance() %>%
          mutate(Method = extrapolations_formatted[i], 
                 Cancer = outcome_cohorts$cohort_name[j],
                 Age = "All" ) %>% 
          slice(rep(1:n(), each = 2)) %>% 
          mutate(Sex = levels(as.factor(data$sex)))
        
        #extract parameters
        #grab the parameters and knots from the model
        coefs.p <- model[["coefficients"]] %>%
          enframe() %>%
          pivot_wider(names_from = name, values_from = value) %>%
          mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohort_name[j], Age = "All") 
        
        knots.p <- model[["knots"]] %>%
          setNames(., c("SplineLowerB", "SplineInternal1" , "SplineInternal2" ,"SplineUpperB")) %>%
          enframe() %>%
          pivot_wider(names_from = name, values_from = value)
        
        parameters_results_temp[[i]] <- bind_cols(coefs.p,  knots.p ) %>% 
          slice(rep(1:n(), each = 2)) %>% 
          mutate(Sex = levels(as.factor(data$sex)))
        
        # hazard over time
        hazot_results_temp[[i]] <- model %>%
          summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
          mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohort_name[j], Age = "All") %>% 
          rename(Sex = sex)
        
        # median and mean predicted survival
        pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
          rename(median = est) %>% 
          mutate(median = round(median, 4),
                 lcl = round(lcl, 4),
                 ucl = round(ucl, 4),
                 "Median Survival in Years (95% CI)"= ifelse(!is.na(median),
                                                             paste0(paste0(nice.num2(median)), " (",
                                                                    paste0(nice.num2(lcl)),"-",
                                                                    paste0(nice.num2(ucl)), ")"),
                                                             NA)) %>% 
          select(-c(lcl, ucl))
        
        pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
          rename(rmean = est) %>% 
          mutate(rmean = round(rmean, 4),
                 se = round(se, 4),
                 time = round(time,4) ,
                 "rmean in years (SE)"= ifelse(!is.na(rmean),
                                               paste0(paste0(nice.num2(rmean)), " (",
                                                      paste0(nice.num2(se)), ")"),
                                               NA)) %>% 
          select(-c(lcl, ucl, se, time))
        
        pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>%
          rename(rmean10yrs = est) %>%
          mutate(rmean10yrs = round(rmean10yrs, 4),
                 se = round(se, 4),
                 time = round(time,4) ,
                 "rmean 10yrs in years (SE)"= ifelse(!is.na(rmean10yrs),
                                                     paste0(paste0(nice.num2(rmean10yrs)), " (",
                                                            paste0(nice.num2(se)), ")"),
                                                     NA)) %>%
          select(-c(lcl, ucl, se, time))
        
        pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
          mutate(est = round((est*100),4),
                 lcl = round((lcl*100),4),
                 ucl = round((ucl*100),4),
                 "Survival Rate % (95% CI)"= ifelse(!is.na(est),
                                                    paste0(paste0(nice.num1(est)), " (",
                                                           paste0(nice.num1(lcl)),"-",
                                                           paste0(nice.num1(ucl)), ")"),
                                                    NA)) %>% 
          rename("surv" = est) %>% 
          select(-c(lcl, ucl)) %>% 
          pivot_wider(names_from = time, 
                      values_from = c(`Survival Rate % (95% CI)`, surv),
                      names_prefix = " year ",
                      names_sep = "")
        
        pred_median_mean_results_temp[[i]] <- inner_join(pr_mean, pr_mean10, by = "sex" ) %>% 
          inner_join(pr_median, by = "sex" ) %>% 
          inner_join(pr_survival_prob, by = "sex")
        pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
          mutate(Method = extrapolations_formatted[i], 
                 Cancer = outcome_cohorts$cohort_name[j], 
                 Age = "All" ) %>% 
          rename(Sex = sex)
        
        rm(model, pr_mean, pr_median, pr_mean10, pr_survival_prob)
        
        print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohort_name[j], " completed"))
        
        }, 
        
        error = function(e) {
          cat(conditionMessage(e), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "model not carried out for sex model", "\n")
          info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
        
        warning = function(w) {
          cat(conditionMessage(w), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "potential problem with model for sex model", "\n")
          info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        
        )
        
      }
      
    } else if(extrapolations[i] == "spline3") {
      # 3knotspline
      
      tryCatch(
        model <- flexsurvspline(formula=Surv(time_years,status-1) ~ sex, data=data, k = 3, scale = "hazard"),
        error = function(e){
          cat(conditionMessage(e), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "error not carried out \n")
          info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
        warning = function(w){
          cat(conditionMessage(w), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "warning problem with model \n")
          info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
      )
      
      if (exists("model") == TRUE) {
        
        tryCatch({
        
        extrap_results_temp[[i]] <- model %>%
          summary(t=t/365, tidy = TRUE) %>%
          mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohort_name[j], Age = "All")%>% 
          rename(Sex = sex)
        
        #get the goodness of fit for each model
        gof_results_temp[[i]] <- model %>%
          glance() %>%
          mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohort_name[j], Age = "All" ) %>% 
          slice(rep(1:n(), each = 2)) %>% 
          mutate(Sex = levels(as.factor(data$sex)))
        
        #extract parameters
        #grab the parameters and knots from the model
        coefs.p <- model[["coefficients"]] %>%
          enframe() %>%
          pivot_wider(names_from = name, values_from = value) %>%
          mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohort_name[j], Age = "All") 
        
        knots.p <- model[["knots"]] %>%
          setNames(., c("SplineLowerB", "SplineInternal1" , "SplineInternal2", "SplineInternal3" ,"SplineUpperB")) %>%
          enframe() %>%
          pivot_wider(names_from = name, values_from = value)
        
        parameters_results_temp[[i]] <- bind_cols(coefs.p,  knots.p ) %>% 
          slice(rep(1:n(), each = 2)) %>% 
          mutate(Sex = levels(as.factor(data$sex)))
        
        # hazard over time
        hazot_results_temp[[i]] <- model %>%
          summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
          mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohort_name[j], Age = "All" ) %>% 
          rename(Sex = sex)
        
        # median and mean predicted survival
        pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
          rename(median = est) %>% 
          mutate(median = round(median, 4),
                 lcl = round(lcl, 4),
                 ucl = round(ucl, 4),
                 "Median Survival in Years (95% CI)"= ifelse(!is.na(median),
                                                             paste0(paste0(nice.num2(median)), " (",
                                                                    paste0(nice.num2(lcl)),"-",
                                                                    paste0(nice.num2(ucl)), ")"),
                                                             NA)) %>% 
          select(-c(lcl, ucl))
        
        pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
          rename(rmean = est) %>% 
          mutate(rmean = round(rmean, 4),
                 se = round(se, 4),
                 time = round(time,4) ,
                 "rmean in years (SE)"= ifelse(!is.na(rmean),
                                               paste0(paste0(nice.num2(rmean)), " (",
                                                      paste0(nice.num2(se)), ")"),
                                               NA)) %>% 
          select(-c(lcl, ucl, se, time))
        
        pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>%
          rename(rmean10yrs = est) %>%
          mutate(rmean10yrs = round(rmean10yrs, 4),
                 se = round(se, 4),
                 time = round(time,4) ,
                 "rmean 10yrs in years (SE)"= ifelse(!is.na(rmean10yrs),
                                                     paste0(paste0(nice.num2(rmean10yrs)), " (",
                                                            paste0(nice.num2(se)), ")"),
                                                     NA)) %>%
          select(-c(lcl, ucl, se, time))
        
        pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
          mutate(est = round((est*100),4),
                 lcl = round((lcl*100),4),
                 ucl = round((ucl*100),4),
                 "Survival Rate % (95% CI)"= ifelse(!is.na(est),
                                                    paste0(paste0(nice.num1(est)), " (",
                                                           paste0(nice.num1(lcl)),"-",
                                                           paste0(nice.num1(ucl)), ")"),
                                                    NA)) %>% 
          rename("surv" = est) %>% 
          select(-c(lcl, ucl)) %>% 
          pivot_wider(names_from = time, 
                      values_from = c(`Survival Rate % (95% CI)`, surv),
                      names_prefix = " year ",
                      names_sep = "")
        
        pred_median_mean_results_temp[[i]] <- inner_join(pr_mean, pr_mean10, by = "sex" ) %>% 
          inner_join(pr_median, by = "sex" ) %>% 
          inner_join(pr_survival_prob, by = "sex")
        pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
          mutate(Method = extrapolations_formatted[i], 
                 Cancer = outcome_cohorts$cohort_name[j], 
                 Age = "All" ) %>% 
          rename(Sex = sex)
        
        rm(model, pr_mean, pr_median, pr_mean10, pr_survival_prob)
        
        #print out progress               
        print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohort_name[j], " completed"))
      
        }, 
        
        error = function(e) {
          cat(conditionMessage(e), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "model not carried out for sex model", "\n")
          info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
        
        warning = function(w) {
          cat(conditionMessage(w), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "potential problem with model for sex model", "\n")
          info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        
        )
        
      }
      
    } else if(extrapolations[i] == "spline5") {
      # 5knotspline
      
      tryCatch(
        model <- flexsurvspline(formula=Surv(time_years,status-1) ~ sex , data=data, k = 5, scale = "hazard"),
        error = function(e){
          cat(conditionMessage(e), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "error not carried out \n")
          info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
        warning = function(w){
          cat(conditionMessage(w), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "warning problem with model \n")
          info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
      )
      
      if (exists("model") == TRUE) {
        
        tryCatch({
        
        extrap_results_temp[[i]] <- model %>%
          summary(t=t/365, tidy = TRUE) %>%
          mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohort_name[j], Age = "All") %>% 
          rename(Sex = sex)
        
        #get the goodness of fit for each model
        gof_results_temp[[i]] <- model %>%
          glance() %>%
          mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohort_name[j], Age = "All" ) %>% 
          slice(rep(1:n(), each = 2)) %>% 
          mutate(Sex = levels(as.factor(data$sex)))
        
        coefs.p <- model[["coefficients"]] %>%
          enframe() %>%
          pivot_wider(names_from = name, values_from = value) %>%
          mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohort_name[j], Age = "All") 
        
        knots.p <- model[["knots"]] %>%
          setNames(., c("SplineLowerB", "SplineInternal1" , "SplineInternal2", "SplineInternal3" ,
                        "SplineInternal4" ,"SplineInternal5" , "SplineUpperB")) %>%
          enframe() %>%
          pivot_wider(names_from = name, values_from = value)
        
        parameters_results_temp[[i]] <- bind_cols(coefs.p,  knots.p ) %>% 
          slice(rep(1:n(), each = 2)) %>% 
          mutate(Sex = levels(as.factor(data$sex)))
        
        # hazard over time
        hazot_results_temp[[i]] <- model %>%
          summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
          mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohort_name[j], Age = "All") %>% 
          rename(Sex = sex)
        
        # median and mean predicted survival
        pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
          rename(median = est) %>% 
          mutate(median = round(median, 4),
                 lcl = round(lcl, 4),
                 ucl = round(ucl, 4),
                 "Median Survival in Years (95% CI)"= ifelse(!is.na(median),
                                                             paste0(paste0(nice.num2(median)), " (",
                                                                    paste0(nice.num2(lcl)),"-",
                                                                    paste0(nice.num2(ucl)), ")"),
                                                             NA)) %>% 
          select(-c(lcl, ucl))
        
        pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
          rename(rmean = est) %>% 
          mutate(rmean = round(rmean, 4),
                 se = round(se, 4),
                 time = round(time,4) ,
                 "rmean in years (SE)"= ifelse(!is.na(rmean),
                                               paste0(paste0(nice.num2(rmean)), " (",
                                                      paste0(nice.num2(se)), ")"),
                                               NA)) %>% 
          select(-c(lcl, ucl, se, time))
        
        pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>%
          rename(rmean10yrs = est) %>%
          mutate(rmean10yrs = round(rmean10yrs, 4),
                 se = round(se, 4),
                 time = round(time,4) ,
                 "rmean 10yrs in years (SE)"= ifelse(!is.na(rmean10yrs),
                                                     paste0(paste0(nice.num2(rmean10yrs)), " (",
                                                            paste0(nice.num2(se)), ")"),
                                                     NA)) %>%
          select(-c(lcl, ucl, se, time))
        
        pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
          mutate(est = round((est*100),4),
                 lcl = round((lcl*100),4),
                 ucl = round((ucl*100),4),
                 "Survival Rate % (95% CI)"= ifelse(!is.na(est),
                                                    paste0(paste0(nice.num1(est)), " (",
                                                           paste0(nice.num1(lcl)),"-",
                                                           paste0(nice.num1(ucl)), ")"),
                                                    NA)) %>% 
          rename("surv" = est) %>% 
          select(-c(lcl, ucl)) %>% 
          pivot_wider(names_from = time, 
                      values_from = c(`Survival Rate % (95% CI)`, surv),
                      names_prefix = " year ",
                      names_sep = "")
        
        pred_median_mean_results_temp[[i]] <- inner_join(pr_mean, pr_mean10, by = "sex" ) %>% 
          inner_join(pr_median, by = "sex" ) %>% 
          inner_join(pr_survival_prob, by = "sex")
        pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
          mutate(Method = extrapolations_formatted[i], 
                 Cancer = outcome_cohorts$cohort_name[j], 
                 Age = "All" ) %>% 
          rename(Sex = sex)
        
        rm(model, pr_mean, pr_median, pr_mean10, pr_survival_prob)
        
        #print out progress               
        print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohort_name[j], " completed"))
      
        }, 
        
        error = function(e) {
          cat(conditionMessage(e), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "model not carried out for sex model", "\n")
          info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
        
        warning = function(w) {
          cat(conditionMessage(w), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "potential problem with model for sex model", "\n")
          info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        
        )
        
      }
      
    } else {
      
      #carry out models for different parametric methods survival
      tryCatch(
        model <- flexsurvreg(Surv(time_years, status) ~ sex, data=data, dist=extrapolations[i]),
        error = function(e){
          cat(conditionMessage(e), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "error not carried out \n")
          info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
        warning = function(w){
          cat(conditionMessage(w), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "warning problem with model \n")
          info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
      )
      
      if (exists("model") == TRUE) {
        
        tryCatch({
        
        # extrapolations
        extrap_results_temp[[i]] <- model %>%
          summary(t=t/365, tidy = TRUE) %>%
          mutate(Method = extrapolations_formatted[i], 
                 Cancer = outcome_cohorts$cohort_name[j], 
                 Age = "All") %>% 
          rename(Sex = sex)
        
        #get the goodness of fit for each model
        gof_results_temp[[i]] <- model %>%
          glance() %>%
          mutate(Method = extrapolations_formatted[i], 
                 Cancer = outcome_cohorts$cohort_name[j],
                 Age = "All" ) %>% 
          slice(rep(1:n(), each = 2)) %>% 
          mutate(Sex = levels(as.factor(data$sex)))
        
        #grab the parameters from the model
        parameters_results_temp[[i]] <- model[["coefficients"]] %>%
          enframe() %>%
          pivot_wider(names_from = name, values_from = value) %>%
          mutate(Method = extrapolations_formatted[i], 
                 Cancer = outcome_cohorts$cohort_name[j], Age = "All" ) %>% 
          slice(rep(1:n(), each = 2)) %>% 
          mutate(Sex = levels(as.factor(data$sex)))
        
        #extract the hazard function over time
        hazot_results_temp[[i]] <- model %>%
          summary(t=(t + 1)/365, type = "hazard",tidy = TRUE) %>%
          mutate(Method = extrapolations_formatted[i], 
                 Cancer = outcome_cohorts$cohort_name[j],
                 Age = "All" ) %>% 
          rename(Sex = sex)
        
        # median and mean predicted survival
        pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
          rename(median = est) %>% 
          mutate(median = round(median, 4),
                 lcl = round(lcl, 4),
                 ucl = round(ucl, 4),
                 "Median Survival in Years (95% CI)"= ifelse(!is.na(median),
                                                             paste0(paste0(nice.num2(median)), " (",
                                                                    paste0(nice.num2(lcl)),"-",
                                                                    paste0(nice.num2(ucl)), ")"),
                                                             NA)) %>% 
          select(-c(lcl, ucl))
        
        pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
          rename(rmean = est) %>% 
          mutate(rmean = round(rmean, 4),
                 se = round(se, 4),
                 time = round(time,4) ,
                 "rmean in years (SE)"= ifelse(!is.na(rmean),
                                               paste0(paste0(nice.num2(rmean)), " (",
                                                      paste0(nice.num2(se)), ")"),
                                               NA)) %>% 
          select(-c(lcl, ucl, se, time))
        
        pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>%
          rename(rmean10yrs = est) %>%
          mutate(rmean10yrs = round(rmean10yrs, 4),
                 se = round(se, 4),
                 time = round(time,4) ,
                 "rmean 10yrs in years (SE)"= ifelse(!is.na(rmean10yrs),
                                                     paste0(paste0(nice.num2(rmean10yrs)), " (",
                                                            paste0(nice.num2(se)), ")"),
                                                     NA)) %>%
          select(-c(lcl, ucl, se, time))
        
        pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
          mutate(est = round((est*100),4),
                 lcl = round((lcl*100),4),
                 ucl = round((ucl*100),4),
                 "Survival Rate % (95% CI)"= ifelse(!is.na(est),
                                                    paste0(paste0(nice.num1(est)), " (",
                                                           paste0(nice.num1(lcl)),"-",
                                                           paste0(nice.num1(ucl)), ")"),
                                                    NA)) %>% 
          rename("surv" = est) %>% 
          select(-c(lcl, ucl)) %>% 
          pivot_wider(names_from = time, 
                      values_from = c(`Survival Rate % (95% CI)`, surv),
                      names_prefix = " year ",
                      names_sep = "")
        
        pred_median_mean_results_temp[[i]] <- inner_join(pr_mean, pr_mean10, by = "sex" ) %>% 
          inner_join(pr_median, by = "sex" ) %>% 
          inner_join(pr_survival_prob, by = "sex")
        pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
          mutate(Method = extrapolations_formatted[i], 
                 Cancer = outcome_cohorts$cohort_name[j], 
                 Age = "All" ) %>% 
          rename(Sex = sex)
        
        rm(model, pr_mean, pr_median, pr_mean10, pr_survival_prob)
        
        #print out progress               
        print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohort_name[j], " completed"))
        
        }, 
        
        error = function(e) {
          cat(conditionMessage(e), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "model not carried out for sex model", "\n")
          info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
        
        warning = function(w) {
          cat(conditionMessage(w), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "potential problem with model for sex model", "\n")
          info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        
        )
      
      
    }
    
    }
    
  }
    
    extrapolatedcombined <- dplyr::bind_rows(extrap_results_temp)
    gofcombined <- dplyr::bind_rows(gof_results_temp)
    hotcombined <- dplyr::bind_rows(hazot_results_temp)   
    parcombined <- dplyr::bind_rows(parameters_results_temp)
    medcombined <- dplyr::bind_rows(pred_median_mean_results_temp)
    
    extrapolations_sex[[j]] <- extrapolatedcombined
    gof_haz_sex[[j]] <- gofcombined
    hazot_sex[[j]] <- hotcombined
    parameters_sex[[j]] <-  parcombined
    pred_median_mean_sex[[j]] <- medcombined

    #print out progress               
    print(paste0(outcome_cohorts$cohort_name[j]," Extrapolation Analysis Completed ", Sys.time()))
    
  } else {
    
    print(paste0("sex stratification extrapolation analysis not carried out for ", outcome_cohorts$cohort_name[j], " due to only 1 sex present " , Sys.time()))
    
  }
  
  
}

# Merge results together from each cancer and extrapolation into a dataframe ---
extrapolatedfinalsex <- dplyr::bind_rows(extrapolations_sex) %>%
  mutate(Stratification = "None", Adjustment = "Sex")
goffinalsex <- dplyr::bind_rows(gof_haz_sex) %>%
  mutate(Stratification = "None" , Adjustment = "Sex")
hazardotfinalsex <- dplyr::bind_rows(hazot_sex) %>%
  mutate(Stratification = "None", Adjustment = "Sex")
parametersfinalsex <- dplyr::bind_rows(parameters_sex)  %>%
  mutate(Stratification = "None", Adjustment = "Sex") %>% 
  relocate(shape, .after = Sex) %>% 
  relocate(rate, .after = Sex) 
predmedmeanfinalsex <- dplyr::bind_rows(pred_median_mean_sex)  %>%
  mutate(Stratification = "None", Adjustment = "Sex")

toc(func.toc=toc_min)

info(logger, 'Extrapolation analysis for sex adjustment COMPLETE')


########################################
# SEX STRATIFICATION EXTRAPOLATION
#######################################

tic("extrapolation analysis with sex stratification")
info(logger, 'extrapolation analysis for sex stratification START')

extrapolations_sexS <- list()
gof_haz_sexS <- list()
hazot_sexS <- list()
parameters_sexS <- list()
pred_median_mean_sexS <- list()

for(j in 1:nrow(outcome_cohorts)) { 

  #temp results
  extrap_results_temp <- list() 
  gof_results_temp <- list() 
  hazot_results_temp <- list()
  parameters_results_temp <- list() 
  pred_median_mean_results_temp <- list() 
  
  #for each sex 
  extrap_sex <- list()
  gof_sex <- list()
  hot_sex <- list()   
  par_sex <- list()
  med_sex <- list()
  
  #subset the data by cancer type
  data <- Pop %>%
    filter(cohort_definition_id == j) 
  
  sexlevels <- data %>%
    group_by(sex) %>% summarise(count = n()) %>% tally()
  
  sexvalues <- data %>%
    group_by(sex) %>% summarise(count = n())
  
  # analysis wont run if only 1 sex present
  if(sexlevels == 2){
    
    for (sexl in 1:nrow(sexvalues)) {
      
      data_sex <- data %>% 
        filter(sex == sexvalues$sex[sexl])
      
      #split per sex then run extrapolations
      print(paste0("extrapolations for stratification"," ", Sys.time()," for " ,outcome_cohorts$cohort_name[j]," ", sexvalues$sex[sexl] ," started"))
    
    #carry out extrapolation for each cancer
    for(i in 1:length(extrapolations)) {   # Head of for-loop
      
      if(extrapolations[i] == "spline1") {
        
        # 1knotspline
        tryCatch(
          model <- flexsurvspline(formula=Surv(time_years,status-1) ~ 1 ,data=data_sex,k = 1, scale = "hazard"),
          error = function(e){
            cat(conditionMessage(e), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "error not carried out \n")
            info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
          warning = function(w){
            cat(conditionMessage(w), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "warning problem with model \n")
            info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        )
        
        if (exists("model") == TRUE) {
          
          tryCatch({
          
          #extrapolation
          extrap_results_temp[[i]] <- model %>%
            summary(t=t/365, tidy = TRUE) %>%
            mutate(Method = extrapolations_formatted[i],
                   Cancer = outcome_cohorts$cohort_name[j],
                   Age = "All",
                   Sex = data_sex$sex[sexl])
          
          #get the goodness of fit for each model
          gof_results_temp[[i]] <- model %>%
            glance() %>%
            mutate(Method = extrapolations_formatted[i], 
                   Cancer = outcome_cohorts$cohort_name[j],
                   Age = "All",
                   Sex = data_sex$sex[sexl])
          
          #grab the parameters and knots from the model
          coefs.p <- model[["coefficients"]] %>%
            enframe() %>%
            pivot_wider(names_from = name, values_from = value) %>%
            mutate(Method = extrapolations_formatted[i], 
                   Cancer = outcome_cohorts$cohort_name[j],
                   Age = "All" ) 
          
          knots.p <- model[["knots"]] %>%
            setNames(., c("SplineLowerB", "SplineInternal1" , "SplineUpperB")) %>%
            enframe() %>%
            pivot_wider(names_from = name, values_from = value)
          
          parameters_results_temp[[i]] <- bind_cols(coefs.p,  knots.p ) %>% 
            mutate(Sex = data_sex$sex[sexl])
          
          # hazard over time
          hazot_results_temp[[i]] <- model %>%
            summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
            mutate(Method = extrapolations_formatted[i], 
                   Cancer = outcome_cohorts$cohort_name[j], 
                   Age = "All",
                   Sex = data_sex$sex[sexl])
          
          # median and mean predicted survival
          pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
            rename(median = est) %>% 
            mutate(median = round(median, 4),
                   lcl = round(lcl, 4),
                   ucl = round(ucl, 4),
                   "Median Survival in Years (95% CI)"= ifelse(!is.na(median),
                                                               paste0(paste0(nice.num2(median)), " (",
                                                                      paste0(nice.num2(lcl)),"-",
                                                                      paste0(nice.num2(ucl)), ")"),
                                                               NA)) %>% 
            select(-c(lcl, ucl))
          
          pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
            rename(rmean = est) %>% 
            mutate(rmean = round(rmean, 4),
                   se = round(se, 4),
                   time = round(time,4) ,
                   "rmean in years (SE)"= ifelse(!is.na(rmean),
                                                 paste0(paste0(nice.num2(rmean)), " (",
                                                        paste0(nice.num2(se)), ")"),
                                                 NA)) %>% 
            select(-c(lcl, ucl, se, time))
          
          pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>%
            rename(rmean10yrs = est) %>%
            mutate(rmean10yrs = round(rmean10yrs, 4),
                   se = round(se, 4),
                   time = round(time,4) ,
                   "rmean 10yrs in years (SE)"= ifelse(!is.na(rmean10yrs),
                                                       paste0(paste0(nice.num2(rmean10yrs)), " (",
                                                              paste0(nice.num2(se)), ")"),
                                                       NA)) %>%
            select(-c(lcl, ucl, se, time))
          
          pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
            mutate(est = round((est*100),4),
                   lcl = round((lcl*100),4),
                   ucl = round((ucl*100),4),
                   "Survival Rate % (95% CI)"= ifelse(!is.na(est),
                                                      paste0(paste0(nice.num1(est)), " (",
                                                             paste0(nice.num1(lcl)),"-",
                                                             paste0(nice.num1(ucl)), ")"),
                                                      NA)) %>% 
            rename("surv" = est) %>% 
            select(-c(lcl, ucl)) %>% 
            pivot_wider(names_from = time, 
                        values_from = c(`Survival Rate % (95% CI)`, surv),
                        names_prefix = " year ",
                        names_sep = "")
          
          pred_median_mean_results_temp[[i]] <- bind_cols(pr_mean,pr_mean10, pr_median, pr_survival_prob )
          pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
            mutate(Method = extrapolations_formatted[i], 
                   Cancer = outcome_cohorts$cohort_name[j], 
                   Age = "All",
                   Sex = data_sex$sex[sexl]) 
          
          rm(model, pr_mean, pr_median, pr_mean10, pr_survival_prob)
          
          #print out progress               
          print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohort_name[j], " completed"))
        
          }, 
          
          error = function(e) {
            cat(conditionMessage(e), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "model not carried out for sex model", "\n")
            info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
          
          warning = function(w) {
            cat(conditionMessage(w), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "potential problem with model for sex model", "\n")
            info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
          
          )
        
        }
        
      } else if(extrapolations[i] == "spline2") {
        # 2knotspline
        
        tryCatch(
          model <- flexsurvspline(formula=Surv(time_years,status-1) ~ 1 , data=data_sex , k = 2, scale = "hazard"),
          error = function(e){
            cat(conditionMessage(e), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "error not carried out \n")
            info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
          warning = function(w){
            cat(conditionMessage(w), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "warning problem with model \n")
            info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        )
        
        if (exists("model") == TRUE) {
          
          tryCatch({
          
          extrap_results_temp[[i]] <- model %>%
            summary(t=t/365, tidy = TRUE) %>%
            mutate(Method = extrapolations_formatted[i],
                   Cancer = outcome_cohorts$cohort_name[j],
                   Age = "All",
                   Sex = data_sex$sex[sexl])
          
          #get the goodness of fit for each model
          gof_results_temp[[i]] <- model %>%
            glance() %>%
            mutate(Method = extrapolations_formatted[i],
                   Cancer = outcome_cohorts$cohort_name[j],
                   Age = "All" ,
                   Sex = data_sex$sex[sexl])
          
          #extract parameters
          #grab the parameters and knots from the model
          coefs.p <- model[["coefficients"]] %>%
            enframe() %>%
            pivot_wider(names_from = name, values_from = value) %>%
            mutate(Method = extrapolations_formatted[i],
                   Cancer = outcome_cohorts$cohort_name[j],
                   Age = "All") 
          
          knots.p <- model[["knots"]] %>%
            setNames(., c("SplineLowerB", "SplineInternal1" , "SplineInternal2" ,"SplineUpperB")) %>%
            enframe() %>%
            pivot_wider(names_from = name, values_from = value)
          
          parameters_results_temp[[i]] <- bind_cols(coefs.p,  knots.p ) %>% 
            mutate(Sex = data_sex$sex[sexl])
          
          # hazard over time
          hazot_results_temp[[i]] <- model %>%
            summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
            mutate(Method = extrapolations_formatted[i], 
                   Cancer = outcome_cohorts$cohort_name[j],
                   Age = "All",
                   Sex = data_sex$sex[sexl])
          
          # median and mean predicted survival
          pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
            rename(median = est) %>% 
            mutate(median = round(median, 4),
                   lcl = round(lcl, 4),
                   ucl = round(ucl, 4),
                   "Median Survival in Years (95% CI)"= ifelse(!is.na(median),
                                                               paste0(paste0(nice.num2(median)), " (",
                                                                      paste0(nice.num2(lcl)),"-",
                                                                      paste0(nice.num2(ucl)), ")"),
                                                               NA)) %>% 
            select(-c(lcl, ucl))
          
          pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
            rename(rmean = est) %>% 
            mutate(rmean = round(rmean, 4),
                   se = round(se, 4),
                   time = round(time,4) ,
                   "rmean in years (SE)"= ifelse(!is.na(rmean),
                                                 paste0(paste0(nice.num2(rmean)), " (",
                                                        paste0(nice.num2(se)), ")"),
                                                 NA)) %>% 
            select(-c(lcl, ucl, se, time))
          
          pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>%
            rename(rmean10yrs = est) %>%
            mutate(rmean10yrs = round(rmean10yrs, 4),
                   se = round(se, 4),
                   time = round(time,4) ,
                   "rmean 10yrs in years (SE)"= ifelse(!is.na(rmean10yrs),
                                                       paste0(paste0(nice.num2(rmean10yrs)), " (",
                                                              paste0(nice.num2(se)), ")"),
                                                       NA)) %>%
            select(-c(lcl, ucl, se, time))
          
          pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
            mutate(est = round((est*100),4),
                   lcl = round((lcl*100),4),
                   ucl = round((ucl*100),4),
                   "Survival Rate % (95% CI)"= ifelse(!is.na(est),
                                                      paste0(paste0(nice.num1(est)), " (",
                                                             paste0(nice.num1(lcl)),"-",
                                                             paste0(nice.num1(ucl)), ")"),
                                                      NA)) %>% 
            rename("surv" = est) %>% 
            select(-c(lcl, ucl)) %>% 
            pivot_wider(names_from = time, 
                        values_from = c(`Survival Rate % (95% CI)`, surv),
                        names_prefix = " year ",
                        names_sep = "")
          
          pred_median_mean_results_temp[[i]] <- bind_cols(pr_mean,pr_mean10, pr_median, pr_survival_prob )
          pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
            mutate(Method = extrapolations_formatted[i], 
                   Cancer = outcome_cohorts$cohort_name[j], 
                   Age = "All",
                   Sex = data_sex$sex[sexl]) 
          
          rm(model, pr_mean, pr_median, pr_mean10, pr_survival_prob)
          
          print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohort_name[j], " completed"))
          
          }, 
          
          error = function(e) {
            cat(conditionMessage(e), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "model not carried out for sex model", "\n")
            info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
          
          warning = function(w) {
            cat(conditionMessage(w), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "potential problem with model for sex model", "\n")
            info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
          
          )
          
        }
        
      } else if(extrapolations[i] == "spline3") {
        # 3knotspline
        
        tryCatch(
          model <- flexsurvspline(formula=Surv(time_years,status-1) ~ 1, data=data_sex, k = 3, scale = "hazard"),
          error = function(e){
            cat(conditionMessage(e), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "error not carried out \n")
            info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
          warning = function(w){
            cat(conditionMessage(w), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "warning problem with model \n")
            info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        )
        
        if (exists("model") == TRUE) {
          
          tryCatch({
          
          extrap_results_temp[[i]] <- model %>%
            summary(t=t/365, tidy = TRUE) %>%
            mutate(Method = extrapolations_formatted[i], 
                   Cancer = outcome_cohorts$cohort_name[j],
                   Age = "All",
                   Sex = data_sex$sex[sexl])
          
          #get the goodness of fit for each model
          gof_results_temp[[i]] <- model %>%
            glance() %>%
            mutate(Method = extrapolations_formatted[i], 
                   Cancer = outcome_cohorts$cohort_name[j],
                   Age = "All",
                   Sex = data_sex$sex[sexl])
          
          #extract parameters
          #grab the parameters and knots from the model
          coefs.p <- model[["coefficients"]] %>%
            enframe() %>%
            pivot_wider(names_from = name, values_from = value) %>%
            mutate(Method = extrapolations_formatted[i], 
                   Cancer = outcome_cohorts$cohort_name[j],
                   Age = "All") 
          
          knots.p <- model[["knots"]] %>%
            setNames(., c("SplineLowerB", "SplineInternal1" , "SplineInternal2", "SplineInternal3" ,"SplineUpperB")) %>%
            enframe() %>%
            pivot_wider(names_from = name, values_from = value)
          
          parameters_results_temp[[i]] <- bind_cols(coefs.p,  knots.p ) %>% 
            mutate(Sex = data_sex$sex[sexl])
          
          # hazard over time
          hazot_results_temp[[i]] <- model %>%
            summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
            mutate(Method = extrapolations_formatted[i], 
                   Cancer = outcome_cohorts$cohort_name[j], 
                   Age = "All" ,
                   Sex = data_sex$sex[sexl])
          
          # median and mean predicted survival
          pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
            rename(median = est) %>% 
            mutate(median = round(median, 4),
                   lcl = round(lcl, 4),
                   ucl = round(ucl, 4),
                   "Median Survival in Years (95% CI)"= ifelse(!is.na(median),
                                                               paste0(paste0(nice.num2(median)), " (",
                                                                      paste0(nice.num2(lcl)),"-",
                                                                      paste0(nice.num2(ucl)), ")"),
                                                               NA)) %>% 
            select(-c(lcl, ucl))
          
          pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
            rename(rmean = est) %>% 
            mutate(rmean = round(rmean, 4),
                   se = round(se, 4),
                   time = round(time,4) ,
                   "rmean in years (SE)"= ifelse(!is.na(rmean),
                                                 paste0(paste0(nice.num2(rmean)), " (",
                                                        paste0(nice.num2(se)), ")"),
                                                 NA)) %>% 
            select(-c(lcl, ucl, se, time))
          
          pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>%
            rename(rmean10yrs = est) %>%
            mutate(rmean10yrs = round(rmean10yrs, 4),
                   se = round(se, 4),
                   time = round(time,4) ,
                   "rmean 10yrs in years (SE)"= ifelse(!is.na(rmean10yrs),
                                                       paste0(paste0(nice.num2(rmean10yrs)), " (",
                                                              paste0(nice.num2(se)), ")"),
                                                       NA)) %>%
            select(-c(lcl, ucl, se, time))
          
          pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
            mutate(est = round((est*100),4),
                   lcl = round((lcl*100),4),
                   ucl = round((ucl*100),4),
                   "Survival Rate % (95% CI)"= ifelse(!is.na(est),
                                                      paste0(paste0(nice.num1(est)), " (",
                                                             paste0(nice.num1(lcl)),"-",
                                                             paste0(nice.num1(ucl)), ")"),
                                                      NA)) %>% 
            rename("surv" = est) %>% 
            select(-c(lcl, ucl)) %>% 
            pivot_wider(names_from = time, 
                        values_from = c(`Survival Rate % (95% CI)`, surv),
                        names_prefix = " year ",
                        names_sep = "")
          
          pred_median_mean_results_temp[[i]] <- bind_cols(pr_mean,pr_mean10, pr_median, pr_survival_prob )
          pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
            mutate(Method = extrapolations_formatted[i], 
                   Cancer = outcome_cohorts$cohort_name[j], 
                   Age = "All",
                   Sex = data_sex$sex[sexl]) 
          
          rm(model, pr_mean, pr_median, pr_mean10, pr_survival_prob)
          
          #print out progress               
          print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohort_name[j], " completed"))
        
          }, 
          
          error = function(e) {
            cat(conditionMessage(e), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "model not carried out for sex model", "\n")
            info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
          
          warning = function(w) {
            cat(conditionMessage(w), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "potential problem with model for sex model", "\n")
            info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
          
          )
          
        }
        
      } else if(extrapolations[i] == "spline5") {
        # 5knotspline
        
        tryCatch(
          model <- flexsurvspline(formula=Surv(time_years,status-1) ~ 1 , data=data_sex, k = 5, scale = "hazard"),
          error = function(e){
            cat(conditionMessage(e), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "error not carried out \n")
            info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
          warning = function(w){
            cat(conditionMessage(w), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "warning problem with model \n")
            info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        )
        
        if (exists("model") == TRUE) {
          
          tryCatch({
          
          extrap_results_temp[[i]] <- model %>%
            summary(t=t/365, tidy = TRUE) %>%
            mutate(Method = extrapolations_formatted[i],
                   Cancer = outcome_cohorts$cohort_name[j],
                   Age = "All",
                   Sex = data_sex$sex[sexl])
          
          #get the goodness of fit for each model
          gof_results_temp[[i]] <- model %>%
            glance() %>%
            mutate(Method = extrapolations_formatted[i],
                   Cancer = outcome_cohorts$cohort_name[j],
                   Age = "All" ,
                   Sex = data_sex$sex[sexl])
          
          coefs.p <- model[["coefficients"]] %>%
            enframe() %>%
            pivot_wider(names_from = name, values_from = value) %>%
            mutate(Method = extrapolations_formatted[i], 
                   Cancer = outcome_cohorts$cohort_name[j],
                   Age = "All") 
          
          knots.p <- model[["knots"]] %>%
            setNames(., c("SplineLowerB", "SplineInternal1" , "SplineInternal2", "SplineInternal3" ,
                          "SplineInternal4" ,"SplineInternal5" , "SplineUpperB")) %>%
            enframe() %>%
            pivot_wider(names_from = name, values_from = value)
          
          parameters_results_temp[[i]] <- bind_cols(coefs.p,  knots.p ) %>% 
            mutate(Sex = data_sex$sex[sexl])
          
          # hazard over time
          hazot_results_temp[[i]] <- model %>%
            summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
            mutate(Method = extrapolations_formatted[i], 
                   Cancer = outcome_cohorts$cohort_name[j],
                   Age = "All",
                   Sex = data_sex$sex[sexl])
          
          # median and mean predicted survival
          pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
            rename(median = est) %>% 
            mutate(median = round(median, 4),
                   lcl = round(lcl, 4),
                   ucl = round(ucl, 4),
                   "Median Survival in Years (95% CI)"= ifelse(!is.na(median),
                                                               paste0(paste0(nice.num2(median)), " (",
                                                                      paste0(nice.num2(lcl)),"-",
                                                                      paste0(nice.num2(ucl)), ")"),
                                                               NA)) %>% 
            select(-c(lcl, ucl))
          
          pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
            rename(rmean = est) %>% 
            mutate(rmean = round(rmean, 4),
                   se = round(se, 4),
                   time = round(time,4) ,
                   "rmean in years (SE)"= ifelse(!is.na(rmean),
                                                 paste0(paste0(nice.num2(rmean)), " (",
                                                        paste0(nice.num2(se)), ")"),
                                                 NA)) %>% 
            select(-c(lcl, ucl, se, time))
          
          pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>%
            rename(rmean10yrs = est) %>%
            mutate(rmean10yrs = round(rmean10yrs, 4),
                   se = round(se, 4),
                   time = round(time,4) ,
                   "rmean 10yrs in years (SE)"= ifelse(!is.na(rmean10yrs),
                                                       paste0(paste0(nice.num2(rmean10yrs)), " (",
                                                              paste0(nice.num2(se)), ")"),
                                                       NA)) %>%
            select(-c(lcl, ucl, se, time))
          
          pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
            mutate(est = round((est*100),4),
                   lcl = round((lcl*100),4),
                   ucl = round((ucl*100),4),
                   "Survival Rate % (95% CI)"= ifelse(!is.na(est),
                                                      paste0(paste0(nice.num1(est)), " (",
                                                             paste0(nice.num1(lcl)),"-",
                                                             paste0(nice.num1(ucl)), ")"),
                                                      NA)) %>% 
            rename("surv" = est) %>% 
            select(-c(lcl, ucl)) %>% 
            pivot_wider(names_from = time, 
                        values_from = c(`Survival Rate % (95% CI)`, surv),
                        names_prefix = " year ",
                        names_sep = "")
          
          pred_median_mean_results_temp[[i]] <- bind_cols(pr_mean,pr_mean10, pr_median, pr_survival_prob )
          pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
            mutate(Method = extrapolations_formatted[i], 
                   Cancer = outcome_cohorts$cohort_name[j], 
                   Age = "All",
                   Sex = data_sex$sex[sexl]) 
          
          rm(model, pr_mean, pr_median, pr_mean10, pr_survival_prob)
          
          #print out progress               
          print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohort_name[j], " completed"))
        
          }, 
          
          error = function(e) {
            cat(conditionMessage(e), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "model not carried out for sex model", "\n")
            info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
          
          warning = function(w) {
            cat(conditionMessage(w), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "potential problem with model for sex model", "\n")
            info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
          
          )
          
        }
        
      } else {
        
        #carry out models for different parametric methods survival
        tryCatch(
          model <- flexsurvreg(Surv(time_years, status) ~ 1, data=data_sex, dist=extrapolations[i]),
          error = function(e){
            cat(conditionMessage(e), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "error not carried out \n")
            info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
          warning = function(w){
            cat(conditionMessage(w), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "warning problem with model \n")
            info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        )
        
        if (exists("model") == TRUE) {
          
          tryCatch({
          
          # extrapolations
          extrap_results_temp[[i]] <- model %>%
            summary(t=t/365, tidy = TRUE) %>%
            mutate(Method = extrapolations_formatted[i],
                   Cancer = outcome_cohorts$cohort_name[j],
                   Age = "All",
                   Sex = data_sex$sex[sexl])
          
          #get the goodness of fit for each model
          gof_results_temp[[i]] <- model %>%
            glance() %>%
            mutate(Method = extrapolations_formatted[i], 
                   Cancer = outcome_cohorts$cohort_name[j],
                   Age = "All",
                   Sex = data_sex$sex[sexl])
          
          #grab the parameters from the model
          parameters_results_temp[[i]] <- model[["coefficients"]] %>%
            enframe() %>%
            pivot_wider(names_from = name, values_from = value) %>%
            mutate(Method = extrapolations_formatted[i], 
                   Cancer = outcome_cohorts$cohort_name[j],
                   Age = "All",
                   Sex = data_sex$sex[sexl])
          
          #extract the hazard function over time
          hazot_results_temp[[i]] <- model %>%
            summary(t=(t + 1)/365, type = "hazard",tidy = TRUE) %>%
            mutate(Method = extrapolations_formatted[i], 
                   Cancer = outcome_cohorts$cohort_name[j], 
                   Age = "All" ,
                   Sex = data_sex$sex[sexl])
          
          # median and mean predicted survival
          pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
            rename(median = est) %>% 
            mutate(median = round(median, 4),
                   lcl = round(lcl, 4),
                   ucl = round(ucl, 4),
                   "Median Survival in Years (95% CI)"= ifelse(!is.na(median),
                                                               paste0(paste0(nice.num2(median)), " (",
                                                                      paste0(nice.num2(lcl)),"-",
                                                                      paste0(nice.num2(ucl)), ")"),
                                                               NA)) %>% 
            select(-c(lcl, ucl))
          
          pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
            rename(rmean = est) %>% 
            mutate(rmean = round(rmean, 4),
                   se = round(se, 4),
                   time = round(time,4) ,
                   "rmean in years (SE)"= ifelse(!is.na(rmean),
                                                 paste0(paste0(nice.num2(rmean)), " (",
                                                        paste0(nice.num2(se)), ")"),
                                                 NA)) %>% 
            select(-c(lcl, ucl, se, time))
          
          pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>%
            rename(rmean10yrs = est) %>%
            mutate(rmean10yrs = round(rmean10yrs, 4),
                   se = round(se, 4),
                   time = round(time,4) ,
                   "rmean 10yrs in years (SE)"= ifelse(!is.na(rmean10yrs),
                                                       paste0(paste0(nice.num2(rmean10yrs)), " (",
                                                              paste0(nice.num2(se)), ")"),
                                                       NA)) %>%
            select(-c(lcl, ucl, se, time))
          
          pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
            mutate(est = round((est*100),4),
                   lcl = round((lcl*100),4),
                   ucl = round((ucl*100),4),
                   "Survival Rate % (95% CI)"= ifelse(!is.na(est),
                                                      paste0(paste0(nice.num1(est)), " (",
                                                             paste0(nice.num1(lcl)),"-",
                                                             paste0(nice.num1(ucl)), ")"),
                                                      NA)) %>% 
            rename("surv" = est) %>% 
            select(-c(lcl, ucl)) %>% 
            pivot_wider(names_from = time, 
                        values_from = c(`Survival Rate % (95% CI)`, surv),
                        names_prefix = " year ",
                        names_sep = "")
          
          pred_median_mean_results_temp[[i]] <- bind_cols(pr_mean,pr_mean10, pr_median, pr_survival_prob )
          pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
            mutate(Method = extrapolations_formatted[i], 
                   Cancer = outcome_cohorts$cohort_name[j], 
                   Age = "All",
                   Sex = data_sex$sex[sexl]) 
          
          rm(model, pr_mean, pr_median, pr_mean10, pr_survival_prob)
          
          #print out progress               
          print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohort_name[j], " completed"))
          
          }, 
          
          error = function(e) {
            cat(conditionMessage(e), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "model not carried out for sex model", "\n")
            info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
          
          warning = function(w) {
            cat(conditionMessage(w), "for", outcome_cohorts$cohort_name[j] , ":", extrapolations[i], "potential problem with model for sex model", "\n")
            info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
          
          )
          
        }
        
        
      }
      
      
    }
      
      #put results for gender here
      extrap_sex[[sexl]] <- dplyr::bind_rows(extrap_results_temp)
      gof_sex[[sexl]] <- dplyr::bind_rows(gof_results_temp)
      hot_sex[[sexl]] <- dplyr::bind_rows(hazot_results_temp)   
      par_sex[[sexl]] <- dplyr::bind_rows(parameters_results_temp)
      med_sex[[sexl]] <- dplyr::bind_rows(pred_median_mean_results_temp)
      
      # clear the lists again ready for next iteration
      extrap_results_temp <- list() 
      gof_results_temp <- list() 
      hazot_results_temp <- list() 
      parameters_results_temp <- list() 
      pred_median_mean_results_temp <- list()
      
    }
    
    extrapolatedcombined <- dplyr::bind_rows(extrap_sex)
    gofcombined <- dplyr::bind_rows(gof_sex)
    hotcombined <- dplyr::bind_rows(hot_sex)   
    parcombined <- dplyr::bind_rows(par_sex)
    medcombined <- dplyr::bind_rows(med_sex)
    
    extrapolations_sexS[[j]] <- extrapolatedcombined
    gof_haz_sexS[[j]] <- gofcombined
    hazot_sexS[[j]] <- hotcombined
    parameters_sexS[[j]] <-  parcombined
    pred_median_mean_sexS[[j]] <- medcombined
    
    #print out progress               
    print(paste0(outcome_cohorts$cohort_name[j]," Extrapolation Analysis Completed ", Sys.time()))
    
  } else {
    
    print(paste0("sex stratification extrapolation analysis not carried out for ", outcome_cohorts$cohort_name[j], " due to only 1 sex present " , Sys.time()))
    
  }
  
  
}


# Merge results together from each cancer and extrapolation into a dataframe ---
extrapolatedfinalsexS <- dplyr::bind_rows(extrapolations_sexS) %>%
  mutate(Stratification = "Sex", Adjustment = "None")
goffinalsexS <- dplyr::bind_rows(gof_haz_sexS) %>%
  mutate(Stratification = "Sex" , Adjustment = "None")
hazardotfinalsexS <- dplyr::bind_rows(hazot_sexS) %>%
  mutate(Stratification = "Sex", Adjustment = "None")
parametersfinalsexS <- dplyr::bind_rows(parameters_sexS)  %>%
  mutate(Stratification = "Sex", Adjustment = "None") %>% 
  relocate(shape, .after = Sex) %>% 
  relocate(rate, .after = Sex) %>% 
  mutate(rate = coalesce(rate, `1`)) %>% 
  select(!c(`1`))
predmedmeanfinalsexS <- dplyr::bind_rows(pred_median_mean_sexS)  %>%
  mutate(Stratification = "Sex", Adjustment = "None")

toc(func.toc=toc_min)

info(logger, 'Extrapolation analysis for sex stratification COMPLETE')
