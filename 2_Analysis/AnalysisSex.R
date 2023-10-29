#################################################
# SEX POPULATION
#################################################

# km survival, risk table, median survival, hazard over time from the observed data for each cancer ----
tic("KM analysis for sex population")
info(logger, 'KM analysis for sex population START')

# capture output in list
observedkm_sex <- list()
observedmedianKM_sex <- list()
observedhazotKM_sex <- list()
observedrisktableKM_sex <- list()
observedsurprobsKM_sex <- list()


# loop to carry out for each cancer
for(j in 1:nrow(outcome_cohorts)) {
  
  options(scipen = 999)
  
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
    if(nrow(observedkm_sex[[j]][observedkm_sex[[j]]$Sex == "Female",]) > 6000){
      observedkm_female <- observedkm_sex[[j]][observedkm_sex[[j]]$Sex == "Female",] %>%
        filter(row_number() %% 4 == 1)
    } else if (nrow(observedkm_sex[[j]][observedkm_sex[[j]]$Sex == "Female",]) > 3000 &
               nrow(observedkm_sex[[j]][observedkm_sex[[j]]$Sex == "Female",]) < 6000){
      observedkm_female <- observedkm_sex[[j]][observedkm_sex[[j]]$Sex == "Female",] %>%
        filter(row_number() %% 3 == 1)
    } else if (nrow(observedkm_sex[[j]][observedkm_sex[[j]]$Sex == "Female",]) > 2000 &
               nrow(observedkm_sex[[j]][observedkm_sex[[j]]$Sex == "Female",]) < 3000){
      observedkm_female <- observedkm_sex[[j]][observedkm_sex[[j]]$Sex == "Female",] %>%
        filter(row_number() %% 2 == 1)
    } else {
      observedkm_female <- observedkm_sex[[j]][observedkm_sex[[j]]$Sex == "Female",]
    }

    if(nrow(observedkm_sex[[j]][observedkm_sex[[j]]$Sex == "Male",]) > 6000){
      observedkm_male <- observedkm_sex[[j]][observedkm_sex[[j]]$Sex == "Male",] %>%
        filter(row_number() %% 4 == 1)
    } else if (nrow(observedkm_sex[[j]][observedkm_sex[[j]]$Sex == "Male",]) > 3000 &
               nrow(observedkm_sex[[j]][observedkm_sex[[j]]$Sex == "Male",]) < 6000){
      observedkm_male <- observedkm_sex[[j]][observedkm_sex[[j]]$Sex == "Male",] %>%
        filter(row_number() %% 3 == 1)
    } else if (nrow(observedkm_sex[[j]][observedkm_sex[[j]]$Sex == "Male",]) > 2000 &
               nrow(observedkm_sex[[j]][observedkm_sex[[j]]$Sex == "Male",]) < 3000){
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
      

    # KM median survival ---
    modelKM <- survfit(Surv(time_years, status) ~ sex, data=data) %>%
      summary()

    observedmedianKM_sex[[j]] <- modelKM$table %>%
      as.data.frame() %>%
      tibble::rownames_to_column() %>%  
      rename(Sex = rowname) %>% 
      mutate(Method = "Kaplan-Meier", 
             Cancer = outcome_cohorts$cohort_name[j],
             Sex = str_replace(Sex, "sex=Male", "Male"), Sex = str_replace(Sex,"sex=Female", "Female") ,
             Age = "All" ) 
      
    print(paste0("Median survival from KM from observed data ", Sys.time()," for ",outcome_cohorts$cohort_name[j], " completed"))
    
    # survival probabilities ----
    observedsurprobsKM_sex[[j]] <- do.call(data.frame, cols) %>%
      select(c(time, surv, lower, upper, strata)) %>% 
      rename(Sex = strata) %>% 
      mutate(Sex = str_replace(Sex, "sex=Male", "Male"), Sex = str_replace(Sex,"sex=Female", "Female")) %>% 
      mutate(Method = "Kaplan-Meier", 
             Cancer = outcome_cohorts$cohort_name[j],
             Age = "All",
             surv = round((surv*100),4),
             lower = round((lower*100),4),
             upper = round((upper*100),4),
             "Survival Rate % (95% CI)"= ifelse(!is.na(surv),
                                                paste0(paste0(nice.num1(surv)), " (",
                                                       paste0(nice.num1(lower)),"-",
                                                       paste0(nice.num1(upper)), ")"),
                                                NA)) %>% 
      relocate("Survival Rate % (95% CI)", .before = Method)
    
    # hazard over time ---
    # paper https://arxiv.org/pdf/1509.03253.pdf states bshazard good package
    observedhazotKM_sex[[j]] <- group_by(data,sex) %>% 
      do(as.data.frame(bshazard(Surv(time_years, status)~1, data=., verbose=FALSE))) %>% 
      ungroup %>%
      mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohort_name[j], Age = "All") %>% 
      rename(Sex = sex) 
    
    # reduce the size of haz over time for plotting
    if(nrow(observedhazotKM_sex[[j]][observedhazotKM_sex[[j]]$Sex == "Female",]) > 6000){
      observedhazotKM_female <- observedhazotKM_sex[[j]][observedhazotKM_sex[[j]]$Sex == "Female",] %>%
        filter(row_number() %% 4 == 1)
    } else if (nrow(observedhazotKM_sex[[j]][observedhazotKM_sex[[j]]$Sex == "Female",]) > 3000 &
               nrow(observedhazotKM_sex[[j]][observedhazotKM_sex[[j]]$Sex == "Female",]) < 6000){
      observedhazotKM_female <- observedhazotKM_sex[[j]][observedhazotKM_sex[[j]]$Sex == "Female",] %>%
        filter(row_number() %% 3 == 1)
    } else if (nrow(observedhazotKM_sex[[j]][observedhazotKM_sex[[j]]$Sex == "Female",]) > 2000 &
               nrow(observedhazotKM_sex[[j]][observedhazotKM_sex[[j]]$Sex == "Female",]) < 3000){
      observedhazotKM_female <- observedhazotKM_sex[[j]][observedhazotKM_sex[[j]]$Sex == "Female",] %>%
        filter(row_number() %% 2 == 1)
    } else {
      observedhazotKM_female <- observedhazotKM_sex[[j]][observedhazotKM_sex[[j]]$Sex == "Female",]
    }
    
    if(nrow(observedhazotKM_sex[[j]][observedhazotKM_sex[[j]]$Sex == "Male",]) > 6000){
      observedhazotKM_male <- observedhazotKM_sex[[j]][observedhazotKM_sex[[j]]$Sex == "Male",] %>%
        filter(row_number() %% 4 == 1)
    } else if (nrow(observedhazotKM_sex[[j]][observedhazotKM_sex[[j]]$Sex == "Male",]) > 3000 &
               nrow(observedhazotKM_sex[[j]][observedhazotKM_sex[[j]]$Sex == "Male",]) < 6000){
      observedhazotKM_male <- observedhazotKM_sex[[j]][observedhazotKM_sex[[j]]$Sex == "Male",] %>%
        filter(row_number() %% 3 == 1)
    } else if (nrow(observedhazotKM_sex[[j]][observedhazotKM_sex[[j]]$Sex == "Male",]) > 2000 &
               nrow(observedhazotKM_sex[[j]][observedhazotKM_sex[[j]]$Sex == "Male",]) < 3000){
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

#generate the risk table and remove entries < 5 patients
risktableskm_sex <- dplyr::bind_rows(observedrisktableKM_sex) %>% 
  filter(details != "n.censor") %>% 
  mutate(Stratification = "Sex", Adjustment = "None") %>% 
  mutate(across(everything(), ~replace(., .==  0 , NA))) %>%
  mutate(across(everything(), ~replace(., .<=  5 , "<5"))) %>% 
  mutate(across(everything(), as.character)) %>%
  replace(is.na(.), "0")

# generate results for survival probabilities
survprobtablekm_sex <- dplyr::bind_rows(observedsurprobsKM_sex) %>% 
  mutate(Stratification = "Sex", Adjustment = "None") %>% 
  filter(time != 0.0)

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

#generate the risk table and remove entries < 5 patients
risktableskm_sexA <- dplyr::bind_rows(observedrisktableKM_sex) %>% 
  filter(details != "n.censor") %>% 
  mutate(Stratification = "None", Adjustment = "Sex") %>% 
  mutate(across(everything(), ~replace(., .==  0 , NA))) %>%
  mutate(across(everything(), ~replace(., .<=  5 , "<5"))) %>% 
  mutate(across(everything(), as.character)) %>%
  replace(is.na(.), "0")

# generate results for survival probabilities
survprobtablekm_sexA <- dplyr::bind_rows(observedsurprobsKM_sex) %>% 
  mutate(Stratification = "None", Adjustment = "Sex") %>% 
  filter(time != 0.0)


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
pred_median_mean_sex <- list() # extract the predicted median and RMST from extrapolation methods
pred_survival_prob_sex <- list() # extract the predicted survival times from extrapolation methods


for(j in 1:nrow(outcome_cohorts)) { 
  
  options(scipen = 999)
  
  #temp lists to store results
  extrap_results_temp <- list() 
  gof_results_temp <- list() 
  hazot_results_temp <- list() 
  parameters_results_temp <- list()
  pred_median_mean_results_temp <- list() 
  pred_survival_prob_results_temp <- list()
  
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
        error = function(e){info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
        warning = function(w){info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
      )
      
      if (exists("model") == TRUE) {
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
        pr_median <- predict(model, type = "quantile", p = 0.5) %>% 
          mutate(Sex = data$sex) %>% 
          distinct()
        
        pr_mean <- predict(model, type = "rmst") %>% 
          mutate(Sex = data$sex) %>% 
          distinct()
        
        pred_median_mean_results_temp[[i]] <- left_join(pr_mean, pr_median, by = join_by(Sex))
        pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
          mutate(Method = extrapolations_formatted[i], 
                 Cancer = outcome_cohorts$cohort_name[j], 
                 Age = "All" ,
                 .pred_rmst = round(.pred_rmst,4) ,
                 .pred_quantile = round(.pred_quantile, 4)) %>% 
          rename("RMST time" = .time,
                 "rmean" = .pred_rmst ,
                 "median" = .pred_quantile) %>% 
          select(!c(.quantile))
        
        # survival predicted probabilities from extrapolations
        pred_survival_prob_results_temp[[i]] <- predict(model, type = "survival", times = grid, conf.int = FALSE ) %>% 
          mutate(Sex = data$sex) %>% 
          distinct() %>% 
          tidyr::unnest(.pred) %>% 
          filter(.time != 0.0) %>% 
          mutate(Method = extrapolations_formatted[i], 
                 Cancer = outcome_cohorts$cohort_name[j],
                 Age = "All",
                 .pred_survival = round((.pred_survival*100),4),
                 "Survival Rate % (95% CI)"= ifelse(!is.na(.pred_survival),
                                                         paste0(nice.num1(.pred_survival)),
                                                         NA)) %>% 
          rename(time = .time,
                 "surv" = .pred_survival)
        
        
        rm(model)
        #print out progress               
        print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohort_name[j], " completed"))
      }
      
      
      
    } else if(extrapolations[i] == "spline2") {
      # 2knotspline
      
      tryCatch(
        model <- flexsurvspline(formula=Surv(time_years,status-1) ~ sex , data=data , k = 2, scale = "hazard"),
        error = function(e){info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
        warning = function(w){info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
      )
      
      if (exists("model") == TRUE) {
        
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
        pr_median <- predict(model, type = "quantile", p = 0.5) %>% 
          mutate(Sex = data$sex) %>% 
          distinct() 
        
        pr_mean <- predict(model, type = "rmst") %>% 
          mutate(Sex = data$sex) %>% 
          distinct() 
        
        pred_median_mean_results_temp[[i]] <- left_join(pr_mean, pr_median, by = join_by(Sex))
        pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
          mutate(Method = extrapolations_formatted[i], 
                 Cancer = outcome_cohorts$cohort_name[j], 
                 Age = "All",
                 .pred_rmst = round(.pred_rmst,4) ,
                 .pred_quantile = round(.pred_quantile, 4)) %>% 
          rename("RMST time" = .time,
                 "rmean" = .pred_rmst ,
                 "median" = .pred_quantile) %>% 
          select(!c(.quantile))
        
        # survival predicted probabilities from extrapolations
        pred_survival_prob_results_temp[[i]] <- predict(model, type = "survival", times = grid, conf.int = FALSE ) %>% 
          mutate(Sex = data$sex) %>% 
          distinct() %>% 
          tidyr::unnest(.pred) %>% 
          filter(.time != 0.0) %>% 
          mutate(Method = extrapolations_formatted[i], 
                 Cancer = outcome_cohorts$cohort_name[j],
                 Age = "All",
                 .pred_survival = round((.pred_survival*100),4),
                 "Survival Rate % (95% CI)"= ifelse(!is.na(.pred_survival),
                                                    paste0(nice.num1(.pred_survival)),
                                                    NA)) %>% 
          rename(time = .time,
                 "surv" = .pred_survival)
        
        #print out progress
        rm(model)
        print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohort_name[j], " completed"))
        
      }
      
    } else if(extrapolations[i] == "spline3") {
      # 3knotspline
      
      tryCatch(
        model <- flexsurvspline(formula=Surv(time_years,status-1) ~ sex, data=data, k = 3, scale = "hazard"),
        error = function(e){info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
        warning = function(w){info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
      )
      
      if (exists("model") == TRUE) {
        
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
        pr_median <- predict(model, type = "quantile", p = 0.5) %>% 
          mutate(Sex = data$sex) %>% 
          distinct()  
        
        pr_mean <- predict(model, type = "rmst") %>% 
          mutate(Sex = data$sex) %>% 
          distinct()  
        
        pred_median_mean_results_temp[[i]] <- left_join(pr_mean, pr_median, by = join_by(Sex))
        pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
          mutate(Method = extrapolations_formatted[i], 
                 Cancer = outcome_cohorts$cohort_name[j], 
                 Age = "All" ,
                 .pred_rmst = round(.pred_rmst,4) ,
                 .pred_quantile = round(.pred_quantile, 4)) %>% 
          rename("RMST time" = .time,
                 "rmean" = .pred_rmst ,
                 "median" = .pred_quantile) %>% 
          select(!c(.quantile))
        
        # survival predicted probabilities from extrapolations
        pred_survival_prob_results_temp[[i]] <- predict(model, type = "survival", times = grid, conf.int = FALSE ) %>% 
          mutate(Sex = data$sex) %>% 
          distinct()  %>% 
          tidyr::unnest(.pred) %>% 
          filter(.time != 0.0) %>% 
          mutate(Method = extrapolations_formatted[i], 
                 Cancer = outcome_cohorts$cohort_name[j],
                 Age = "All",
                 .pred_survival = round((.pred_survival*100),4),
                 "Survival Rate % (95% CI)"= ifelse(!is.na(.pred_survival),
                                                    paste0(nice.num1(.pred_survival)),
                                                    NA)) %>% 
          rename(time = .time,
                 "surv" = .pred_survival)
        
        rm(model)
        #print out progress               
        print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohort_name[j], " completed"))
      }
      
    } else if(extrapolations[i] == "spline5") {
      # 5knotspline
      
      tryCatch(
        model <- flexsurvspline(formula=Surv(time_years,status-1) ~ sex , data=data, k = 5, scale = "hazard"),
        error = function(e){info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
        warning = function(w){info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
      )
      
      if (exists("model") == TRUE) {
        
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
        pr_median <- predict(model, type = "quantile", p = 0.5) %>% 
          mutate(Sex = data$sex) %>% 
          distinct()  
        
        pr_mean <- predict(model, type = "rmst") %>% 
          mutate(Sex = data$sex) %>% 
          distinct() 
        
        pred_median_mean_results_temp[[i]] <- left_join(pr_mean, pr_median, by = join_by(Sex))
        pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
          mutate(Method = extrapolations_formatted[i], 
                 Cancer = outcome_cohorts$cohort_name[j], 
                 Age = "All" ,
                 .pred_rmst = round(.pred_rmst,4) ,
                 .pred_quantile = round(.pred_quantile, 4)) %>% 
          rename("RMST time" = .time,
                 "rmean" = .pred_rmst ,
                 "median" = .pred_quantile) %>% 
          select(!c(.quantile))
        
        # survival predicted probabilities from extrapolations
        pred_survival_prob_results_temp[[i]] <- predict(model, type = "survival", times = grid, conf.int = FALSE ) %>% 
          mutate(Sex = data$sex) %>% 
          distinct() %>% 
          tidyr::unnest(.pred) %>% 
          filter(.time != 0.0) %>% 
          mutate(Method = extrapolations_formatted[i], 
                 Cancer = outcome_cohorts$cohort_name[j],
                 Age = "All",
                 .pred_survival = round((.pred_survival*100),4),
                 "Survival Rate % (95% CI)"= ifelse(!is.na(.pred_survival),
                                                    paste0(nice.num1(.pred_survival)),
                                                    NA)) %>% 
          rename(time = .time,
                 "surv" = .pred_survival)
        
        rm(model)
        #print out progress               
        print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohort_name[j], " completed"))
      }
      
    } else {
      
      #carry out models for different parametric methods survival
      tryCatch(
        model <- flexsurvreg(Surv(time_years, status) ~ sex, data=data, dist=extrapolations[i]),
        error = function(e){info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
        warning = function(w){info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
      )
      
      if (exists("model") == TRUE) {
        
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
        pr_median <- predict(model, type = "quantile", p = 0.5) %>% 
          mutate(Sex = data$sex) %>% 
          distinct() 
        
        pr_mean <- predict(model, type = "rmst") %>% 
          mutate(Sex = data$sex) %>% 
          distinct() 
        
        pred_median_mean_results_temp[[i]] <- left_join(pr_mean, pr_median, by = join_by(Sex))
        pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
          mutate(Method = extrapolations_formatted[i], 
                 Cancer = outcome_cohorts$cohort_name[j], 
                 Age = "All" ,
                 .pred_rmst = round(.pred_rmst,4) ,
                 .pred_quantile = round(.pred_quantile, 4)) %>% 
          rename("RMST time" = .time,
                 "rmean" = .pred_rmst ,
                 "median" = .pred_quantile) %>% 
          select(!c(.quantile))
        
        # survival predicted probabilities from extrapolations
        pred_survival_prob_results_temp[[i]] <- predict(model, type = "survival", times = grid, conf.int = FALSE ) %>% 
          mutate(Sex = data$sex) %>% 
          distinct() %>% 
          tidyr::unnest(.pred) %>% 
          filter(.time != 0.0) %>% 
          mutate(Method = extrapolations_formatted[i], 
                 Cancer = outcome_cohorts$cohort_name[j],
                 Age = "All",
                 .pred_survival = round((.pred_survival*100),4),
                 "Survival Rate % (95% CI)"= ifelse(!is.na(.pred_survival),
                                                    paste0(nice.num1(.pred_survival)),
                                                    NA)) %>% 
          rename(time = .time,
                 "surv" = .pred_survival)
        
        rm(model)
        #print out progress               
        print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohort_name[j], " completed"))
        
      }
      
      
    }
    
    
  }
    
    extrapolatedcombined <- dplyr::bind_rows(extrap_results_temp)
    gofcombined <- dplyr::bind_rows(gof_results_temp)
    hotcombined <- dplyr::bind_rows(hazot_results_temp)   
    parcombined <- dplyr::bind_rows(parameters_results_temp)
    surcombined <- dplyr::bind_rows(pred_survival_prob_results_temp)
    medcombined <- dplyr::bind_rows(pred_median_mean_results_temp)
    
    extrapolations_sex[[j]] <- extrapolatedcombined
    gof_haz_sex[[j]] <- gofcombined
    hazot_sex[[j]] <- hotcombined
    parameters_sex[[j]] <-  parcombined
    pred_median_mean_sex[[j]] <- medcombined
    pred_survival_prob_sex[[j]] <- surcombined 
    
    
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
predsurvivalprobfinalsex <- dplyr::bind_rows(pred_survival_prob_sex)  %>%
  mutate(Stratification = "None", Adjustment = "Sex")
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
pred_survival_prob_sexS <- list()

for(j in 1:nrow(outcome_cohorts)) { 
  
  options(scipen = 999)
  
  #temp results
  extrap_results_temp <- list() 
  gof_results_temp <- list() 
  hazot_results_temp <- list()
  parameters_results_temp <- list() 
  pred_median_mean_results_temp <- list() 
  pred_survival_prob_results_temp <- list()
  
  #for each sex 
  extrap_sex <- list()
  gof_sex <- list()
  hot_sex <- list()   
  par_sex <- list()
  med_sex <- list()   
  surprob_sex <- list()
  
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
      
      #split per gender then run extrapolations
      print(paste0("extrapolations for stratification"," ", Sys.time()," for " ,outcome_cohorts$cohort_name[j]," ", sexvalues$sex[sexl] ," started"))
    
    #carry out extrapolation for each cancer
    for(i in 1:length(extrapolations)) {   # Head of for-loop
      
      if(extrapolations[i] == "spline1") {
        
        # 1knotspline
        tryCatch(
          model <- flexsurvspline(formula=Surv(time_years,status-1) ~ 1 ,data=data_sex,k = 1, scale = "hazard"),
          error = function(e){info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
          warning = function(w){info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        )
        
        if (exists("model") == TRUE) {
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
          
          # median and mean survival predictions from extrapolation
          pr_median <- predict(model, type = "quantile", p = 0.5) %>% 
            distinct()  
          
          pr_mean <- predict(model, type = "rmst") %>% 
            distinct()  
          
          pred_median_mean_results_temp[[i]] <- bind_cols(pr_mean, pr_median)
          pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
            mutate(Method = extrapolations_formatted[i], 
                   Cancer = outcome_cohorts$cohort_name[j], 
                   Age = "All", 
                   Sex = data_sex$sex[sexl] ,
                   .pred_rmst = round(.pred_rmst,4) ,
                   .pred_quantile = round(.pred_quantile, 4)) %>% 
            rename("RMST time" = .time,
                   "rmean" = .pred_rmst ,
                   "median" = .pred_quantile) %>% 
            select(!c(.quantile))
          
          # survival predicted probabilities from extrapolations
          pred_survival_prob_results_temp[[i]] <- predict(model, type = "survival", times = grid, conf.int = FALSE ) %>% 
            distinct() %>% 
            tidyr::unnest(.pred) %>% 
            filter(.time != 0.0) %>% 
            mutate(Method = extrapolations_formatted[i], 
                   Cancer = outcome_cohorts$cohort_name[j],
                   Age = "All",
                   Sex = data_sex$sex[sexl],
                   .pred_survival = round((.pred_survival*100),4),
                   "Survival Rate % (95% CI)"= ifelse(!is.na(.pred_survival),
                                                      paste0(nice.num1(.pred_survival)),
                                                      NA)) %>% 
            rename(time = .time,
                   "surv" = .pred_survival)
          
          
          rm(model)
          #print out progress               
          print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohort_name[j], " completed"))
        }
        
        
        
      } else if(extrapolations[i] == "spline2") {
        # 2knotspline
        
        tryCatch(
          model <- flexsurvspline(formula=Surv(time_years,status-1) ~ 1 , data=data_sex , k = 2, scale = "hazard"),
          error = function(e){info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
          warning = function(w){info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        )
        
        if (exists("model") == TRUE) {
          
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
          
          # median and mean survival predictions from extrapolation
          pr_median <- predict(model, type = "quantile", p = 0.5) %>% 
            distinct()  
          
          pr_mean <- predict(model, type = "rmst") %>% 
            distinct()  
          
          pred_median_mean_results_temp[[i]] <- bind_cols(pr_mean, pr_median)
          pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
            mutate(Method = extrapolations_formatted[i], 
                   Cancer = outcome_cohorts$cohort_name[j], 
                   Age = "All", 
                   Sex = data_sex$sex[sexl]  ,
                   .pred_rmst = round(.pred_rmst,4) ,
                   .pred_quantile = round(.pred_quantile, 4)) %>% 
            rename("RMST time" = .time,
                   "rmean" = .pred_rmst ,
                   "median" = .pred_quantile) %>% 
            select(!c(.quantile))
          
          # survival predicted probabilities from extrapolations
          pred_survival_prob_results_temp[[i]] <- predict(model, type = "survival", times = grid, conf.int = FALSE ) %>% 
            distinct() %>% 
            tidyr::unnest(.pred) %>% 
            filter(.time != 0.0) %>% 
            mutate(Method = extrapolations_formatted[i], 
                   Cancer = outcome_cohorts$cohort_name[j],
                   Age = "All",
                   Sex = data_sex$sex[sexl],
                   .pred_survival = round((.pred_survival*100),4),
                   "Survival Rate % (95% CI)"= ifelse(!is.na(.pred_survival),
                                                      paste0(nice.num1(.pred_survival)),
                                                      NA)) %>% 
            rename(time = .time,
                   "surv" = .pred_survival)
          
          #print out progress
          rm(model)
          print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohort_name[j], " completed"))
          
        }
        
      } else if(extrapolations[i] == "spline3") {
        # 3knotspline
        
        tryCatch(
          model <- flexsurvspline(formula=Surv(time_years,status-1) ~ 1, data=data_sex, k = 3, scale = "hazard"),
          error = function(e){info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
          warning = function(w){info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        )
        
        if (exists("model") == TRUE) {
          
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
          
          # median and mean survival predictions from extrapolation
          pr_median <- predict(model, type = "quantile", p = 0.5) %>% 
            distinct()  
          
          pr_mean <- predict(model, type = "rmst") %>% 
            distinct()  
          
          pred_median_mean_results_temp[[i]] <- bind_cols(pr_mean, pr_median)
          pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
            mutate(Method = extrapolations_formatted[i], 
                   Cancer = outcome_cohorts$cohort_name[j], 
                   Age = "All", 
                   Sex = data_sex$sex[sexl] ,
                   .pred_rmst = round(.pred_rmst,4) ,
                   .pred_quantile = round(.pred_quantile, 4)) %>% 
            rename("RMST time" = .time,
                   "rmean" = .pred_rmst ,
                   "median" = .pred_quantile) %>% 
            select(!c(.quantile))
          
          # survival predicted probabilities from extrapolations
          pred_survival_prob_results_temp[[i]] <- predict(model, type = "survival", times = grid, conf.int = FALSE ) %>% 
            distinct() %>% 
            tidyr::unnest(.pred) %>% 
            filter(.time != 0.0) %>% 
            mutate(Method = extrapolations_formatted[i], 
                   Cancer = outcome_cohorts$cohort_name[j],
                   Age = "All",
                   Sex = data_sex$sex[sexl],
                   .pred_survival = round((.pred_survival*100),4),
                   "Survival Rate % (95% CI)"= ifelse(!is.na(.pred_survival),
                                                      paste0(nice.num1(.pred_survival)),
                                                      NA)) %>% 
            rename(time = .time,
                   "surv" = .pred_survival)
          
          rm(model)
          #print out progress               
          print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohort_name[j], " completed"))
        }
        
      } else if(extrapolations[i] == "spline5") {
        # 5knotspline
        
        tryCatch(
          model <- flexsurvspline(formula=Surv(time_years,status-1) ~ 1 , data=data_sex, k = 5, scale = "hazard"),
          error = function(e){info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
          warning = function(w){info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        )
        
        if (exists("model") == TRUE) {
          
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
          
          # median and mean survival predictions from extrapolation
          pr_median <- predict(model, type = "quantile", p = 0.5) %>% 
            distinct()  
          
          pr_mean <- predict(model, type = "rmst") %>% 
            distinct()  
          
          pred_median_mean_results_temp[[i]] <- bind_cols(pr_mean, pr_median)
          pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
            mutate(Method = extrapolations_formatted[i], 
                   Cancer = outcome_cohorts$cohort_name[j], 
                   Age = "All", 
                   Sex = data_sex$sex[sexl]  ,
                   .pred_rmst = round(.pred_rmst,4) ,
                   .pred_quantile = round(.pred_quantile, 4)) %>% 
            rename("RMST time" = .time,
                   "rmean" = .pred_rmst ,
                   "median" = .pred_quantile) %>% 
            select(!c(.quantile))
          
          # survival predicted probabilities from extrapolations
          pred_survival_prob_results_temp[[i]] <- predict(model, type = "survival", times = grid, conf.int = FALSE ) %>% 
            distinct() %>% 
            tidyr::unnest(.pred) %>% 
            filter(.time != 0.0) %>% 
            mutate(Method = extrapolations_formatted[i], 
                   Cancer = outcome_cohorts$cohort_name[j],
                   Age = "All",
                   Sex = data_sex$sex[sexl],
                   .pred_survival = round((.pred_survival*100),4),
                   "Survival Rate % (95% CI)"= ifelse(!is.na(.pred_survival),
                                                      paste0(nice.num1(.pred_survival)),
                                                      NA)) %>% 
            rename(time = .time,
                   "surv" = .pred_survival)
          
          rm(model)
          #print out progress               
          print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohort_name[j], " completed"))
        }
        
      } else {
        
        #carry out models for different parametric methods survival
        tryCatch(
          model <- flexsurvreg(Surv(time_years, status) ~ 1, data=data_sex, dist=extrapolations[i]),
          error = function(e){info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
          warning = function(w){info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        )
        
        if (exists("model") == TRUE) {
          
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
          
          # median and mean survival predictions from extrapolation
          pr_median <- predict(model, type = "quantile", p = 0.5) %>% 
            distinct()  
          
          pr_mean <- predict(model, type = "rmst") %>% 
            distinct()  
          
          pred_median_mean_results_temp[[i]] <- bind_cols(pr_mean, pr_median)
          pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
            mutate(Method = extrapolations_formatted[i], 
                   Cancer = outcome_cohorts$cohort_name[j], 
                   Age = "All", 
                   Sex = data_sex$sex[sexl] ,
                   .pred_rmst = round(.pred_rmst,4) ,
                   .pred_quantile = round(.pred_quantile, 4)) %>% 
            rename("RMST time" = .time,
                   "rmean" = .pred_rmst ,
                   "median" = .pred_quantile) %>% 
            select(!c(.quantile))
          
          # survival predicted probabilities from extrapolations
          pred_survival_prob_results_temp[[i]] <- predict(model, type = "survival", times = grid, conf.int = FALSE ) %>% 
            distinct() %>% 
            tidyr::unnest(.pred) %>% 
            filter(.time != 0.0) %>% 
            mutate(Method = extrapolations_formatted[i], 
                   Cancer = outcome_cohorts$cohort_name[j],
                   Age = "All",
                   Sex = data_sex$sex[sexl],
                   .pred_survival = round((.pred_survival*100),4),
                   "Survival Rate % (95% CI)"= ifelse(!is.na(.pred_survival),
                                                      paste0(nice.num1(.pred_survival)),
                                                      NA)) %>% 
            rename(time = .time,
                   "surv" = .pred_survival)
          
          rm(model)
          #print out progress               
          print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohort_name[j], " completed"))
          
        }
        
        
      }
      
      
    }
      
      #put results for gender here
      extrap_sex[[sexl]] <- dplyr::bind_rows(extrap_results_temp)
      gof_sex[[sexl]] <- dplyr::bind_rows(gof_results_temp)
      hot_sex[[sexl]] <- dplyr::bind_rows(hazot_results_temp)   
      par_sex[[sexl]] <- dplyr::bind_rows(parameters_results_temp)
      med_sex[[sexl]] <- dplyr::bind_rows(pred_median_mean_results_temp)
      surprob_sex[[sexl]] <- dplyr::bind_rows(pred_survival_prob_results_temp)
      
      # clear the lists again ready for next iteration
      extrap_results_temp <- list() 
      gof_results_temp <- list() 
      hazot_results_temp <- list() 
      parameters_results_temp <- list() 
      pred_median_mean_results_temp <- list() 
      pred_survival_prob_results_temp <- list()
      
    }
    
    extrapolatedcombined <- dplyr::bind_rows(extrap_sex)
    gofcombined <- dplyr::bind_rows(gof_sex)
    hotcombined <- dplyr::bind_rows(hot_sex)   
    parcombined <- dplyr::bind_rows(par_sex)
    surcombined <- dplyr::bind_rows(surprob_sex)
    medcombined <- dplyr::bind_rows(med_sex)
    
    extrapolations_sexS[[j]] <- extrapolatedcombined
    gof_haz_sexS[[j]] <- gofcombined
    hazot_sexS[[j]] <- hotcombined
    parameters_sexS[[j]] <-  parcombined
    pred_median_mean_sexS[[j]] <- medcombined
    pred_survival_prob_sexS[[j]] <- surcombined
    
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
predsurvivalprobfinalsexS <- dplyr::bind_rows(pred_survival_prob_sexS)  %>%
  mutate(Stratification = "Sex", Adjustment = "None") 
predmedmeanfinalsexS <- dplyr::bind_rows(pred_median_mean_sexS)  %>%
  mutate(Stratification = "Sex", Adjustment = "None")

toc(func.toc=toc_min)

info(logger, 'Extrapolation analysis for sex stratification COMPLETE')
