#################################################
# WHOLE POPULATION
#################################################

# km survival, risk table, median survival, hazard over time from the observed data for each cancer ----

tic("KM analysis for whole population")
info(logger, 'KM analysis for whole population START')

# capture output in list
observedkm <- list()
observedmedianKM <- list()
observedhazotKM <- list()
observedrisktableKM <- list()
observedsurprobsKM <- list()

# loop to carry out for each cancer
for(j in 1:nrow(outcome_cohorts)) {
  
options(scipen = 999)

#subset the data by cancer type
data <- Pop %>%
  filter(cohort_definition_id == j)

#carry out km estimate ---
observedkm[[j]] <- survfit (Surv(time_years, status) ~ 1, data=data) %>%
  tidy() %>%
  mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohort_name[j], Age = "All", Sex = "Both")

print(paste0("KM for observed data ", Sys.time()," for ",outcome_cohorts$cohort_name[j], " completed"))

# get risk table for specific times ---
grid <- seq(0,floor(max(data$time_years)),by=0.5) #get the number of years every half year
grid <-  grid[(str_detect(grid, "[1-9]\\.5", negate = TRUE )) & (str_detect(grid, "10.5", negate = TRUE )) &
                (str_detect(grid, "20.5", negate = TRUE )) & (str_detect(grid, "30.5", negate = TRUE ))] # remove all the half years apart from the first half year
  
sprob <- survfit(Surv(time_years, status) ~ 1, data=data) %>% 
  summary(times = grid, extend = TRUE)
cols <- lapply(c(2:15) , function(x) sprob[x])

observedrisktableKM[[j]] <- do.call(data.frame, cols) %>%
  select(c(n.risk, n.event, n.censor)) %>% 
  t() %>% 
  as_tibble() %>% 
  `colnames<-`(grid) %>% 
  mutate(Method = "Kaplan-Meier", 
         Cancer = outcome_cohorts$cohort_name[j],
         Sex = "Both" ,
         Age = "All",
         details = c("n.risk", "n.event", "n.censor")) %>% 
  relocate(details)

print(paste0("Extract risk table ", Sys.time()," for ",outcome_cohorts$cohort_name[j], " completed"))


# get surv probs for specific times ---
observedsurprobsKM[[j]] <- do.call(data.frame, cols) %>%
  select(c(time, surv, lower, upper)) %>% 
  mutate(Method = "Kaplan-Meier", 
         Cancer = outcome_cohorts$cohort_name[j],
         Sex = "Both" ,
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

# KM median survival---
modelKM <- survfit(Surv(time_years, status) ~ 1, data=data) %>%
  summary()

observedmedianKM[[j]] <- modelKM$table %>%
  as.data.frame() %>%
  tibble::rownames_to_column() %>%  
  pivot_longer(-rowname) %>% 
  pivot_wider(names_from=rowname, values_from=value) %>%
  mutate(Method = "Kaplan-Meier", 
         Cancer = outcome_cohorts$cohort_name[j],
         Sex = "Both" ,
         Age = "All" ) %>%
          select(-name)
  

print(paste0("Median survival from KM from observed data ", Sys.time()," for ",outcome_cohorts$cohort_name[j], " completed"))

# hazard function over time ----
# paper https://arxiv.org/pdf/1509.03253.pdf states bshazard good package

observedhazotKM[[j]] <- as.data.frame.bshazard(bshazard(Surv(time_years, status) ~ 1, data=data, verbose=FALSE)) %>%
  mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohort_name[j], Age = "All", Sex = "Both")

print(paste0("Hazard over time results for KM ", Sys.time()," for ",outcome_cohorts$cohort_name[j], " completed"))

}

# take the results from a list (one element for each cancer) and put into dataframe for KM survival
observedkmcombined <- dplyr::bind_rows(observedkm) %>%
  rename(est = estimate ,ucl = conf.high, lcl = conf.low )  %>%
  mutate(Stratification = "None", Adjustment = "None")

medkmcombined <- dplyr::bind_rows(observedmedianKM) %>%
  mutate(Stratification = "None", Adjustment = "None")

hotkmcombined <- dplyr::bind_rows(observedhazotKM) %>%
  rename(est = hazard, ucl = upper.ci, lcl = lower.ci ) %>%
  mutate(Stratification = "None", Adjustment = "None")

# generate results for risk table with those at risk 
risktableskm <- dplyr::bind_rows(observedrisktableKM) %>% 
  filter(details != "n.censor") %>% 
  mutate(Stratification = "None", Adjustment = "None") %>% 
  mutate(across(everything(), ~replace(., .==  0 , NA))) %>%
  mutate(across(everything(), ~replace(., .<=  5 , "<5"))) %>% 
  mutate(across(everything(), as.character)) %>%
  replace(is.na(.), "0") 

# generate results for survival probabilities
survprobtablekm <- dplyr::bind_rows(observedsurprobsKM) %>% 
  mutate(Stratification = "None", Adjustment = "None") %>% 
  filter(time != 0.0)
  
toc(func.toc=toc_min)
info(logger, 'KM analysis for whole population COMPLETE')

###########################################

# Extrapolation analysis for whole population ------

tic("Extrapolation analysis for whole population")
info(logger, 'Extrapolation analysis for whole population START')

# Initiate lists to store output ---- 
extrapolations_all <- list() # extrapolation over time
gof_haz_all <- list() # goodness of fit
hazot_all <- list() # hazard over time 
parameters_all <- list() # parameters from each model
pred_median_mean_all <- list() # extract the predicted median and RMST from extrapolation methods
pred_survival_prob_all <- list() # extract the predicted survival times from extrapolation methods

# Running analysis for each cancer
for(j in 1:nrow(outcome_cohorts)) {
  
  options(scipen = 999)
  
  # create empty lists for temp results for each cancer
  extrap_results_temp <- list() 
  gof_results_temp <- list()
  hazot_results_temp <- list()
  parameters_results_temp <- list() 
  pred_median_mean_results_temp <- list() 
  pred_survival_prob_results_temp <- list()

  #subset the data by cancer type
  data <- Pop %>%
    filter(cohort_definition_id == j) 
  
  #carry out extrapolation for each cancer
  for(i in 1:length(extrapolations)) {   # Head of for-loop
    
    if(extrapolations[i] == "spline1") {
      
      # 1knotspline
     tryCatch(
        model <- flexsurvspline(formula=Surv(time_years,status-1)~1,data=data,k = 1, scale = "hazard"),
        error = function(e){info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
        warning = function(w){info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
      )

      if (exists("model") == TRUE) {
      #extrapolation
      extrap_results_temp[[i]] <- model %>%
        summary(t=t/365, tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
      
      #get the goodness of fit for each model
      gof_results_temp[[i]] <- model %>%
        glance() %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
      
      #grab the parameters and knots from the model
      coefs.p <- model[["coefficients"]] %>%
        enframe() %>%
        pivot_wider(names_from = name, values_from = value) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohort_name[j], Age = "All", Sex = "Both" ) 
      
      knots.p <- model[["knots"]] %>%
        setNames(., c("SplineLowerB", "SplineInternal1" , "SplineUpperB")) %>%
        enframe() %>%
        pivot_wider(names_from = name, values_from = value)
      
      parameters_results_temp[[i]] <- bind_cols(coefs.p,  knots.p )

      # hazard over time
      hazot_results_temp[[i]] <- model %>%
        summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
      
      
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
               Sex = "Both" ,
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
        mutate(Method = extrapolations_formatted[i], 
               Cancer = outcome_cohorts$cohort_name[j],
               .pred_survival = round((.pred_survival*100),4),
               Age = "All",
               Sex = "Both",
               "Survival Rate % (95% CI)"= ifelse(!is.na(.pred_survival),
                                                       paste0(nice.num1(.pred_survival)),
                                                       NA)) %>% 
        rename(time = .time,
               "surv" = .pred_survival )
 
      


      
      rm(model)
      #print out progress               
      print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohort_name[j], " completed"))
      }
      
      
    } else if(extrapolations[i] == "spline2") {
      # 2knotspline
      
      tryCatch(
        model <- flexsurvspline(formula=Surv(time_years,status-1)~1,data=data,k = 2, scale = "hazard"),
        error = function(e){info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
        warning = function(w){info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
      )
      
      if (exists("model") == TRUE) {
      
      extrap_results_temp[[i]] <- model %>%
        summary(t=t/365, tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
      
      #get the goodness of fit for each model
      gof_results_temp[[i]] <- model %>%
        glance() %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
      
      #extract parameters
      #grab the parameters and knots from the model
      coefs.p <- model[["coefficients"]] %>%
        enframe() %>%
        pivot_wider(names_from = name, values_from = value) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohort_name[j], Age = "All", Sex = "Both" ) 
      
      knots.p <- model[["knots"]] %>%
        setNames(., c("SplineLowerB", "SplineInternal1" , "SplineInternal2" ,"SplineUpperB")) %>%
        enframe() %>%
        pivot_wider(names_from = name, values_from = value)
      parameters_results_temp[[i]] <- bind_cols(coefs.p,  knots.p )
      
      # hazard over time
      hazot_results_temp[[i]] <- model %>%
        summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
      
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
               Sex = "Both" ,
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
               .pred_survival = round((.pred_survival*100),4),
               Age = "All",
               Sex = "Both",
               "Survival Rate % (95% CI)"= ifelse(!is.na(.pred_survival),
                                                  paste0(nice.num1(.pred_survival)),
                                                  NA)) %>% 
        rename(time = .time,
               "surv" = .pred_survival )
      
      
      #print out progress
      rm(model)
      print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohort_name[j], " completed"))
      
      }
      
    } else if(extrapolations[i] == "spline3") {
      # 3knotspline
      
      tryCatch(
        model <- flexsurvspline(formula=Surv(time_years,status-1)~1, data=data, k = 3, scale = "hazard"),
        error = function(e){info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
        warning = function(w){info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
      )
      
      if (exists("model") == TRUE) {
      
      extrap_results_temp[[i]] <- model %>%
        summary(t=t/365, tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohort_name[j], Age = "All", Sex = "Both" )

      #get the goodness of fit for each model
      gof_results_temp[[i]] <- model %>%
        glance() %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
      
      #extract parameters
      #grab the parameters and knots from the model
      coefs.p <- model[["coefficients"]] %>%
        enframe() %>%
        pivot_wider(names_from = name, values_from = value) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohort_name[j], Age = "All", Sex = "Both" ) 
      
      knots.p <- model[["knots"]] %>%
        setNames(., c("SplineLowerB", "SplineInternal1" , "SplineInternal2", "SplineInternal3" ,"SplineUpperB")) %>%
        enframe() %>%
        pivot_wider(names_from = name, values_from = value)
      
      parameters_results_temp[[i]] <- bind_cols(coefs.p,  knots.p )
      
      # hazard over time
      hazot_results_temp[[i]] <- model %>%
        summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
      
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
               Sex = "Both" ,
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
        mutate(Method = extrapolations_formatted[i], 
               Cancer = outcome_cohorts$cohort_name[j],
               .pred_survival = round((.pred_survival*100),4),
               Age = "All",
               Sex = "Both",
               "Survival Rate % (95% CI)"= ifelse(!is.na(.pred_survival),
                                                  paste0(nice.num1(.pred_survival)),
                                                  NA)) %>% 
        rename(time = .time,
               "surv" = .pred_survival )
      
      
      rm(model)
      #print out progress               
      print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohort_name[j], " completed"))
      }
      
    } else if(extrapolations[i] == "spline5") {
      # 5knotspline
      
      tryCatch(
        model <- flexsurvspline(formula=Surv(time_years,status-1)~1, data=data, k = 5, scale = "hazard"),
        error = function(e){info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
        warning = function(w){info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
      )
      
      if (exists("model") == TRUE) {
      
      extrap_results_temp[[i]] <- model %>%
        summary(t=t/365, tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
      
      #get the goodness of fit for each model
      gof_results_temp[[i]] <- model %>%
        glance() %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
      
      coefs.p <- model[["coefficients"]] %>%
        enframe() %>%
        pivot_wider(names_from = name, values_from = value) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohort_name[j], Age = "All", Sex = "Both" ) 
      
      knots.p <- model[["knots"]] %>%
        setNames(., c("SplineLowerB", "SplineInternal1" , "SplineInternal2", "SplineInternal3" ,
                      "SplineInternal4" ,"SplineInternal5" , "SplineUpperB")) %>%
        enframe() %>%
        pivot_wider(names_from = name, values_from = value)
      
      parameters_results_temp[[i]] <- bind_cols(coefs.p,  knots.p )
      
      # hazard over time
      hazot_results_temp[[i]] <- model %>%
        summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
      
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
               Sex = "Both" ,
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
        mutate(Method = extrapolations_formatted[i], 
               Cancer = outcome_cohorts$cohort_name[j],
               .pred_survival = round((.pred_survival*100),4),
               Age = "All",
               Sex = "Both",
               "Survival Rate % (95% CI)"= ifelse(!is.na(.pred_survival),
                                                  paste0(nice.num1(.pred_survival)),
                                                  NA)) %>% 
        rename(time = .time,
               "surv" = .pred_survival )
      
      
      rm(model)
      #print out progress               
      print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohort_name[j], " completed"))
      }
      
    } else {
      
      #carry out models for different parametric methods survival
      tryCatch(
      model <- flexsurvreg(Surv(time_years, status)~1, data=data, dist=extrapolations[i]),
      error = function(e){info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
      warning = function(w){info(logger, paste0(outcome_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
      )
      
      if (exists("model") == TRUE) {
      
      # extrapolations
      extrap_results_temp[[i]] <- model %>%
        summary(t=t/365, tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
      
      #get the goodness of fit for each model
      gof_results_temp[[i]] <- model %>%
        glance() %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
      
      #grab the parameters from the model
      parameters_results_temp[[i]] <- model[["coefficients"]] %>%
        enframe() %>%
        pivot_wider(names_from = name, values_from = value) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohort_name[j], Age = "All", Sex = "Both" ) 
      
      #extract the hazard function over time
      hazot_results_temp[[i]] <- model %>%
        summary(t=(t + 1)/365, type = "hazard",tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
      
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
               Sex = "Both" ,
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
        mutate(Method = extrapolations_formatted[i], 
               Cancer = outcome_cohorts$cohort_name[j],
               .pred_survival = round((.pred_survival*100),4),
               Age = "All",
               Sex = "Both",
               "Survival Rate % (95% CI)"= ifelse(!is.na(.pred_survival),
                                                  paste0(nice.num1(.pred_survival)),
                                                  NA)) %>% 
        rename(time = .time,
               "surv" = .pred_survival )
      
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
  
  extrapolations_all[[j]] <- extrapolatedcombined
  gof_haz_all[[j]] <- gofcombined
  hazot_all[[j]] <- hotcombined
  parameters_all[[j]] <-  parcombined
  pred_median_mean_all[[j]] <- medcombined
  pred_survival_prob_all[[j]] <- surcombined

  #print out progress               
  print(paste0(outcome_cohorts$cohort_name[j]," Extrapolation Analysis Completed ", Sys.time()))

}

# Merge results together from each cancer and extrapolation into a dataframe ---
extrapolatedfinal <- dplyr::bind_rows(extrapolations_all)  %>%
  mutate(Stratification = "None", Adjustment = "None")
goffinal <- dplyr::bind_rows(gof_haz_all)  %>%
  mutate(Stratification = "None", Adjustment = "None")
hazardotfinal <- dplyr::bind_rows(hazot_all)  %>%
  mutate(Stratification = "None", Adjustment = "None")
parametersfinal <- dplyr::bind_rows(parameters_all)  %>%
  mutate(Stratification = "None", Adjustment = "None") %>% 
  relocate(shape, .after = Sex) %>% 
  relocate(rate, .after = Sex) %>% 
  mutate(rate = coalesce(rate, `1`)) %>% 
  select(!c(`1`))
predsurvivalprobfinal <- dplyr::bind_rows(pred_survival_prob_all)  %>%
  mutate(Stratification = "None", Adjustment = "None")
predmedmeanfinal <- dplyr::bind_rows(pred_median_mean_all)  %>%
  mutate(Stratification = "None", Adjustment = "None")

toc(func.toc=toc_min)

info(logger, 'Extrapolation analysis for whole population COMPLETED')
