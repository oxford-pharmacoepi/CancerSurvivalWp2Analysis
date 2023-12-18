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

# loop to carry out for each cancer
for(j in 1:nrow(cancer_cohorts)) {

#subset the data by cancer type
data <- Pop %>%
  dplyr::filter(cohort_definition_id == cancer_cohorts$cohort_definition_id[j])

#carry out km estimate ---
#take every other row from results
observedkm[[j]] <- survival::survfit(Surv(time_years, status) ~ 1, data=data) %>%
  tidy() %>%
  dplyr::mutate(Method = "Kaplan-Meier", Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both")

# reduce the size of KM for plotting
if(nrow(observedkm[[j]]) > 6000){
  observedkm[[j]] <- observedkm[[j]] %>%
    dplyr::filter(row_number() %% 10 == 1)
} else if(nrow(observedkm[[j]]) > 4000 & nrow(observedkm[[j]]) < 6000 ){
  observedkm[[j]] <- observedkm[[j]] %>%
    dplyr::filter(row_number() %% 8 == 1)
} else if(nrow(observedkm[[j]]) > 3000 & nrow(observedkm[[j]]) < 4000 ){
  observedkm[[j]] <- observedkm[[j]] %>%
    dplyr::filter(row_number() %% 5 == 1)
} else if(nrow(observedkm[[j]]) > 1500 & nrow(observedkm[[j]]) < 3000 ){
  observedkm[[j]] <- observedkm[[j]] %>%
    dplyr::filter(row_number() %% 3 == 1)
} else if(nrow(observedkm[[j]]) > 750 & nrow(observedkm[[j]]) < 1500 ){
  observedkm[[j]] <- observedkm[[j]] %>%
    dplyr::filter(row_number() %% 2 == 1)
}


print(paste0("KM for observed data ", Sys.time()," for ",cancer_cohorts$cohort_name[j], " completed"))

# get risk table for specific times ---
grid <- seq(0,floor(max(data$time_years)),by=0.5) #get the number of years every half year
grid <-  grid[(stringr::str_detect(grid, "[1-9]\\.5", negate = TRUE )) & (stringr::str_detect(grid, "10.5", negate = TRUE )) &
                (stringr::str_detect(grid, "20.5", negate = TRUE )) & (stringr::str_detect(grid, "30.5", negate = TRUE ))] # remove all the half years apart from the first half year
  
sprob <- survival::survfit(Surv(time_years, status) ~ 1, data=data) %>% 
  summary(times = grid, extend = TRUE)
cols <- lapply(c(2:15) , function(x) sprob[x])

observedrisktableKM[[j]] <- do.call(data.frame, cols) %>%
  dplyr::select(c(n.risk, n.event, n.censor)) %>% 
  t() %>% 
  as_tibble() %>% 
  `colnames<-`(grid) %>% 
  dplyr::mutate(Method = "Kaplan-Meier", 
         Cancer = cancer_cohorts$cohort_name[j],
         Sex = "Both" ,
         Age = "All",
         details = c("n.risk", "n.event", "n.censor")) %>% 
  dplyr::relocate(details)

print(paste0("Extract risk table ", Sys.time()," for ",cancer_cohorts$cohort_name[j], " completed"))


# get surv probs for specific times ---
surprobsKM <- do.call(data.frame, cols) %>%
  dplyr::select(c(time, surv, lower, upper)) %>% 
  dplyr::filter(time == 1 | time == 5 | time == 10 ) %>% 
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

# KM median survival---
modelKM <- survival::survfit(Surv(time_years, status) ~ 1, data=data) %>%
  summary()

medianKM <- modelKM$table %>%
  as.data.frame() %>%
  tibble::rownames_to_column() %>%  
  tidyr::pivot_longer(-rowname) %>% 
  tidyr::pivot_wider(names_from=rowname, values_from=value) %>% 
  dplyr::rename(n = records, se =`se(rmean)`) %>% 
  dplyr::mutate(rmean = round(rmean, 4),
         median = round(median, 4),
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
  dplyr::select(-c(`0.95LCL`,`0.95UCL`, name, n.max, n.start)) %>% 
  dplyr::mutate(n  = replace(n, n ==  0 , NA),
         events = replace(events, events ==  0 , NA)) %>%
  dplyr::mutate(n  = replace(n, n <=  10 , "<10"),
         events  = replace(events, events <=  10 , "<10"))  %>%
  dplyr::mutate(n  = replace_na(n, "0"),
         events  = replace_na(events, "0")) %>% 
  dplyr::mutate(n = as.character(n),
         events = as.character(events))

# Extract rmean at 10 years
model_rm <- survival::survfit(Surv(time_years, status) ~ 1, data=data)
rmean10 <- survival:::survmean(model_rm, rmean=c(10))$matrix %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column() %>%  
  tidyr::pivot_longer(-rowname) %>% 
  tidyr::pivot_wider(names_from=rowname, values_from=value) %>% 
  dplyr::select(rmean, `se(rmean)`) %>% 
  dplyr::rename(rmean10yr = rmean, se10yr =`se(rmean)`) %>% 
  dplyr::mutate("rmean 10yrs in years (SE)"= ifelse(!is.na(rmean10yr),
                                  paste0(paste0(nice.num2(rmean10yr)), " (",
                                         paste0(nice.num2(se10yr)), ")"),
                                  NA))

rmean5 <- survival:::survmean(model_rm, rmean=c(5))$matrix %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column() %>%  
  tidyr::pivot_longer(-rowname) %>% 
  tidyr::pivot_wider(names_from=rowname, values_from=value) %>% 
  dplyr::select(rmean, `se(rmean)`) %>% 
  dplyr::rename(rmean5yr = rmean, se5yr =`se(rmean)`) %>% 
  dplyr::mutate("rmean 5yrs in years (SE)"= ifelse(!is.na(rmean5yr),
                                                    paste0(paste0(nice.num2(rmean5yr)), " (",
                                                           paste0(nice.num2(se5yr)), ")"),
                                                    NA))

observedmedianKM[[j]] <- dplyr::bind_cols(medianKM, rmean5, rmean10, surprobsKM)
observedmedianKM[[j]] <- observedmedianKM[[j]] %>% 
  dplyr::mutate(Method = "Kaplan-Meier", 
                Cancer = cancer_cohorts$cohort_name[j] ,
                Age = "All", 
                Sex = "Both",
                study_period = as.numeric(max(data$cohort_start_date) - min(data$cohort_start_date)) / 365.25)
  
rm(surprobsKM,medianKM,rmean5,rmean10,model_rm,modelKM)

print(paste0("Median survival from KM from observed data ", Sys.time()," for ", cancer_cohorts$cohort_name[j] , " completed"))

# hazard function over time ----
# paper https://arxiv.org/pdf/1509.03253.pdf states bshazard good package

observedhazotKM[[j]] <- as.data.frame.bshazard(bshazard(Surv(time_years, status) ~ 1, data=data, verbose=FALSE)) %>%
  dplyr::mutate(Method = "Kaplan-Meier", Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both") 

# reduce the size of haz over time for plotting
if(nrow(observedhazotKM[[j]]) > 6000){
  observedhazotKM[[j]] <- observedhazotKM[[j]] %>%
    dplyr::filter(row_number() %% 10 == 1)
}else if(nrow(observedhazotKM[[j]]) > 4000 & nrow(observedhazotKM[[j]]) < 6000 ){
  observedhazotKM[[j]] <- observedhazotKM[[j]] %>%
    dplyr::filter(row_number() %% 8 == 1)
}else if(nrow(observedhazotKM[[j]]) > 3000 & nrow(observedhazotKM[[j]]) < 4000 ){
  observedhazotKM[[j]] <- observedhazotKM[[j]] %>%
    dplyr::filter(row_number() %% 5 == 1)
}else if(nrow(observedhazotKM[[j]]) > 1500 & nrow(observedhazotKM[[j]]) < 4000 ){
  observedhazotKM[[j]] <- observedhazotKM[[j]] %>%
    dplyr::filter(row_number() %% 3 == 1)
}else if(nrow(observedhazotKM[[j]]) > 750 & nrow(observedhazotKM[[j]]) < 1500 ){
  observedhazotKM[[j]] <- observedhazotKM[[j]] %>%
    dplyr::filter(row_number() %% 2 == 1)
}

print(paste0("Hazard over time results for KM ", Sys.time()," for ",cancer_cohorts$cohort_name[j], " completed"))

}

# take the results from a list (one element for each cancer) and put into dataframe for KM survival
observedkmcombined <- dplyr::bind_rows(observedkm) %>%
  dplyr::rename(est = estimate ,ucl = conf.high, lcl = conf.low )  %>%
  dplyr::mutate(Stratification = "None", Adjustment = "None")

medkmcombined <- dplyr::bind_rows(observedmedianKM) %>%
  dplyr::mutate(Stratification = "None", Adjustment = "None")

hotkmcombined <- dplyr::bind_rows(observedhazotKM) %>%
  dplyr::rename(est = hazard, ucl = upper.ci, lcl = lower.ci ) %>%
  dplyr::mutate(Stratification = "None", Adjustment = "None")

# generate results for risk table with those at risk and censor < 10 cases
risktableskm <- dplyr::bind_rows(observedrisktableKM) %>% 
  dplyr::filter(details != "n.censor") %>% 
  dplyr::mutate(Stratification = "None", Adjustment = "None") %>% 
  dplyr::mutate(across(everything(), ~replace(., .==  0 , NA))) %>%
  dplyr::mutate(across(everything(), ~replace(., .<=  10 , "<10"))) %>% 
  dplyr::mutate(across(everything(), as.character)) %>%
  replace(is.na(.), "0") 
  
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
pred_median_mean_all <- list() # extract the predicted median and RMST, surv prob 1,5,10 from extrapolation methods

# Running analysis for each cancer
for(j in 1:nrow(cancer_cohorts)) {
  
  # create empty lists for temp results for each cancer
  extrap_results_temp <- list() 
  gof_results_temp <- list()
  hazot_results_temp <- list()
  parameters_results_temp <- list() 
  pred_median_mean_results_temp <- list() 

  #subset the data by cancer type
  data <- Pop %>%
    dplyr::filter(cohort_definition_id == cancer_cohorts$cohort_definition_id[j])
  
  #carry out extrapolation for each cancer
  for(i in 1:length(extrapolations)) {   # Head of for-loop
    
    if(extrapolations[i] == "spline1") {
      
      # 1knotspline
     tryCatch(
        model <- flexsurv::flexsurvspline (formula=Surv(time_years,status-1)~1,data=data,k = 1, scale = "hazard"),
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
        dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
      
      #get the goodness of fit for each model
      gof_results_temp[[i]] <- model %>%
        broom::glance() %>%
        dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
      
      #grab the parameters and knots from the model
      coefs.p <- model[["coefficients"]] %>%
        tibble::enframe() %>%
        tidyr::pivot_wider(names_from = name, values_from = value) %>%
        dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" ) 
      
      knots.p <- model[["knots"]] %>%
        stats::setNames(., c("SplineLowerB", "SplineInternal1" , "SplineUpperB")) %>%
        tibble::enframe() %>%
        tidyr::pivot_wider(names_from = name, values_from = value)
      
      parameters_results_temp[[i]] <- dplyr::bind_cols(coefs.p,  knots.p )

      # hazard over time
      hazot_results_temp[[i]] <- model %>%
        summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
        dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
      
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
      
      pred_median_mean_results_temp[[i]] <- dplyr::bind_cols(pr_mean, pr_mean5, pr_mean10, pr_median, pr_survival_prob )
      pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
        dplyr::mutate(Method = extrapolations_formatted[i], 
               Cancer = cancer_cohorts$cohort_name[j], 
               Age = "All", 
               Sex = "Both" )
      
      
      rm(model,pr_survival_prob, pr_mean, pr_median, pr_mean10, pr_mean5 )
      
      #print out progress               
      print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cancer_cohorts$cohort_name[j], " completed"))
        }, 
      
      error = function(e) {
        cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for overall model", "\n")
        info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
      
      warning = function(w) {
        cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for overall model", "\n")
        info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
      
        )
        
      }
      
      
    } else if(extrapolations[i] == "spline2") {
      # 2knotspline
      
      tryCatch(
        model <- flexsurv::flexsurvspline(formula=Surv(time_years,status-1)~1,data=data,k = 2, scale = "hazard"),
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
        dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
      
      #get the goodness of fit for each model
      gof_results_temp[[i]] <- model %>%
        broom::glance() %>%
        dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
      
      #extract parameters
      #grab the parameters and knots from the model
      coefs.p <- model[["coefficients"]] %>%
        tibble::enframe() %>%
        tidyr::pivot_wider(names_from = name, values_from = value) %>%
        dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" ) 
      
      knots.p <- model[["knots"]] %>%
        stats::setNames(., c("SplineLowerB", "SplineInternal1" , "SplineInternal2" ,"SplineUpperB")) %>%
        tibble::enframe() %>%
        tidyr::pivot_wider(names_from = name, values_from = value)
      parameters_results_temp[[i]] <- dplyr::bind_cols(coefs.p,  knots.p )
      
      # hazard over time
      hazot_results_temp[[i]] <- model %>%
        summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
        dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
      
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
      
      pred_median_mean_results_temp[[i]] <- dplyr::bind_cols(pr_mean, prmean5, pr_mean10, pr_median, pr_survival_prob )
      pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
        dplyr::mutate(Method = extrapolations_formatted[i], 
               Cancer = cancer_cohorts$cohort_name[j], 
               Age = "All", 
               Sex = "Both" )
      
      
      rm(model,pr_survival_prob, pr_mean, pr_median, pr_mean10, pr_mean5 )
      
      print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cancer_cohorts$cohort_name[j], " completed"))
      
        }, 
      
      error = function(e) {
        cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for overall model", "\n")
        info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
      
      warning = function(w) {
        cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for overall model", "\n")
        info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
      
        )
        
      }
      
    } else if(extrapolations[i] == "spline3") {
      # 3knotspline
      
      tryCatch(
        model <- flexsurv::flexsurvspline(formula=Surv(time_years,status-1)~1, data=data, k = 3, scale = "hazard"),
        error = function(e){info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
        warning = function(w){info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
      )
      
      if (exists("model") == TRUE) {
        
        tryCatch({
      
      extrap_results_temp[[i]] <- model %>%
        summary(t=t/365, tidy = TRUE) %>%
        dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" )

      #get the goodness of fit for each model
      gof_results_temp[[i]] <- model %>%
        broom::glance() %>%
        dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
      
      #extract parameters
      #grab the parameters and knots from the model
      coefs.p <- model[["coefficients"]] %>%
        tibble::enframe() %>%
        tidyr::pivot_wider(names_from = name, values_from = value) %>%
        dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" ) 
      
      knots.p <- model[["knots"]] %>%
        stats::setNames(., c("SplineLowerB", "SplineInternal1" , "SplineInternal2", "SplineInternal3" ,"SplineUpperB")) %>%
        tibble::enframe() %>%
        tidyr::pivot_wider(names_from = name, values_from = value)
      
      parameters_results_temp[[i]] <- dplyr::bind_cols(coefs.p,  knots.p )
      
      # hazard over time
      hazot_results_temp[[i]] <- model %>%
        summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
        dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
      
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
      
      pred_median_mean_results_temp[[i]] <- dplyr::bind_cols(pr_mean, pr_mean5, pr_mean10, pr_median, pr_survival_prob )
      pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
        dplyr::mutate(Method = extrapolations_formatted[i], 
               Cancer = cancer_cohorts$cohort_name[j], 
               Age = "All", 
               Sex = "Both")
      
      
      rm(model,pr_survival_prob, pr_mean, pr_median, pr_mean10, pr_mean5)
      
      #print out progress               
      print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cancer_cohorts$cohort_name[j], " completed"))
      
        }, 
      
      error = function(e) {
        cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for overall model", "\n")
        info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
      
      warning = function(w) {
        cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for overall model", "\n")
        info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
      
        )
        
      }
      
    } else if(extrapolations[i] == "spline5") {
      # 5knotspline
      
      tryCatch(
        model <- flexsurv::flexsurvspline(formula=Surv(time_years,status-1)~1, data=data, k = 5, scale = "hazard"),
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
        dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
      
      #get the goodness of fit for each model
      gof_results_temp[[i]] <- model %>%
        broom::glance() %>%
        dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
      
      coefs.p <- model[["coefficients"]] %>%
        tibble::enframe() %>%
        tidyr::pivot_wider(names_from = name, values_from = value) %>%
        dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" ) 
      
      knots.p <- model[["knots"]] %>%
        stats::setNames(., c("SplineLowerB", "SplineInternal1" , "SplineInternal2", "SplineInternal3" ,
                      "SplineInternal4" ,"SplineInternal5" , "SplineUpperB")) %>%
        tibble::enframe() %>%
        tidyr::pivot_wider(names_from = name, values_from = value)
      
      parameters_results_temp[[i]] <- dplyr::bind_cols(coefs.p,  knots.p )
      
      # hazard over time
      hazot_results_temp[[i]] <- model %>%
        summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
        dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
      
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
      
      pred_median_mean_results_temp[[i]] <- dplyr::bind_cols(pr_mean,pr_mean5, pr_mean10, pr_median, pr_survival_prob )
      pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
        dplyr::mutate(Method = extrapolations_formatted[i], 
               Cancer = cancer_cohorts$cohort_name[j], 
               Age = "All", 
               Sex = "Both" )
      
      
      rm(model,pr_survival_prob, pr_mean, pr_median, pr_mean10, pr_mean5 )
      
      #print out progress               
      print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cancer_cohorts$cohort_name[j], " completed"))
      
        }, 
      
      error = function(e) {
        cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for overall model", "\n")
        info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
      
      warning = function(w) {
        cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for overall model", "\n")
        info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
      
        )
        
      }
      
    } else {
      
      #carry out models for different parametric methods survival
      tryCatch(
      model <- flexsurv::flexsurvreg(Surv(time_years, status)~1, data=data, dist=extrapolations[i]),
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
        dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
      
      #get the goodness of fit for each model
      gof_results_temp[[i]] <- model %>%
        broom::glance() %>%
        dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
      
      #grab the parameters from the model
      parameters_results_temp[[i]] <- model[["coefficients"]] %>%
        tibble::enframe() %>%
        tidyr::pivot_wider(names_from = name, values_from = value) %>%
        dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" ) 
      
      #extract the hazard function over time
      hazot_results_temp[[i]] <- model %>%
        summary(t=(t + 1)/365, type = "hazard", tidy = TRUE) %>%
        dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
      
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
      
      pred_median_mean_results_temp[[i]] <- dplyr::bind_cols(pr_mean, pr_mean5, pr_mean10, pr_median, pr_survival_prob )
      pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
        dplyr::mutate(Method = extrapolations_formatted[i], 
               Cancer = cancer_cohorts$cohort_name[j], 
               Age = "All", 
               Sex = "Both" )
      
      
      rm(model,pr_survival_prob, pr_mean, pr_median, pr_mean10, pr_mean5) 
      
      #print out progress               
      print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cancer_cohorts$cohort_name[j], " completed"))
      
        }, 
      
      error = function(e) {
        cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for overall model", "\n")
        info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
      
      warning = function(w) {
        cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for overall model", "\n")
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
  
  extrapolations_all[[j]] <- extrapolatedcombined
  gof_haz_all[[j]] <- gofcombined
  hazot_all[[j]] <- hotcombined
  parameters_all[[j]] <-  parcombined
  pred_median_mean_all[[j]] <- medcombined

  #print out progress               
  print(paste0(cancer_cohorts$cohort_name[j]," Extrapolation Analysis Completed ", Sys.time()))

}

# Merge results together from each cancer and extrapolation into a dataframe ---
extrapolatedfinal <- dplyr::bind_rows(extrapolations_all)  %>%
  dplyr::mutate(Stratification = "None", Adjustment = "None")
goffinal <- dplyr::bind_rows(gof_haz_all)  %>%
  dplyr::mutate(Stratification = "None", Adjustment = "None")
hazardotfinal <- dplyr::bind_rows(hazot_all)  %>%
  dplyr::mutate(Stratification = "None", Adjustment = "None")
parametersfinal <- dplyr::bind_rows(parameters_all)  %>%
  dplyr::mutate(Stratification = "None", Adjustment = "None") %>% 
  dplyr::relocate(shape, .after = Sex) %>% 
  dplyr::relocate(rate, .after = Sex) %>% 
  dplyr::mutate(rate = coalesce(rate, `1`)) %>% 
  dplyr::select(!c(`1`))
predmedmeanfinal <- dplyr::bind_rows(pred_median_mean_all)  %>%
  dplyr::mutate(Stratification = "None", Adjustment = "None")

toc(func.toc=toc_min)

info(logger, 'Extrapolation analysis for whole population COMPLETED')
