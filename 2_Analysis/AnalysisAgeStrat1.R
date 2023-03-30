########################################
# AGE STRATIFICATION
#######################################

# km survival, risk table, median survival, hazard over time from the observed data for each cancer
tic("KM analysis with age stratification")
info(logger, 'KM analysis for age stratification START')

# KM observed
observedkm_age <- list()
observedmedianKM_age <- list()
observedhazotKM_age <- list()
observedrisktableKM_age <- list()
target_age <- list()

# loop to carry out for each cancer
for(j in 1:nrow(outcome_cohorts)) { 
  
  #subset the data by cancer type
  data <- Pop %>%
    filter(cohort_definition_id == j) 
  
  # get the risk table ---
  grid <- seq(0,floor(max(data$time_years)),by=2)
  filter4age <- RiskSetCount(grid,data$time_years[data$age_gr == "<30"]) %>%
    rbind(grid) %>% as.data.frame() %>%
    `colnames<-`(grid) %>%
    slice(1) %>%
    rbind(RiskSetCount(grid,data$time_years[data$age_gr == "30-39"]))%>%
    rbind(RiskSetCount(grid,data$time_years[data$age_gr == "40-49"]))%>%
    rbind(RiskSetCount(grid,data$time_years[data$age_gr == "50-59"]))%>%
    rbind(RiskSetCount(grid,data$time_years[data$age_gr == "60-69"]))%>%
    rbind(RiskSetCount(grid,data$time_years[data$age_gr == "70-79"]))%>%
    rbind(RiskSetCount(grid,data$time_years[data$age_gr == "80-89"]))%>%
    rbind(RiskSetCount(grid,data$time_years[data$age_gr == ">=90"]))%>%
    mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohortName[j], Gender = "Both", Age = c("<30" ,"30-39", "40-49" ,"50-59" ,"60-69", "70-79", "80-89" ,">=90")) 
  
  # filter that removes data where 3 or less data points after obscuring results
  # remove entries < 5 patients turn to zero
  filterdatatest <- filter4age %>%
    mutate_at(.vars = c(1:(ncol(filter4age)-4)), funs(ifelse(.== 0, NA, .))) %>%  
    mutate_at(.vars = c(1:(ncol(filter4age)-4)), funs(ifelse(.<= 5, 0, .))) %>%
    replace(is.na(.), 0) 
  
  # calculate the number of columns 
  elgcols <- ncol(filterdatatest) - 4 
  #count the number of zeros across the rows
  filterdatatest <- filterdatatest %>% 
    mutate(count=rowSums(.[1:elgcols]==0), percentzero = ((count/elgcols)*100) ) %>%
    filter(percentzero != 60) %>%
    filter(percentzero < 60)
  
  #create filter function to put into results below
  target_age[[j]] <- filterdatatest$Age
  
  #filter data removing data
  data <- data %>%
    filter((age_gr %in% target_age[[j]])) %>%
    droplevels()
  
  #risk table
  grid <- seq(0,floor(max(data$time_years)),by=2)
  observedrisktableKM_age[[j]] <- RiskSetCount(grid,data$time_years[data$age_gr == "<30"]) %>%
    rbind(grid) %>% as.data.frame() %>%
    `colnames<-`(grid) %>%
    slice(1) %>%
    rbind(RiskSetCount(grid,data$time_years[data$age_gr == "30-39"]))%>%
    rbind(RiskSetCount(grid,data$time_years[data$age_gr == "40-49"]))%>%
    rbind(RiskSetCount(grid,data$time_years[data$age_gr == "50-59"]))%>%
    rbind(RiskSetCount(grid,data$time_years[data$age_gr == "60-69"]))%>%
    rbind(RiskSetCount(grid,data$time_years[data$age_gr == "70-79"]))%>%
    rbind(RiskSetCount(grid,data$time_years[data$age_gr == "80-89"]))%>%
    rbind(RiskSetCount(grid,data$time_years[data$age_gr == ">=90"]))%>%
    mutate(Method = "Kaplan-Meier", 
           Cancer = outcome_cohorts$cohortName[j],
           Age = filter4age$Age ,
           Gender = "Both" ) %>%
    filter((Age %in% target_age[[j]])) 
  
  observedrisktableKM_age[[j]] <- observedrisktableKM_age[[j]] %>%
  mutate_at(.vars = c(1:(ncol(observedrisktableKM_age[[j]])-4)), funs(ifelse(.== 0, NA, .))) %>%  
    mutate_at(.vars = c(1:(ncol(observedrisktableKM_age[[j]])-4)), funs(ifelse(.<= 5, "<5", .))) %>%
    replace(is.na(.), 0) %>%
    mutate(across(everything(), as.character)) 
  
  #put a flag here for log that sub groups were removed due to missingness so can keep a record
  #TBC
  
  #carry out km estimate
  observedkm_age[[j]] <- survfit (Surv(time_years, status) ~ age_gr, data=data) %>%
    tidy() %>%
    rename(Age = strata) %>%
    mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohortName[j], 
           Age = str_replace(Age, "age_gr=<30", "<30"),
           Age = str_replace(Age, "age_gr=30-39", "30-39"),
           Age = str_replace(Age, "age_gr=40-49", "40-49"),
           Age = str_replace(Age, "age_gr=50-59", "50-59"),
           Age = str_replace(Age, "age_gr=60-69", "60-69"),
           Age = str_replace(Age, "age_gr=70-79", "70-79"),
           Age = str_replace(Age, "age_gr=80-89", "80-89"),
           Age = str_replace(Age, "age_gr=>=90", ">=90"),
           Gender = "Both")
  
  print(paste0("KM for observed data age strat ", Sys.time()," for ",outcome_cohorts$cohortName[j], " completed"))
  
  
  # KM median survival---
  modelKM <- survfit(Surv(time_years, status) ~ age_gr, data=data) %>%
    summary()
  
  observedmedianKM_age[[j]] <- modelKM$table %>%
    as.data.frame() %>%
    mutate(Method = "Kaplan-Meier", 
           Cancer = outcome_cohorts$cohortName[j], 
           Age = target_age[[j]] ,
           Gender = "Both")
  
  print(paste0("Median survival from KM from observed data ", Sys.time()," for ",outcome_cohorts$cohortName[j], " completed"))
  
  # hazard function over time ----
  observedhazotKM_age[[j]] <- group_by(data,age_gr) %>% 
    do(as.data.frame(bshazard(Surv(time_years, status)~1, data=., verbose=FALSE))) %>% 
    ungroup %>%
    mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohortName[j], Gender = "Both")
  
  print(paste0("Hazard over time results ", Sys.time()," for ",outcome_cohorts$cohortName[j], " age strat completed"))
  
  
}

# take the results from a list (one element for each cancer) and put into dataframe ----
observedkmcombined_age <- dplyr::bind_rows(observedkm_age) %>%
  rename(est = estimate ,ucl = conf.high, lcl = conf.low ) %>%
  mutate(Stratification = "Age")

medkmcombined_age <- dplyr::bind_rows(observedmedianKM_age)  %>%
  mutate(Stratification = "Age")

hotkmcombined_age <- dplyr::bind_rows(observedhazotKM_age) %>%
  rename(est = hazard, ucl = upper.ci, lcl = lower.ci, Age = age_gr )  %>%
  mutate(Stratification = "Age")

#generate the risk table and remove entries < 5 patients
risktableskm_age <- dplyr::bind_rows(observedrisktableKM_age) %>%
  replace(is.na(.), 0)  %>%
  mutate(Stratification = "Age")

toc(func.toc=toc_min)

info(logger, 'KM analysis for AGE stratification COMPLETE')

###########################################


###########################################

# Extrapolation analysis for age stratification ------

tic("Extrapolation analysis for age stratification")
info(logger, 'Extrapolation analysis for age stratification START')

# Initiate templists to store output ---- will have to make folders for each cancer and loop
extrapolations_age <- list()
gof_haz_age <- list()
hazot_age <- list()
parameters_age <- list()

# Initiate templists to store output ---- 
extrap_results_temp <- list() # Create empty list for extrapolations
gof_results_temp <- list() # required to assess goodness of fit (AIC/BIC)
hazot_results_temp <- list() #required
parameters_results_temp <- list() #required


# Running analysis for each cancer
for(j in 1:nrow(outcome_cohorts)) { 
  
  #subset the data by cancer type
  data <- Pop %>%
    filter(cohort_definition_id == j)
  
  #filter data removing data groups with > 60% missingness
  data <- data %>%
    filter((age_gr %in% target_age[[j]])) %>%
    droplevels()
  
  #carry out extrapolation for each cancer
  for(i in 1:length(extrapolations)) {   # Head of for-loop
    
    if(extrapolations[i] == "spline1") {
      
      # 1knotspline
      model <- flexsurvspline(formula=Surv(time_years,status-1)~age_gr,data=data,k = 1, scale = "hazard")
      
      #extrapolation # will need this to check results can remove once checked
      extrap_results_temp[[i]] <- model %>%
        summary(t=t/365, tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Gender = "Both" ) %>%
        rename(Age = age_gr)
      
      #grab the parameters and knots from the model
      coefs.p <- model[["coefficients"]] %>%
        enframe() %>%
        pivot_wider(value, name) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "Age", Gender = "Both" ) 
      
      knots.p <- model[["knots"]] %>%
        setNames(., c("SplineLowerB", "SplineInternal1" , "SplineUpperB")) %>%
        enframe() %>%
        pivot_wider(value, name)
      
      parameters_results_temp[[i]] <- bind_cols(coefs.p,  knots.p )
      
      # hazard over time
      hazot_results_temp[[i]] <- model %>%
        summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Gender = "Both" ) %>%
        rename(Age = age_gr)
      
      #get the goodness of fit for each model
      gof_results_temp[[i]] <- model %>%
        glance() %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Gender = "Both" ) %>%
        slice(rep(1:n(), each = length(target_age[[j]]))) %>%
        mutate(Gender = target_age[[j]])
      
      
      #print out progress               
      print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohortName[j], " completed"))
      
    } else if(extrapolations[i] == "spline2") {
      # 2knotspline
      
      model <- flexsurvspline(formula=Surv(time_years,status-1)~age_gr,data=data,k = 2, scale = "hazard")
      
      extrap_results_temp[[i]] <- model %>%
        summary(t=t/365, tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Gender = "Both" ) %>%
        rename(Age = age_gr )
      
      #extract parameters
      #grab the parameters and knots from the model
      coefs.p <- model[["coefficients"]] %>%
        enframe() %>%
        pivot_wider(value, name) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "Age", Gender = "Both" ) 
      
      knots.p <- model[["knots"]] %>%
        setNames(., c("SplineLowerB", "SplineInternal1" , "SplineInternal2" ,"SplineUpperB")) %>%
        enframe() %>%
        pivot_wider(value, name)
      
      parameters_results_temp[[i]] <- bind_cols(coefs.p,  knots.p )
      
      # hazard over time
      hazot_results_temp[[i]] <- model %>%
        summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Gender = "Both" ) %>%
        rename(Age = age_gr )
      
      #get the goodness of fit for each model
      gof_results_temp[[i]] <- model %>%
        glance() %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Gender = "Both" ) %>%
        slice(rep(1:n(), each = length(target_age[[j]]))) %>%
        mutate(Gender = target_age[[j]])
      
      #print out progress               
      print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohortName[j], " completed"))
      
      
    } else if(extrapolations[i] == "spline3") {
      # 3knotspline
      
      model <- flexsurvspline(formula=Surv(time_years,status-1)~age_gr,data=data,k = 3, scale = "hazard")
      
      extrap_results_temp[[i]] <- model %>%
        summary(t=t/365, tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Gender = "Both" ) %>%
        rename(Age = age_gr )
      
      #extract parameters
      #grab the parameters and knots from the model
      coefs.p <- model[["coefficients"]] %>%
        enframe() %>%
        pivot_wider(value, name) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "Age", Gender = "Both" ) 
      
      knots.p <- model[["knots"]] %>%
        setNames(., c("SplineLowerB", "SplineInternal1" , "SplineInternal2", "SplineInternal3" ,"SplineUpperB")) %>%
        enframe() %>%
        pivot_wider(value, name)
      
      parameters_results_temp[[i]] <- bind_cols(coefs.p,  knots.p )
      
      # hazard over time
      hazot_results_temp[[i]] <- model %>%
        summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Gender = "Both" ) %>%
        rename(Age = age_gr )
      
      #get the goodness of fit for each model
      gof_results_temp[[i]] <- model %>%
        glance() %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Gender = "Both" ) %>%
        slice(rep(1:n(), each = length(target_age[[j]]))) %>%
        mutate(Gender = target_age[[j]])
      
      #print out progress               
      print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohortName[j], " completed"))
      
    } else if(extrapolations[i] == "spline5") {
      # 5knotspline
      model <- flexsurvspline(formula=Surv(time_years,status-1)~age_gr,data=data,k = 5, scale = "hazard")
      
      extrap_results_temp[[i]] <- model %>%
        summary(t=t/365, tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j],  Gender = "Both" ) %>%
        rename(Age = age_gr)
      
      coefs.p <- model[["coefficients"]] %>%
        enframe() %>%
        pivot_wider(value, name) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "Age", Gender = "Both" ) 
      
      knots.p <- model[["knots"]] %>%
        setNames(., c("SplineLowerB", "SplineInternal1" , "SplineInternal2", "SplineInternal3" ,
                      "SplineInternal4" ,"SplineInternal5" , "SplineUpperB")) %>%
        enframe() %>%
        pivot_wider(value, name)
      
      parameters_results_temp[[i]] <- bind_cols(coefs.p,  knots.p )
      
      
      # hazard over time
      hazot_results_temp[[i]] <- model %>%
        summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Gender = "Both" ) %>%
        rename( Age = age_gr )
      
      #get the goodness of fit for each model
      gof_results_temp[[i]] <- model %>%
        glance() %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Gender = "Both" ) %>%
        slice(rep(1:n(), each = length(target_age[[j]]))) %>%
        mutate(Gender = target_age[[j]])
      
      #print out progress               
      print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohortName[j], " completed"))
      
      
    } else {
      
      #carry out models for different parametric methods survival
      model <- flexsurvreg(Surv(time_years, status)~age_gr, data=data, dist=extrapolations[i])
      
      # extrapolations
      extrap_results_temp[[i]] <- model %>%
        summary(t=t/365, tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Gender = "Both" ) %>%
        rename(Age = age_gr)
      
      #grab the parameters from the model
      parameters_results_temp[[i]] <- model[["coefficients"]] %>%
        enframe() %>%
        pivot_wider(value, name) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "Age", Gender = "Both" ) 
      
      #extract the hazard function over time
      hazot_results_temp[[i]] <- model %>%
        summary(t=(t + 1)/365, type = "hazard",tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Gender = "Both" ) %>%
        rename(Age = age_gr)
      
      #get the goodness of fit for each model
      gof_results_temp[[i]] <- model %>%
        glance() %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Gender = "Both" ) %>%
        slice(rep(1:n(), each = length(target_age[[j]]))) %>%
        mutate(Gender = target_age[[j]])
      
      
      #print out progress               
      print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohortName[j], " completed"))
      
    }
    
    #combine all results
    extrapolatedcombined <- dplyr::bind_rows(extrap_results_temp)
    gofcombined <- dplyr::bind_rows(gof_results_temp)
    hotcombined <- dplyr::bind_rows(hazot_results_temp) %>%
      filter(time > 0) # remove rows with inf/NAs
    
    
    #put the results from each cancer in separate list
    extrapolations_age[[j]] <- extrapolatedcombined
    gof_haz_age[[j]] <- gofcombined
    hazot_age[[j]] <- hotcombined
    parameters_age[[j]] <-  parameters_results_temp
    
  }
  
  
  #print out progress               
  print(paste0(outcome_cohorts$cohortName[j]," Extrapolation Analysis Completed ", Sys.time()))
  
}

# Merge results together from each cancer and extrapolation into a dataframe ---
extrapolatedfinalAge <- dplyr::bind_rows(extrapolations_age)  %>%
  mutate(Stratification = "Age")
goffinalAge <- dplyr::bind_rows(gof_haz_age)  %>%
  mutate(Stratification = "Age")
hazardotfinalAge <- dplyr::bind_rows(hazot_age) %>%
  mutate(Stratification = "Age")


# extracting parameters for each model for each cancer
# create empty lists for parameters extraction
GompertzP <- list()
weibullP <- list()
weibullPHP <- list()
ExponentialP <- list()
LoglogP <- list()
LognormP <- list()
GenGammaP <- list()
Spline1kP <- list()
Spline3kP <- list()
Spline5kP <- list()


# pull out the extrapolation parameters in a separate list for each cancer 
# (so for example all exponential parameters for all cancers will be in same list)

for(j in 1:nrow(outcome_cohorts)) { 
  
  GompertzP[[j]] <- parameters_age[[j]] %>% pluck(1) 
  weibullP[[j]] <- parameters_age[[j]] %>% pluck(2) 
  weibullPHP[[j]] <- parameters_age[[j]] %>% pluck(3) 
  ExponentialP[[j]] <- parameters_age[[j]] %>% pluck(4) 
  LoglogP[[j]] <- parameters_age[[j]] %>% pluck(5) 
  LognormP[[j]] <- parameters_age[[j]] %>% pluck(6) 
  GenGammaP[[j]] <- parameters_age[[j]] %>% pluck(7) 
  Spline1kP[[j]] <- parameters_age[[j]] %>% pluck(8) 
  Spline3kP[[j]] <- parameters_age[[j]] %>% pluck(9) 
  Spline5kP[[j]] <- parameters_age[[j]] %>% pluck(10) 
  
}


# grab the parameters from the list and row bind
GompertzParametersAge <- dplyr::bind_rows(GompertzP)
weibullParametersAge <- dplyr::bind_rows(weibullP)
weibullPHParametersAge <- dplyr::bind_rows(weibullPHP)
ExponentialParametersAge <- dplyr::bind_rows(ExponentialP) %>%
  rename(rate = 1)
LoglogParametersAge <- dplyr::bind_rows(LoglogP)
LognormParametersAge <- dplyr::bind_rows(LognormP)
GenGammaParametersAge <- dplyr::bind_rows(GenGammaP)
Spline1kParametersAge <- dplyr::bind_rows(Spline1kP)
Spline3kParametersAge <- dplyr::bind_rows(Spline3kP)
Spline5kParametersAge <- dplyr::bind_rows(Spline5kP)

ParametersAge <- bind_rows(
  GompertzParametersAge ,
  weibullParametersAge ,
  weibullPHParametersAge,
  ExponentialParametersAge, 
  LoglogParametersAge,
  LognormParametersAge, 
  GenGammaParametersAge, 
  Spline1kParametersAge ,
  Spline3kParametersAge ,
  Spline5kParametersAge ) %>%
  mutate(Stratification = "Age")

toc(func.toc=toc_min)

info(logger, 'Extrapolation analysis for age extrapolation COMPLETED')
