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
for(j in 1:nrow(outcome_cohorts)) { 

#subset the data by cancer type
data <- Pop %>%
  filter(cohort_definition_id == j)

#carry out km estimate
observedkm[[j]] <- survfit (Surv(time_years, status) ~ 1, data=data) %>%
  tidy() %>%
  mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "Both") %>%
  filter(n.risk >= 5) #remove entries with less than 5 patients

print(paste0("KM for observed data ", Sys.time()," for ",outcome_cohorts$cohortName[j], " completed"))

# get the risk table ---
grid <- seq(0,floor(max(data$time_years)),by=2)
observedrisktableKM[[j]] <- RiskSetCount(grid,data$time_years) %>%
  rbind(grid) %>% as.data.frame() %>%
  `colnames<-`(grid) %>%
  mutate(Method = "Observed", Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "Both" ) %>%
  slice(1)

print(paste0("Extract risk table ", Sys.time()," for ",outcome_cohorts$cohortName[j], " completed"))


# KM median survival---
modelKM <- survfit(Surv(time_years, status) ~ 1, data=data) %>%
  summary()

observedmedianKM[[j]] <- modelKM$table %>%
  as.data.frame() %>%
  tibble::rownames_to_column() %>%  
  pivot_longer(-rowname) %>% 
  pivot_wider(names_from=rowname, values_from=value) %>%
  mutate(Method = "Kaplan-Meier", 
         Cancer = outcome_cohorts$cohortName[j],
         Gender = "Both" ,
         Age = "All" ) %>%
          select(-name)
  

print(paste0("Median survival from KM from observed data ", Sys.time()," for ",outcome_cohorts$cohortName[j], " completed"))

# hazard function over time ----
# paper https://arxiv.org/pdf/1509.03253.pdf states bshazard good package

observedhazotKM[[j]] <- as.data.frame.bshazard(bshazard(Surv(time_years, status) ~ 1, data=data, verbose=FALSE)) %>%
  mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "Both")

print(paste0("Hazard over time results for KM ", Sys.time()," for ",outcome_cohorts$cohortName[j], " completed"))

}

# take the results from a list (one element for each cancer) and put into dataframe for KM survival
observedkmcombined <- dplyr::bind_rows(observedkm) %>%
  rename(est = estimate ,ucl = conf.high, lcl = conf.low )

medkmcombined <- dplyr::bind_rows(observedmedianKM) 

hotkmcombined <- dplyr::bind_rows(observedhazotKM) %>%
  rename(est = hazard, ucl = upper.ci, lcl = lower.ci )

# generate the risk table and remove entries < 5 patients
risktableskm <- dplyr::bind_rows(observedrisktableKM)%>%
  mutate(across(everything(), ~replace(., . <=  5 , NA))) %>%
  replace(is.na(.), "<5") %>%
  relocate(Cancer)


# once all done can merge all the results together in a rdata file
# put all the results into a list
ResultsKM_ALL <- list("KM_observed_all" = observedkmcombined, 
                    "KM_MedianSur_all" = medkmcombined,
                    "KM_hazard_rate_all" = hotkmcombined,
                    "KM_risktable_all" = risktableskm)

#write to excel
openxlsx::write.xlsx(ResultsKM_ALL, file = here("Results", db.name ,"cancer_KM_observed_results_ALL.xlsx"))

# observedkmcombined_all <- read.xlsx(xlsxFile = here("Results", db.name , "cancer_KM_observed_results_ALL.xlsx"), sheet = 1)
# medkmcombined_all <- read.xlsx(xlsxFile = here("Results", db.name , "cancer_KM_observed_results_ALL.xlsx"), sheet = 2)
# hotkmcombined_all <- read.xlsx(xlsxFile = here("Results", db.name , "cancer_KM_observed_results_ALL.xlsx"), sheet = 3)
# risktableskm_all <- read.xlsx(xlsxFile = here("Results", db.name , "cancer_KM_observed_results_ALL.xlsx"), sheet = 4)

toc(func.toc=toc_min)
info(logger, 'KM analysis for whole population COMPLETE')

###########################################

# Extrapolation analysis for whole population ------

tic("Extrapolation analysis for whole population")
info(logger, 'Extrapolation analysis for whole population START')

# Initiate templists to store output ---- will have to make folders for each cancer and loop
extrapolations_all <- list()
gof_haz_all <- list()
hazot_all <- list()
parameters_all <- list()

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
  
  #carry out extrapolation for each cancer
  for(i in 1:length(extrapolations)) {   # Head of for-loop
    
    if(extrapolations[i] == "spline1") {
      
      # 1knotspline
      model <- flexsurvspline(formula=Surv(time_years,status-1)~1,data=data,k = 1, scale = "hazard")
        
      #extrapolation # will need this to check results can remove once checked
      extrap_results_temp[[i]] <- model %>%
        summary(t=t/365, tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "Both" )
      
      #grab the parameters from the model
      parameters_results_temp[[i]] <- model[["coefficients"]] %>%
        enframe() %>%
        pivot_wider(value, name) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "Both" ) 
      
      # hazard over time
      hazot_results_temp[[i]] <- model %>%
        summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "Both" )
      
      #get the goodness of fit for each model
      gof_results_temp[[i]] <- model %>%
        glance() %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "Both" )
      
      
      #print out progress               
      print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohortName[j], " completed"))
      
    } else if(extrapolations[i] == "spline3") {
      # 3knotspline
      
      model <- flexsurvspline(formula=Surv(time_years,status-1)~1,data=data,k = 3, scale = "hazard")
      
      extrap_results_temp[[i]] <- model %>%
        summary(t=t/365, tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "Both" )

      #extract parameters
      parameters_results_temp[[i]] <- model[["coefficients"]] %>%
        enframe() %>%
        pivot_wider(value, name) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "Both" ) 
      
      # hazard over time
      hazot_results_temp[[i]] <- model %>%
        summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "Both" )
      
      #get the goodness of fit for each model
      gof_results_temp[[i]] <- model %>%
        glance() %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "Both" )
      
      #print out progress               
      print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohortName[j], " completed"))
      
    } else if(extrapolations[i] == "spline5") {
      # 5knotspline
      model <- flexsurvspline(formula=Surv(time_years,status-1)~1,data=data,k = 5, scale = "hazard")
      
      extrap_results_temp[[i]] <- model %>%
        summary(t=t/365, tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "Both" )
      
      parameters_results_temp[[i]] <- model[["coefficients"]] %>%
        enframe() %>%
        pivot_wider(value, name) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "Both" ) 
      
      # hazard over time
      hazot_results_temp[[i]] <- model %>%
        summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "Both" )
      
      #get the goodness of fit for each model
      gof_results_temp[[i]] <- model %>%
        glance() %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "Both" )
      
      #print out progress               
      print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohortName[j], " completed"))
      
      
    } else {
      
      #carry out models for different parametric methods survival
      model <- flexsurvreg(Surv(time_years, status)~1, data=data, dist=extrapolations[i])
      
      # extrapolations
      extrap_results_temp[[i]] <- model %>%
        summary(t=t/365, tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "Both" )
      
      #grab the parameters from the model
      parameters_results_temp[[i]] <- model[["coefficients"]] %>%
        enframe() %>%
        pivot_wider(value, name) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "Both" ) 
      
      #extract the hazard function over time
      hazot_results_temp[[i]] <- model %>%
        summary(t=(t + 1)/365, type = "hazard",tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "Both" )

      #get the goodness of fit for each model
      gof_results_temp[[i]] <- model %>%
        glance() %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "Both" )
      
      
      #print out progress               
      print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohortName[j], " completed"))
      
    }
    
    #combine all results
    extrapolatedcombined <- dplyr::bind_rows(extrap_results_temp)
    gofcombined <- dplyr::bind_rows(gof_results_temp)
    hotcombined <- dplyr::bind_rows(hazot_results_temp) %>%
      filter(time > 0) # remove rows with inf/NAs
    
    
    #put the results from each cancer in separate list
    extrapolations_all[[j]] <- extrapolatedcombined
    gof_haz_all[[j]] <- gofcombined
    hazot_all[[j]] <- hotcombined
    parameters_all[[j]] <-  parameters_results_temp
    
  }
  
  
  #print out progress               
  print(paste0(outcome_cohorts$cohortName[j]," Extrapolation Analysis Completed ", Sys.time()))
  
}


# Merge results together from each cancer and extrpolation into a dataframe ---
extrapolatedfinal <- dplyr::bind_rows(extrapolations_all)
goffinal <- dplyr::bind_rows(gof_haz_all)
hazardotfinal <- dplyr::bind_rows(hazot_all)


# will remove this when other stratifications are done and merging in run study page
#save files in results folder ---
Results_ALL <- list("extrapolation_all" = extrapolatedfinal, 
                    "hazardrate_all" = hazardotfinal,
                    "GOF_all" = goffinal)

#write results to excel ---
openxlsx::write.xlsx(Results_ALL, file = here("Results", db.name , "cancer_extrapolation_results_ALL.xlsx"))


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
  
  GompertzP[[j]] <- parameters_all[[j]] %>% pluck(1) 
  weibullP[[j]] <- parameters_all[[j]] %>% pluck(2) 
  weibullPHP[[j]] <- parameters_all[[j]] %>% pluck(3) 
  ExponentialP[[j]] <- parameters_all[[j]] %>% pluck(4) 
  LoglogP[[j]] <- parameters_all[[j]] %>% pluck(5) 
  LognormP[[j]] <- parameters_all[[j]] %>% pluck(6) 
  GenGammaP[[j]] <- parameters_all[[j]] %>% pluck(7) 
  Spline1kP[[j]] <- parameters_all[[j]] %>% pluck(8) 
  Spline3kP[[j]] <- parameters_all[[j]] %>% pluck(9) 
  Spline5kP[[j]] <- parameters_all[[j]] %>% pluck(10) 
  
}


# grab the parameters from the list and row bind
GompertzParametersAll <- dplyr::bind_rows(GompertzP)
weibullParametersAll <- dplyr::bind_rows(weibullP)
weibullPHParametersAll <- dplyr::bind_rows(weibullPHP)
ExponentialParametersAll <- dplyr::bind_rows(ExponentialP) %>%
  rename(rate = 1)
LoglogParametersAll <- dplyr::bind_rows(LoglogP)
LognormParametersAll <- dplyr::bind_rows(LognormP)
GenGammaParametersAll <- dplyr::bind_rows(GenGammaP)
Spline1kParametersAll <- dplyr::bind_rows(Spline1kP)
Spline3kParametersAll <- dplyr::bind_rows(Spline3kP)
Spline5kParametersAll <- dplyr::bind_rows(Spline5kP)

#save files in results folder ---
Results_Parameters_ALL <- list(
  "GompertzParametersAll" =  GompertzParametersAll ,
  "weibullParametersAll" =  weibullParametersAll ,
  "weibullPHParametersAll" =  weibullPHParametersAll,
  "ExponentialParametersAll" = ExponentialParametersAll,
  "LoglogParametersAll" = LoglogParametersAll,
  "LognormParametersAll" =  LognormParametersAll,
  "GenGammaParametersAll" = GenGammaParametersAll,
  "Spline1kParametersAll" = Spline1kParametersAll,
  "Spline3kParametersAll" = Spline3kParametersAll,
  "Spline5kParametersAll" = Spline5kParametersAll)

#write results to excel ---
openxlsx::write.xlsx(Results_Parameters_ALL, file = here("Results", db.name , "cancer_extrapolation_modelParameters_ALL.xlsx"))

toc(func.toc=toc_min)

info(logger, 'Extrapolation analysis for whole population COMPLETED')
