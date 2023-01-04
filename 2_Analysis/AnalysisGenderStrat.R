########################################
# GENDER STRATIFICATION
#######################################

  info(logger, 'KM analysis for gender stratification START')
  
  # KM observed
  observedkm_gender <- list()
  observedmedianKM_gender <- list()
  observedhazotKM_gender <- list()
  observedrisktableKM_gender <- list()
  
  
  
  
  
  # loop to carry out for each cancer
  for(j in 1:nrow(cohortDefinitionSet)) { 
    
    #subset the data by cancer type
    data <- Pop %>%
      filter(cohort_definition_id == j) 
    
    
    # get the data for each cancer
    
    # if when table function is less than 2 then skip
    
    
    if(j != PC_id){
      
      #carry out km estimate
      observedkm_gender[[j]] <- survfit (Surv(time_years, status) ~ gender, data=data) %>%
        tidy() %>%
        rename(Gender = strata) %>%
        mutate(Method = "Kaplan-Meier", Cancer = cohortDefinitionSet$cohortName[j], Age = "All", Gender = str_replace(Gender, "gender=Male", "Male"), Gender = str_replace(Gender,"gender=Female", "Female"))
      
      print(paste0("KM for observed data ", Sys.time()," for ",cohortDefinitionSet$cohortName[j], " completed"))
      
      # get the risk table ---
      grid <- seq(0,floor(max(data$time_years)),by=2)
      observedrisktableKM_gender[[j]] <- RiskSetCount(grid,data$time_years[data$gender == "Male"])%>%
        rbind(grid) %>% as.data.frame() %>%
        `colnames<-`(grid) %>%
        mutate(Method = "Kaplan-Meier", Cancer = cohortDefinitionSet$cohortName[j], Age = "All") %>%
        slice(1) %>%
        rbind(RiskSetCount(grid,data$time_years[data$gender == "Female"]))%>%
        mutate(Method = "Kaplan-Meier", Cancer = cohortDefinitionSet$cohortName[j], Age = "All", Gender = c("Male", "Female"))
      
      print(paste0("Extract risk table ", Sys.time()," for ",cohortDefinitionSet$cohortName[j], " completed"))
      
      
      # KM median survival ---
      modelKM <- survfit(Surv(time_years, status) ~ gender, data=data) %>%
        summary()
      
      # median survival ---
      observedmedianKM_gender[[j]] <- modelKM$table %>%
        as.data.frame() %>%
        mutate(Method = "Kaplan-Meier", 
               Cancer = cohortDefinitionSet$cohortName[j], 
               Age = "All" ,
               Gender = c("Male", "Female"))
      
      print(paste0("Median survival from KM from observed data ", Sys.time()," for ",cohortDefinitionSet$cohortName[j], " completed"))
      
      # hazard over time ---
      observedhazotKM_gender[[j]] <- group_by(data,gender) %>% 
        do(as.data.frame(bshazard(Surv(time_years, status)~1, data=., verbose=FALSE))) %>% 
        ungroup %>%
        mutate(Method = "Kaplan-Meier", Cancer = cohortDefinitionSet$cohortName[j], Age = "All")
      
      # max_data <- max(data$time_years) # need this for axis scales
      # hazardsot <- observedhazotKM_gender[[j]] %>%
      #   ggplot(observedhazotKM_gender[[j]], mapping = aes(x = time, y = hazard,group=gender)) +
      #   geom_line(aes(col=gender)) +
      #   xlab('Follow-up Time') + ylab('Hazard Rate') +
      #   scale_x_continuous(breaks = seq(0, max_data, by = 2)) +
      #   theme_bw()
      # 
      # plotname1 <- paste0("plot_hazard_over_time ",cohortDefinitionSet$cohortName[j],"_GENDER_STRAT",".png")
      # 
      # ggsave(hazardsot, file= here("Results", db.name,"Plots", plotname1)
      #        , width = 14, height = 10, units = "cm")
      
      print(paste0("Hazard over time results ", Sys.time()," for ",cohortDefinitionSet$cohortName[j], "gender strat completed"))
      
      
    }
  }
  
  # take the results from a list (one element for each cancer) and put into dataframe for KM survival
  observedkmcombined_gender <- dplyr::bind_rows(observedkm_gender) %>%
    rename(est = estimate ,ucl = conf.high, lcl = conf.low )
  
  medkmcombined_gender <- dplyr::bind_rows(observedmedianKM_gender) 
  
  hotkmcombined_gender <- dplyr::bind_rows(observedhazotKM_gender) %>%
    rename(est = hazard, ucl = upper.ci, lcl = lower.ci, Gender = gender )
  
  #generate the risk table and remove entries < 5 patients
  risktableskm_gender <- dplyr::bind_rows(observedrisktableKM_gender) 
  risktableskm_gender <- risktableskm_gender %>%
    mutate_at(.vars = c(1:(ncol(risktableskm_gender)-4)), funs(ifelse(.== 0, NA, .))) %>%  
    mutate_at(.vars = c(1:(ncol(risktableskm_gender)-4)), funs(ifelse(.<= 5, "<5", .))) %>%
    replace(is.na(.), 0) %>%
    relocate(Cancer)
  
  
  ResultsKM_GENDER <- list("KM_observed_gender" = observedkmcombined_gender, 
                           "KM_MedianSur_gender" = medkmcombined_gender,
                           "KM_hazard_rate_gender" = hotkmcombined_gender,
                           "KM_risktable_gender" = risktableskm_gender)
  
  #write to excel
  openxlsx::write.xlsx(ResultsKM_GENDER, file = here("Results", db.name ,"cancer_KM_observed_results_GENDER.xlsx"))
  
  # observedkmcombined_gender <- read.xlsx(xlsxFile = here("Results", db.name , "cancer_KM_observed_results_GENDER.xlsx"), sheet = 1)
  # medkmcombined_gender <- read.xlsx(xlsxFile = here("Results", db.name , "cancer_KM_observed_results_GENDER.xlsx"), sheet = 2)
  # hotkmcombined_gender <- read.xlsx(xlsxFile = here("Results", db.name , "cancer_KM_observed_results_GENDER.xlsx"), sheet = 3)
  # risktableskm_gender <- read.xlsx(xlsxFile = here("Results", db.name , "cancer_KM_observed_results_GENDER.xlsx"), sheet = 4)
  
  info(logger, 'KM analysis for gender stratification COMPLETE')