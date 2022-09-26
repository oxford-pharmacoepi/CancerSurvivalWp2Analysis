

# TESTS TO CHECK DATA INPUT IS CORRECT tbc

#1 Gender in format "male, "female" and factor
#2 age is numeric
#3 age group in 3 groups (or whatever has been decided) and factor
#4 another input file detailing the cancer cohorts csv (instantiateCohorts CohortsToCreate) or if running instantiate check cohortDefinitionSet is present
#5 check prostate only contains males

# Creating plots to check the data

-------# this code creates pdf plots of the km survival for each cancer for the overall population

for (cancer in 1:nrow(cohortDefinitionSet)){
  
  Pop1 <- Pop %>%
    filter(cohort_definition_id == cancer)
  
    plot_km <- ggsurvplot(
    fit = survfit(Surv(time_years, status) ~ 1, data = Pop1), 
    xlab = "Years", 
    ylab = "Overall survival probability" ,
    risk.table = TRUE,
    conf.int = TRUE,
    censor = FALSE,
    size = 0.5,
    surv.median.line = "hv",
    palette = c("#AD002AFF"),
    conf.int.fill = c("#AD002AFF") ,
    legend = "none" ,
    fontsize = 10,
    break.time.by = 2,
    font.x = 10,
    font.y = 10,
    font.tickslab = 10,
    risk.table.fontsize = 3,
    surv.scale="percent",
    tables.theme = theme_survminer(base_size = 10,
                                   font.main = 10,
                                   fontsize = 10,
                                   font.submain = 10,
                                   font.caption = 10,
                                   font.x = 10,
                                   font.y = 10,
                                   font.tickslab = 10))
  
  
  plotname <- paste0("plot_KM_survival_", cohortDefinitionSet$cohortName[cancer],".pdf")
  

  pdf(paste0("3_ExamplePlots","/", plotname),
      width = 7, height = 5)
  print(plot_km, newpage = FALSE)
  dev.off()
  
}


-------# this code creates KM plots for all cancers stratified by GENDER
for (cancer in 1:nrow(cohortDefinitionSet)){
  
  Pop1 <- Pop %>%
    filter(cohort_definition_id == cancer) %>%
    droplevels()
  
  #if gender factor only has 1 level then dont run the plot
  
  if(nlevels(Pop1$gender) == 2 ) {
  
  plot_km <- ggsurvplot(
    fit = survfit(Surv(time_years, status) ~ gender, data = Pop1), 
    xlab = "Years", 
    ylab = "Overall survival probability" ,
    risk.table = TRUE,
    conf.int = TRUE,
    censor = FALSE,
    surv.median.line = "hv",
    legend.labs = c("Male", "Female"),
    palette = c("#2E9FDF", "hotpink"),
    size = 0.5,
    legend = "none" ,
    fontsize = 10,
    break.time.by = 2,
    font.x = 10,
    font.y = 10,
    font.tickslab = 10,
    risk.table.fontsize = 3,
    surv.scale="percent",
    #risk.table.height = 0.35,
    tables.theme = theme_survminer(base_size = 10,
                                   font.main = 10,
                                   fontsize = 10,
                                   font.submain = 10,
                                   font.caption = 10,
                                   font.x = 10,
                                   font.y = 10,
                                   font.tickslab = 10))
  
  plotname <- paste0("plot_KM_survival_gender_strat", cohortDefinitionSet$cohortName[cancer],".pdf")
  
  
  pdf(paste0("3_ExamplePlots","/", plotname),
      width = 7, height = 5)
  print(plot_km, newpage = FALSE)
  dev.off()
  
  } else {
  
    print(paste("Sex stratified plot not run for cancer:", cohortDefinitionSet$cohortName[cancer]))
}

}
  

-------# this code creates KM plots for all cancers stratified by AGE 50 strat 2 groups
  for (cancer in 1:nrow(cohortDefinitionSet)){
    
    Pop1 <- Pop %>%
      filter(cohort_definition_id == cancer)
  
      plot_km <- ggsurvplot(
        fit = survfit(Surv(time_years, status) ~ age_gr2, data = Pop1), 
        xlab = "Years", 
        ylab = "Overall survival probability" ,
        risk.table = TRUE,
        conf.int = TRUE,
        censor = FALSE,
        surv.median.line = "hv",
        legend.labs = c("Age <50", "Age >50"),
        palette = c("#E7B800", "mediumorchid"),
        size = 0.5,
        legend = "none" ,
        fontsize = 10,
        break.time.by = 2,
        font.x = 10,
        font.y = 10,
        font.tickslab = 10,
        risk.table.fontsize = 3,
        surv.scale="percent",
        #risk.table.height = 0.35,
        tables.theme = theme_survminer(base_size = 10,
                                       font.main = 10,
                                       fontsize = 10,
                                       font.submain = 10,
                                       font.caption = 10,
                                       font.x = 10,
                                       font.y = 10,
                                       font.tickslab = 10))
      
      plotname <- paste0("plot_KM_survival_age50_strat", cohortDefinitionSet$cohortName[cancer],".pdf")
      
      
      pdf(paste0("3_ExamplePlots","/", plotname),
          width = 7, height = 5)
      print(plot_km, newpage = FALSE)
      dev.off()
      
    } 

# age group stratification # 8 age stratifications 10 year age bands

for (cancer in 1:nrow(cohortDefinitionSet)){
  
Pop1 <- Pop %>%
  filter(cohort_definition_id == cancer)

plot_km <- ggsurvplot(
  fit = survfit(Surv(time_years, status) ~ age_gr, data = Pop1), 
  xlab = "Years", 
  ylab = "Overall survival probability" ,
  risk.table = TRUE,
  conf.int = TRUE,
  censor = FALSE,
  #surv.median.line = "hv",
  # tables.height = 0.2,
  legend.labs = c("<30", 
                  "30-39",
                  "40-49",
                  "50-59",
                  "60-69",
                  "70-79",
                  "80-89",
                  "90+"),
  palette = c("#fde725", 
              "#90d743",
              "#35b779",
              "#21918c",
              "#31688e",
              "#443983",
              "#440154",
              "black"),
  legend = "none" ,
  fontsize = 10,
  size = 0.5,
  break.time.by = 2,
  font.x = 10,
  font.y = 10,
  #xlim = c(0.5,max(Pop1$time_years)),
  font.tickslab = 10,
  risk.table.fontsize = 3,
  surv.scale="percent",
  #risk.table.height = 0.35,
  tables.theme = theme_survminer(base_size = 10,
                                 font.main = 10,
                                 fontsize = 10,
                                 font.submain = 10,
                                 font.caption = 10,
                                 font.x = 10,
                                 font.y = 10,
                                 font.tickslab = 10))


plotname <- paste0("plot_KM_survival_agegps_strat", cohortDefinitionSet$cohortName[cancer],".pdf")


pdf(paste0("3_ExamplePlots","/", plotname),
    width = 7, height = 7)
print(plot_km, newpage = FALSE)
dev.off()

}



#############################################################################

# extrapolations

extrapolations <- c("gompertz", "weibull", "exp", "llogis", "lnorm", "gengamma", "spline1", "spline3") 
extrapolations_formatted <- c("Gompertz", "Weibull", "Exponential", "Log-logistic", "Log-normal", "Generalised Gamma", "Spline (1 Knot)", "Spline (3 knots)")
timeinyrs <- 25
t <- seq(0, timeinyrs*365, by=1) # calculates the extrapolation 

#################################################
# WHOLE POPULATION
#################################################
# OBSERVED DATA ANALYSIS
# km survival, log cumulative and hazard over time from the observed data for each cancer ----

# capture output
observedkm <- list()

# loop to carry out for each cancer
for(j in 1:nrow(cohortDefinitionSet)) { 

#subset the data by cancer type
data <- Pop %>%
  filter(cohort_definition_id == j)

#carry out km estimate
observedkm[[j]] <- survfit (Surv(time_years, status) ~ 1, data=data) %>%
  tidy() %>%
  mutate(Method = "Observed", cancer = cohortDefinitionSet$cohortName[j]) %>%
  filter(n.risk >= 5) #remove entries with less than 5 patients

print(paste0("KM for observed data ", Sys.time()," for ",cohortDefinitionSet$cohortName[j], " completed"))


# log cumulative hazard plots ------
logcumhazplot <- ggplot(observedkm[[j]], aes(x = log(time), y = log(-log(estimate)))) + 
  xlab("log(time)") + ylab("log(-log(y)") +
  geom_line() +
  geom_ribbon(aes(ymin = log(-log(conf.low)), ymax = log(-log(conf.high))), linetype = 2, alpha = 0.1) +
  theme_bw()

plotname <- paste0("plot_log_cumulative_hazard ",cohortDefinitionSet$cohortName[j],"_ALL",".png")

ggsave(logcumhazplot, file= here("Results", db.name,"Plots", plotname)
       , width = 14, height = 10, units = "cm")

print(paste0("Plot log cumulative hazard plot ", Sys.time()," for ",cohortDefinitionSet$cohortName[j], " completed"))

# hazard function over time ----
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time,hazard,lower.ci,upper.ci))
}

max_data <- max(data$time_years) # need this for axis scales
hazardsot <- as.data.frame.bshazard(bshazard(Surv(time_years, status) ~ 1, data=data, verbose=FALSE)) %>% 
  ggplot(hazardsot, mapping = aes(x = time, y = hazard)) +
  geom_line() +
  geom_ribbon(aes(ymin=lower.ci, ymax=upper.ci), alpha=0.3) +
  #ylim(0,0.1) + if you want to change the hazard rate axis
  xlab('Follow-up Time') + ylab('Hazard Rate') +
  scale_x_continuous(breaks = seq(0, max_data, by = 2)) +
  theme_bw()


plotname1 <- paste0("plot_hazard_over_time ",cohortDefinitionSet$cohortName[j],"_ALL",".png")

ggsave(hazardsot, file= here("Results", db.name,"Plots", plotname)
       , width = 14, height = 10, units = "cm")

print(paste0("Plot hazard over time plot ", Sys.time()," for ",cohortDefinitionSet$cohortName[j], " completed"))


}

# take the results for each cancer and combine into one
observedkmcombined <- dplyr::bind_rows(observedkm) %>%
  rename(est = estimate ,ucl = conf.high, lcl = conf.low )

openxlsx::write.xlsx(observedkmcombined, file = here("Results", db.name ,"cancer_KM_observed_results_ALL.xlsx"))


###########################################
# EXTRAPOLATION ANALYSIS
# generating extrpolations ----
# Initiate templists to store output ---- will have to make folders for each cancer and loop
extrapolations_all <- list()
cum_haz_all <- list() # not sure need this now
gof_haz_all <- list()
extrapolated_predictions_all <- list() # this will contain results for predicted survival at 1,5,10 years 

# Initiate templists to store output ---- 
extrap_results_temp <- list() # Create empty list for extrapolations
gof_results_temp <- list() # required to assess goodness of fit (AIC/BIC)
cumhaz_results_temp <- list() #required for cumhaz plots
extrapolated_predictions_temp <- list() # extrapolated predictions 1,5,10 years

# Run extrapolations for all cancers for ALL population ---
for(j in 1:nrow(cohortDefinitionSet)) { 
  
  #subset the data by cancer type
  
  data <- Pop %>%
    filter(cohort_definition_id == j)
  
  #carry out extrapolation for each cancer
  for(i in 1:length(extrapolations)) {   # Head of for-loop
    
    if(extrapolations[i] == "spline1") {
      
      # 1knotspline
      extrap_results_temp[[i]] <- flexsurvspline(formula=Surv(time_years,status-1)~1,data=data,k = 1, scale = "hazard") %>%
        summary(t=t/365, tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], cancer = cohortDefinitionSet$cohortName[j])
      
      #carry out models for different parametric methods cumhaz
      cumhaz_results_temp[[i]] <- flexsurvspline(formula=Surv(time_years,status-1)~1,data=data,k = 1, scale = "hazard") %>%
        summary(t=t/365, tidy = TRUE, type = "cumhaz") %>%
        mutate(Method = extrapolations_formatted[i], cancer = cohortDefinitionSet$cohortName[j])
      
      #get the goodness of fit for each model
      gof_results_temp[[i]] <- flexsurvspline(formula=Surv(time_years,status-1)~1,data=data,k = 1, scale = "hazard") %>%
        glance() %>%
        mutate(Method = extrapolations_formatted[i], cancer = cohortDefinitionSet$cohortName[j])
      
      
      #print out progress               
      print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cohortDefinitionSet$cohortName[j], " completed"))
      
    } else if(extrapolations[i] == "spline3") {
      # 3knotspline
      extrap_results_temp[[i]] <- flexsurvspline(formula=Surv(time_years,status-1)~1,data=data,k = 3, scale = "hazard") %>%
      summary(t=t/365, tidy = TRUE) %>%
      mutate(Method = extrapolations_formatted[i], cancer = cohortDefinitionSet$cohortName[j])
      
      #carry out models for different parametric methods cumhaz
      cumhaz_results_temp[[i]] <- flexsurvspline(formula=Surv(time_years,status-1)~1,data=data,k = 3, scale = "hazard") %>%
        summary(t=t/365, tidy = TRUE, type = "cumhaz") %>%
        mutate(Method = extrapolations_formatted[i], cancer = cohortDefinitionSet$cohortName[j])
      
      #get the goodness of fit for each model
      gof_results_temp[[i]] <- flexsurvspline(formula=Surv(time_years,status-1)~1,data=data,k = 3, scale = "hazard") %>%
        glance() %>%
        mutate(Method = extrapolations_formatted[i], cancer = cohortDefinitionSet$cohortName[j])
      
      #print out progress               
      print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cohortDefinitionSet$cohortName[j], " completed"))
      
      
    } else {
      #carry out models for different parametic methods survival
      extrap_results_temp[[i]] <- flexsurvreg(Surv(time_years, status)~1, data=data, dist=extrapolations[i]) %>%
        summary(t=t/365, tidy = TRUE) %>%
      mutate(Method = extrapolations_formatted[i], cancer = cohortDefinitionSet$cohortName[j])
      
      #carry out models for different parametric methods cumhaz
      cumhaz_results_temp[[i]] <- flexsurvreg(Surv(time_years, status)~1, data=data, dist=extrapolations[i]) %>%
        summary(t=t/365, tidy = TRUE, type = "cumhaz") %>%
        mutate(Method = extrapolations_formatted[i], cancer = cohortDefinitionSet$cohortName[j])

      #get the goodness of fit for each model
      gof_results_temp[[i]] <- flexsurvreg(Surv(time_years, status)~1, data=data, dist=extrapolations[i]) %>%
      glance() %>%
        mutate(Method = extrapolations_formatted[i], cancer = cohortDefinitionSet$cohortName[j])
      

      #print out progress               
      print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cohortDefinitionSet$cohortName[j], " completed"))
      
    }
    
    #combine all results
    extrapolatedcombined <- dplyr::bind_rows(extrap_results_temp)
    cumhazcombined <- dplyr::bind_rows(cumhaz_results_temp)
    gofcombined <- dplyr::bind_rows(gof_results_temp)

    #put the results from each cancer in separate list
    extrapolations_all[[j]] <- extrapolatedcombined
    cum_haz_all[[j]] <- cumhazcombined
    gof_haz_all[[j]] <- gofcombined
    

    
  }
  #print out progress               
  print(paste0(cohortDefinitionSet$cohortName[j]," Extrapolation Analysis Completed ", Sys.time()))
  
}

# Merge results together from each cancer and extrpolation into a dataframe ---
extrapolatedfinal <- dplyr::bind_rows(extrapolations_all)
cumhazfinal <- dplyr::bind_rows(cum_haz_all)
goffinal <- dplyr::bind_rows(gof_haz_all)

#save files in results folder ---
Results_ALL <- list("extrapolation_all" = extrapolatedfinal, 
                    "cumulativehaz_all" = cumhazfinal,
                    "GOF_all" = goffinal)

#write results to excel ---
openxlsx::write.xlsx(Results_ALL, file = here("Results", "cancer_extrapolation_results_ALL.xlsx"))

# plots KM observed data and extrapolated data -----

#merge extrapolation and observed results
plot_combined_all <- bind_rows(extrapolatedfinal, observedkmcombined)

# Create plots for whole population ---
for(j in 1:nrow(cohortDefinitionSet)) { 

data <- plot_combined_all %>%
  filter(cancer == cohortDefinitionSet$cohortName[j])

#carry out plot for each extrapolation
for(i in 1:length(extrapolations)) {

#extract for each extrapolation  
  data_extrap <- data %>%
    filter(Method == extrapolations_formatted[i] | Method == "Observed")
  
  cols <- c("#00468BFF", #dark blue
            "#ED0000FF", # red
            "#42B540FF", #green
            "#0099B4FF", #lightblue
            "#925E9FFF", # purple
            "#FF6F0EFF", #orange
            "#E377C2FF", #pink
            "#BCBD22FF", #olive
            "#AD002AFF" # dark red
  ) 
  
  my_colors <- c("darkgrey", cols[i])
  
  data_extrap$Method <- factor(data_extrap$Method, levels=c('Observed', extrapolations_formatted[i]))
  
  plot_km1 <- ggplot(data_extrap, aes(x = time, y = est, colour = Method)) + 
    xlab("Years") + ylab("Survival Probability") +
    geom_line() +
    geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Method), linetype = 2, alpha = 0.1) +
    scale_color_manual(values = my_colors) +
    scale_fill_manual(values = my_colors) +
    theme_bw()+ 
    theme( legend.position = 'top', legend.direction = "horizontal") +
    scale_x_continuous(limits = c(0,max(data_extrap$time)), expand =c(0,0) ,
                       breaks = seq(0,max(data_extrap$time), by = 2 ) ) +
    #scale_y_continuous(limits = c(0,1.02), expand =c(0.01,0)) 
  scale_y_continuous(limits = c(min(data_extrap$lcl)- 0.1,1.02), expand =c(0.01,0)) 
  
  
  #name plot
  plotname <- paste0("plot_survival_extrapolation_",cohortDefinitionSet$cohortName[j],"_", extrapolations_formatted[i],"_ALL",".png")
  
  ggsave(plot_km1, file= here("Results", db.name,"Plots", plotname)
         , width = 14, height = 10, units = "cm")
  
  #plot created
  print(paste0("Plot ", Sys.time()," for extrapolation method ",extrapolations_formatted[i]," for ",cohortDefinitionSet$cohortName[j], " completed"))
  
  }
  
#plot created
print(paste0("Plots ", Sys.time()," for extrapolation method for ",cohortDefinitionSet$cohortName[j], " completed"))

}
  
# to incorporate

# calculating the 1, 5 and 10 year surival for each cancer

km_pred <- summary(survfit(Surv(time_years, status) ~ 1, data = data), times = c(1,5,10))


extrapolation_pred <- subset(extrapolatedfinal, extrapolatedfinal$time == 1 |
                               extrapolatedfinal$time == 5 |
                               extrapolatedfinal$time == 10  )

################END ####################

### code to improve ###
# this works below that extracts the value closest to prediction

data212 <-  observedkmcombined %>%
  filter(time >= 10) %>%
  group_by(cancer) %>%
  slice(1)

data212 <-  observedkmcombined %>%
  filter(time >= 5) %>%
  group_by(cancer) %>%
  slice(1)

data212 <-  observedkmcombined %>%
  filter(time >= 1) %>%
  group_by(cancer) %>%
  slice(1)

################################################



       
  





# for colorectal cancer only



#extrapolation the time is in year for the observed but the extrapolation is in days BUG
# extrap_results_temp <- flexsurvreg(Surv(time_years, status)~1, data=data, dist=extrapolations[1]) %>%
#   summary(t=t/365, tidy = TRUE) %>%
#   mutate(Method = extrapolations_formatted[1], cancer = cohortDefinitionSet$cohortName[2])


extrap_results_temp <- flexsurvspline(formula=Surv(time_years,status-1)~1,data=data,k = 3, scale = "hazard") %>%
  summary(t=t/365, tidy = TRUE) %>%
  mutate(Method = extrapolations_formatted[8], cancer = cohortDefinitionSet$cohortName[2])


cols <- c("#00468BFF", #dark blue
          "#ED0000FF", # red
          "#42B540FF", #green
          "#0099B4FF", #lightblue
          "#925E9FFF", # purple
          "#FF6F0EFF", #orange
          "#E377C2FF", #pink
          "#BCBD22FF", #olive
          "#AD002AFF"
) # dark red

#  carry out basic plots
#survival extrapolations
#for(i in 1:length(extrapolations)) { 

my_colors <- c("darkgrey", cols[i])


#rbind the observed results to the extrapolated ones
extrap_results1 <- rbind(km_CRC, extrap_results_temp)

extrap_results1$Method <- factor(extrap_results1$Method, levels=c('Observed', extrap_results1$Method[nrow(extrap_results1)] ))


plot_km1 <- ggplot(extrap_results1, aes(x = time, y = est, colour = Method)) + 
  xlab("Years") + ylab("Survival Probability") +
  geom_line() +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Method), linetype = 2, alpha = 0.1) +
  scale_color_manual(values = my_colors) +
  scale_fill_manual(values = my_colors) +
  theme_bw() + 
  theme( legend.position = 'top', legend.direction = "horizontal") +
  scale_x_continuous(limits = c(0,max(extrap_results1$time)), expand =c(0,0) ,
                     breaks = seq(0,max(extrap_results1$time), by = 2 ) ) +
  scale_y_continuous(limits = c(0,1.02), expand =c(0.01,0)) 











################# OLD CODE ##################


# function to carry out extrapolation produces survival data and goodness of fit
for(i in 1:length(extrapolations)) {   # Head of for-loop
  
  if(extrapolations[i] == "spline1") {
    
    # 1knotspline
    model <- flexsurvspline(formula=Surv(time,status-1)~1,data=data[j],k = 1, scale = "hazard")
    model_out <-summary(model,t=t)[[1]] # extract the data
    model_out$Method <- extrapolations_formatted[i]
    model_out$cancer <- cohortDefinitionSet$cohortName[j]
    list_extrap_results[[i]] <- model_out   # Store output in list
    
    #carry out models for different parametric methods cumhaz
    model_out2 <- summary(model, t=t , type = "cumhaz")[[1]]
    model_out2$Method <- extrapolations_formatted[i]
    model_out2$cancer <- cohortDefinitionSet$cohortName[j]
    cumhaz_results[[i]] <- model_out2   # Store output in list
    
    #get the goodness of fit for each model
    gof_results[[i]] <- round(glance(model)[,c(6:8)],2)
    
    #print out progress               
    print(paste0(extrapolations_formatted[i]," ", Sys.time(),"for " ,cohortDefinitionSet$cohortName[j], " completed"))
    
  } else if(extrapolations[i] == "spline3") {
    # 3knotspline
    model <- flexsurvspline(formula=Surv(time,status-1)~1,data=data[j],k = 3, scale = "hazard")
    model_out <-summary(model,t=t)[[1]] # extract the data
    model_out$Method <- extrapolations_formatted[i]
    model_out$cancer <- cohortDefinitionSet$cohortName[j]
    list_extrap_results[[i]] <- model_out   # Store output in list
    
    #carry out models for different parametric methods cumhaz
    model_out2 <- summary(model, t=t , type = "cumhaz")[[1]]
    model_out2$Method <- extrapolations_formatted[i]
    model_out2$cancer <- cohortDefinitionSet$cohortName[j]
    cumhaz_results[[i]] <- model_out2   # Store output in list
    
    #get the goodness of fit for each model
    gof_results[[i]] <- round(glance(model)[,c(6:8)],2)
    
    #print out progress               
    print(paste0(extrapolations_formatted[i]," ", Sys.time(),"for " ,cohortDefinitionSet$cohortName[j], " completed"))
    
    
  } else {
    #carry out models for different parametic methods survival
    model<-flexsurvreg(Surv(time, status)~1, data=data[j], dist=extrapolations[i])
    model_out <-summary(model,t=t)[[1]] # extract the data
    model_out$Method <- extrapolations_formatted[i]
    model_out$cancer <- cohortDefinitionSet$cohortName[j]
    list_extrap_results[[i]] <- model_out   # Store output in list
    
    #carry out models for different parametric methods cumhaz
    model_out2 <- summary(model, t=t , type = "cumhaz")[[1]]
    model_out2$Method <- extrapolations_formatted[i]
    model_out2$cancer <- cohortDefinitionSet$cohortName[j]
    cumhaz_results[[i]] <- model_out2   # Store output in list
    
    #get the goodness of fit for each model
    gof_results[[i]] <- round(glance(model)[,c(6:8)],2)
    
    #print out progress               
    print(paste0(extrapolations_formatted[i]," ", Sys.time(),"for " ,cohortDefinitionSet$cohortName[j], " completed"))
    
  }
}

#get the observed data and output the results survival
kmsurvival <- survfit (Surv(time, status) ~ 1, data=data[j])
km_result <- as.data.frame(cbind(kmsurvival$time, kmsurvival$surv, kmsurvival$lower, kmsurvival$upper))
colnames(km_result) <- c("time", "est", "lcl", "ucl")
km_result$Method <- "Observed"
km_result$cancer <- cohortDefinitionSet$cohortName[j]


print(paste0("KM for observed data completed", Sys.time()))









#remove entries with less than 5 entries
km_result <- km_result %>%
  filter(nrisk >= 5)





# get the results from whole population

kmsurvival <- survfit(Surv(time_years, status) ~ 1, data = Pop1)

km_result <- as.data.frame(cbind(kmsurvival$time, kmsurvival$surv, kmsurvival$lower, kmsurvival$upper, kmsurvival$n.risk))
colnames(km_result) <- c("time", "est", "lcl", "ucl", "nrisk")
km_result$Method <- "Observed"

#remove those with 5 or less
km_result <- km_result %>%
  filter(nrisk >= 5)



















# original code using dummy data ----

# how to include survimer and extrapolation
#https://stackoverflow.com/questions/63469108/set-x-axis-limits-in-ggplot2-when-adding-another-regression-line-numbers-on

# Initiate lists to store output ---- will have to make folders for each cancer and loop
list_extrap_results <- list() # Create empty list for extrapolations
gof_results <- list() # required to assess goodness of fit (AIC/BIC)
cumhaz_results <- list() #required for cumhaz plots

# Bring in data (this will be removed when we have data) -----
data(cancer, package="survival")
lung
# what i imagine is that we will have a csv file listing the cancer cohorts names at least

# set up inputs ----- at the moment one dataset and alot of these settings will be in other scripts
output.folder<-here("lung")
db.name<-"CPRD"
data <- lung
time <- "time"
status <- "status"
sex <- "gender"
age <- "age" # or are we doing age bands?
extrapolations <- c("gompertz", "weibull", "exp", "llogis", "lnorm", "gengamma", "spline1", "spline3") 
extrapolations_formatted <- c("Gompertz", "Weibull", "Exponential", "Log-logistic", "Log-normal", "Generalised Gamma", "Spline (1 Knot)", "Spline (3 knots)")
timeinyrs <- 10
t <- seq(0, timeinyrs*365.25, by=1) # calculates the extrapolation for 10 years

# function to carry out extrapolation produces survival data, cum hazard and goodness of fit
for(i in 1:length(extrapolations)) {   # Head of for-loop

  if(extrapolations[i] == "spline1") {
  
    # 1knotspline
    model <- flexsurvspline(formula=Surv(time,status-1)~1,data=lung,k = 1, scale = "hazard")
    model_out <-summary(model,t=t)[[1]] # extract the data
    model_out$Method <- extrapolations_formatted[i]
    list_extrap_results[[i]] <- model_out   # Store output in list
    
    #carry out models for different parametric methods cumhaz
    model_out2 <- summary(model, t=t , type = "cumhaz")[[1]]
    model_out2$Method <- extrapolations_formatted[i]
    cumhaz_results[[i]] <- model_out2   # Store output in list
    
    #get the goodness of fit for each model
    gof_results[[i]] <- round(glance(model)[,c(6:8)],2)
    
    #print out progress               
    print(paste0(extrapolations_formatted[i]," ", Sys.time(), " completed"))

  } else if(extrapolations[i] == "spline3") {
    # 3knotspline
    model <- flexsurvspline(formula=Surv(time,status-1)~1,data=lung,k = 3, scale = "hazard")
    model_out <-summary(model,t=t)[[1]] # extract the data
    model_out$Method <- extrapolations_formatted[i]
    list_extrap_results[[i]] <- model_out   # Store output in list
    
    #carry out models for different parametric methods cumhaz
    model_out2 <- summary(model, t=t , type = "cumhaz")[[1]]
    model_out2$Method <- extrapolations_formatted[i]
    cumhaz_results[[i]] <- model_out2   # Store output in list
    
    #get the goodness of fit for each model
    gof_results[[i]] <- round(glance(model)[,c(6:8)],2)
    
    #print out progress               
    print(paste0(extrapolations_formatted[i]," ", Sys.time(), " completed"))
    
  } else {
  #carry out models for different parametic methods survival
  model<-flexsurvreg(Surv(time, status)~1, data=data, dist=extrapolations[i])
  model_out <-summary(model,t=t)[[1]] # extract the data
  model_out$Method <- extrapolations_formatted[i]
  list_extrap_results[[i]] <- model_out   # Store output in list
  
  #carry out models for different parametric methods cumhaz
  model_out2 <- summary(model, t=t , type = "cumhaz")[[1]]
  model_out2$Method <- extrapolations_formatted[i]
  cumhaz_results[[i]] <- model_out2   # Store output in list
  
  #get the goodness of fit for each model
  gof_results[[i]] <- round(glance(model)[,c(6:8)],2)
  
  #print out progress               
  print(paste0(extrapolations_formatted[i]," ", Sys.time(), " completed"))
}
}

#get the observed data and output the results survival
kmsurvival <- survfit (Surv(time, status) ~ 1, data=data)
km_result <- as.data.frame(cbind(kmsurvival$time, kmsurvival$surv, kmsurvival$lower, kmsurvival$upper))
colnames(km_result) <- c("time", "est", "lcl", "ucl")
km_result$Method <- "Observed"


# get the mean and median survival from observed data
asd <- as.data.frame(summary(kmsurvival)$table)

# get the survival probabilities for 1, 5 and 10 years
#1 year
year1 <- summary(survfit(Surv(time, status) ~ 1, data = lung), times = 365.25)$surv

#5 years
year5 <- summary(survfit(Surv(time, status) ~ 1, data = lung), times = 1826.25)$surv # comes up with an error

#10 years
year10 <- summary(survfit(Surv(time, status) ~ 1, data = lung), times = 3652.5) # comes up with an error




# carry out the for the observed data using km cum hazard # cumulative hazard is y -log(y) -----
km_result_cumhaz <- km_result %>%
  mutate(lcl = -log(lcl),
         ucl = -log(ucl),
         est = -log(est))


#save the results in the output ----- so people can create their own plots for each cancer
Survival.summary_all <- list_extrap_results %>%
  map(as_tibble) %>%
  reduce(bind_rows) %>% 
  bind_rows(km_result) %>% 
  collect()

#save gof outputs -------
Survival.gof_all <- gof_results %>%
  map(as_tibble) %>%
  reduce(bind_rows) %>% 
  mutate(Method = extrapolations_formatted,
         .before=logLik) %>%
  collect()

#save cumhaz outputs ------
CumHaz.summary_all <- cumhaz_results %>%
  map(as_tibble) %>%
  reduce(bind_rows) %>% 
  bind_rows(km_result_cumhaz) %>% 
  collect()


#save in output files ----------
#first check if there is a folder
if (!file.exists(output.folder))
  dir.create(output.folder, recursive = TRUE)

#write out the results extrapolation results, cumulative hazard and gof plots ALL POPULATION
write.csv(Survival.summary_all, file=paste0(output.folder, "/Survival_extrapolations_", db.name, "_ALL.csv"), row.names = FALSE)
write.csv(Survival.gof_all, file=paste0(output.folder, "/GOF_", db.name, "_ALL.csv"), row.names = FALSE)
write.csv(CumHaz.summary_all, file=paste0(output.folder, "/CumHaz_", db.name, "_ALL.csv"), row.names = FALSE)

## plots ------

# get some colours for plots --------
cols <- c("#00468BFF", #dark blue
          "#ED0000FF", # red
          "#42B540FF", #green
          "#0099B4FF", #lightblue
          "#925E9FFF", # purple
          "#FF6F0EFF", #orange
          "#E377C2FF", #pink
          "#BCBD22FF", #olive
          "#AD002AFF"
          ) # dark red

#  carry out basic plots
#survival extrapolations
for(i in 1:length(extrapolations)) { 
  
  my_colors <- c("darkgrey", cols[i])
  
  # for each extrapolation method rbind with each observed and create a plot
  extrap_results <- list_extrap_results[[i]]
  
  km_data <-  km_result
  
  #rbind the observed results to the extrapolated ones
  extrap_results1 <- rbind(km_data, extrap_results)
  
  extrap_results1$Method <- factor(extrap_results1$Method, levels=c('Observed', extrap_results1$Method[nrow(extrap_results1)] ))
  
  # convert days to years
  extrap_results1 <- extrap_results1 %>% 
    mutate(Years = round(time/365.25, digit=5))
  
  plot_km1 <- ggplot(extrap_results1, aes(x = Years, y = est, colour = Method)) + 
    xlab("Years") + ylab("Survival Probability") +
    geom_line() +
    geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Method), linetype = 2, alpha = 0.1) +
    scale_color_manual(values = my_colors) +
    scale_fill_manual(values = my_colors) +
    theme_bw() + 
    theme( legend.position = 'top', legend.direction = "horizontal") +
    scale_x_continuous(limits = c(0,max(extrap_results1$Years)), expand =c(0,0) ,
                       breaks = seq(0,max(extrap_results1$Years), by = 2 ) ) +
    scale_y_continuous(limits = c(0,1.02), expand =c(0.01,0)) 
  
  
  #name plot
  plotname <- paste0("plot_survival_", extrapolations_formatted[i],".png")
  
  # ggsave(plot_km1, file= here("Github", "CancerSurvivalExtrapolation","3_ExamplePlots", plotname)
  #        , width = 14, height = 10, units = "cm")
  
  ggsave(plot_km1, file= paste0(output.folder,"/", plotname)
         , width = 14, height = 10, units = "cm")
  
  
}


#cumulative hazard
for(i in 1:length(extrapolations)) {

  my_colors <- c("darkgrey", cols[i])

  # for each extrapolation method rbind with each observed and create a plot
  extrap_results <- cumhaz_results[[i]]

  km_data <-  km_result_cumhaz

  #rbind the observed results to the extrapolated ones
  extrap_results1 <- rbind(km_data, extrap_results)

  extrap_results1$Method <- factor(extrap_results1$Method, levels=c('Observed', extrap_results1$Method[nrow(extrap_results1)] ))

  # convert days to years
  extrap_results1 <- extrap_results1 %>%
    mutate(Years = round(time/365.25, digit=5))

  plot_km1 <- ggplot(extrap_results1, aes(x = Years, y = est, colour = Method)) +
    xlab("Years") + ylab("Survival Probability") +
    geom_line() +
    geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Method), linetype = 2, alpha = 0.1) +
    scale_color_manual(values = my_colors) +
    scale_fill_manual(values = my_colors) +
    theme_bw() +
    theme( legend.position = 'top', legend.direction = "horizontal") +
    scale_x_continuous(limits = c(0,max(extrap_results1$Years)), expand =c(0,0) ,
                       breaks = seq(0,max(extrap_results1$Years), by = 2 ) ) +
    scale_y_continuous(limits = c(0,max(extrap_results1$ucl)), expand =c(0.01,0))


  #name plot
  plotname <- paste0("plot_cum_haz_", extrapolations_formatted[i],".png")


  ggsave(plot_km1, file= paste0(output.folder,"/", plotname)
         , width = 14, height = 10, units = "cm")


}


# log cumulative hazard plots
for(i in 1:length(extrapolations)) { 
  
  my_colors <- c("darkgrey", cols[i])
  
  # for each extrapolation method rbind with each observed and create a plot
  extrap_results <- list_extrap_results[[i]]
  
  km_data <-  km_result
  
  #rbind the observed results to the extrapolated ones
  extrap_results1 <- rbind(km_data, extrap_results)
  
  extrap_results1$Method <- factor(extrap_results1$Method, levels=c('Observed', extrap_results1$Method[nrow(extrap_results1)] ))
  
  # convert days to years
  extrap_results1 <- extrap_results1 %>% 
    mutate(Years = round(time/365.25, digit=5))
  
  #convert the data to log format and remove inf values -----
  extrap_results2 <- extrap_results1 %>%
    mutate(Years = log(Years),
           est = -log(-log(est)) ,
           ucl = -log(-log(ucl)) ,
           lcl = -log(-log(lcl))) %>%
           replace(.== Inf | .== -Inf , NA)
  
  #plot the results
  plot_km1 <- ggplot(extrap_results2, aes(x = Years, y = est, colour = Method)) + 
    xlab("log(Years)") + ylab("-log(-log(s(t)))") +
    geom_line() +
    geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Method), linetype = 2, alpha = 0.1) +
    scale_color_manual(values = my_colors) +
    scale_fill_manual(values = my_colors) +
    theme_bw() + 
    theme( legend.position = 'top', legend.direction = "horizontal") +
    scale_x_continuous(limits = c(min(extrap_results2$Years, na.rm = T),max(extrap_results2$Years, na.rm = T)), expand =c(0.0,0) ,
                       breaks = seq(round(min(extrap_results2$Years, na.rm = T),0) ,max(extrap_results2$Years, na.rm = T), by = 2 ) ) +
    scale_y_continuous(limits = c(min(extrap_results2$lcl, na.rm = T),max(extrap_results2$ucl, na.rm = T)), expand =c(0.01,0)) 
  
  
  #name plot
  plotname <- paste0("plot_log_cum_haz_", extrapolations_formatted[i],".png")
  
  # ggsave(plot_km1, file= here("Github", "CancerSurvivalExtrapolation","3_ExamplePlots", plotname)
  #        , width = 14, height = 10, units = "cm")
  
  ggsave(plot_km1, file= paste0(output.folder,"/", plotname)
         , width = 14, height = 10, units = "cm")
  
  
}


plot(model)

plot(model, type = "cumhaz")


######################################################
# under development not working yet
#####################################################


# stratification
# Initiate lists to store output ---- 
list_extrap_results_strat <- list() # Create empty list for extrapolations
gof_results_strat <- list() # required to assess goodness of fit (AIC/BIC)
cumhaz_results_strat <- list() #not sure if we need this

# need to check the variable for stratification are factors if not then turn them into factor
data <- data %>%
  mutate_at(vars(sex), 
            list(factor))


# structure of list [[strata]] >> [[results]]: strata is gender, age, gender*age
# function to carry out extrapolation produces survival data, cum hazard and goodness of fit
for(i in 1:length(extrapolations)) {   # Head of for-loop
  
  if(extrapolations[i] == "spline1") {
    
    # 1knotspline
    # model <- flexsurvspline(formula=Surv(time,status-1)~sex + gamma1(sex), data=lung,k = 1, scale = "hazard")
    # model <- flexsurvspline(formula=Surv(time,status-1)~sex , data=lung,k = 1, scale = "hazard")
    # 
    # model_out <-summary(model,t=t)[[1]] # extract the data
    # model_outa <-summary(model,t=t)[[2]] # extract the data
    # #add names of strata
    # model_out$strata <- levels(data$sex)[1]
    # model_outa$strata <- levels(data$sex)[2]
    # #bind the results
    # model_out <- rbind(model_out, model_outa)
    # model_out$Method <- extrapolations_formatted[i]
    # list_extrap_results[[i]] <- model_out   # Store output in list
    # 
    # #carry out models for different parametric methods cumhaz
    # model_out2 <- summary(model, t=t , type = "cumhaz")[[1]]
    # model_out2a <- summary(model, t=t , type = "cumhaz")[[2]]
    # #add names of strata
    # model_out2$strata <- levels(data$sex)[1]
    # model_out2a$strata <- levels(data$sex)[2]
    # #bind the results
    # model_out2 <- rbind(model_out2, model_out2a)
    # 
    # model_out2$Method <- extrapolations_formatted[i]
    # cumhaz_results[[i]] <- model_out2   # Store output in list
    # 
    # #get the goodness of fit for each model
    # gof_results[[i]] <- round(glance(model)[,c(6:8)],2)
    
    #print out progress               
    print(paste0(extrapolations_formatted[i]," ", Sys.time(), " completed"))
    
  } else if(extrapolations[i] == "spline3") {
    # 3knotspline
    # model <- flexsurvspline(formula=Surv(time,status-1)~sex,data=lung,k = 3, scale = "hazard")
    # model_out <-summary(model,t=t)[[1]] # extract the data
    # model_outa <-summary(model,t=t)[[2]] # extract the data
    # 
    # #add names of strata
    # model_out$strata <- levels(data$sex)[1]
    # model_outa$strata <- levels(data$sex)[2]
    # #bind the results
    # model_out <- rbind(model_out, model_outa)
    # 
    # model_out$Method <- extrapolations_formatted[i]
    # list_extrap_results[[i]] <- model_out   # Store output in list
    # 
    # #carry out models for different parametric methods cumhaz
    # model_out2 <- summary(model, t=t , type = "cumhaz")[[1]]
    # model_out2a <- summary(model, t=t , type = "cumhaz")[[2]]
    # 
    # #add names of strata
    # model_out2$strata <- levels(data$sex)[1]
    # model_out2a$strata <- levels(data$sex)[2]
    # #bind the results
    # model_out2 <- rbind(model_out2, model_out2a)
    # 
    # 
    # model_out2$Method <- extrapolations_formatted[i]
    # cumhaz_results[[i]] <- model_out2   # Store output in list
    # 
    # #get the goodness of fit for each model
    # gof_results[[i]] <- round(glance(model)[,c(6:8)],2)
    # 
    #print out progress               
    print(paste0(extrapolations_formatted[i]," ", Sys.time(), " completed"))
    
  } else {
    #carry out models for different parametic methods survival
    model<-flexsurvreg(Surv(time, status)~sex, data=data, dist=extrapolations[i])
    model_out1 <-summary(model,t=t)[[1]] # extract the data
    model_out2 <-summary(model,t=t)[[2]] # extract the data
    #add names of strata
    model_out1$strata <- levels(data$sex)[1]
    model_out2$strata <- levels(data$sex)[2]
    #bind the results
    model_out <- rbind(model_out1, model_out2)
    #add in the method name
    model_out$Method <- extrapolations_formatted[i]
    list_extrap_results_strat[[i]] <- model_out   # Store output in list
    
    #carry out models for different parametric methods cumhaz
    model_out3 <- summary(model, t=t , type = "cumhaz")[[1]]
    model_out4 <- summary(model, t=t , type = "cumhaz")[[2]]
    #add names of strata
    model_out3$strata <- levels(data$sex)[1]
    model_out4$strata <- levels(data$sex)[2]
    model_out5 <- rbind(model_out3, model_out4)
    model_out5$Method <- extrapolations_formatted[i]
    cumhaz_results_strat[[i]] <- model_out5   # Store output in list
    
    #get the goodness of fit for each model
    gof_results_strat[[i]] <- round(glance(model)[,c(6:8)],2)
    
    #print out progress               
    print(paste0(extrapolations_formatted[i]," ", Sys.time(), " completed"))
  }
}

#get the observed data and output the results survival
kmsurvival <- survfit (Surv(time, status) ~ sex, data=data)
km_result_strat <- as.data.frame(cbind(kmsurvival$time, kmsurvival$surv, kmsurvival$lower, kmsurvival$upper))
colnames(km_result_strat) <- c("time", "est", "lcl", "ucl")
# add in gender strata terms
km_result_strat$strata <- c(rep("1", kmsurvival$strata[1]),
                            rep("2", kmsurvival$strata[2]) )
km_result_strat$Method <- "Observed"

#save the results in the output ----- so people can create their own plots for each cancer
Survival.summary_strat <- list_extrap_results_strat %>%
  map(as_tibble) %>%
  reduce(bind_rows) %>% 
  bind_rows(km_result_strat) %>% 
  collect()

#save gof outputs -------
Survival.gof_strat <- gof_results_strat %>%
  map(as_tibble) %>%
  reduce(bind_rows) %>% 
  mutate(Method = extrapolations_formatted[1:6],
         .before=logLik) %>%
  collect()

#save in output files ----------
#first check if there is a folder
if (!file.exists(output.folder))
  dir.create(output.folder, recursive = TRUE)

#write out the results extrapolation results, cumulative hazard and gof plots ALL POPULATION
write.csv(Survival.summary_strat, file=paste0(output.folder, "/Survival_extrapolations_", db.name, "_strata.csv"), row.names = FALSE)
write.csv(Survival.gof_strat, file=paste0(output.folder, "/GOF_", db.name, "_strata.csv"), row.names = FALSE)



