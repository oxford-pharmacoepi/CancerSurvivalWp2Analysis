

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
  

-------# this code creates KM plots for all cancers stratified by AGE
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


# not used
# age group stratification # lots of age stratifications
ggsurvplot(
  fit = survfit(Surv(time_years, status) ~ age_gr, data = Pop1), 
  xlab = "Years", 
  ylab = "Overall survival probability" ,
  risk.table = TRUE,
  conf.int = TRUE,
  censor = FALSE,
  #surv.median.line = "hv",
  # tables.height = 0.2,
  legend.labs = c("18-20", 
                  "20-44",
                  "45-54",
                  "55-64",
                  "65-74",
                  "75-84",
                  "85+"),
  palette = c("#fde725", 
              "#90d743",
              "#35b779",
              "#21918c",
              "#31688e",
              "#443983",
              "#440154"),
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

#############################################################################

# extrapolations

extrapolations <- c("gompertz", "weibull", "exp", "llogis", "lnorm", "gengamma", "spline1", "spline3") 
extrapolations_formatted <- c("Gompertz", "Weibull", "Exponential", "Log-logistic", "Log-normal", "Generalised Gamma", "Spline (1 Knot)", "Spline (3 knots)")
timeinyrs <- 15
t <- seq(0, timeinyrs*365.25, by=1) # calculates the extrapolation for 10 years

# WHOLE POPULATION

# Initiate lists to store output ---- will have to make folders for each cancer and loop
list_extrap_results <- list() # Create empty list for extrapolations
gof_results <- list() # required to assess goodness of fit (AIC/BIC)
cumhaz_results <- list() #required for cumhaz plots
data <- Pop
time <- "time_years"
status <- "status"

# function to carry out extrapolation produces survival data and goodness of fit
for(i in 1:length(extrapolations)) {   # Head of for-loop
  
  if(extrapolations[i] == "spline1") {
    
    # 1knotspline
    model <- flexsurvspline(formula=Surv(time,status-1)~1,data=data[cancer],k = 1, scale = "hazard")
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
    model <- flexsurvspline(formula=Surv(time,status-1)~1,data=data[cancer],k = 3, scale = "hazard")
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
    model<-flexsurvreg(Surv(time, status)~1, data=data[cancer], dist=extrapolations[i])
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
kmsurvival <- survfit (Surv(time, status) ~ 1, data=data[cancer])
km_result <- as.data.frame(cbind(kmsurvival$time, kmsurvival$surv, kmsurvival$lower, kmsurvival$upper))
colnames(km_result) <- c("time", "est", "lcl", "ucl")
km_result$Method <- "Observed"


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



