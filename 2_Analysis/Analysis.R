# Load packages ------ these will be in another script but for now are here (codetorun.r)
# load r packages
rm(list = ls()) # clear environment (needs removing)
library(SqlRender)
library(DatabaseConnector)
library(FeatureExtraction)
library(here)
library(lubridate)
library(stringr)
library(ggplot2)
library(DBI)
library(dbplyr)
library(dplyr)
library(tidyr)
library(kableExtra)
library(RSQLite)
library(rmarkdown)
library(tableone)
library(scales)
library(forcats)
library(epiR)
library(RPostgres)
library(cmprsk)
library(mstate)
library(broom)
library(rms)
library(glue)
library(readr)
library(log4r)
library(survival)
library(flexsurv)
library(tictoc)
library(purrr)

# Initiate lists to store output ---- will have to make folders for each cancer and loop
list_extrap_results <- list() # Create empty list for extrapolations
gof_results <- list() # required to assess goodness of fit (AIC/BIC)
cumhaz_results <- list() #required for cumhaz plots

# Bring in data (this will be removed when we have data) -----
data(cancer, package="survival")
lung

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


# cumulative hazard
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

# stratification
# Initiate lists to store output ---- 
list_extrap_results_strat <- list() # Create empty list for extrapolations
gof_results_strat <- list() # required to assess goodness of fit (AIC/BIC)

# need to check the variable for stratification are factors if not then turn them into factor
data <- data %>%
  mutate_at(vars(sex), 
            list(factor))


# structure of list [[strata]] >> [[results]]: strata is gender, age, gender*age
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
    list_extrap_results[[i]] <- model_out   # Store output in list
    
    #carry out models for different parametric methods cumhaz
    model_out3 <- summary(model, t=t , type = "cumhaz")[[1]]
    model_out4 <- summary(model, t=t , type = "cumhaz")[[2]]
    #add names of strata
    model_out3$strata <- levels(data$sex)[1]
    model_out4$strata <- levels(data$sex)[2]
    model_out5 <- rbind(model_out3, model_out4)
    model_out5$Method <- extrapolations_formatted[i]
    cumhaz_results[[i]] <- model_out5   # Store output in list
    
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


