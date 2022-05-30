
# Initiate lists to store output ----
Patient.characteristcis<-list() 
Survival.summary<-list() 

# Bring in data (this will be removed when we have data) -----
data(cancer, package="survival")
lung

# set up inputs ----- at the moment one dataset but will do it so it runs multiple cancers
data <- lung
time <- "time"
status <- "status"
extrapolations <- c("gompertz", "weibull", "exp", "llogis", "lnorm", "gengamma") # will include flex ones in later
extrapolations_formatted <- c("Gompertz", "Weibull", "Exponential", "Log-logistic", "Log-normal", "Generalised Gamma")
timeinyrs <- 10
t <- seq(0, timeinyrs*10, by=1) # calculates the extrapolation for 10 years


# for each function use flexsurvreg to model and then extract the output and goodness of fit ----
list_extrap_results <- list() # Create empty list
gof_results <- list() # required to assess goodness of fit (AIC/BIC)
cumhaz_results <- list() #required for cumhaz plots

# function to carry out extrapolation produces survival data, cum hazard and goodness of fit
for(i in 1:length(extrapolations)) {   # Head of for-loop
  
  #carry out models for different parametic methods survival
  model<-flexsurvreg(Surv(time, status)~1, data=data, dist=extrapolations[i])
  model_out <-summary(model,t=t)[[1]] # extract the data
  model_out$Method <- extrapolations_formatted[i]
  list_extrap_results[[i]] <- model_out   # Store output in list
  
  #carry out models for different parametric methods cumhaz
  model_out2 <- summary(model, t=t , type = "cumhaz")[[1]]
  cumhaz_results[[i]] <- model_out2   # Store output in list
  
  #get the goodness of fit for each model
  gof_results[[i]] <- round(glance(model)[,c(6:8)],2)
  
  #print out progress               
  print(paste0(extrapolations_formatted[i]," ", Sys.time(), " completed"))

}

# carry out the for the observed data using km survival -----
kmsurvival <- survfit (Surv(time, status) ~ 1, data=data)
km_data <- as.data.frame(cbind(kmsurvival$time, kmsurvival$surv, kmsurvival$lower, kmsurvival$upper))
colnames(km_data) <- c("time", "est", "lcl", "ucl")
km_data$Method <- "Observed"

# carry out the for the observed data using km cum hazard -----


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

for(i in 1:length(extrapolations)) { 
  
  my_colors <- c("darkgrey", cols[i])
  
  # for each extrapolation method rbind with each observed and create a plot
  extrap_results <- list_extrap_results[[i]]
  
  #rbind the observed results to the extrapolated ones
  extrap_results1 <- rbind(km_data, extrap_results)
  
  extrap_results1$Method <- factor(extrap_results1$Method, levels=c('Observed', extrap_results1$Method[nrow(extrap_results1)] ))
  
  # might need to convert days to years
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
    scale_x_continuous(limits = c(0,max(combined_data$Years)), expand =c(0,0) ,
                       breaks = seq(0,max(combined_data$Years), by = 2 ) ) +
    scale_y_continuous(limits = c(0,1.02), expand =c(0.01,0)) 
  
  ggsave(plot_km1, file= here(paste0("plot_survival", extrapolations_formatted[i],".png"))
         , width = 14, height = 10, units = "cm")
  
}


