

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
extrapolations_formatted <- c("Gompertz", "Weibull", "Exponential", "Log-logistic", "Log-normal", "Generalised Gamma", "Spline (1 knot)", "Spline (3 knots)")
t <- seq(0, timeinyrs*365, by=1) # calculates the extrapolation 

#################################################
# WHOLE POPULATION
#################################################
# OBSERVED DATA ANALYSIS
# km survival, log cumulative and hazard over time from the observed data for each cancer ----

# capture output
observedkm <- list()
observedkmpred <- list()
observedmedianKM <- list()
observedhazotKM <- list()
observedrisktableKM <- list()

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

# get the risk table
RiskSetCount <- function(timeindex, survivaltime) {
  atrisk <- NULL
  for (t in timeindex)
    atrisk <- c(atrisk, sum(survivaltime >= t))
  return(atrisk)
}

grid <- seq(0,floor(max(data$time_years)),by=2)
observedrisktableKM[[j]] <- RiskSetCount(grid,data$time_years) %>%
  rbind(grid) %>% as.data.frame() %>%
  `colnames<-`(grid) %>%
  mutate(cancer = cohortDefinitionSet$cohortName[j] ) %>%
  slice(1)


print(paste0("Extract risk table ", Sys.time()," for ",cohortDefinitionSet$cohortName[j], " completed"))


# KM survival for 1,5,10 years and median survival---
# needs tidying up

modelKM <- survfit(Surv(time_years, status) ~ 1, data=data) #get model
modelKMpred <- summary(modelKM, tidy = TRUE, times = c(1,5,10), )# get summary results for 1,5,10 years
modelKM1510 <- as.data.frame(cbind(modelKMpred$n, 
                                   modelKMpred$time,
                                   modelKMpred$n.risk,
                                   modelKMpred$surv,
                                   modelKMpred$std.err,
                                   modelKMpred$lower,
                                   modelKMpred$upper
))
colnames(modelKM1510) <- c("N", "time", "n.risk", "est", "SE", "lcl", "ucl")
modelKM1510$Method <- "observed"
modelKM1510$cancer <- cohortDefinitionSet$cohortName[j]
observedkmpred[[j]] <- modelKM1510
  
  

# median survival
modelKMMed <- summary(modelKM)$table
modelKMMed <- t(modelKMMed)
modelKMMed <- as.data.frame(modelKMMed)
modelKMMed$Method <- "observed"
modelKMMed$cancer <- cohortDefinitionSet$cohortName[j]
observedmedianKM[[j]] <- modelKMMed

print(paste0("Median survival and 1,5,10 years surival from KM from observed data ", Sys.time()," for ",cohortDefinitionSet$cohortName[j], " completed"))


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

#calculate hazard over time
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time,hazard,lower.ci,upper.ci))
}
# paper https://arxiv.org/pdf/1509.03253.pdf states bshazard good package

observedhazotKM[[j]] <- as.data.frame.bshazard(bshazard(Surv(time_years, status) ~ 1, data=data, verbose=FALSE)) %>%
  mutate(Method = "Observed", cancer = cohortDefinitionSet$cohortName[j])

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

ggsave(hazardsot, file= here("Results", db.name,"Plots", plotname1)
       , width = 14, height = 10, units = "cm")

print(paste0("Plot hazard over time plot and results ", Sys.time()," for ",cohortDefinitionSet$cohortName[j], " completed"))


}

# take the results from a list (one element for each cancer) and put into dataframe for KM survival
observedkmcombined <- dplyr::bind_rows(observedkm) %>%
  rename(est = estimate ,ucl = conf.high, lcl = conf.low )

predkmcombined <- dplyr::bind_rows(observedkmpred)

medkmcombined <- dplyr::bind_rows(observedmedianKM) 

hotkmcombined <- dplyr::bind_rows(observedhazotKM) %>%
  rename(est = hazard, ucl = upper.ci, lcl = lower.ci )

#generate the risk table and remove entries < 5 patients
risktableskm <- dplyr::bind_rows(observedrisktableKM)%>%
  mutate(across(everything(), ~replace(., . <=  5 , NA))) %>%
  replace(is.na(.), "<5") %>%
  relocate(cancer)
  

ResultsKM_ALL <- list("KM_observed_all" = observedkmcombined, 
                    "prediction1510KM_all" = predkmcombined,
                    "KM_MedianSur_all" = medkmcombined,
                    "KM_hazard_rate_all" = hotkmcombined,
                    "KM_risktable_all" = risktableskm)

#write to excel
openxlsx::write.xlsx(ResultsKM_ALL, file = here("Results", db.name ,"cancer_KM_observed_results_ALL.xlsx"))


###########################################
# EXTRAPOLATION ANALYSIS

# generating extrpolations ----
# Initiate templists to store output ---- will have to make folders for each cancer and loop
extrapolations_all <- list()
cum_haz_all <- list() # not sure need this now
gof_haz_all <- list()
hazot_all <- list()

# Initiate templists to store output ---- 
extrap_results_temp <- list() # Create empty list for extrapolations
gof_results_temp <- list() # required to assess goodness of fit (AIC/BIC)
cumhaz_results_temp <- list() #required for cumhaz plots
hazot_all_temp <- list() #required

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
      
      # hazard over time
      hazot_all_temp[[i]] <- flexsurvspline(formula=Surv(time_years,status-1)~1,data=data,k = 1, scale = "hazard") %>%
        summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
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
      
      # hazard over time
      hazot_all_temp[[i]] <- flexsurvspline(formula=Surv(time_years,status-1)~1,data=data,k = 3, scale = "hazard") %>%
        summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
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
      
      #extract the hazard function over time
      hazot_all_temp[[i]] <- flexsurvreg(Surv(time_years, status)~1, data=data, dist=extrapolations[i]) %>%
        summary(t=(t + 1)/365, type = "hazard",tidy = TRUE) %>%
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
    hotcombined <- dplyr::bind_rows(hazot_all_temp) %>%
      filter(time > 0) # remove rows with inf/NAs


    #put the results from each cancer in separate list
    extrapolations_all[[j]] <- extrapolatedcombined
    cum_haz_all[[j]] <- cumhazcombined
    gof_haz_all[[j]] <- gofcombined
    hazot_all[[j]] <- hotcombined

    
  }
  #print out progress               
  print(paste0(cohortDefinitionSet$cohortName[j]," Extrapolation Analysis Completed ", Sys.time()))
  
}

# Merge results together from each cancer and extrpolation into a dataframe ---
extrapolatedfinal <- dplyr::bind_rows(extrapolations_all)
cumhazfinal <- dplyr::bind_rows(cum_haz_all)
goffinal <- dplyr::bind_rows(gof_haz_all)
hazardotfinal <- dplyr::bind_rows(hazot_all)

# extract results for 1,5,10 years for extrapolated data
extrapolation_pred <- subset(extrapolatedfinal, extrapolatedfinal$time == 1 |
                               extrapolatedfinal$time == 5 |
                               extrapolatedfinal$time == 10  )


#save files in results folder ---
Results_ALL <- list("extrapolation_all" = extrapolatedfinal, 
                    "cumulativehaz_all" = cumhazfinal,
                    "hazardrate_all" = hazardotfinal,
                    "GOF_all" = goffinal,
                    "prediction1510"= extrapolation_pred)

#write results to excel ---
openxlsx::write.xlsx(Results_ALL, file = here("Results", db.name , "cancer_extrapolation_results_ALL.xlsx"))


# extrapolatedfinal <- read.xlsx(xlsxFile = here("Results", db.name , "cancer_extrapolation_results_ALL.xlsx"), sheet = 1)
# cumhazfinal <- read.xlsx(xlsxFile = here("Results", db.name , "cancer_extrapolation_results_ALL.xlsx"), sheet = 2)
# hazardotfinal <- read.xlsx(xlsxFile = here("Results", db.name , "cancer_extrapolation_results_ALL.xlsx"), sheet = 3)
# goffinal <- read.xlsx(xlsxFile = here("Results", db.name , "cancer_extrapolation_results_ALL.xlsx"), sheet = 4)
# extrapolation_pred <- read.xlsx(xlsxFile = here("Results", db.name , "cancer_extrapolation_results_ALL.xlsx"), sheet = 5)

# plots KM observed data and extrapolated data -----
# merge extrapolation and observed results
plot_combined_all <- bind_rows(extrapolatedfinal, observedkmcombined)

# Create plots for whole population ---
for(j in 1:nrow(cohortDefinitionSet)) { 

data <- plot_combined_all %>%
  filter(cancer == cohortDefinitionSet$cohortName[j])


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

#carry out plot with all extrapolations on one plot
my_colors <- c(cols[1:length(extrapolations_formatted)], "black")

data$Method <- factor(data$Method, levels=c(extrapolations_formatted, 'Observed' ))

plot_km2 <- ggplot(data, aes(x = time, y = est, colour = Method)) + 
  xlab("Time (Years)") + ylab("Survival Probability (%)") +
  geom_line() +
  geom_line(data = filter(data, Method == "Observed"), size = 1) +
  #geom_ribbon(data = filter(data, Method == "Observed"), aes(ymin = lcl, ymax = ucl, fill = Method), linetype = 2, alpha = 0.1) +
  scale_color_manual(values = my_colors) +
  scale_fill_manual(values = my_colors) +
  theme_bw()+ 
  theme( legend.position = 'right') +
  scale_x_continuous(limits = c(0,max(data$time)), expand =c(0,0) ,
                     breaks = seq(0,max(data$time), by = 2 ) ) +
  #scale_y_continuous(limits = c(0,1.02), expand =c(0.01,0)) 
  scale_y_continuous(limits = c(min(data$lcl)- 0.1,1.02), expand =c(0.01,0)) +
  annotate("text", x = 16, y=0.8, label= 
             if(is.na(medkmcombined$median[j])){
               paste0("Median OS = Not Achieved") 
             } else {
               paste0("Median OS = ",round(medkmcombined$median[j], 2) , " years") 
             }
             )
                      

#name plot
plotname <- paste0("plot_survival_extrapolation_",cohortDefinitionSet$cohortName[j],"_ALL",".png")

ggsave(plot_km2, file= here("Results", db.name,"Plots", plotname)
       , width = 14, height = 10, units = "cm")



#carry out plot for each extrapolation
for(i in 1:length(extrapolations)) {

#extract for each extrapolation  
  data_extrap <- data %>%
    filter(Method == extrapolations_formatted[i] | Method == "Observed")
  
  my_colors <- c("darkgrey", cols[i])
  
  data_extrap$Method <- factor(data_extrap$Method, levels=c('Observed', extrapolations_formatted[i]))
  
  plot_km1 <- ggplot(data_extrap, aes(x = time, y = est, colour = Method)) + 
    xlab("Time (Years)") + ylab("Survival Probability (%)") +
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


# plot log cumulative hazard plot

for(j in 1:nrow(cohortDefinitionSet)) { 
  
  data <- plot_combined_all %>%
    filter(cancer == cohortDefinitionSet$cohortName[j])
  
  
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
  
  #carry out plot with all extrapolations on one plot
  my_colors <- c(cols[1:length(extrapolations_formatted)], "black")
  
  data$Method <- factor(data$Method, levels=c(extrapolations_formatted, 'Observed' ))
  
  logcumhazplot2 <- ggplot(filter(data, est != 1), aes(x = log(time), y = log(-log(est)), colour = Method)) + 
    xlab("Log(Time [Years])") + ylab("log(-log(y)") +
    geom_line() +
    geom_line(data = filter(data, Method == "Observed"), size = 1) +
    scale_color_manual(values = my_colors) +
    scale_fill_manual(values = my_colors) +
    theme_bw() + 
    theme( legend.position = 'right')
  
  
  #name plot
  plotname <- paste0("plot_log_cum_hazard_",cohortDefinitionSet$cohortName[j],"_ALL",".png")
  
  ggsave(logcumhazplot2, file= here("Results", db.name,"Plots", plotname)
         , width = 14, height = 10, units = "cm")
  
  
  
  #carry out plot for each extrapolation
  for(i in 1:length(extrapolations)) {
    
    #extract for each extrapolation  
    data_extrap <- data %>%
      filter(Method == extrapolations_formatted[i] | Method == "Observed")
    
    my_colors <- c("black", cols[i])
    
    data_extrap$Method <- factor(data_extrap$Method, levels=c('Observed', extrapolations_formatted[i]))
    
    logcumhazplot3 <- ggplot(filter(data_extrap, est != 1), aes(x = log(time), y = log(-log(est)), colour = Method)) + 
      xlab("Log(Time [Years])") + ylab("log(-log(y)") +
      geom_line() +
      geom_line(data = filter(data_extrap, Method == "Observed"), size = 1) +
      scale_color_manual(values = my_colors) +
      scale_fill_manual(values = my_colors) +
      theme_bw() + 
      theme( legend.position = 'right')
    
    #name plot
    plotname <- paste0("plot_log_cum_hazard_",cohortDefinitionSet$cohortName[j],"_", extrapolations_formatted[i],"_ALL",".png")
    
    ggsave(logcumhazplot3, file= here("Results", db.name,"Plots", plotname)
           , width = 14, height = 10, units = "cm")
    
    #plot created
    print(paste0("Plot ", Sys.time()," for extrapolation method ",extrapolations_formatted[i]," for ",cohortDefinitionSet$cohortName[j], " completed"))
    
  }
  
  #plot created
  print(paste0("Plots ", Sys.time()," for extrapolation method for ",cohortDefinitionSet$cohortName[j], " completed"))
  
}

# plot hazard over time for KM observed data and extrapolated data -----
# merge extrapolation and observed results

plot_hazot_combined_all <- bind_rows(hazardotfinal, hotkmcombined)

for(j in 1:nrow(cohortDefinitionSet)) { 
  
  data <- plot_hazot_combined_all %>%
    filter(cancer == cohortDefinitionSet$cohortName[j])
  
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
  
  #carry out plot with all extrapolations on one plot
  my_colors <- c(cols[1:length(extrapolations_formatted)], "black")
  
  data$Method <- factor(data$Method, levels=c(extrapolations_formatted, 'Observed' ))
  
  plot_hot_all <- ggplot(data, aes(x = time, y = est, colour = Method)) + 
    xlab("Time (Years)") + ylab("Hazard Rate") +
    geom_line() +
    geom_line(data = filter(data, Method == "Observed"), size = 1) +
    #geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Method), linetype = 2, alpha = 0.1) +
    scale_color_manual(values = my_colors) +
    scale_fill_manual(values = my_colors) +
    scale_x_continuous(breaks = seq(0, max(data$time), by = 2)) +
    # scale_x_continuous(breaks = seq(0, 14, by = 2)) +
    coord_cartesian(xlim = c(-0.25, 14.5)) +
    scale_y_continuous(limits = c(min(data$lcl),max(data$ucl)), expand =c(0.01,0)) +
    theme_bw()
 
  #name plot
  plotname <- paste0("plot_hazard_over_time_",cohortDefinitionSet$cohortName[j],"_ALL",".png")
  
  ggsave(plot_hot_all, file= here("Results", db.name,"Plots", plotname)
         , width = 14, height = 10, units = "cm")
  
  
  #carry out plot for each extrapolation and km on separate graph
  for(i in 1:length(extrapolations)) {
    
    #extract for each extrapolation  
    data_extrap <- data %>%
      filter(Method == extrapolations_formatted[i] | Method == "Observed")
    
    my_colors <- c("black", cols[i])
    
    data_extrap$Method <- factor(data_extrap$Method, levels=c('Observed', extrapolations_formatted[i]))
    
    
    plot_hot <- ggplot(data_extrap, aes(x = time, y = est, colour = Method)) + 
      xlab("Years") + ylab("Hazard Rate") +
      geom_line() +
      #geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Method), linetype = 2, alpha = 0.1) +
      scale_color_manual(values = my_colors) +
      scale_fill_manual(values = my_colors) +
      scale_x_continuous(breaks = seq(0, max(data_extrap$time), by = 2)) +
      scale_y_continuous(limits = c(min(data_extrap$lcl),max(data_extrap$ucl)), expand =c(0.01,0)) +
      theme_bw()
    
    #name plot
    plotname <- paste0("plot_hazard_over_time_",cohortDefinitionSet$cohortName[j],"_", extrapolations_formatted[i],"_ALL",".png")
    
    ggsave(plot_hot, file= here("Results", db.name,"Plots", plotname)
           , width = 14, height = 10, units = "cm")
    
    #plot created
    print(paste0("Plot hazard over time", Sys.time()," for extrapolation method ",extrapolations_formatted[i]," for ",cohortDefinitionSet$cohortName[j], " completed"))
    
  }
  
  #plot created
  print(paste0("Plots ", Sys.time()," for extrapolation method for ",cohortDefinitionSet$cohortName[j], " completed"))
  
}






# predicted survival at 1, 5 and 10 years plots ---

oneyearpred <- extrapolation_pred %>%
  filter(extrapolation_pred$time == 10 & extrapolation_pred$cancer == "BreastCancer" )

oneyearkm <- predkmcombined %>%
  filter(predkmcombined$time == 10 & predkmcombined$cancer == "BreastCancer" )

oneyearpred <- oneyearpred %>%
  mutate(difference_km = oneyearkm$est - oneyearpred$est )



########################################
# GENDER STRATIFICATION

########################################
# AGE STRATIFICATION

########################################
# GENDER*AGE STRATIFICATION

########################################

# using muhaz plotting KM observed and can plot the extrapolated plots

test_muhaz <- muhaz(times = data$time_years, delta = data$status -1)
weib <- flexsurvreg(Surv(time_years,status-1)~1,data=data,dist="weibull")
plot(test_muhaz)

plot(test_muhaz,ylim=c(0,1.2),xlim=c(0,14))
lines(weib, type="hazard",col="red")


# trying it with bahazards and extracting hazards from extrapolations
testytest <- flexsurvreg(Surv(time_years, status)~1, data=data, dist= "weibull")
testytest1 <- flexsurvreg(Surv(time_years, status)~1, data=data, dist= "exp")
testytest2 <- flexsurvreg(Surv(time_years, status)~1, data=data, dist= "gamma")
testytest3 <- flexsurvreg(Surv(time_years, status)~1, data=data, dist= "llogis")

# plot(testytest)
# plot(testytest, type = "hazard")

asdf <- summary(testytest,type="hazard") 
asdf <- as.data.frame(asdf) # hazard
asdf$Method <- "Weiball"

asdf1 <- summary(testytest1,type="hazard") 
asdf1 <- as.data.frame(asdf1) # hazard
asdf1$Method <- "exponential"

asdf2 <- summary(testytest2,type="hazard") 
asdf2 <- as.data.frame(asdf2) # hazard
asdf2$Method <- "Gamma"

asdf3 <- summary(testytest3,type="hazard") 
asdf3 <- as.data.frame(asdf3) # hazard
asdf3$Method <- "Log-Logistic"


max_data <- max(data$time_years) # need this for axis scales
hazardsot <- bshazard(Surv(time_years, status) ~ 1, data=data, verbose=FALSE)
hazardsot1 <- cbind(hazardsot$time,
                    hazardsot$hazard,
                    hazardsot$lower.ci,
                    hazardsot$upper.ci
                    )
hazardsot1 <- as.data.frame(hazardsot1)
hazardsot1$Method <- "Observed"
colnames(hazardsot1) <- c("time" ,  "est"  ,  "lcl"  ,  "ucl"  ,  "Method")

# plot(hazardsot)
# lines(testytest)

#bind observed and extrapolated
comb_hot <- rbind(hazardsot1, asdf, asdf1, asdf2, asdf3 )

#plot

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

my_colors <- c("darkgrey", cols[i], cols[i-1], cols[i-2], cols[i-3] )

plot_hot <- ggplot(comb_hot, aes(x = time, y = est, colour = Method)) + 
  xlab("Years") + ylab("Hazard Rate") +
  geom_line() +
  #geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Method), linetype = 2, alpha = 0.1) +
  scale_color_manual(values = my_colors) +
  scale_fill_manual(values = my_colors) +
  scale_x_continuous(breaks = seq(0, max_data, by = 2)) +
  scale_y_continuous(limits = c(min(comb_hot$lcl)- 0.1,1.25), expand =c(0.01,0)) +
  theme_bw()

plot_hot



################END ####################




       
  





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



