

# TESTS TO CHECK DATA INPUT IS CORRECT tbc

#1 Gender in format "male, "female" and factor
#2 age is numeric
#3 age group in 3 groups (or whatever has been decided) and factor
#4 another input file detailing the cancer cohorts csv (instantiateCohorts CohortsToCreate) or if running instantiate check cohortDefinitionSet is present
#5 check prostate only contains males

# Creating survival plots for QC checking of the data

# whole population
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

# gender stratification
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
# Setting up information for extrapolation methods to be used
# extrapolations
extrapolations <- c("gompertz", "weibull", "exp", "llogis", "lnorm", "gengamma", "spline1", "spline3") 
extrapolations_formatted <- c("Gompertz", "Weibull", "Exponential", "Log-logistic", "Log-normal", "Generalised Gamma", "Spline (1 knot)", "Spline (3 knots)")
# setting up time for extrapolation
t <- seq(0, timeinyrs*365, by=1)

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
  mutate(Method = "Observed", Cancer = cohortDefinitionSet$cohortName[j], Age = "All", Gender = "Both") %>%
  filter(n.risk >= 5) #remove entries with less than 5 patients

print(paste0("KM for observed data ", Sys.time()," for ",cohortDefinitionSet$cohortName[j], " completed"))

#get the risk table

grid <- seq(0,floor(max(data$time_years)),by=2)
observedrisktableKM[[j]] <- RiskSetCount(grid,data$time_years) %>%
  rbind(grid) %>% as.data.frame() %>%
  `colnames<-`(grid) %>%
  mutate(Method = "Observed", Cancer = cohortDefinitionSet$cohortName[j], Age = "All", Gender = "Both" ) %>%
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
modelKM1510$Cancer <- cohortDefinitionSet$cohortName[j]
modelKM1510$Age <- "All"
modelKM1510$Gender <- "Both"
observedkmpred[[j]] <- modelKM1510
  
  

# median survival
modelKMMed <- summary(modelKM)$table
modelKMMed <- t(modelKMMed)
modelKMMed <- as.data.frame(modelKMMed)
modelKMMed$Method <- "observed"
modelKMMed$Cancer <- cohortDefinitionSet$cohortName[j]
modelKMMed$Age <- "All"
modelKMMed$Gender <- "Both"
observedmedianKM[[j]] <- modelKMMed

print(paste0("Median survival and 1,5,10 years surival from KM from observed data ", Sys.time()," for ",cohortDefinitionSet$cohortName[j], " completed"))

# hazard function over time ----
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time,hazard,lower.ci,upper.ci))
}
# paper https://arxiv.org/pdf/1509.03253.pdf states bshazard good package

observedhazotKM[[j]] <- as.data.frame.bshazard(bshazard(Surv(time_years, status) ~ 1, data=data, verbose=FALSE)) %>%
  mutate(Method = "Observed", Cancer = cohortDefinitionSet$cohortName[j], Age = "All", Gender = "Both")

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
  relocate(Cancer)
  

ResultsKM_ALL <- list("KM_observed_all" = observedkmcombined, 
                    "prediction1510KM_all" = predkmcombined,
                    "KM_MedianSur_all" = medkmcombined,
                    "KM_hazard_rate_all" = hotkmcombined,
                    "KM_risktable_all" = risktableskm)

#write to excel
openxlsx::write.xlsx(ResultsKM_ALL, file = here("Results", db.name ,"cancer_KM_observed_results_ALL.xlsx"))


###########################################
# EXTRAPOLATION ANALYSIS ALL POPULATION

# generating extrapolations ----
# Initiate templists to store output ---- will have to make folders for each cancer and loop
extrapolations_all <- list()
gof_haz_all <- list()
hazot_all <- list()

# Initiate templists to store output ---- 
extrap_results_temp <- list() # Create empty list for extrapolations
gof_results_temp <- list() # required to assess goodness of fit (AIC/BIC)
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
        mutate(Method = extrapolations_formatted[i], Cancer = cohortDefinitionSet$cohortName[j], Age = "All", Gender = "Both" )
      
      # hazard over time
      hazot_all_temp[[i]] <- flexsurvspline(formula=Surv(time_years,status-1)~1,data=data,k = 1, scale = "hazard") %>%
        summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = cohortDefinitionSet$cohortName[j], Age = "All", Gender = "Both" )
      
      #get the goodness of fit for each model
      gof_results_temp[[i]] <- flexsurvspline(formula=Surv(time_years,status-1)~1,data=data,k = 1, scale = "hazard") %>%
        glance() %>%
        mutate(Method = extrapolations_formatted[i], Cancer = cohortDefinitionSet$cohortName[j], Age = "All", Gender = "Both" )
      
      
      #print out progress               
      print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cohortDefinitionSet$cohortName[j], " completed"))
      
    } else if(extrapolations[i] == "spline3") {
      # 3knotspline
      extrap_results_temp[[i]] <- flexsurvspline(formula=Surv(time_years,status-1)~1,data=data,k = 3, scale = "hazard") %>%
      summary(t=t/365, tidy = TRUE) %>%
      mutate(Method = extrapolations_formatted[i], Cancer = cohortDefinitionSet$cohortName[j], Age = "All", Gender = "Both" )
      
      # hazard over time
      hazot_all_temp[[i]] <- flexsurvspline(formula=Surv(time_years,status-1)~1,data=data,k = 3, scale = "hazard") %>%
        summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = cohortDefinitionSet$cohortName[j], Age = "All", Gender = "Both" )
      
      #get the goodness of fit for each model
      gof_results_temp[[i]] <- flexsurvspline(formula=Surv(time_years,status-1)~1,data=data,k = 3, scale = "hazard") %>%
        glance() %>%
        mutate(Method = extrapolations_formatted[i], Cancer = cohortDefinitionSet$cohortName[j], Age = "All", Gender = "Both" )
      
      #print out progress               
      print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cohortDefinitionSet$cohortName[j], " completed"))
      
      
    } else {
      #carry out models for different parametic methods survival
      extrap_results_temp[[i]] <- flexsurvreg(Surv(time_years, status)~1, data=data, dist=extrapolations[i]) %>%
        summary(t=t/365, tidy = TRUE) %>%
      mutate(Method = extrapolations_formatted[i], Cancer = cohortDefinitionSet$cohortName[j], Age = "All", Gender = "Both" )
      
      #extract the hazard function over time
      hazot_all_temp[[i]] <- flexsurvreg(Surv(time_years, status)~1, data=data, dist=extrapolations[i]) %>%
        summary(t=(t + 1)/365, type = "hazard",tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = cohortDefinitionSet$cohortName[j], Age = "All", Gender = "Both" ) 
      
      #get the goodness of fit for each model
      gof_results_temp[[i]] <- flexsurvreg(Surv(time_years, status)~1, data=data, dist=extrapolations[i]) %>%
      glance() %>%
        mutate(Method = extrapolations_formatted[i], Cancer = cohortDefinitionSet$cohortName[j], Age = "All", Gender = "Both" )
      

      #print out progress               
      print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cohortDefinitionSet$cohortName[j], " completed"))
      
    }
    
    #combine all results
    extrapolatedcombined <- dplyr::bind_rows(extrap_results_temp)
    gofcombined <- dplyr::bind_rows(gof_results_temp)
    hotcombined <- dplyr::bind_rows(hazot_all_temp) %>%
      filter(time > 0) # remove rows with inf/NAs


    #put the results from each cancer in separate list
    extrapolations_all[[j]] <- extrapolatedcombined
    gof_haz_all[[j]] <- gofcombined
    hazot_all[[j]] <- hotcombined

    
  }
  #print out progress               
  print(paste0(cohortDefinitionSet$cohortName[j]," Extrapolation Analysis Completed ", Sys.time()))
  
}

# Merge results together from each cancer and extrpolation into a dataframe ---
extrapolatedfinal <- dplyr::bind_rows(extrapolations_all)
goffinal <- dplyr::bind_rows(gof_haz_all)
hazardotfinal <- dplyr::bind_rows(hazot_all)

# extract results for 1,5,10 years for extrapolated data
extrapolation_pred <- subset(extrapolatedfinal, extrapolatedfinal$time == 1 |
                               extrapolatedfinal$time == 5 |
                               extrapolatedfinal$time == 10  )


#save files in results folder ---
Results_ALL <- list("extrapolation_all" = extrapolatedfinal, 
                    "hazardrate_all" = hazardotfinal,
                    "GOF_all" = goffinal,
                    "prediction1510"= extrapolation_pred)

#write results to excel ---
openxlsx::write.xlsx(Results_ALL, file = here("Results", db.name , "cancer_extrapolation_results_ALL.xlsx"))

# extrapolatedfinal <- read.xlsx(xlsxFile = here("Results", db.name , "cancer_extrapolation_results_ALL.xlsx"), sheet = 1)
# hazardotfinal <- read.xlsx(xlsxFile = here("Results", db.name , "cancer_extrapolation_results_ALL.xlsx"), sheet = 2)
# goffinal <- read.xlsx(xlsxFile = here("Results", db.name , "cancer_extrapolation_results_ALL.xlsx"), sheet = 3)
# extrapolation_pred <- read.xlsx(xlsxFile = here("Results", db.name , "cancer_extrapolation_results_ALL.xlsx"), sheet = 4)


#######################################
# Create plots for whole population ---
#######################################

# Survival plots
# merge extrapolation and observed results
plot_combined_all <- bind_rows(extrapolatedfinal, observedkmcombined)

for(j in 1:nrow(cohortDefinitionSet)) { 

data <- plot_combined_all %>%
  filter(Cancer == cohortDefinitionSet$cohortName[j])


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

# plot hazard over time for KM observed data and extrapolated data -----
# merge extrapolation and observed results

plot_hazot_combined_all <- bind_rows(hazardotfinal, hotkmcombined)

for(j in 1:nrow(cohortDefinitionSet)) { 
  
  data <- plot_hazot_combined_all %>%
    filter(Cancer == cohortDefinitionSet$cohortName[j])
  
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


########################################
# GENDER STRATIFICATION

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
  
  if(j != PC_id){
  
  #carry out km estimate
  observedkm_gender[[j]] <- survfit (Surv(time_years, status) ~ gender, data=data) %>%
    tidy() %>%
    rename(Gender = strata) %>%
    mutate(Method = "Observed", Cancer = cohortDefinitionSet$cohortName[j], Age = "All", Gender = str_replace(Gender, "gender=Male", "Male"), Gender = str_replace(Gender,"gender=Female", "Female"))
  
  print(paste0("KM for observed data ", Sys.time()," for ",cohortDefinitionSet$cohortName[j], " completed"))

  # # get the risk table

  grid <- seq(0,floor(max(data$time_years)),by=2)
  observedrisktableKM_gender[[j]] <- RiskSetCount(grid,data$time_years[data$gender == "Male"])%>%
    rbind(grid) %>% as.data.frame() %>%
    `colnames<-`(grid) %>%
    mutate(Method = "Observed", Cancer = cohortDefinitionSet$cohortName[j], Age = "All") %>%
    slice(1) %>%
    rbind(RiskSetCount(grid,data$time_years[data$gender == "Female"]))%>%
    mutate(Method = "Observed", Cancer = cohortDefinitionSet$cohortName[j], Age = "All", Gender = c("Male", "Female"))

 print(paste0("Extract risk table ", Sys.time()," for ",cohortDefinitionSet$cohortName[j], " completed"))
 

  # KM median survival---

modelKM <- survfit(Surv(time_years, status) ~ gender, data=data) #get model

  # median survival
  modelKMMed <- summary(modelKM)$table
  modelKMMed <- as.data.frame(modelKMMed)
  modelKMMed$Method <- "observed"
  modelKMMed$Cancer <- cohortDefinitionSet$cohortName[j]
  modelKMMed$Age <- "All"
  modelKMMed$Gender <- c("Male", "Female")
  observedmedianKM_gender[[j]] <- modelKMMed

print(paste0("Median survival from KM from observed data ", Sys.time()," for ",cohortDefinitionSet$cohortName[j], " completed"))

  # hazard function over time ----

  as.data.frame.bshazard <- function(x, ...) {
    with(x, data.frame(time,hazard,lower.ci,upper.ci))
  }

observedhazotKM_gender[[j]] <- group_by(data,gender) %>% 
  do(as.data.frame(bshazard(Surv(time_years, status)~1, data=., verbose=FALSE))) %>% 
  ungroup %>%
  mutate(Method = "Observed", Cancer = cohortDefinitionSet$cohortName[j], Age = "All")

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


###########################################
# EXTRAPOLATION ANALYSIS GENDER EXTRAPOLATION
# not carried out for prostate cancer as male only

# generating extrapolations ----
# Initiate templists to store output ---- will have to make folders for each cancer and loop
extrapolations_gender <- list()
gof_haz_gender <- list()
hazot_gender <- list()

# Initiate templists to store output ---- 
extrap_results_temp <- list() # Create empty list for extrapolations
gof_results_temp <- list() # required to assess goodness of fit (AIC/BIC)
hazot_all_temp <- list() #required

# Run extrapolations for all cancers for gender extrapolation ---
for(j in 1:nrow(cohortDefinitionSet)) { 
  
  #subset the data by cancer type
  
  data <- Pop %>%
    filter(cohort_definition_id == j)
  
  if(j != PC_id){
  
  #carry out extrapolation for each cancer
  for(i in 1:length(extrapolations)) {   # Head of for-loop
    
    if(extrapolations[i] == "spline1") {
      
      # 1knotspline
      extrap_results_temp[[i]] <- flexsurvspline(formula=Surv(time_years,status-1)~gender,data=data,k = 1, scale = "hazard") %>%
        summary(t=t/365, tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = cohortDefinitionSet$cohortName[j], Age = "All" ) %>%
        rename(Gender = gender)
      
      # hazard over time
      hazot_all_temp[[i]] <-  flexsurvspline(formula=Surv(time_years,status-1)~gender,data=data,k = 1, scale = "hazard") %>%
        summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = cohortDefinitionSet$cohortName[j], Age = "All" ) %>%
        rename(Gender = gender)
      
      
      #get the goodness of fit for each model
      gof_results_temp[[i]] <- flexsurvspline(formula=Surv(time_years,status-1)~gender,data=data,k = 1, scale = "hazard") %>%
        glance() %>%
        mutate(Method = extrapolations_formatted[i], Cancer = cohortDefinitionSet$cohortName[j], Age = "All", Gender = "Gender" )

      
      #print out progress               
      print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cohortDefinitionSet$cohortName[j], " completed"))
      
    } else if(extrapolations[i] == "spline3") {
      # 3knotspline
      extrap_results_temp[[i]] <- flexsurvspline(formula=Surv(time_years,status-1)~gender,data=data,k = 3, scale = "hazard") %>%
        summary(t=t/365, tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = cohortDefinitionSet$cohortName[j], Age = "All" ) %>%
        rename(Gender = gender)
      
      # hazard over time
      hazot_all_temp[[i]] <- flexsurvspline(formula=Surv(time_years,status-1)~gender,data=data,k = 3, scale = "hazard") %>%
        summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = cohortDefinitionSet$cohortName[j], Age = "All" ) %>%
        rename(Gender = gender)
      
      #get the goodness of fit for each model
      gof_results_temp[[i]] <- flexsurvspline(formula=Surv(time_years,status-1)~gender,data=data,k = 3, scale = "hazard") %>%
        glance() %>%
        mutate(Method = extrapolations_formatted[i], Cancer = cohortDefinitionSet$cohortName[j], Age = "All", Gender = "Gender" )
      
      #print out progress               
      print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cohortDefinitionSet$cohortName[j], " completed"))
      
      
    } else {
      #carry out models for different parametic methods survival
      extrap_results_temp[[i]] <- flexsurvreg(Surv(time_years, status)~gender, data=data, dist=extrapolations[i]) %>%
        summary(t=t/365, tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = cohortDefinitionSet$cohortName[j], Age = "All") %>%
        rename(Gender = gender)
      
      #extract the hazard function over time
      hazot_all_temp[[i]] <- flexsurvreg(Surv(time_years, status)~gender, data=data, dist=extrapolations[i]) %>%
        summary(t=(t + 1)/365, type = "hazard",tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = cohortDefinitionSet$cohortName[j], Age = "All") %>%
        rename(Gender = gender)
    
      
      #get the goodness of fit for each model
      gof_results_temp[[i]] <- flexsurvreg(Surv(time_years, status)~gender, data=data, dist=extrapolations[i]) %>%
        glance() %>%
        mutate(Method = extrapolations_formatted[i], Cancer = cohortDefinitionSet$cohortName[j], Age = "All", Gender = "Gender") 

      #print out progress               
      print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cohortDefinitionSet$cohortName[j], " completed"))
      
    }
    
    #combine all results
    extrapolatedcombined <- dplyr::bind_rows(extrap_results_temp)
    gofcombined <- dplyr::bind_rows(gof_results_temp)
    hotcombined <- dplyr::bind_rows(hazot_all_temp) %>%
      filter(time > 0) # remove rows with inf/NAs
    
    
    #put the results from each cancer in separate list
    extrapolations_gender[[j]] <- extrapolatedcombined
    gof_haz_gender[[j]] <- gofcombined
    hazot_gender[[j]] <- hotcombined
    
    
  }
  #print out progress               
  print(paste0(cohortDefinitionSet$cohortName[j]," Extrapolation Analysis Completed ", Sys.time()))
  
  }
  
}

# Merge results together from each cancer and extrpolation into a dataframe ---
extrapolatedfinalGender <- dplyr::bind_rows(extrapolations_gender)
goffinalGender <- dplyr::bind_rows(gof_haz_gender)
hazardotfinalGender <- dplyr::bind_rows(hazot_gender)

# extract results for 1,5,10 years for extrapolated data
extrapolation_predGender <- subset(extrapolatedfinalGender, extrapolatedfinalGender$time == 1 |
                               extrapolatedfinalGender$time == 5 |
                               extrapolatedfinalGender$time == 10  )


#save files in results folder ---
Results_GENDER <- list("extrapolation_gender" = extrapolatedfinalGender, 
                    "hazardrate_gender" = hazardotfinalGender,
                    "GOF_gender" = goffinalGender,
                    "prediction1510_gender"= extrapolation_predGender)

#write results to excel ---
openxlsx::write.xlsx(Results_GENDER, file = here("Results", db.name , "cancer_extrapolation_results_GENDER.xlsx"))

# extrapolatedfinalGender <- read.xlsx(xlsxFile = here("Results", db.name , "cancer_extrapolation_results_GENDER.xlsx"), sheet = 1)
# hazardotfinalGender <- read.xlsx(xlsxFile = here("Results", db.name , "cancer_extrapolation_results_GENDER.xlsx"), sheet = 2)
# goffinalGender <- read.xlsx(xlsxFile = here("Results", db.name , "cancer_extrapolation_results_GENDER.xlsx"), sheet = 3)
# extrapolation_predGender <- read.xlsx(xlsxFile = here("Results", db.name , "cancer_extrapolation_results_GENDER.xlsx"), sheet = 4)



#######################################
# Create plots for stratification population GENDER ---
#######################################

#merge KM and extrapolated data

extrap_combinedGender <- bind_rows(observedkmcombined_gender, extrapolatedfinalGender)

#run over each cancer apart from prostate (males only)

for(j in 1:nrow(cohortDefinitionSet)) { 
  
  
  #set up the median survival for plot
  medsurv4plotGender <- medkmcombined_gender %>%
    mutate(median = paste0("Median OS = ",round(median, 2))) %>%
    mutate(median = str_replace(median, "Median OS = NA", "Median OS = Not achieved")) %>%
    select( Gender, median, Cancer)
  
data <- extrap_combinedGender %>%
  filter(Cancer == cohortDefinitionSet$cohortName[j])

if(j != PC_id){


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

plot_extrap_gender <- ggplot(data, aes(x = time, y = est, colour = Method)) + 
  xlab("Time (Years)") + ylab("Survival Probability (%)") +
  geom_line() +
geom_line(data = filter(data, Method == "Observed"), size = 1) +
scale_color_manual(values = my_colors) +
  scale_fill_manual(values = my_colors) +
  theme_bw() +
  theme( legend.position = 'right') +
  scale_x_continuous(limits = c(0,max(data$time)), expand =c(0,0) ,
                     breaks = seq(0,max(data$time), by = 2 ) ) + 
#facet_wrap( ~ Gender,scales = "free") +
  facet_wrap( ~ Gender ) + 
  geom_text(data = medsurv4plotGender[medsurv4plotGender$Cancer == cohortDefinitionSet$cohortName[j] ,], aes(label=median), 
             x = Inf , y = Inf,
             inherit.aes = FALSE,
            size = 3,
            hjust = 1.1,
            vjust = 1.9
            )

plotname1 <- paste0("plot_extrapolations_ ",cohortDefinitionSet$cohortName[j],"_GENDER_STRAT",".png")

ggsave(plot_extrap_gender, file= here("Results", db.name,"Plots", plotname1)
       , width = 18, height = 10, units = "cm")

print(paste0("Plot KM and extrapolation plot ", Sys.time()," for ",cohortDefinitionSet$cohortName[j], "gender strat completed"))



}

}

# hazard over time by gender

hazot_combinedGender <- bind_rows(hotkmcombined_gender, hazardotfinalGender)

for(j in 1:nrow(cohortDefinitionSet)) { 
  
  data <- hazot_combinedGender %>%
    filter(Cancer == cohortDefinitionSet$cohortName[j])
  
  if(j != PC_id){
    
    
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
    
    plot_hot_gender <- ggplot(data, aes(x = time, y = est, colour = Method)) + 
      xlab('Follow-up Time') + ylab('Hazard Rate') +
      geom_line() +
      geom_line(data = filter(data, Method == "Observed"), size = 1) +
      scale_color_manual(values = my_colors) +
      scale_fill_manual(values = my_colors) +
      theme_bw() +
      theme( legend.position = 'right') +
      scale_x_continuous(breaks = seq(0,max(data$time), by = 2 ) ) +
      coord_cartesian(xlim = c(-0.25, 14.5)) +
      facet_wrap( ~ Gender, scales = "free")
    
    plotname1 <- paste0("plot_extrapolations_HOT_ ",cohortDefinitionSet$cohortName[j],"_GENDER_STRAT",".png")
    
    ggsave(plot_hot_gender, file= here("Results", db.name,"Plots", plotname1)
           , width = 18, height = 10, units = "cm")
    
    print(paste0("Plot hazard over time plot ", Sys.time()," for ",cohortDefinitionSet$cohortName[j], "gender strat completed"))
    
    
    
  }
  
}


########################################
# AGE STRATIFICATION 

# KM observed
observedkm_age <- list()
observedmedianKM_age <- list()
observedhazotKM_age <- list()
observedrisktableKM_age <- list()

# loop to carry out for each cancer
for(j in 1:nrow(cohortDefinitionSet)) { 
  
  #subset the data by cancer type
  data <- Pop %>%
    filter(cohort_definition_id == j) 
    
    #carry out km estimate
    observedkm_age[[j]] <- survfit (Surv(time_years, status) ~ age_gr, data=data) %>%
      tidy() %>%
      rename(Age = strata) %>%
      mutate(Method = "Observed", Cancer = cohortDefinitionSet$cohortName[j], 
             Age = str_replace(Age, "age_gr=<30", "<30"),
             Age = str_replace(Age, "age_gr=30-39", "30-39"),
             Age = str_replace(Age, "age_gr=40-49", "40-49"),
             Age = str_replace(Age, "age_gr=50-59", "50-59"),
             Age = str_replace(Age, "age_gr=60-69", "60-69"),
             Age = str_replace(Age, "age_gr=70-79", "70-79"),
             Age = str_replace(Age, "age_gr=80-89", "80-89"),
             Age = str_replace(Age, "age_gr=>=90", ">=90"),
             Gender = "Both")
    
    print(paste0("KM for observed data age strat ", Sys.time()," for ",cohortDefinitionSet$cohortName[j], " completed"))
    
    # # get the risk table
    
    grid <- seq(0,floor(max(data$time_years)),by=2)
    
    observedrisktableKM_age[[j]] <- RiskSetCount(grid,data$time_years[data$age_gr == "<30"]) %>%
      rbind(grid) %>% as.data.frame() %>%
      `colnames<-`(grid) %>%
      mutate(Method = "Observed", Cancer = cohortDefinitionSet$cohortName[j], Gender = "Both") %>%
      slice(1) %>%
      rbind(RiskSetCount(grid,data$time_years[data$age_gr == "30-39"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$age_gr == "40-49"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$age_gr == "50-59"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$age_gr == "60-69"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$age_gr == "70-79"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$age_gr == "80-89"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$age_gr == ">=90"]))%>%
      mutate(Method = "Observed", Cancer = cohortDefinitionSet$cohortName[j], Gender = "Both", Age = c("<30" ,"30-39", "40-49" ,"50-59" ,"60-69", "70-79", "80-89" ,">=90"))
    
    print(paste0("Extract risk table ", Sys.time()," for ",cohortDefinitionSet$cohortName[j], " completed"))
    
    
    # KM median survival---
    
    modelKM <- survfit(Surv(time_years, status) ~ age_gr, data=data) #get model
    
    # median survival
    modelKMMed <- summary(modelKM)$table
    modelKMMed <- as.data.frame(modelKMMed)
    modelKMMed$Method <- "observed"
    modelKMMed$Cancer <- cohortDefinitionSet$cohortName[j]
    modelKMMed$Age <- c("<30" ,"30-39", "40-49" ,"50-59" ,"60-69", "70-79", "80-89" ,">=90")
    modelKMMed$Gender <- "Both"
    observedmedianKM_age[[j]] <- modelKMMed
    
    print(paste0("Median survival from KM from observed data ", Sys.time()," for ",cohortDefinitionSet$cohortName[j], " completed"))
    
    # hazard function over time ----
    
    as.data.frame.bshazard <- function(x, ...) {
      with(x, data.frame(time,hazard,lower.ci,upper.ci))
    }
    
    # this falls over when there are small numbers of patients
    # do a function based on the risk table that creates a flag that removes 
    # the stratification from the hazard over time function
    
    # create a copy of the hazard table
    hazrisktab <- observedrisktableKM_age[[j]]
    # calculate the number of columns 
    elgcols <- ncol(hazrisktab) - 4 
    #count the number of zeros across the rows
    hazrisktab <- hazrisktab %>% 
    mutate(count=rowSums(.[1:elgcols]==0), percentzero = 100 -((count/elgcols)*100) ) %>%
    filter(percentzero <= 25)

  #create filter function to put into results below
  target <- hazrisktab$Age
  
  #only include age strats that have more than 25% values in risk table
    observedhazotKM_age[[j]] <- group_by(data,age_gr) %>% 
      filter(!(age_gr %in% target)) %>%
      do(as.data.frame(bshazard(Surv(time_years, status)~1, data=., verbose=FALSE))) %>% 
      ungroup %>%
      mutate(Method = "Observed", Cancer = cohortDefinitionSet$cohortName[j], Gender = "Both")
    
    # max_data <- max(data$time_years) # need this for axis scales
    # hazardsot <- observedhazotKM_age[[j]] %>%
    #   ggplot(observedhazotKM_age[[j]], mapping = aes(x = time, y = hazard,group=age_gr)) +
    #   geom_line(aes(col=age_gr)) +
    #   xlab('Follow-up Time') + ylab('Hazard Rate') +
    #   scale_x_continuous(breaks = seq(0, max_data, by = 2)) +
    #   theme_bw()
    # 
    # plotname1 <- paste0("plot_hazard_over_time ",cohortDefinitionSet$cohortName[j],"_AGE_STRAT",".png")
    # 
    # ggsave(hazardsot, file= here("Results", db.name,"Plots", plotname1)
    #        , width = 14, height = 10, units = "cm")
    # 
    print(paste0("Hazard over time plot results ", Sys.time()," for ",cohortDefinitionSet$cohortName[j], "age strat completed"))
    
    
  }


# take the results from a list (one element for each cancer) and put into dataframe for KM survival
observedkmcombined_age <- dplyr::bind_rows(observedkm_age) %>%
  rename(est = estimate ,ucl = conf.high, lcl = conf.low )

medkmcombined_age <- dplyr::bind_rows(observedmedianKM_age) 

hotkmcombined_age <- dplyr::bind_rows(observedhazotKM_age) %>%
  rename(est = hazard, ucl = upper.ci, lcl = lower.ci, Age = age_gr )

#generate the risk table and remove entries < 5 patients
risktableskm_age <- dplyr::bind_rows(observedrisktableKM_age) %>%
  mutate_at(.vars = c(1:(ncol(risktableskm_age)-4)), funs(ifelse(.== 0, NA, .))) %>%  
  mutate_at(.vars = c(1:(ncol(risktableskm_age)-4)), funs(ifelse(.<= 5, "<5", .))) %>%
  replace(is.na(.), 0) %>%
  relocate(Cancer)


ResultsKM_AGE <- list("KM_observed_age" = observedkmcombined_age, 
                         "KM_MedianSur_age" = medkmcombined_age,
                         "KM_hazard_rate_age" = hotkmcombined_age,
                         "KM_risktable_age" = risktableskm_age)

#write to excel
openxlsx::write.xlsx(ResultsKM_AGE, file = here("Results", db.name ,"cancer_KM_observed_results_AGE.xlsx"))

# observedkmcombined_age <- read.xlsx(xlsxFile = here("Results", db.name , "cancer_KM_observed_results_AGE.xlsx"), sheet = 1)
# medkmcombined_age <- read.xlsx(xlsxFile = here("Results", db.name , "cancer_KM_observed_results_AGE.xlsx"), sheet = 2)
# hotkmcombined_age <- read.xlsx(xlsxFile = here("Results", db.name , "cancer_KM_observed_results_AGE.xlsx"), sheet = 3)
#risktableskm_age <- read.xlsx(xlsxFile = here("Results", db.name , "cancer_KM_observed_results_AGE.xlsx"), sheet = 4)



###########################################
# EXTRAPOLATION ANALYSIS AGE EXTRAPOLATION

# generating extrapolations ----
# Initiate templists to store output ---- will have to make folders for each cancer and loop
extrapolations_age <- list()
gof_haz_age <- list()
hazot_age <- list()

# Initiate templists to store output ---- 
extrap_results_temp <- list() # Create empty list for extrapolations
gof_results_temp <- list() # required to assess goodness of fit (AIC/BIC)
hazot_all_temp <- list() #required

# Run extrapolations for all cancers for age extrapolation ---
for(j in 1:nrow(cohortDefinitionSet)) { 
  
  data <- Pop %>%
    filter(cohort_definition_id == j) 
  
  
  #carry out extrapolation for each cancer
  for(i in 1:length(extrapolations)) {   # Head of for-loop
    
    if(extrapolations[i] == "spline1") {
      
      #for spline models if not enough data will not run therefore to catch it
      #code below removes levels in a factor that have 75% or more no data
      
      #remove levels that have more than 75% no data (analysis unable to run)
      target <- observedrisktableKM_age[[j]] %>%
        mutate(count=rowSums(.[1:elgcols]==0), percentzero = 100 -((count/elgcols)*100) ) %>%
        filter(percentzero <= 25) %>% pull(Age)
      
      #subset the data by cancer type and removing level of data from previous step
      data1 <- Pop %>%
        filter(cohort_definition_id == j) %>%
        filter(!(age_gr %in% target))
      
      # 1knotspline
      extrap_results_temp[[i]] <- flexsurvspline(formula=Surv(time_years,status-1)~age_gr,data=data1,k = 1, scale = "hazard") %>%
        summary(t=t/365, tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = cohortDefinitionSet$cohortName[j], Gender = "Both" ) %>%
        rename(Age = age_gr)
      
      # hazard over time
      hazot_all_temp[[i]] <-  flexsurvspline(formula=Surv(time_years,status-1)~age_gr,data=data1,k = 1, scale = "hazard") %>%
        summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = cohortDefinitionSet$cohortName[j], Gender = "Both" ) %>%
        rename(Age = age_gr)
      
      
      #get the goodness of fit for each model
      gof_results_temp[[i]] <- flexsurvspline(formula=Surv(time_years,status-1)~age_gr,data=data1,k = 1, scale = "hazard") %>%
        glance() %>%
        mutate(Method = extrapolations_formatted[i], Cancer = cohortDefinitionSet$cohortName[j], Age = "Age", Gender = "Both" )
      
      
      #print out progress               
      print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cohortDefinitionSet$cohortName[j], " completed"))
      
    } else if(extrapolations[i] == "spline3") {
      
      #for spline models if not enough data will not run therefore to catch it
      #code below removes levels in a factor that have 75% or more no data
      
      #remove levels that have more than 75% no data (analysis unable to run)
      target <- observedrisktableKM_age[[j]] %>%
        mutate(count=rowSums(.[1:elgcols]==0), percentzero = 100 -((count/elgcols)*100) ) %>%
        filter(percentzero <= 25) %>% pull(Age)
      
      #subset the data by cancer type and removing level of data from previous step
      data1 <- Pop %>%
        filter(cohort_definition_id == j) %>%
        filter(!(age_gr %in% target))
      
      # 3knotspline
      extrap_results_temp[[i]] <- flexsurvspline(formula=Surv(time_years,status-1)~age_gr,data=data1,k = 3, scale = "hazard") %>%
        summary(t=t/365, tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = cohortDefinitionSet$cohortName[j], Gender = "Both" ) %>%
        rename(Age = age_gr)
      
      # hazard over time
      hazot_all_temp[[i]] <- flexsurvspline(formula=Surv(time_years,status-1)~age_gr,data=data1,k = 3, scale = "hazard") %>%
        summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = cohortDefinitionSet$cohortName[j], Gender = "Both" ) %>%
        rename(Age = age_gr)
      
      #get the goodness of fit for each model
      gof_results_temp[[i]] <- flexsurvspline(formula=Surv(time_years,status-1)~age_gr,data=data1,k = 3, scale = "hazard") %>%
        glance() %>%
        mutate(Method = extrapolations_formatted[i], Cancer = cohortDefinitionSet$cohortName[j], Age = "Age", Gender = "Both" )
      
      #print out progress               
      print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cohortDefinitionSet$cohortName[j], " completed"))
      
      
    } else {
      #carry out models for different parametic methods survival
      extrap_results_temp[[i]] <- flexsurvreg(Surv(time_years, status)~age_gr, data=data, dist=extrapolations[i]) %>%
        summary(t=t/365, tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = cohortDefinitionSet$cohortName[j], Gender = "Both" ) %>%
        rename(Age = age_gr)
      
      #extract the hazard function over time
      hazot_all_temp[[i]] <- flexsurvreg(Surv(time_years, status)~age_gr, data=data, dist=extrapolations[i]) %>%
        summary(t=(t + 1)/365, type = "hazard",tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = cohortDefinitionSet$cohortName[j], Gender = "Both" ) %>%
        rename(Age = age_gr)
      
      
      #get the goodness of fit for each model
      gof_results_temp[[i]] <- flexsurvreg(Surv(time_years, status)~age_gr, data=data, dist=extrapolations[i]) %>%
        glance() %>%
        mutate(Method = extrapolations_formatted[i], Cancer = cohortDefinitionSet$cohortName[j], Age = "Age", Gender = "Both") 
      
      #print out progress               
      print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cohortDefinitionSet$cohortName[j], " completed"))
      
    }
    
    #combine all results
    extrapolatedcombinedAge <- dplyr::bind_rows(extrap_results_temp)
    gofcombinedAge <- dplyr::bind_rows(gof_results_temp)
    hotcombinedAge <- dplyr::bind_rows(hazot_all_temp) %>%
      filter(time > 0) # remove rows with inf/NAs
    
    
    #put the results from each cancer in separate list
    extrapolations_age[[j]] <- extrapolatedcombinedAge
    gof_haz_age[[j]] <- gofcombinedAge
    hazot_age[[j]] <- hotcombinedAge
    
    
  }
  #print out progress               
  print(paste0(cohortDefinitionSet$cohortName[j]," Extrapolation Analysis Completed ", Sys.time()))
  
}

# Merge results together from each cancer and extrpolation into a dataframe ---
extrapolatedfinalAge <- dplyr::bind_rows(extrapolations_age)
goffinalAge <- dplyr::bind_rows(gof_haz_age)
hazardotfinalAge <- dplyr::bind_rows(hazot_age)

# extract results for 1,5,10 years for extrapolated data
extrapolation_predAge <- subset(extrapolatedfinalAge, extrapolatedfinalAge$time == 1 |
                                     extrapolatedfinalAge$time == 5 |
                                     extrapolatedfinalAge$time == 10  )

# catch to remove hazard extrapolation where hazard cant be generated on observed data
#create the filters pulls out the age and the cancer type where there is no data
filterage <- as.data.frame(table(hotkmcombined_age$Age, hotkmcombined_age$Cancer)) %>%
  rename(Age = Var1, Cancer = Var2, n = Freq ) %>%
  filter(n == 0) %>% 
  mutate_if(is.factor, as.character) %>%
  pull(Age) 

filtercancer <- as.data.frame(table(hotkmcombined_age$Age, hotkmcombined_age$Cancer)) %>%
  rename(Age = Var1, Cancer = Var2, n = Freq ) %>%
  filter(n == 0) %>% 
  mutate_if(is.factor, as.character) %>%
  pull(Cancer) 
  
#filter out the extrapolated data which doesnt have hazard over time for observed
hazardotfinalAge <- hazardotfinalAge %>%
  filter(!Cancer %in% filtercancer | !Age %in% filterage ) 

  
#save files in results folder ---
Results_AGE <- list("extrapolation_age" = extrapolatedfinalAge, 
                       "hazardrate_age" = hazardotfinalAge,
                       "GOF_age" = goffinalAge,
                       "prediction1510_age"= extrapolation_predAge)

#write results to excel ---
openxlsx::write.xlsx(Results_AGE, file = here("Results", db.name , "cancer_extrapolation_results_AGE.xlsx"))

# extrapolatedfinalAge <- read.xlsx(xlsxFile = here("Results", db.name , "cancer_extrapolation_results_AGE.xlsx"), sheet = 1)
# hazardotfinalAge <- read.xlsx(xlsxFile = here("Results", db.name , "cancer_extrapolation_results_AGE.xlsx"), sheet = 2)
# goffinalAge <- read.xlsx(xlsxFile = here("Results", db.name , "cancer_extrapolation_results_AGE.xlsx"), sheet = 3)
# extrapolation_predAge <- read.xlsx(xlsxFile = here("Results", db.name , "cancer_extrapolation_results_AGE.xlsx"), sheet = 4)



#######################################
# Create plots for stratification population AGE ---
#######################################

#merge KM and extrapolated data
extrap_combinedAge <- bind_rows(observedkmcombined_age, extrapolatedfinalAge)

for(j in 1:nrow(cohortDefinitionSet)) { 
  
  
  #set up the median survival for plot
  medsurv4plotAge <- medkmcombined_age %>%
    mutate(median = paste0("Median OS = ",round(median, 2))) %>%
    mutate(median = str_replace(median, "Median OS = NA", "Median OS = Not achieved")) %>%
    select( Age, median, Cancer)
  
  data <- extrap_combinedAge %>%
    filter(Cancer == cohortDefinitionSet$cohortName[j])

    
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
    data$Age <- factor(data$Age, levels=c(">=90",
                                          "80-89" ,
                                          "70-79" ,  
                                          "60-69" ,   
                                          "50-59"   ,
                                          "40-49"  ,
                                          "30-39" ,  
                                          "<30"
                                          ))
    
    
    plot_extrap_age <- ggplot(data, aes(x = time, y = est, colour = Method)) + 
      xlab("Time (Years)") + ylab("Survival Probability (%)") +
      geom_line() +
      geom_line(data = filter(data, Method == "Observed"), size = 1) +
      scale_color_manual(values = my_colors) +
      scale_fill_manual(values = my_colors) +
      theme_bw() +
      theme( legend.position = 'right') +
      scale_x_continuous(limits = c(0,max(data$time)), expand =c(0,0) ,
                         breaks = seq(0,max(data$time), by = 2 ) ) + 
      facet_wrap( ~ fct_rev(Age),scales = "free") +
      geom_text(data = medsurv4plotAge[medsurv4plotAge$Cancer == cohortDefinitionSet$cohortName[j] ,], aes(label=median), 
                x = Inf , y = Inf,
                inherit.aes = FALSE,
                size = 2.75,
                hjust = 1.1,
                vjust = 1.9
      )
    
    
    plotname1 <- paste0("plot_extrapolations_ ",cohortDefinitionSet$cohortName[j],"_AGE_STRAT",".png")
    
    ggsave(plot_extrap_age, file= here("Results", db.name,"Plots", plotname1)
           , width = 24, height = 18, units = "cm")
    
    print(paste0("Plot KM and extrapolation plot ", Sys.time()," for ",cohortDefinitionSet$cohortName[j], "age strat completed"))
    
    
    
  }
  
# hazard over time by age
hazot_combinedAge <- bind_rows(hotkmcombined_age, hazardotfinalAge)

for(j in 1:nrow(cohortDefinitionSet)) { 
  
  data <- hazot_combinedAge %>%
    filter(Cancer == cohortDefinitionSet$cohortName[j])

    
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
    data$Age <- factor(data$Age, levels=c(">=90",
                                          "80-89" ,
                                          "70-79" ,  
                                          "60-69" ,   
                                          "50-59"   ,
                                          "40-49"  ,
                                          "30-39" ,  
                                          "<30"
    ))
    
    plot_hot_age <- ggplot(data, aes(x = time, y = est, colour = Method)) + 
      xlab('Follow-up Time') + ylab('Hazard Rate') +
      geom_line() +
      geom_line(data = filter(data, Method == "Observed"), size = 1) +
      scale_color_manual(values = my_colors) +
      scale_fill_manual(values = my_colors) +
      theme_bw() +
      theme( legend.position = 'right') +
      scale_x_continuous(breaks = seq(0,max(data$time), by = 2 ) ) +
      coord_cartesian(xlim = c(-0.25, 14.5)) +
      facet_wrap( ~ fct_rev(Age),scales = "free")
    
    plotname1 <- paste0("plot_extrapolations_HOT_ ",cohortDefinitionSet$cohortName[j],"_AGE_STRAT",".png")
    
    ggsave(plot_hot_age, file= here("Results", db.name,"Plots", plotname1)
           , width = 24, height = 18, units = "cm")
    
    print(paste0("Plot hazard over time plot ", Sys.time()," for ",cohortDefinitionSet$cohortName[j], "age strat completed"))
    
  
}

########################################
# GENDER*AGE STRATIFICATION
########################################

#TBC



################END ####################
