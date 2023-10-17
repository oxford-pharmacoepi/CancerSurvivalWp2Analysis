#### PACKAGES -----
options(encoding = "UTF-8")

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
# library(ggthemes)
library(plotly)
library(here)
library(scales)
library(dplyr)
library(stringr)
library(tidyr)
library(ggalt)

#### UI -----
#ui <-  fluidPage(theme = shinytheme("spacelab"),
ui <-  fluidPage(theme = shinytheme("cerulean"),
                 
                 # title ------ 
                 # shown across tabs
                 titlePanel("Extrapolating survival for common cancers: a multinational cohort study"),
                 
                 # set up: pages along the side -----  
                 navlistPanel(
                   
                   
                   ## Introduction  -----  
                   tabPanel("Background", 
                            tags$h3("Background"),
                            tags$hr(),
                            tags$h4(tags$strong("Please note, the results presented here should be considered as 
                       preliminary and subject to change.")),
                            tags$hr(),
                            tags$h5(
 "This app is a companion to the study focussing on determining the incidence,prevalence and survival for 9 different cancers
 (Breast, Colorectal, Lung, Liver, Stomach, Head & Neck (including subsites), Prostate, Oesophagus, and Pancreas) between 2000 to 2021 using Primary care GP records
 from the United Kingdom (", tags$a(href="https://cprd.com/", "Clinical Practice Research Datalink"), "(CPRD) GOLD). We additionally validated all results using CPRD Aurum between 2000 to 2019"), 
 
 tags$h5(
 "In the following pages you can find information on annualised period prevalence, annualised and overall incidence, 
 survival for the whole population, calendar year survival and a description of the characteristics of the study populations 
of patients with an cancer outcome. All results have been stratified by age group and sex."),

                           # HTML('<br>'),

                        tags$h5("The results of this study are published in the following journal:"
                                ),
 tags$ol(
   tags$li(strong("TBC"),"(",tags$a(href="https://www.ndorms.ox.ac.uk/research/research-groups/Musculoskeletal-Pharmacoepidemiology","Paper Link"),")" )),
 
 tags$h5("The analysis code used to generate these results can be found",
         tags$a(href="https://github.com/oxford-pharmacoepi", "here"),
         ".The cohort diagnostics including the clinical codelists for each of the 8 cancers can be found",
         tags$a(href="https://dpa-pde-oxford.shinyapps.io/CancerExtrapolationDiagnostics/", "here")
         
         ),
 
 tags$h5("Any questions regarding these results or problems with the app please contact",
         tags$a(href="mailto:danielle.newby@ndorms.ox.ac.uk", "Danielle Newby")
         
 ),
 
 
                            tags$hr()
 
 
                   ), 
                   ## Prevalence ------ 
                   tabPanel("Population Prevalence",	  
                            tags$h3("Prevalence Estimates"),
                            tags$h5("Prevalence estimates are shown below...."),
                            tags$hr(),
                            tags$h5("Database and study outcome"),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "prevalence_database_name_selector",
                                            label = "Database",
                                            choices = unique(prevalence_estimates$database_name),
                                            selected = unique(prevalence_estimates$database_name),
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = TRUE)
                            ),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "prevalence_outcome_cohort_name_selector",
                                            label = "Outcome",
                                            choices = sort(unique(prevalence_estimates$outcome_cohort_name)),
                                            selected = c("Breast"),
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = TRUE)
                            ),
                            tags$hr(),
                            tags$h5("Population Settings"),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "prevalence_denominator_age_group_selector",
                                            label = "Age group",
                                            choices = levels(prevalence_estimates$denominator_age_group),
                                            selected = "All",
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = TRUE)
                            ),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "prevalence_denominator_sex_selector",
                                            label = "Sex",
                                            choices = unique(prevalence_estimates$denominator_sex),
                                            selected = "Both",
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = TRUE)
                            # ),
                            # div(style="display: inline-block;vertical-align:top; width: 150px;",
                            #     pickerInput(inputId = "prevalence_denominator_days_prior_history_selector",
                            #                 label = "Days Prior History",
                            #                 choices = unique(prevalence_estimates$denominator_days_prior_history),
                            #                 selected = 365,
                            #                 options = list(
                            #                   `actions-box` = TRUE,
                            #                   size = 10,
                            #                   `selected-text-format` = "count > 3"),
                            #                 multiple = TRUE)
                            ),
                            tags$hr(),
                            tags$h5("Analysis Settings"),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "prevalence_start_date_selector",
                                            label = "Prevalence Start Date",
                                            choices = as.character(unique(prevalence_estimates$prevalence_start_date)),
                                            selected = as.character(unique(prevalence_estimates$prevalence_start_date)),
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = TRUE)
                            ),
                            tabsetPanel(type = "tabs",
                                        tabPanel("Table of Estimates", 
                                                 DTOutput('tbl_prevalence_estimates') %>% withSpinner()), 
                                        tabPanel("Plot of Estimates",
                                                 tags$hr(),
                                                 tags$h5("Plotting Options"),
                                                 div(style="display: inline-block;vertical-align:top; width: 150px;",
                                                     pickerInput(inputId = "prevalence_x_axis",
                                                                 label = "X axis",
                                                                 choices = c("denominator_age_group", 
                                                                             "denominator_sex",
                                                                             #"denominator_days_prior_history",
                                                                             "outcome_cohort_name",
                                                                             "database_name",
                                                                             "prevalence_start_date"),
                                                                 selected = "prevalence_start_date",
                                                                 options = list(
                                                                   `actions-box` = TRUE,
                                                                   size = 10,
                                                                   `selected-text-format` = "count > 3"),
                                                                 multiple = FALSE,)
                                                 ),
                                                 div(style="display: inline-block;vertical-align:top; width: 150px;",
                                                     pickerInput(inputId = "prevalence_plot_facet",
                                                                 label = "Facet by",
                                                                 choices = c("denominator_age_group", 
                                                                             "denominator_sex",
                                                                            # "denominator_days_prior_history",
                                                                             "outcome_cohort_name",
                                                                             "database_name",
                                                                             "prevalence_start_date"),
                                                                 selected = c("outcome_cohort_name",
                                                                              "database_name"),
                                                                 options = list(
                                                                   `actions-box` = TRUE,
                                                                   size = 10,
                                                                   `selected-text-format` = "count > 3"),
                                                                 multiple = TRUE,)
                                                 ),
                                                 div(style="display: inline-block;vertical-align:top; width: 150px;",
                                                     pickerInput(inputId = "prevalence_plot_group",
                                                                 label = "Colour by",
                                                                 choices = c("denominator_age_group", 
                                                                             "denominator_sex",
                                                                            # "denominator_days_prior_history",
                                                                             "outcome_cohort_name",
                                                                             "database_name",
                                                                             "prevalence_start_date"),
                                                                 options = list(
                                                                   `actions-box` = TRUE,
                                                                   size = 10,
                                                                   `selected-text-format` = "count > 3"),
                                                                 multiple = TRUE,)
                                                 ),
                                                 plotlyOutput('plot_prevalence_estimates', height = "800px") %>% withSpinner() ), 
                                        tabPanel("Attrition table", 
                                                 DTOutput('tbl_prevalence_attrition') %>% withSpinner())
                            )
                   ),
                   
                   
                   ## Incidence ------ 
                   tabPanel("Population Incidence",	  
                            tags$h3("Incidence Estimates"),
                            tags$h5("Incidence estimates are shown below...."),
                            tags$hr(),
                            tags$h5("Database and Study Outcome"),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "incidence_database_name_selector",
                                            label = "Database",
                                            choices = unique(incidence_estimates$database_name),
                                            selected = unique(incidence_estimates$database_name),
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = TRUE)
                            ),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "incidence_outcome_cohort_name_selector",
                                            label = "Outcome",
                                            choices = sort(unique(incidence_estimates$outcome_cohort_name)),
                                            selected = c("Breast"),
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = TRUE)
                            ),
                            tags$hr(),
                            tags$h5("Population Settings"),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "incidence_denominator_age_group_selector",
                                            label = "Age group",
                                            choices = levels(incidence_estimates$denominator_age_group),
                                            selected = "All",
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = TRUE)
                            ),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "incidence_denominator_sex_selector",
                                            label = "Sex",
                                            choices = unique(incidence_estimates$denominator_sex),
                                            selected = "Both",
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = TRUE)
                            # ),
                            # div(style="display: inline-block;vertical-align:top; width: 150px;",
                            #     pickerInput(inputId = "incidence_denominator_days_prior_history_selector",
                            #                 label = "Days Prior History",
                            #                 choices = unique(incidence_estimates$denominator_days_prior_history),
                            #                 selected = 365,
                            #                 options = list(
                            #                   `actions-box` = TRUE,
                            #                   size = 10,
                            #                   `selected-text-format` = "count > 3"),
                            #                 multiple = TRUE)
                         
                            ),
                            tags$hr(),
                            tags$h5("Analysis Settings"),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "incidence_start_date_selector",
                                            label = "Incidence Start Date",
                                            choices = as.character(unique(incidence_estimates$incidence_start_date)),
                                            selected = as.character(unique(incidence_estimates$incidence_start_date)),
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = TRUE)
                                
                                
                            ),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "incidence_denominator_analysis_interval_selector",
                                            label = "Analysis Interval",
                                            choices = unique(incidence_estimates$analysis_interval),
                                            selected = "years",
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = TRUE)
                            ),
                            tabsetPanel(type = "tabs",
                                        tabPanel("Table of Estimates", 
                                                 DTOutput('tbl_incidence_estimates') %>% withSpinner()), 
                                        tabPanel("Plot of Estimates",
                                                 tags$hr(),
                                                 tags$h5("Plotting Options"),
                                                 div(style="display: inline-block;vertical-align:top; width: 150px;",
                                                     pickerInput(inputId = "incidence_x_axis",
                                                                 label = "X axis",
                                                                 choices = c("denominator_age_group", 
                                                                             "denominator_sex",
                                                                             "outcome_cohort_name",
                                                                             "database_name",
                                                                             "incidence_start_date"),
                                                                 selected = "incidence_start_date",
                                                                 options = list(
                                                                   `actions-box` = TRUE,
                                                                   size = 10,
                                                                   `selected-text-format` = "count > 3"),
                                                                 multiple = FALSE,)
                                                 ),
                                                 div(style="display: inline-block;vertical-align:top; width: 150px;",
                                                     pickerInput(inputId = "incidence_plot_facet",
                                                                 label = "Facet by",
                                                                 choices = c("denominator_age_group", 
                                                                             "denominator_sex",
                                                                             "outcome_cohort_name",
                                                                             "database_name",
                                                                             "incidence_start_date"),
                                                                 selected = c("outcome_cohort_name",
                                                                              "database_name"),
                                                                 options = list(
                                                                   `actions-box` = TRUE,
                                                                   size = 10,
                                                                   `selected-text-format` = "count > 3"),
                                                                 multiple = TRUE,)
                                                 ),
                                                 div(style="display: inline-block;vertical-align:top; width: 150px;",
                                                     pickerInput(inputId = "incidence_plot_group",
                                                                 label = "Colour by",
                                                                 choices = c("denominator_age_group", 
                                                                             "denominator_sex",
                                                                             "outcome_cohort_name",
                                                                             "database_name",
                                                                             "incidence_start_date"),
                                                                 options = list(
                                                                   `actions-box` = TRUE,
                                                                   size = 10,
                                                                   `selected-text-format` = "count > 3"),
                                                                 multiple = TRUE,)
                                                 ),
                                                 plotlyOutput('plot_incidence_estimates', height = "800px") %>% withSpinner() ), 
                                        tabPanel("Attrition table", 
                                                 DTOutput('tbl_incidence_attrition') %>% withSpinner())
                            )
                   ) ,
                   
                   
                   ## Survival ------ 
                   tabPanel("Whole Population Survival",	  
                            tags$h3("KM Survival Analysis"),
                            tags$h5("For this study we also calculated overall survival using the kaplan meier method. The results contain the estimates (including median survival). risk tables and KM survival plots which are shown below...."),
                            tags$hr(),
                            tags$h5("Database and Study Outcome"),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "survival_database_name_selector",
                                            label = "Database",
                                            choices = unique(survival_estimates_whole$Database),
                                            selected = unique(survival_estimates_whole$Database),
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = TRUE)
                            ),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "survival_outcome_cohort_name_selector",
                                            label = "Outcome",
                                            choices = sort(unique(survival_estimates_whole$Cancer)),
                                            selected = c("Breast"),
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = TRUE)
                            )
                            ,
                            tags$hr(),
                            tags$h5("Population Settings"),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "survival_age_group_selector",
                                            label = "Age group",
                                            choices = levels(survival_estimates_whole$Age),
                                            selected = "All",
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = TRUE)
                            ),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "survival_sex_selector",
                                            label = "Sex",
                                            choices = unique(survival_estimates_whole$Sex),
                                            selected = "Both",
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = TRUE)
                            ),
                            tags$hr(),
                            # tags$h5("Analysis Settings"),
                            # div(style="display: inline-block;vertical-align:top; width: 150px;",
                            #     pickerInput(inputId = "calendar_year_selector",
                            #                 label = "Calendar Years",
                            #                 choices = "2000 to 2019",
                            #                 selected =  "2000 to 2019",
                            #                 options = list(
                            #                   `actions-box` = TRUE,
                            #                   size = 10,
                            #                   `selected-text-format` = "count > 3"),
                            #                 multiple = TRUE)
                            # ),
                            tabsetPanel(type = "tabs",
                                        tabPanel("Plot of KM survival curve",
                                                 tags$hr(),
                                                 tags$h5("Plotting Options"),
                                                 div(style="display: inline-block;vertical-align:top; width: 150px;",
                                                     pickerInput(inputId = "time",
                                                                 label = "X axis",
                                                                 choices = c("time"),
                                                                 selected = "time",
                                                                 options = list(
                                                                   `actions-box` = TRUE,
                                                                   size = 10,
                                                                   `selected-text-format` = "count > 3"),
                                                                 multiple = FALSE,)
                                                 ),
                                                 div(style="display: inline-block;vertical-align:top; width: 150px;",
                                                     pickerInput(inputId = "survival_plot_facet",
                                                                 label = "Facet by",
                                                                 choices = c("Cancer",
                                                                             "Database",
                                                                             "Sex",
                                                                             "Age"
                                                                             ),
                                                                 selected = c("Cancer"),
                                                                 options = list(
                                                                   `actions-box` = TRUE,
                                                                   size = 10,
                                                                   `selected-text-format` = "count > 3"),
                                                                 multiple = TRUE,)
                                                 ),
                                                 div(style="display: inline-block;vertical-align:top; width: 150px;",
                                                     pickerInput(inputId = "survival_plot_group",
                                                                 label = "Colour by",
                                                                 choices = c("Sex",
                                                                             "Age",
                                                                             "Cancer",
                                                                             "Database"),
                                                                 selected = c("Database"),
                                                                 options = list(
                                                                   `actions-box` = TRUE,
                                                                   size = 10,
                                                                   `selected-text-format` = "count > 3"),
                                                                 multiple = TRUE,)
                                                 ),
                                                 plotlyOutput('plot_survival_estimates', height = "800px") %>% withSpinner() ), 
                                        
                                        
                                        
                                        tabPanel("KM risk table", 
                                                 DTOutput('tbl_survival_risk_table') %>% withSpinner()),
                                        tabPanel("Median survival estimates", 
                                                 tags$hr(),
                                                 DTOutput('tbl_survival_median_table') %>% withSpinner()),
                                        tabPanel("Survival Probabilities", 
                                                 DTOutput('tbl_survival_rates_table') %>% withSpinner()),
                                        
                                        tabPanel("Survival Years Follow Up", 
                                                 DTOutput('tbl_survival_followup_table') %>% withSpinner())
                                        
                                        
                                        
                                               
                            )
                            
                   ) ,
                   
                   
                   
                   ## Survival ------ 
                   tabPanel("Calendar Year Population Survival",	  
                            tags$h3("KM Survival Analysis"),
                            tags$h5("For this study we also calculated overall survival using the kaplan meier method. The results contain the estimates (including median survival). risk tables and KM survival plots which are shown below split by calendar year of cancer diagnosis..."),
                            tags$hr(),
                            tags$h5("Database and Study Outcome"),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "survival_database_name_selector_cy",
                                            label = "Database",
                                            choices = unique(survival_estimates_calendar$Database),
                                            selected = unique(survival_estimates_calendar$Database),
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = TRUE)
                            ),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "survival_outcome_cohort_name_selector_cy",
                                            label = "Outcome",
                                            choices = sort(unique(survival_estimates_calendar$Cancer)),
                                            selected = c("Breast"),
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = TRUE)
                            ),
                            tags$hr(),
                            tags$h5("Population Settings"),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "survival_age_group_selector_cy",
                                            label = "Age group",
                                            choices = levels(survival_estimates_calendar$Age),
                                            selected = "All",
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = TRUE)
                            ),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "survival_sex_selector_cy",
                                            label = "Sex",
                                            choices = unique(survival_estimates_calendar$Sex),
                                            selected = "Both",
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = TRUE)
                            ),
                            tags$hr(),
                            tags$h5("Analysis Settings"),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "calendar_year_selector_cy",
                                            label = "Calendar Years",
                                            choices = as.character(unique(survival_estimates_calendar$CalendarYearGp)),
                                            selected = as.character(unique(survival_estimates_calendar$CalendarYearGp)),
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = TRUE)
                            ),
                            tabsetPanel(type = "tabs",
                                        tabPanel("Plot of KM survival curve",
                                                 tags$hr(),
                                                 tags$h5("Plotting Options"),
                                                 div(style="display: inline-block;vertical-align:top; width: 150px;",
                                                     pickerInput(inputId = "time",
                                                                 label = "X axis",
                                                                 choices = c("time"),
                                                                 selected = "time",
                                                                 options = list(
                                                                   `actions-box` = TRUE,
                                                                   size = 10,
                                                                   `selected-text-format` = "count > 3"),
                                                                 multiple = FALSE,)
                                                 ),
                                                 div(style="display: inline-block;vertical-align:top; width: 150px;",
                                                     pickerInput(inputId = "survival_plot_facet_cy",
                                                                 label = "Facet by",
                                                                 choices = c("Cancer",
                                                                             "Database",
                                                                             "Sex",
                                                                             "Age"
                                                                 ),
                                                                 selected = c("Cancer","Database"),
                                                                 options = list(
                                                                   `actions-box` = TRUE,
                                                                   size = 10,
                                                                   `selected-text-format` = "count > 3"),
                                                                 multiple = TRUE)
                                                 ),
                                                 plotlyOutput('plot_survival_estimates_cy', height = "800px") %>% withSpinner() ), 
                                        
                                        
                                        tabPanel("KM risk table", 
                                                 DTOutput('tbl_survival_risk_table_cy') %>% withSpinner()),
                                        tabPanel("Median survival estimates", 
                                                 tags$hr(),
                                                 DTOutput('tbl_survival_median_table_cy') %>% withSpinner()),
                                        tabPanel("Survival Probabilities", 
                                                 DTOutput('tbl_survival_rates_table_cy') %>% withSpinner()),
                                        tabPanel("Plots of 1 and 5 year survival",
                                                 tags$hr(),
                                                 tags$h5("Plotting Options"),
                                                 div(style="display: inline-block;vertical-align:top; width: 150px;",
                                                     pickerInput(inputId = "survival_rate_plot_group",
                                                                 label = "Colour by",
                                                                 choices = c("Sex",
                                                                             "Age",
                                                                             "Cancer",
                                                                             "Database",
                                                                             "time"),
                                                                 selected = c("time", "Database"),
                                                                 options = list(
                                                                   `actions-box` = TRUE,
                                                                   size = 10,
                                                                   `selected-text-format` = "count > 3"),
                                                                 multiple = TRUE,)    
                                                 ),
                                                 div(style="display: inline-block;vertical-align:top; width: 150px;",
                                                     pickerInput(inputId = "survival_rate_facet_cy",
                                                                 label = "Facet by",
                                                                 choices = c("Cancer",
                                                                             "Database",
                                                                             "Sex",
                                                                             "Age"
                                                                 ),
                                                                 selected = c("Cancer","Database"),
                                                                 options = list(
                                                                   `actions-box` = TRUE,
                                                                   size = 10,
                                                                   `selected-text-format` = "count > 3"),
                                                                 multiple = TRUE)
                                                 ),
                                                 plotlyOutput('plot_survival_probs_cy', height = "800px") %>% withSpinner() )
                                        
                                        
                                        
                            )
                            
                   ) ,
                   
                   ## Population characteristics ------ 
                   tabPanel("Population Characteristics",	  
                            tags$h3("Study Population Characteristics"),
                            tags$h5("The population characteristics are shown below. For all conditions unless otherwise specified this was obtained looking at any time in history before cancer diagnosis. For smoking we obtained smoking status looking back either 5 or 10 years in previous history from cancer diagnosis"),
                            tags$hr(),
                            tags$h5("Study outcome") ,
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "table1_outcome_cohort_name_selector",
                                            label = "Outcome",
                                            choices = sort(unique(table_one_results$Cancer)),
                                            selected = c("Breast"),
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = TRUE)



                            ),
                            
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "table1_sex_selector",
                                            label = "Sex",
                                            choices = sort(unique(table_one_results$Sex)),
                                            selected = c("Both"),
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = TRUE)
                            ),
                            
                            
                            tabsetPanel(type = "tabs",
                                        tabPanel("Study Population Characteristics", 
                                                 DTOutput('tbl_table_one') %>% withSpinner()
                                                 
                                                 )
                                        
                            )
                            
                            
                            
                   ) 
                   
                   
                   
                   # close -----
                 ))
