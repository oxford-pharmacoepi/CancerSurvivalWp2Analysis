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
library(bslib)

#### UI -----
#ui <-  fluidPage(theme = shinytheme("spacelab"),
#ui <-  fluidPage(theme = shinytheme("cerulean"),
ui <-  fluidPage(theme = bs_theme(version = 4, bootswatch = "minty"),
#ui <-  fluidPage(theme = bs_theme(bootswatch = "pulse"),
                 
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
 "This app is a companion to the study focussing on the assessment and prediction of survival for eight different cancers
 (Breast, Colorectal, Lung, Liver, Stomach, Head & Neck, Prostate, and Pancreas) for a variety of different electronic health records and cancer registries across Europe (Spain, Netherlands, Italy, Germany, Norway, Finland, Portugal, Estonia, Switzerland, Hungary, and the United Kingdom)."), 
 tags$h5(
 "In the following pages you can find information on the survival using the KM method, median survival, mean survival and survival as one, five and ten years. Additionally, the results of eight extrapolation methods to predict survival with goodness of fit measures and predicted survival at one, five and ten years for stratified and adjusted model types. Finally and a description of the characteristics of the study populations and attrition is also reported.
for each cancer. All results have been performed for the whole population and for each age group (10 year age bands) and also for each sex (apart from prostate cancer)."),

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
                   # ## Prevalence ------
                   # tabPanel("Population Prevalence",
                   #          tags$h3("Prevalence Estimates"),
                   #          tags$h5("Prevalence estimates are shown below...."),
                   #          tags$hr(),
                   #          tags$h5("Database and study outcome"),
                   #          div(style="display: inline-block;vertical-align:top; width: 150px;",
                   #              pickerInput(inputId = "prevalence_database_name_selector",
                   #                          label = "Database",
                   #                          choices = unique(prevalence_estimates$database_name),
                   #                          selected = unique(prevalence_estimates$database_name),
                   #                          options = list(
                   #                            `actions-box` = TRUE,
                   #                            size = 10,
                   #                            `selected-text-format` = "count > 3"),
                   #                          multiple = TRUE)
                   #          ),
                   #          div(style="display: inline-block;vertical-align:top; width: 150px;",
                   #              pickerInput(inputId = "prevalence_outcome_cohort_name_selector",
                   #                          label = "Outcome",
                   #                          choices = sort(unique(prevalence_estimates$outcome_cohort_name)),
                   #                          selected = c("Breast"),
                   #                          options = list(
                   #                            `actions-box` = TRUE,
                   #                            size = 10,
                   #                            `selected-text-format` = "count > 3"),
                   #                          multiple = TRUE)
                   #          ),
                   #          tags$hr(),
                   #          tags$h5("Population Settings"),
                   #          div(style="display: inline-block;vertical-align:top; width: 150px;",
                   #              pickerInput(inputId = "prevalence_denominator_age_group_selector",
                   #                          label = "Age group",
                   #                          choices = levels(prevalence_estimates$denominator_age_group),
                   #                          selected = "All",
                   #                          options = list(
                   #                            `actions-box` = TRUE,
                   #                            size = 10,
                   #                            `selected-text-format` = "count > 3"),
                   #                          multiple = TRUE)
                   #          ),
                   #          div(style="display: inline-block;vertical-align:top; width: 150px;",
                   #              pickerInput(inputId = "prevalence_denominator_sex_selector",
                   #                          label = "Sex",
                   #                          choices = unique(prevalence_estimates$denominator_sex),
                   #                          selected = "Both",
                   #                          options = list(
                   #                            `actions-box` = TRUE,
                   #                            size = 10,
                   #                            `selected-text-format` = "count > 3"),
                   #                          multiple = TRUE)
                   #          # ),
                   #          # div(style="display: inline-block;vertical-align:top; width: 150px;",
                   #          #     pickerInput(inputId = "prevalence_denominator_days_prior_history_selector",
                   #          #                 label = "Days Prior History",
                   #          #                 choices = unique(prevalence_estimates$denominator_days_prior_history),
                   #          #                 selected = 365,
                   #          #                 options = list(
                   #          #                   `actions-box` = TRUE,
                   #          #                   size = 10,
                   #          #                   `selected-text-format` = "count > 3"),
                   #          #                 multiple = TRUE)
                   #          ),
                   #          tags$hr(),
                   #          tags$h5("Analysis Settings"),
                   #          div(style="display: inline-block;vertical-align:top; width: 150px;",
                   #              pickerInput(inputId = "prevalence_start_date_selector",
                   #                          label = "Prevalence Start Date",
                   #                          choices = as.character(unique(prevalence_estimates$prevalence_start_date)),
                   #                          selected = as.character(unique(prevalence_estimates$prevalence_start_date)),
                   #                          options = list(
                   #                            `actions-box` = TRUE,
                   #                            size = 10,
                   #                            `selected-text-format` = "count > 3"),
                   #                          multiple = TRUE)
                   #          ),
                   #          tabsetPanel(type = "tabs",
                   #                      tabPanel("Table of Estimates",
                   #                               DTOutput('tbl_prevalence_estimates') %>% withSpinner()),
                   #                      tabPanel("Plot of Estimates",
                   #                               tags$hr(),
                   #                               tags$h5("Plotting Options"),
                   #                               div(style="display: inline-block;vertical-align:top; width: 150px;",
                   #                                   pickerInput(inputId = "prevalence_x_axis",
                   #                                               label = "X axis",
                   #                                               choices = c("denominator_age_group",
                   #                                                           "denominator_sex",
                   #                                                           #"denominator_days_prior_history",
                   #                                                           "outcome_cohort_name",
                   #                                                           "database_name",
                   #                                                           "prevalence_start_date"),
                   #                                               selected = "prevalence_start_date",
                   #                                               options = list(
                   #                                                 `actions-box` = TRUE,
                   #                                                 size = 10,
                   #                                                 `selected-text-format` = "count > 3"),
                   #                                               multiple = FALSE,)
                   #                               ),
                   #                               div(style="display: inline-block;vertical-align:top; width: 150px;",
                   #                                   pickerInput(inputId = "prevalence_plot_facet",
                   #                                               label = "Facet by",
                   #                                               choices = c("denominator_age_group",
                   #                                                           "denominator_sex",
                   #                                                          # "denominator_days_prior_history",
                   #                                                           "outcome_cohort_name",
                   #                                                           "database_name",
                   #                                                           "prevalence_start_date"),
                   #                                               selected = c("outcome_cohort_name",
                   #                                                            "database_name"),
                   #                                               options = list(
                   #                                                 `actions-box` = TRUE,
                   #                                                 size = 10,
                   #                                                 `selected-text-format` = "count > 3"),
                   #                                               multiple = TRUE,)
                   #                               ),
                   #                               div(style="display: inline-block;vertical-align:top; width: 150px;",
                   #                                   pickerInput(inputId = "prevalence_plot_group",
                   #                                               label = "Colour by",
                   #                                               choices = c("denominator_age_group",
                   #                                                           "denominator_sex",
                   #                                                          # "denominator_days_prior_history",
                   #                                                           "outcome_cohort_name",
                   #                                                           "database_name",
                   #                                                           "prevalence_start_date"),
                   #                                               options = list(
                   #                                                 `actions-box` = TRUE,
                   #                                                 size = 10,
                   #                                                 `selected-text-format` = "count > 3"),
                   #                                               multiple = TRUE,)
                   #                               ),
                   #                               plotlyOutput('plot_prevalence_estimates', height = "800px") %>% withSpinner() ),
                   #                      tabPanel("Attrition table",
                   #                               DTOutput('tbl_prevalence_attrition') %>% withSpinner())
                   #          )
                   # ),


                   ## Survival ------
 tabPanel("Population Survival and extrapolations",
          tags$h3("KM Survival Analysis"),
          tags$h5("For this study we also calculated overall survival using the kaplan meier method. The results contain the estimates (including median survival). risk tables and KM survival plots which are shown below...."),
          tags$hr(),
          tags$h5("Strata"),
          div(style="display: inline-block;vertical-align:top; width: 150px;",
              pickerInput(inputId = "survival_database_name_selector",
                          label = "Database",
                          choices = unique(survival_estimates$Database),
                          selected = unique(survival_estimates$Database),
                          options = list(
                            `actions-box` = TRUE,
                            size = 10,
                            `selected-text-format` = "count > 3"),
                          multiple = TRUE)
          ),
          div(style="display: inline-block;vertical-align:top; width: 150px;",
              pickerInput(inputId = "survival_outcome_cohort_name_selector",
                          label = "Outcome",
                          choices = sort(unique(survival_estimates$Cancer)),
                          selected = c("IncidentBreastCancer"),
                          options = list(
                            `actions-box` = TRUE,
                            size = 10,
                            `selected-text-format` = "count > 3"),
                          multiple = TRUE)
          ),
          div(style="display: inline-block;vertical-align:top; width: 150px;",
              pickerInput(inputId = "survival_strat_selector",
                          label = "Stratification",
                          choices = sort(unique(survival_estimates$Stratification)),
                          selected = c("None"),
                          options = list(
                            `actions-box` = TRUE,
                            size = 10,
                            `selected-text-format` = "count > 3"),
                          multiple = TRUE)
          ),
          div(style="display: inline-block;vertical-align:top; width: 150px;",
              pickerInput(inputId = "survival_adjust_selector",
                          label = "Adjustment",
                          choices = sort(unique(survival_estimates$Adjustment)),
                          selected = c("None"),
                          options = list(
                            `actions-box` = TRUE,
                            size = 10,
                            `selected-text-format` = "count > 3"),
                          multiple = TRUE)
          ),
          div(style="display: inline-block;vertical-align:top; width: 150px;",
              pickerInput(inputId = "survival_method_selector",
                          label = "Method",
                          choices = sort(unique(survival_estimates$Method)),
                          selected = unique(survival_estimates$Method),
                          options = list(
                            `actions-box` = TRUE,
                            size = 10,
                            `selected-text-format` = "count > 3"),
                          multiple = TRUE)
          ),
          
          
          tags$hr(),
          tags$h5("Population Settings"),
          div(style="display: inline-block;vertical-align:top; width: 150px;",
              pickerInput(inputId = "survival_age_group_selector",
                          label = "Age group",
                          choices = unique(survival_estimates$Age),
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
                          choices = unique(survival_estimates$Sex),
                          selected = "Both",
                          options = list(
                            `actions-box` = TRUE,
                            size = 10,
                            `selected-text-format` = "count > 3"),
                          multiple = TRUE)
          ),
          tags$hr(),
          tabsetPanel(type = "tabs",
                      tabPanel("Plot of KM survival curve and extrapolations",
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
                                                           "Database",
                                                           "Method"),
                                               selected = c("Database", "Method"),
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
                               DTOutput('tbl_survival_rates_table') %>% withSpinner())


          )

 ) ,



 
                   # ## Incidence ------
                   # tabPanel("Goodness of fit for statistical models",
                   #          tags$h3("Incidence Estimates"),
                   #          tags$h5("Incidence estimates are shown below...."),
                   #          tags$hr(),
                   #          tags$h5("Database and Study Outcome"),
                   #          div(style="display: inline-block;vertical-align:top; width: 150px;",
                   #              pickerInput(inputId = "incidence_database_name_selector",
                   #                          label = "Database",
                   #                          choices = unique(incidence_estimates$database_name),
                   #                          selected = unique(incidence_estimates$database_name),
                   #                          options = list(
                   #                            `actions-box` = TRUE,
                   #                            size = 10,
                   #                            `selected-text-format` = "count > 3"),
                   #                          multiple = TRUE)
                   #          ),
                   #          div(style="display: inline-block;vertical-align:top; width: 150px;",
                   #              pickerInput(inputId = "incidence_outcome_cohort_name_selector",
                   #                          label = "Outcome",
                   #                          choices = sort(unique(incidence_estimates$outcome_cohort_name)),
                   #                          selected = c("Breast"),
                   #                          options = list(
                   #                            `actions-box` = TRUE,
                   #                            size = 10,
                   #                            `selected-text-format` = "count > 3"),
                   #                          multiple = TRUE)
                   #          ),
                   #          tags$hr(),
                   #          tags$h5("Population Settings"),
                   #          div(style="display: inline-block;vertical-align:top; width: 150px;",
                   #              pickerInput(inputId = "incidence_denominator_age_group_selector",
                   #                          label = "Age group",
                   #                          choices = levels(incidence_estimates$denominator_age_group),
                   #                          selected = "All",
                   #                          options = list(
                   #                            `actions-box` = TRUE,
                   #                            size = 10,
                   #                            `selected-text-format` = "count > 3"),
                   #                          multiple = TRUE)
                   #          ),
                   #          div(style="display: inline-block;vertical-align:top; width: 150px;",
                   #              pickerInput(inputId = "incidence_denominator_sex_selector",
                   #                          label = "Sex",
                   #                          choices = unique(incidence_estimates$denominator_sex),
                   #                          selected = "Both",
                   #                          options = list(
                   #                            `actions-box` = TRUE,
                   #                            size = 10,
                   #                            `selected-text-format` = "count > 3"),
                   #                          multiple = TRUE)
                   #          # ),
                   #          # div(style="display: inline-block;vertical-align:top; width: 150px;",
                   #          #     pickerInput(inputId = "incidence_denominator_days_prior_history_selector",
                   #          #                 label = "Days Prior History",
                   #          #                 choices = unique(incidence_estimates$denominator_days_prior_history),
                   #          #                 selected = 365,
                   #          #                 options = list(
                   #          #                   `actions-box` = TRUE,
                   #          #                   size = 10,
                   #          #                   `selected-text-format` = "count > 3"),
                   #          #                 multiple = TRUE)
                   # 
                   #          ),
                   #          tags$hr(),
                   #          tags$h5("Analysis Settings"),
                   #          div(style="display: inline-block;vertical-align:top; width: 150px;",
                   #              pickerInput(inputId = "incidence_start_date_selector",
                   #                          label = "Incidence Start Date",
                   #                          choices = as.character(unique(incidence_estimates$incidence_start_date)),
                   #                          selected = as.character(unique(incidence_estimates$incidence_start_date)),
                   #                          options = list(
                   #                            `actions-box` = TRUE,
                   #                            size = 10,
                   #                            `selected-text-format` = "count > 3"),
                   #                          multiple = TRUE)
                   # 
                   # 
                   #          ),
                   #          div(style="display: inline-block;vertical-align:top; width: 150px;",
                   #              pickerInput(inputId = "incidence_denominator_analysis_interval_selector",
                   #                          label = "Analysis Interval",
                   #                          choices = unique(incidence_estimates$analysis_interval),
                   #                          selected = "years",
                   #                          options = list(
                   #                            `actions-box` = TRUE,
                   #                            size = 10,
                   #                            `selected-text-format` = "count > 3"),
                   #                          multiple = TRUE)
                   #          ),
                   #          tabsetPanel(type = "tabs",
                   #                      tabPanel("Table of Estimates",
                   #                               DTOutput('tbl_incidence_estimates') %>% withSpinner()),
                   #                      tabPanel("Plot of Estimates",
                   #                               tags$hr(),
                   #                               tags$h5("Plotting Options"),
                   #                               div(style="display: inline-block;vertical-align:top; width: 150px;",
                   #                                   pickerInput(inputId = "incidence_x_axis",
                   #                                               label = "X axis",
                   #                                               choices = c("denominator_age_group",
                   #                                                           "denominator_sex",
                   #                                                           "outcome_cohort_name",
                   #                                                           "database_name",
                   #                                                           "incidence_start_date"),
                   #                                               selected = "incidence_start_date",
                   #                                               options = list(
                   #                                                 `actions-box` = TRUE,
                   #                                                 size = 10,
                   #                                                 `selected-text-format` = "count > 3"),
                   #                                               multiple = FALSE,)
                   #                               ),
                   #                               div(style="display: inline-block;vertical-align:top; width: 150px;",
                   #                                   pickerInput(inputId = "incidence_plot_facet",
                   #                                               label = "Facet by",
                   #                                               choices = c("denominator_age_group",
                   #                                                           "denominator_sex",
                   #                                                           "outcome_cohort_name",
                   #                                                           "database_name",
                   #                                                           "incidence_start_date"),
                   #                                               selected = c("outcome_cohort_name",
                   #                                                            "database_name"),
                   #                                               options = list(
                   #                                                 `actions-box` = TRUE,
                   #                                                 size = 10,
                   #                                                 `selected-text-format` = "count > 3"),
                   #                                               multiple = TRUE,)
                   #                               ),
                   #                               div(style="display: inline-block;vertical-align:top; width: 150px;",
                   #                                   pickerInput(inputId = "incidence_plot_group",
                   #                                               label = "Colour by",
                   #                                               choices = c("denominator_age_group",
                   #                                                           "denominator_sex",
                   #                                                           "outcome_cohort_name",
                   #                                                           "database_name",
                   #                                                           "incidence_start_date"),
                   #                                               options = list(
                   #                                                 `actions-box` = TRUE,
                   #                                                 size = 10,
                   #                                                 `selected-text-format` = "count > 3"),
                   #                                               multiple = TRUE,)
                   #                               ),
                   #                               plotlyOutput('plot_incidence_estimates', height = "800px") %>% withSpinner() ),
                   #                      tabPanel("Attrition table",
                   #                               DTOutput('tbl_incidence_attrition') %>% withSpinner())
                   #          )
                   # ) ,
                   # 
                   # 
                   # ## Survival ------
                   # tabPanel("Whole Population Survival",
                   #          tags$h3("KM Survival Analysis"),
                   #          tags$h5("For this study we also calculated overall survival using the kaplan meier method. The results contain the estimates (including median survival). risk tables and KM survival plots which are shown below...."),
                   #          tags$hr(),
                   #          tags$h5("Database and Study Outcome"),
                   #          div(style="display: inline-block;vertical-align:top; width: 150px;",
                   #              pickerInput(inputId = "survival_database_name_selector",
                   #                          label = "Database",
                   #                          choices = unique(survival_estimates_whole$Database),
                   #                          selected = unique(survival_estimates_whole$Database),
                   #                          options = list(
                   #                            `actions-box` = TRUE,
                   #                            size = 10,
                   #                            `selected-text-format` = "count > 3"),
                   #                          multiple = TRUE)
                   #          ),
                   #          div(style="display: inline-block;vertical-align:top; width: 150px;",
                   #              pickerInput(inputId = "survival_outcome_cohort_name_selector",
                   #                          label = "Outcome",
                   #                          choices = sort(unique(survival_estimates_whole$Cancer)),
                   #                          selected = c("Breast"),
                   #                          options = list(
                   #                            `actions-box` = TRUE,
                   #                            size = 10,
                   #                            `selected-text-format` = "count > 3"),
                   #                          multiple = TRUE)
                   #          )
                   #          ,
                   #          tags$hr(),
                   #          tags$h5("Population Settings"),
                   #          div(style="display: inline-block;vertical-align:top; width: 150px;",
                   #              pickerInput(inputId = "survival_age_group_selector",
                   #                          label = "Age group",
                   #                          choices = levels(survival_estimates_whole$Age),
                   #                          selected = "All",
                   #                          options = list(
                   #                            `actions-box` = TRUE,
                   #                            size = 10,
                   #                            `selected-text-format` = "count > 3"),
                   #                          multiple = TRUE)
                   #          ),
                   #          div(style="display: inline-block;vertical-align:top; width: 150px;",
                   #              pickerInput(inputId = "survival_sex_selector",
                   #                          label = "Sex",
                   #                          choices = unique(survival_estimates_whole$Sex),
                   #                          selected = "Both",
                   #                          options = list(
                   #                            `actions-box` = TRUE,
                   #                            size = 10,
                   #                            `selected-text-format` = "count > 3"),
                   #                          multiple = TRUE)
                   #          ),
                   #          tags$hr(),
                   #          # tags$h5("Analysis Settings"),
                   #          # div(style="display: inline-block;vertical-align:top; width: 150px;",
                   #          #     pickerInput(inputId = "calendar_year_selector",
                   #          #                 label = "Calendar Years",
                   #          #                 choices = "2000 to 2019",
                   #          #                 selected =  "2000 to 2019",
                   #          #                 options = list(
                   #          #                   `actions-box` = TRUE,
                   #          #                   size = 10,
                   #          #                   `selected-text-format` = "count > 3"),
                   #          #                 multiple = TRUE)
                   #          # ),
                   #          tabsetPanel(type = "tabs",
                   #                      tabPanel("Plot of KM survival curve",
                   #                               tags$hr(),
                   #                               tags$h5("Plotting Options"),
                   #                               div(style="display: inline-block;vertical-align:top; width: 150px;",
                   #                                   pickerInput(inputId = "time",
                   #                                               label = "X axis",
                   #                                               choices = c("time"),
                   #                                               selected = "time",
                   #                                               options = list(
                   #                                                 `actions-box` = TRUE,
                   #                                                 size = 10,
                   #                                                 `selected-text-format` = "count > 3"),
                   #                                               multiple = FALSE,)
                   #                               ),
                   #                               div(style="display: inline-block;vertical-align:top; width: 150px;",
                   #                                   pickerInput(inputId = "survival_plot_facet",
                   #                                               label = "Facet by",
                   #                                               choices = c("Cancer",
                   #                                                           "Database",
                   #                                                           "Sex",
                   #                                                           "Age"
                   #                                                           ),
                   #                                               selected = c("Cancer"),
                   #                                               options = list(
                   #                                                 `actions-box` = TRUE,
                   #                                                 size = 10,
                   #                                                 `selected-text-format` = "count > 3"),
                   #                                               multiple = TRUE,)
                   #                               ),
                   #                               div(style="display: inline-block;vertical-align:top; width: 150px;",
                   #                                   pickerInput(inputId = "survival_plot_group",
                   #                                               label = "Colour by",
                   #                                               choices = c("Sex",
                   #                                                           "Age",
                   #                                                           "Cancer",
                   #                                                           "Database"),
                   #                                               selected = c("Database"),
                   #                                               options = list(
                   #                                                 `actions-box` = TRUE,
                   #                                                 size = 10,
                   #                                                 `selected-text-format` = "count > 3"),
                   #                                               multiple = TRUE,)
                   #                               ),
                   #                               plotlyOutput('plot_survival_estimates', height = "800px") %>% withSpinner() ),
                   # 
                   # 
                   # 
                   #                      tabPanel("KM risk table",
                   #                               DTOutput('tbl_survival_risk_table') %>% withSpinner()),
                   #                      tabPanel("Median survival estimates",
                   #                               tags$hr(),
                   #                               DTOutput('tbl_survival_median_table') %>% withSpinner()),
                   #                      tabPanel("Survival Probabilities",
                   #                               DTOutput('tbl_survival_rates_table') %>% withSpinner()),
                   # 
                   #                      tabPanel("Survival Years Follow Up",
                   #                               DTOutput('tbl_survival_followup_table') %>% withSpinner())
                   # 
                   # 
                   # 
                   # 
                   #          )
                   # 
                   # ) ,
                   # 
                   # 

                    ## Population characteristics ------ 
                   tabPanel("Population Characteristics",	  
                            tags$h3("Study Population Characteristics"),
                            tags$h5("The population characteristics are shown for the different cancers with sex and age strata is below. For conditions any time prior from cancer diagnosis was used to capture conditions and for medications up to one year prior was used to capture medication utilisation."),
                            tags$hr(),
                            tags$h5("Study outcome") ,
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "table1_outcome_cohort_name_selector",
                                            label = "Cancer",
                                            choices = sort(unique(tableone_summary$group_level)),
                                            selected = c("Breastcancer"),
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = TRUE)



                            ),
                            
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "table1_strata_selector",
                                            label = "Strata",
                                            choices = sort(unique(tableone_summary$strata_level)),
                                            selected = c("Overall"),
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
                            
                            
                            
                   ) ,
                   
                    ## Population attrition ------                 
 tabPanel("Population attrition",	  
          tags$h3("Study Population Attrition"),
          tags$h5("Below is the attrition for each cancer showing how the final study numbers were obtained for the study."),
          tags$hr(),
          tags$h5("Cancer") ,
          div(style="display: inline-block;vertical-align:top; width: 150px;",
              pickerInput(inputId = "attrition_cohort_name_selector",
                          label = "Cancer",
                          choices = sort(unique(cohort_attrition$cohort_name)),
                          selected = c("IncidentBreastCancer"),
                          options = list(
                            `actions-box` = TRUE,
                            size = 10,
                            `selected-text-format` = "count > 3"),
                          multiple = TRUE)
              
              
              
          ),
          
          
          tabsetPanel(type = "tabs",
                      tabPanel("Study Population Attrition", 
                               DTOutput('tbl_table_attrition') %>% withSpinner()
                               
                      )
                      
          )
          
          
          
 ) ,
 
                   
                    ## Database Information CDM snapshot ------                 
 tabPanel("Database Information",	  
          tags$h3("CDM database information"),
          tags$h5("Below is the CDM snapshot for each database included in this study."),
          tags$hr(),
          DTOutput('tbl_database_info')
 ) 
 
 
                   
 # close -----
                 ))
