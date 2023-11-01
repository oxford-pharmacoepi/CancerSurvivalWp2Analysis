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
                 titlePanel("Overall survival and extrapolation for common cancers: a multinational cohort study"),
                 
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

                        tags$h5("The results of this study are published in the following journals:"
                                ),
 tags$ol(
   tags$li(strong("TBC"),"(",tags$a(href="https://www.ndorms.ox.ac.uk/research/research-groups/Musculoskeletal-Pharmacoepidemiology","Paper Link"),")" ),
    tags$li(strong("TBC"),"(",tags$a(href="https://www.ndorms.ox.ac.uk/research/research-groups/Musculoskeletal-Pharmacoepidemiology","Paper Link"),")" )),

 tags$h5("The analysis code used to generate these results can be found",
         tags$a(href="https://github.com/oxford-pharmacoepi", "here"),
         ".The cohort diagnostics including the clinical codelists for each of the 8 cancers can be found",
         tags$a(href="https://dpa-pde-oxford.shinyapps.io/CancerExtrapolationDiagnostics/", "here")
         
         ),
 
 tags$h5("Any questions regarding these results or problems with this shiny app please contact",
         tags$a(href="mailto:danielle.newby@ndorms.ox.ac.uk", "Danielle Newby")
         
 ),
 
 
                            tags$hr()
 
 
                   ), 



## KM ------                 
tabPanel("Population Survival",	  
         tags$h3("Study Population Survival"),
         tags$h5("This section contains the overall survival for each cancer for each database. Kaplan-Meier plots, median and mean survival are also presented as well as survival at one, five and ten years for the whole population and stratified by sex and age group."),
         tags$hr(),
         tags$h5("Attributes") ,
         div(style="display: inline-block;vertical-align:top; width: 150px;",
             pickerInput(inputId = "km_database_name_selector",
                         label = "Database",
                         choices = unique(survival_km$Database),
                         selected = unique(survival_km$Database),
                         options = list(
                           `actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
                         multiple = TRUE)
         ),
         
         div(style="display: inline-block;vertical-align:top; width: 150px;",
             pickerInput(inputId = "km_outcome_cohort_name_selector",
                         label = "Cancer",
                         choices = unique(survival_km$Cancer),
                         selected = c("Breast"),
                         options = list(
                           `actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
                         multiple = TRUE)
         ),      
         
         div(style="display: inline-block;vertical-align:top; width: 150px;",
             pickerInput(inputId = "km_sex_selector",
                         label = "Sex",
                         choices = unique(survival_km$Sex),
                         selected = c("Both"),
                         options = list(
                           `actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
                         multiple = TRUE)
         ),    
         
         div(style="display: inline-block;vertical-align:top; width: 150px;",
             pickerInput(inputId = "km_age_group_selector",
                         label = "Age",
                         choices = unique(survival_km$Age),
                         selected = c("All"),
                         options = list(
                           `actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
                         multiple = TRUE)
         ),    
         
         tags$hr(),
         tabsetPanel(type = "tabs",
                     tabPanel("Plot of KM survival curve",
                              tags$hr(),
                              tags$h5("Plotting Options"),
                              div(style="display: inline-block;vertical-align:top; width: 150px;",
                                  pickerInput(inputId = "km_plot_facet",
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
                                  pickerInput(inputId = "km_plot_group",
                                              label = "Colour by",
                                              choices = c("Cancer",
                                                          "Database",
                                                          "Age",
                                                          "Sex"),
                                              selected = c("Database"),
                                              options = list(
                                                `actions-box` = TRUE,
                                                size = 10,
                                                `selected-text-format` = "count > 3"),
                                              multiple = TRUE,)
                              ),
                              plotlyOutput('plot_km', height = "800px") %>% withSpinner() ),
                     
                     tabPanel("Plot of hazard over time curve",
                              tags$hr(),
                              tags$h5("Plotting Options"),
                              div(style="display: inline-block;vertical-align:top; width: 150px;",
                                  pickerInput(inputId = "km_plot_facet",
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
                                  pickerInput(inputId = "km_plot_group",
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
                              plotlyOutput('plot_hot_km', height = "800px") %>% withSpinner() ),                    
                     
                     tabPanel("KM risk table",
                              DTOutput('tbl_survival_risk_table') %>% withSpinner()),
                     
                     tabPanel("Median survival estimates",
                              DTOutput('tbl_survival_median_table') %>% withSpinner()),
                     
                     tabPanel("Survival Probabilities",
                              tags$hr(),
                              div(style="display: inline-block;vertical-align:top; width: 150px;",
                                  pickerInput(inputId = "km_time_selector",
                                              label = "Filter time",
                                              choices = unique(survival_probabilities$time),
                                              selected = unique(survival_probabilities$time),
                                              options = list(
                                                `actions-box` = TRUE,
                                                size = 10,
                                                `selected-text-format` = "count > 3"),
                                              multiple = TRUE,)
                              ),
                              DTOutput('tbl_survival_probs_table') %>% withSpinner())
                     
                     
         )
         
) ,
 
## Survival extrapolation EXTRAPOLATION ------
 tabPanel("Population Survival Extrapolations: Stratification",
          tags$h3("Stratified Extrapolation Analysis"),
          tags$h5("For this analysis we used nine extrapolation methods to model the observed survival. Below are the predicted survival curves, hazard over time, median and mean survival and survival at one, five and ten years. The analysis was performed on the whole database and stratified by sex and age "),
          tags$hr(),
          tags$h5("Attributes"),
          div(style="display: inline-block;vertical-align:top; width: 150px;",
              pickerInput(inputId = "survival_database_name_selector",
                          label = "Database",
                          choices = unique(survival_est_strat$Database),
                          selected = unique(survival_est_strat$Database),
                          options = list(
                            `actions-box` = TRUE,
                            size = 10,
                            `selected-text-format` = "count > 3"),
                          multiple = TRUE)
          ),
          div(style="display: inline-block;vertical-align:top; width: 150px;",
              pickerInput(inputId = "survival_outcome_cohort_name_selector",
                          label = "Cancer",
                          choices = sort(unique(survival_est_strat$Cancer)),
                          selected = c("Breast"),
                          options = list(
                            `actions-box` = TRUE,
                            size = 10,
                            `selected-text-format` = "count > 3"),
                          multiple = TRUE)
          ),

          div(style="display: inline-block;vertical-align:top; width: 150px;",
              pickerInput(inputId = "survival_sex_selector",
                          label = "Sex",
                          choices = unique(survival_est_strat$Sex),
                          selected = "Both",
                          options = list(
                            `actions-box` = TRUE,
                            size = 10,
                            `selected-text-format` = "count > 3"),
                          multiple = TRUE)
          ),
          
          div(style="display: inline-block;vertical-align:top; width: 150px;",
              pickerInput(inputId = "survival_age_group_selector",
                          label = "Age",
                          choices = unique(survival_est_strat$Age),
                          selected = "All",
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
                                                           "Method"),
                                               selected = c("Database", "Method"),
                                               options = list(
                                                 `actions-box` = TRUE,
                                                 size = 10,
                                                 `selected-text-format` = "count > 3"),
                                               multiple = TRUE,)
                               ),
                               plotlyOutput('plot_survival_estimates', height = "800px") %>% withSpinner() ),
                      
                      tabPanel("Goodness of Fit" , 
                      DTOutput('tbl_gof') %>% withSpinner()),

                      tabPanel("Extrapolation Parameters", 
                      DTOutput('tbl_parameters') %>% withSpinner()
                      
                      ),
                      
                      
                      



          )

 ) ,


## Population characteristics ------ 
                   tabPanel("Population Characteristics",	  
                            tags$h3("Study Population Characteristics"),
                            tags$h5("The population characteristics are shown for the different cancers with sex and age strata is below. For databases with information on prior history for conditions and medication, for conditions any time prior from cancer diagnosis was used to capture conditions and for medications up to one year prior was used to capture medication utilisation."),
                            tags$hr(),
                            tags$h5("Attributes") ,
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "table1_outcome_cohort_name_selector",
                                            label = "Cancer",
                                            choices = sort(unique(tableone_summary$Cancer)),
                                            selected = c("Breast"),
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = FALSE)


                            ),
                            
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "table1_database_selector",
                                            label = "Database",
                                            choices = unique(tableone_summary$Database),
                                            selected = unique(tableone_summary$Database),
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = TRUE)
                            ),
                            
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "table1_sex_selector",
                                            label = "Sex",
                                            choices = sort(unique(tableone_summary$Sex)),
                                            selected = c("Both"),
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = TRUE)
                            ),

                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "table1_age_selector",
                                            label = "Age",
                                            choices = sort(unique(tableone_summary$Age)),
                                            selected = c("All"),
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
 tabPanel("Population Attrition",	  
          tags$h3("Study Population Attrition"),
          tags$h5("Below is the attrition for each cancer showing how the final study numbers were obtained for the study."),
          tags$hr(),
          tags$h5("Attributes") ,
          div(style="display: inline-block;vertical-align:top; width: 150px;",
              pickerInput(inputId = "attrition_cohort_name_selector",
                          label = "Cancer",
                          choices = sort(unique(cohort_attrition$Cancer)),
                          selected = c("Breast"),
                          options = list(
                            `actions-box` = TRUE,
                            size = 10,
                            `selected-text-format` = "count > 3"),
                          multiple = TRUE)
              
              
              
          ),
          
          div(style="display: inline-block;vertical-align:top; width: 150px;",
              pickerInput(inputId = "attrition_database_name_selector",
                          label = "Database",
                          choices = unique(cohort_attrition$Database),
                          selected = unique(cohort_attrition$Database),
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
