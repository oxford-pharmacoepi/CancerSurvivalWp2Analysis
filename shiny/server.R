#### SERVER ------
server <-	function(input, output, session) {
  
# survival estimates: km only
  get_km <- reactive({
    
    #table <- surv_prob_km %>%
    table <- survival_km %>%
      filter(Database %in% input$km_database_name_selector)  %>%
      filter(Cancer %in% input$km_outcome_cohort_name_selector) %>%
      filter(Age %in% input$km_age_group_selector)     %>%
      filter(Sex %in% input$km_sex_selector)      
    
    table
  })
  output$tbl_km <-  DT::renderDataTable({
    
    table <- get_km()
    
    validate(need(ncol(table)>1,
                  "No results for selected inputs"))
    
    table <- table %>%
      mutate(time=nice.num3(time)) %>%
      mutate(est=nice.num3(est)) %>%
      mutate(ucl=nice.num3(ucl)) %>%
      mutate(lcl=nice.num3(lcl)) %>%
      relocate(Cancer) %>%
      relocate(Estimate = est, .after = time) %>%
      relocate(upperCI = ucl, .after = Estimate) %>%
      relocate(lowerCI = lcl, .after = upperCI)
    
    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'tB',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv",
                                                 text = "Download results as csv",
                                                 filename = "km_survival_estimates"))
              ))
  } )
  output$plot_km <- renderPlotly({
    table <- get_km()
    validate(need(ncol(table) > 1, "No results for selected inputs"))
    
    p <- NULL  # Initialize an empty ggplot object
    
    if (is.null(input$km_plot_group)) {
      if (!is.null(input$km_plot_facet)) {
        p <- table %>%
          unite("facet_var", c(all_of(input$km_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = facet_var)) +
          geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = facet_var), alpha = 0.3) +
          geom_line() +
          xlab("Time (Years)") +
          ylab("Survival Probability") +
          facet_wrap(vars(facet_var), ncol = 2) +
          scale_y_continuous(limits = c(0, NA)) +
          theme_bw()
      } else {
        p <- table %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl)) +
          geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = "blue"), alpha = 0.3) +
          xlab("Time (Years)") +
          ylab("Survival Probability") +
          scale_y_continuous(limits = c(0, NA)) +
          theme_bw()
      }
    }
    
    if (!is.null(input$km_plot_group)) {
      if (!is.null(input$km_plot_facet)) {
        p <- table %>%
          unite("Group", c(all_of(input$km_plot_group)), remove = FALSE, sep = "; ") %>%
          unite("facet_var", c(all_of(input$km_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Group, colour = Group), alpha = 0.3) +
          geom_line() +
          xlab("Time (Years)") +
          ylab("Survival Probability") +
          facet_wrap(vars(facet_var), ncol = 2) +
          scale_y_continuous(limits = c(0, NA)) +
          theme_bw()
      } else {
        p <- table %>%
          unite("Group", c(all_of(input$km_plot_group)), remove = FALSE, sep = "; ") %>%
          mutate(Group = factor(Group)) +
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Group, colour = Group), alpha = 0.3) +
          geom_line() +
          xlab("Time (Years)") +
          ylab("Survival Probability") +
          scale_y_continuous(limits = c(0, NA)) +
          theme_bw()
      }
    }
    
    p
  })
  
  
# km haz over time plot no extrapolations
  get_hot_km <- reactive({
    
    table <- hot_km %>%
      filter(Database %in% input$km_database_name_selector)  %>%
      filter(Cancer %in% input$km_outcome_cohort_name_selector) %>%
      filter(Age %in% input$km_age_group_selector)     %>%
      filter(Sex %in% input$km_sex_selector)      
    
    table
  })
  output$plot_hot_km <- renderPlotly({
    table <- get_hot_km()
    validate(need(ncol(table) > 1, "No results for selected inputs"))
    
    p <- NULL  # Initialize an empty ggplot object
    
    if (is.null(input$km_plot_group)) {
      if (!is.null(input$km_plot_facet)) {
        p <- table %>%
          unite("facet_var", c(all_of(input$km_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = facet_var)) +
          geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = facet_var), alpha = 0.5) +
          geom_line() +
          facet_wrap(vars(facet_var), ncol = 2) +
          scale_y_continuous(limits = c(0, NA)) +
          theme_bw()
      } else {
        p <- table %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl)) +
          geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = "blue"), alpha = 0.5) +
          scale_y_continuous(limits = c(0, NA)) +
          theme_bw()
      }
    }
    
    if (!is.null(input$km_plot_group)) {
      if (!is.null(input$km_plot_facet)) {
        p <- table %>%
          unite("Group", c(all_of(input$km_plot_group)), remove = FALSE, sep = "; ") %>%
          unite("facet_var", c(all_of(input$km_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Group, colour = Group), alpha = 0.5) +
          geom_line() +
          facet_wrap(vars(facet_var), ncol = 2) +
          scale_y_continuous(limits = c(0, NA)) +
          theme_bw()
      } else {
        p <- table %>%
          unite("Group", c(all_of(input$km_plot_group)), remove = FALSE, sep = "; ") %>%
          mutate(Group = factor(Group)) +
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Group, colour = Group), alpha = 0.5) +
          geom_line() +
          scale_y_continuous(limits = c(0, NA)) +
          theme_bw()
      }
    }
    
    p
  })
  
# survival estimates: whole population with extrapolations STRATIFICATION
  get_survival_estimates<-reactive({

    table <- survival_est_strat %>%
      filter(Database %in% input$survival_database_name_selector)  %>%
      filter(Cancer %in% input$survival_outcome_cohort_name_selector) %>%
      filter(Age %in% input$survival_age_group_selector)     %>%
      filter(Sex %in% input$survival_sex_selector)    

    table
  })
  output$tbl_survival_estimates<-  DT::renderDataTable({

    table<-get_survival_estimates()

    validate(need(ncol(table)>1,
                  "No results for selected inputs"))

    table <- table %>%
      mutate(time=nice.num3(time)) %>%
      mutate(est=nice.num3(est)) %>%
      mutate(ucl=nice.num3(ucl)) %>%
      mutate(lcl=nice.num3(lcl)) %>%
      relocate(Cancer) %>%
      relocate(Estimate = est, .after = time) %>%
      relocate(upperCI = ucl, .after = Estimate) %>%
      relocate(lowerCI = lcl, .after = upperCI)

    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'tB',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv",
                                                 text = "Download results as csv",
                                                 filename = "survival_estimates"))
              ))
  } )

# KM plots with extrapolations STRATIFICATION
  output$plot_survival_estimates<- renderPlotly({

    table<-get_survival_estimates()
    validate(need(ncol(table)>1,
                  "No results for selected inputs"))

    if(is.null(input$survival_plot_group)){
      if(!is.null(input$survival_plot_facet)){
        p<-table %>%
          unite("facet_var",
                c(all_of(input$survival_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes_string(x="time", y="est",
                            ymin = "lcl",
                            ymax = "ucl")) +
          geom_line() +
          xlab("Time (Years)") +
          ylab("Survival Probability") +
          facet_wrap(vars(facet_var),ncol = 2)+
          scale_y_continuous(
            limits = c(0, NA)
          ) +
          theme_bw()
      } else{
        p<-table %>%
          ggplot(aes_string(x="time", y="est",
                            ymin = "lcl",
                            ymax = "ucl")) +
          xlab("Time (Years)") +
          ylab("Survival Probability") +
          scale_y_continuous(
            limits = c(0, NA)
          ) +
          theme_bw()
      }
    }


    if(!is.null(input$survival_plot_group) ){

      if(is.null(input$survival_plot_facet) ){
        p<-table %>%
          unite("Group",
                c(all_of(input$survival_plot_group)), remove = FALSE, sep = "; ") %>%
          ggplot(aes_string(x="time", y="est",
                            ymin = "lcl",
                            ymax = "ucl",
                            group="Group",
                            colour="Group")) +
          geom_line() +
          xlab("Time (Years)") +
          ylab("Survival Probability") +
          theme_bw()
      }

      if(!is.null(input$survival_plot_facet) ){
        if(!is.null(input$survival_plot_group) ){
          p<-table %>%
            unite("Group",
                  c(all_of(input$survival_plot_group)), remove = FALSE, sep = "; ") %>%
            unite("facet_var",
                  c(all_of(input$survival_plot_facet)), remove = FALSE, sep = "; ") %>%
            ggplot(aes_string(x="time", y="est",
                              ymin = "lcl",
                              ymax = "ucl",
                              group="Group",
                              colour="Group")) +
            xlab("Time (Years)") +
            ylab("Survival Probability") +
            geom_line() +
            facet_wrap(vars(facet_var),ncol = 2)+
            scale_y_continuous(
              limits = c(0, NA)
            )  +
            theme_bw()
        }
      }

    }

    p

  })

# risk table KM
  get_survival_risktable<-reactive({

    table<-risk_table_results %>%
      # first deselect settings which did not vary for this study
      select(!c(Method, Adjustment, Stratification)) %>% 
      filter(Database %in% input$km_database_name_selector)  %>%
      filter(Cancer %in% input$km_outcome_cohort_name_selector) %>%
      filter(Age %in% input$km_age_group_selector)     %>%
      filter(Sex %in% input$km_sex_selector)    

    table
  })
  output$tbl_survival_risk_table<-  DT::renderDataTable({

    table<-get_survival_risktable()

    validate(need(ncol(table)>1,
                  "No results for selected inputs"))

    table <- table %>% 
      relocate(Cancer)

    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'tB',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv",
                                                 text = "Download results as csv",
                                                 filename = "survival_risk_table"))
              ))
  } )
 
# median/mean survival KM
  get_survival_median_table <- reactive({
    
    table <- med_surv_km %>%
      # first deselect settings which did not vary for this study
      select(!c(Method, Stratification)) %>% 
      filter(Database %in% input$km_database_name_selector)  %>%
      filter(Cancer %in% input$km_outcome_cohort_name_selector) %>%
      filter(Age %in% input$km_age_group_selector)     %>%
      filter(Sex %in% input$km_sex_selector)    
    
    table
  })
  output$tbl_survival_median_table <-  DT::renderDataTable({
    
    table <- get_survival_median_table()
    
    validate(need(ncol(table)>1,
                  "No results for selected inputs"))
    
    table <- table %>% 
    mutate(rmean=nice.num2(rmean)) %>%
    mutate(`se(rmean)`=nice.num2(`se(rmean)`)) %>%
    mutate(median=nice.num2(median)) %>%
    mutate(`0.95LCL`=nice.num2(`0.95LCL`)) %>%
    mutate(`0.95UCL`=nice.num2(`0.95UCL`)) %>%
    relocate(Cancer) %>% 
    mutate(median= ifelse(!is.na(median),
                              paste0(median, " (",
                                     `0.95LCL`," to ", 
                                     `0.95UCL`, ")")),
           rmean = ifelse(!is.na(rmean),
                          paste0(rmean, " (",
                                 `se(rmean)`, ")"))
           
           
           ) %>% 
      rename(`Median Survival in Years (95%CI)` = median) %>% 
      rename(`RMean Survival in Years (SE)` = rmean) %>% 
      select(-c(`0.95LCL`, `0.95UCL`, `se(rmean)` ))
    
    
    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'tB',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv",
                                                 text = "Download results as csv",
                                                 filename = "median_survival_table"))
              ))
  } )

# survival probabilities for 1, 5 and 10 years KM
  get_surv_prob_table <-reactive({
    
    table <- surv_prob_km %>%
      # first deselect settings which did not vary for this study
      filter(Database %in% input$km_database_name_selector)  %>%
      filter(Cancer %in% input$km_outcome_cohort_name_selector) %>%
      filter(Age %in% input$km_age_group_selector)     %>%
      filter(Sex %in% input$km_sex_selector)    %>% 
      filter(time %in% input$km_time_selector )
    
    table
  })
  output$tbl_survival_probs_table <-  DT::renderDataTable({
    
    table <- get_surv_prob_table()
    
    validate(need(ncol(table)>1,
                  "No results for selected inputs"))
    
    table <- table %>% 
    mutate(surv=nice.num(surv)) %>%
      mutate(lower=nice.num(lower)) %>%
      mutate(upper=nice.num(upper)) %>% 
      select(!c(surv, lower, upper, Method, Stratification)) %>% 
     rename(`Time (years)` = time)
      
    
    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'tB',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv",
                                                 text = "Download results as csv",
                                                 filename = "survprobs_table"))
              ))
  } )

# table 1
  get_table_one <-reactive({
    
    table <- tableone_summary %>% 
      filter(Cancer %in% input$table1_outcome_cohort_name_selector) %>% 
      filter(Sex %in% input$table1_sex_selector) %>% 
      filter(Age %in% input$table1_age_selector) %>% 
      filter(Database %in% input$table1_database_selector)
    
    table
  }) 
  output$tbl_table_one <-  DT::renderDataTable({
    
    table <- get_table_one()
    
    validate(need(ncol(table)>1,
                  "No results for selected inputs"))
    
    table <- table %>%
      select(!c(Stratification )) %>% 
      filter(Description != "Antineoplastic agents n (%)")
    
    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'tB',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv", 
                                                 text = "Download results as csv",
                                                 filename = "table_one"))
              ))
  } )
  
# table for extrapolation parameters
  get_parameters <-reactive({
    
    table <- extrapolation_parameters %>% 
      filter(Database %in% input$survival_database_name_selector)  %>%
      filter(Cancer %in% input$survival_outcome_cohort_name_selector) %>%
      filter(Age %in% input$survival_age_group_selector)  %>%
      filter(Sex %in% input$survival_sex_selector) 
    
    table
  }) 
  output$tbl_parameters <-  DT::renderDataTable({
    
    table <- get_parameters()
    
    validate(need(ncol(table)>1,
                  "No results for selected inputs"))
    
    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'tB',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv", 
                                                 text = "Download results as csv",
                                                 filename = "extrapolation_paramters"))
              ))
  } ) 
  
  
# gof fit results
  get_gof <-reactive({
    
    table <- goodness_of_fit_results %>% 
      filter(Database %in% input$survival_database_name_selector)  %>%
      filter(Cancer %in% input$survival_outcome_cohort_name_selector) %>%
      filter(Age %in% input$survival_age_group_selector)  %>%
      filter(Sex %in% input$survival_sex_selector) 
    
    table
  }) 
  output$tbl_gof <-  DT::renderDataTable({
    
    table <- get_gof()
    
    validate(need(ncol(table)>1,
                  "No results for selected inputs"))
    
    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'tB',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv", 
                                                 text = "Download results as csv",
                                                 filename = "GOF_results"))
              ))
  } ) 
  
  
  
# table for cohort attrition
  get_table_attrition <-reactive({
    
    table <- cohort_attrition %>% 
      filter(Cancer %in% input$attrition_cohort_name_selector) %>% 
    filter(Database %in% input$attrition_database_name_selector) 
    
    table
  }) 
  output$tbl_table_attrition <-  DT::renderDataTable({
    
    table <- get_table_attrition()
    
    validate(need(ncol(table)>1,
                  "No results for selected inputs"))
    
    table <- table %>%
      select(!c("cohort_definition_id" )) 
    
    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'tB',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv", 
                                                 text = "Download results as csv",
                                                 filename = "cohortAttrition"))
              ))
  } )
  
# table for cdm snapshot
  get_database_info <-reactive({
    
    table <- cdm_snapshot 
    
    table
  }) 
  output$tbl_database_info <-  DT::renderDataTable({
    
    table <- get_database_info()
    
    validate(need(ncol(table)>1,
                  "No results for selected inputs"))
    
    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'tB',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv", 
                                                 text = "Download results as csv",
                                                 filename = "CDMsnapshot"))
              ))
  } )

  
  
  
   
}