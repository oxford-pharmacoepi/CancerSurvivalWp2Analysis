# functions for plots

library(dplyr)
library(tidyr)
library(stringr)
library(here)
library(quarto)
library(ggplot2)
library(scales)
library(ggh4x)

# SURVIVAL
#survival figure1 whole population and stratified by database
survivalFigure1 <- function(survivalData) {
  
  survivalFigureData <- survivalData %>%
    filter(Stratification == "None") %>%
    filter(CalendarYearGp == "2000 to 2019") %>%
    ggplot(aes(x = time,
               y = est,
               group = Database,
               col = Database )) +
    scale_y_continuous( labels = scales::percent, limits = c(0, NA)) +
    scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
    scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
    geom_ribbon(aes(ymin = lcl, 
                    ymax = ucl, 
                    fill = Database), alpha = .15, color = NA, show.legend = FALSE) +
    geom_line(aes(linetype = Database),size = 0.5) +
    scale_linetype_manual(values = c("solid", "dashed", "twodash","dotted")) +
    labs(x = "Time (Years)",
         y = "Survival Probability",
         col = "Database name",
         linetype = "Database name") +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
          strip.background = element_rect(color = "black", size = 0.6) ,
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.key = element_rect(fill = "transparent", colour = "transparent")) +
    scale_x_continuous(breaks=seq(0, 20, 2))
  
  return(survivalFigureData)
  
}

#survival figure3 age stratification and stratified by database (AGE AS FACETS) for both genders
survivalFigure3a <- function(survivalData) {
  
  survivalFigureData <- survivalData %>%
    filter(Age != "All") %>%
    filter(CalendarYearGp == "2000 to 2019") %>%
    filter(Gender == "Both") %>%
    ggplot(aes(x = time,
               y = est,
               group = Database,
               col = Database )) +
    scale_y_continuous( labels = scales::percent, limits = c(0, NA)) +
    scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
    scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
    geom_ribbon(aes(ymin = lcl, 
                    ymax = ucl, 
                    fill = Database), alpha = .15, color = NA, show.legend = FALSE) +
    geom_line(aes(linetype = Database),size = 0.5) +
    scale_linetype_manual(values = c("solid", "dashed", "twodash","dotted")) +
    labs(x = "Time (Years)",
         y = "Survival Probability",
         col = "Database name",
         linetype = "Database name") +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
          strip.background = element_rect(color = "black", size = 0.6) ,
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.key = element_rect(fill = "transparent", colour = "transparent")) +
    scale_x_continuous(breaks=seq(0, 20, 2)) +
    facet_wrap(~ Age, ncol = 2, scales = "free_x") +
    coord_cartesian(xlim = c(0, 22))
  
  
  return(survivalFigureData)
}

#survival figure3 age stratification and stratified by database (AGE AS FACETS) for 1 gender
survivalFigure3b <- function(survivalData) {
  
  survivalFigureData <- survivalData %>%
    filter(Age != "All") %>%
    filter(CalendarYearGp == "2000 to 2019") %>%
    ggplot(aes(x = time,
               y = est,
               group = Database,
               col = Database )) +
    scale_y_continuous( labels = scales::percent, limits = c(0, NA)) +
    scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
    scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
    geom_ribbon(aes(ymin = lcl, 
                    ymax = ucl, 
                    fill = Database), alpha = .15, color = NA, show.legend = FALSE) +
    geom_line(aes(linetype = Database),size = 0.5) +
    scale_linetype_manual(values = c("solid", "dashed", "twodash","dotted")) +
    labs(x = "Time (Years)",
         y = "Survival Probability",
         col = "Database name",
         linetype = "Database name") +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
          strip.background = element_rect(color = "black", size = 0.6) ,
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.key = element_rect(fill = "transparent", colour = "transparent")) +
    scale_x_continuous(breaks=seq(0, 20, 2)) +
    facet_wrap(~ Age, ncol = 2, scales = "free_x") +
    coord_cartesian(xlim = c(0, 22))
  
  
  return(survivalFigureData)
}

#survival figure4 whole population and gender stratification by database
survivalFigure4 <- function(survivalData) {
  
  survivalFigureData <- survivalData %>%
    filter(Age == "All") %>%
    filter(CalendarYearGp == "2000 to 2019" | CalendarYearGp == "2000 to 2021") %>%
    ggplot(aes(x = time,
               y = est,
               group = Database,
               col = Database )) +
    scale_y_continuous( labels = scales::percent, limits = c(0, NA)) +
    scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
    scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
    geom_ribbon(aes(ymin = lcl, 
                    ymax = ucl, 
                    fill = Database), alpha = .15, color = NA, show.legend = FALSE) +
    geom_line(aes(linetype = Database),size = 0.5) +
    scale_linetype_manual(values = c("solid", "dashed", "twodash","dotted")) +
    labs(x = "Time (Years)",
         y = "Survival Probability",
         col = "Database name",
         linetype = "Database name") +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
          strip.background = element_rect(color = "black", size = 0.6) ,
          panel.background = element_blank() ,
          legend.position='bottom',
          legend.box.spacing = unit(0, "pt") ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.key = element_rect(fill = "transparent", colour = "transparent")) +
    scale_x_continuous(breaks=seq(0, 22, 2)) +
    facet_grid(cols = vars(Gender)) 
  
  return(survivalFigureData)
  
}

# survival figure 5 whole population and gender strat with calendar year strat (puts gender as column and database as rows)
survivalFigure5 <- function(survivalData) {
  
  survivalFigureData <- survivalData %>%
    filter(Stratification == "None"| Stratification == "Gender") %>%
    filter(CalendarYearGp != "2000 to 2019") %>%
    filter(CalendarYearGp != "2000 to 2021") %>%
    
    ggplot(aes(x = time,
               y = est,
               group = CalendarYearGp,
               col = CalendarYearGp )) +
    scale_y_continuous( labels = label_percent() ) +
    scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark red, gry
    scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
    geom_line(aes(linetype = CalendarYearGp),size = 0.5) +
    scale_linetype_manual(values = c("dotted","dashed", "dotdash", "twodash", "solid", "longdash")) +
    geom_ribbon(aes(ymin = lcl, 
                    ymax = ucl, 
                    fill = CalendarYearGp), alpha = .1, color = NA, show.legend = FALSE) +
    labs(x = "Time (Years)",
         y = "Survival Probability",
         col = "Calendar Year Group",
         linetype = "Calendar Year Group") +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
          strip.background = element_rect(color = "black", size = 0.6) ,
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.box.spacing = unit(0, "pt") ,
          legend.position='bottom',
          legend.key = element_rect(fill = "transparent", colour = "transparent")) +
    ggh4x::facet_grid2(cols = vars(Gender), vars(Database), scales="free", independent = "y") 
  
  
  return(survivalFigureData)
  
}

# survival figure 5 whole population and gender strat with calendar year strat (puts gender as column and database as rows) - short term survival
survivalFigure5a <- function(survivalData) {
  
  survivalFigureData <- survivalData %>%
    filter(Stratification == "None"| Stratification == "Gender") %>%
    filter(CalendarYearGp != "2000 to 2019") %>%
    filter(CalendarYearGp != "2000 to 2021") %>%
    
    ggplot(aes(x = time,
               y = est,
               group = CalendarYearGp,
               col = CalendarYearGp )) +
    scale_y_continuous( labels = label_percent() ) +
    scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark red, gry
    scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
    geom_line(aes(linetype = CalendarYearGp),size = 0.5) +
    scale_linetype_manual(values = c("dotted","dashed", "dotdash", "twodash", "solid", "longdash")) +
    geom_ribbon(aes(ymin = lcl, 
                    ymax = ucl, 
                    fill = CalendarYearGp), alpha = .1, color = NA, show.legend = FALSE) +
    labs(x = "Time (Years)",
         y = "Survival Probability",
         col = "Calendar Year Group",
         linetype = "Calendar Year Group") +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
          strip.background = element_rect(color = "black", size = 0.6) ,
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.box.spacing = unit(0, "pt") ,
          legend.position='bottom',
          legend.key = element_rect(fill = "transparent", colour = "transparent")) +
    xlim(0, 1) +
    ggh4x::facet_grid2(cols = vars(Gender), vars(Database), scales="free_y", independent = "y") 
  
  return(survivalFigureData)
  
}

# survival figure 6 whole population and gender strat with calendar year strat BUT puts database as columns and gender as rows
survivalFigure6 <- function(survivalData) {
  
  survivalFigureData <- survivalData %>%
    filter(Stratification == "None"| Stratification == "Gender") %>%
    filter(CalendarYearGp != "2000 to 2019") %>%
    filter(CalendarYearGp != "2000 to 2021") %>%
    ggplot(aes(x = time,
               y = est,
               group = CalendarYearGp,
               col = CalendarYearGp )) +
    scale_y_continuous( labels = label_percent() ) +
    scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark red, gry
    scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
    geom_line(aes(linetype = CalendarYearGp),size = 0.5) +
    scale_linetype_manual(values = c("dotted","dashed", "dotdash", "solid")) +
    labs(x = "Time (Years)",
         y = "Survival Probability",
         col = "Calendar Year Group",
         linetype = "Calendar Year Group") +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
          strip.background = element_rect(color = "black", size = 0.6) ,
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.key = element_rect(fill = "transparent", colour = "transparent")) +
    ggh4x::facet_grid2(cols = vars(Database) ,vars(Gender), scales="free", independent = "y") 
  
  
  return(survivalFigureData)
  
}

# survival figure 7 whole population only GOLD calendar time effects
survivalFigure7 <- function(survivalData) {
  
  survivalFigureData <- survivalData %>%
    # #filter(Stratification == "None") %>%
    # filter(CalendarYearGp != "2000 to 2019") %>%
    # filter(CalendarYearGp != "2000 to 2021") %>%
    # filter(Database == "CPRD GOLD") %>% 
    ggplot(aes(x = time,
               y = est,
               group = CalendarYearGp,
               col = CalendarYearGp )) +
    scale_y_continuous( labels = label_percent() ) +
    scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark red, gry
    scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
    geom_line(aes(linetype = CalendarYearGp),size = 0.5) +
    scale_linetype_manual(values = c("dotted","dashed", "dotdash", "twodash","solid", "longdash")) +
    geom_ribbon(aes(ymin = lcl, 
                    ymax = ucl, 
                    fill = CalendarYearGp), alpha = .15, color = NA, show.legend = FALSE) +
    labs(x = "Time (Years)",
         y = "Survival Probability",
         col = "Calendar Year Group",
         linetype = "Calendar Year Group") +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
          strip.background = element_rect(color = "black", size = 0.6) ,
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.box.spacing = unit(0, "pt") ,
          legend.position='bottom',
          legend.key = element_rect(fill = "transparent", colour = "transparent")) +
    xlim(0, 2) +
    facet_grid(cols = vars(Cancer)) 
  
  return(survivalFigureData)
  
}

# survival figure 8 whole population only GOLD calendar time effects with new colours
survivalFigure8 <- function(survivalData) {
  
  survivalFigureData <- survivalData %>%
    # #filter(Stratification == "None") %>%
    # filter(CalendarYearGp != "2000 to 2019") %>%
    # filter(CalendarYearGp != "2000 to 2021") %>%
    # filter(Database == "CPRD GOLD") %>% 
    ggplot(aes(x = time,
               y = est,
               group = CalendarYearGp,
               col = CalendarYearGp )) +
    scale_y_continuous( labels = label_percent() ) +
    scale_colour_manual(values = c("black", "black", "black", "black", "#ED0000FF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark red, gry
    scale_fill_manual(values = c("black", "black", "black", "black", "#ED0000FF", "#FDAF91FF", "#AD002AFF", "grey")) +
    geom_line(aes(linetype = CalendarYearGp),size = 0.5) +
    scale_linetype_manual(values = c("dotted","dashed", "dotdash", "twodash","solid", "longdash")) +
    geom_ribbon(aes(ymin = lcl, 
                    ymax = ucl, 
                    fill = CalendarYearGp), alpha = .15, color = NA, show.legend = FALSE) +
    labs(x = "Time (Years)",
         y = "Survival Probability",
         col = "Calendar Year Group",
         linetype = "Calendar Year Group") +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
          strip.background = element_rect(color = "black", size = 0.6) ,
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.key = element_rect(fill = "transparent", colour = "transparent")) +
    xlim(0, 2) +
    facet_grid(cols = vars(Cancer)) 
  
  return(survivalFigureData)
  
}