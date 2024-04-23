# This script is used to create demographic plots

# function creates dodged count and stacked percentage plots for demographic factors of unpaid carers identified by LA/GP
plot_demog_lagp <- function(plot_df, la_name, if_count, factor_name, factor_palette){
  y_lab <- ifelse(if_count==1,"Count", "Percentage (%)")
   plot_type <- ifelse(if_count==1,"dodge", "stack")
  
  if(if_count == 1){
    # dodged plots for count plots
    out_plot <- plot_df %>% 
      mutate(factor_levels = fct_rev(factor_levels),
             identifiedby = factor(identifiedby, levels = c("LA", "GP"))) %>% 
      ggplot(aes(x = factor_levels, 
                 y = count,
                 group = factor_levels)) +
      geom_col(aes(fill = factor_levels), position = plot_type) +
      labs(x = factor_name,
           y = y_lab,
           title = paste0(factor_name," distribution of unpaid carers in ", la_name, " \nidentified by GP vs LA") )  + 
      scale_fill_manual(values = rev(factor_palette), guide = guide_legend(reverse = FALSE))  + 
      geom_text(aes(x = factor_levels,  
                    y = count, 
                    label = count, 
                    group = factor_levels), 
                position= position_dodge(width = 1), 
                vjust=-.25,
                size = 3.5)+facet_wrap(~ identifiedby, ncol = 2)
  }else(
    # stacked plots for percentage plots
    out_plot <- plot_df %>%
      mutate(identifiedby = factor(identifiedby, levels = c("LA", "GP"))) %>%
      ggplot(aes(x = identifiedby, 
                 y = percentage,
                 group = factor_levels)) +
      geom_col(aes(fill = factor_levels), position = plot_type) +
      scale_y_continuous(name = y_lab,
                         expand = expansion(mult = c(0, 0.05))) +
      labs(x = "Identified by",
           y = y_lab,
           title = paste0(factor_name, " distribution of unpaid carers in ", la_name, " \nfor unpaid carer type") )  + 
      scale_fill_manual(values = factor_palette, guide = guide_legend(reverse = FALSE, title = factor_name))+
      geom_text(aes(x = identifiedby,  
                    y = percentage, 
                    label = sprintf("%.1f", percentage),
                    group = factor_levels), 
                position = position_stack(vjust=0.5),
                size = 6)

  )
  
  out_plot <- out_plot +  
    scale_y_continuous(expand = expansion(mult = c(0,0.05))) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          text = element_text(size = 18))
  
  return(out_plot)
}


# function creates plot for total counts of unpaid carers by first identified method
demog_plot_overall_counts <- function(plot_df, la_name,factor_palette){
  out_plot <- plot_df %>% 
    ggplot() +
    geom_col(aes(x = identifiedby, 
                 y = count, 
                 fill = identifiedby), position = "dodge") +
    labs(x = "Identified by",
         y = "Number of unpaid carers identified",
         title = paste0("Number of unpaid carers in ", la_name," identified by GP vs LA")) + 
    geom_text(aes(x = identifiedby,  y = count,label=count), 
              position=position_dodge2(width = 0.9), 
              vjust=-0.25) +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(0, 2100),
                       breaks = seq(0, 2100, 500)) +
    scale_fill_manual(values = factor_palette) +
    scale_y_continuous(expand = expansion(mult = c(0,0.05))) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.background = element_blank(), 
          legend.position = "none",
          axis.line = element_line(colour = "black"))
  
  return(out_plot)
}

# function creates plots for percentage of identified unpaid carers per age group and by first identified method
demog_plot_age_hist <- function(plot_df, la_name,factor_palette){
  
  out_plot <- plot_df %>% 
    ggplot() +
    geom_col(aes(x = Age_Group,
                 y = percentage,
                 fill = Age_Group), position = "dodge") +
    labs(x = "Age group", 
         y = "Percentage (%)",
         title = paste0("Age group distribution of unpaid carers in ", la_name, " by LA GP")) + 
    scale_fill_manual(values = factor_palette, guide = guide_legend(reverse = FALSE))+theme(legend.position = "none")+
    geom_text(aes(x = Age_Group,  y = percentage, label=sprintf("%.1f", percentage), group =Age_Group ),  
              position= position_dodge(width = 1), vjust=-.25,
              size = 3.5)+
    scale_y_continuous(expand = c(0, 0), limits = c(0, 30),
                       breaks = seq(0, 30, 10)) +
    
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"))
  
  return(out_plot)
}

# function creates plots for percentage of identified unpaid carers by sex and first identified method
demog_plot_sexwimd_lagp <- function(plot_df, la_name,factor_palette){
  
  out_plot <- plot_df %>% 
    ggplot()+
    geom_col(aes(x = sex, 
                 y = percentage,
                 fill = interaction(WIMD_Quintile_2019, sex)) ) +
    labs(x = "Sex",
         y = "Percentage (%)",
         fill = "Deprivation quintile by sex",
         title = paste0("Deprivation quintile distribution by sex of unpaid carers in ", la_name," \n LA and GP identified cohorts")) + 
    facet_wrap(~ identifiedby, ncol = 2)+ 
    scale_fill_manual(values = sexwimd_palette ,guide = guide_legend(reverse = FALSE)) +
    scale_y_continuous(expand = expansion(mult = c(0,0.05))) +
    geom_text(aes(x = sex,  y = percentage, label = sprintf("%.1f", percentage), group = WIMD_Quintile_2019 ), position= position_stack(vjust=0.5), size = 3.5) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  return(out_plot)
}
