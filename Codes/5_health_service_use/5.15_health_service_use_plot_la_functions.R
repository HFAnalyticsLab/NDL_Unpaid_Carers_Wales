plot_hsu_signif_la <- function(ratio_df, analysis_group, anno = NULL, la_name){
  plot_data <- ratio_df %>% 
    filter(tolower(LA) == tolower(la_name)) %>% 
    mutate(identifier = factor(identifier, levels = c("LA-identified", "GP-identified")))
  fill_label <- "Unpaid carers cohort"
  
  if(analysis_group == "ed_pedw"){
    # for ed & pedw
    scale_y_label <- "Crude rates per 1,000" 
    plot_title <- paste("Emergency Department attendances, planned and emergency admission \nin ", la_name," LA- and GP-identified unpaid carers \nusing crude prevalence rates per 1,000 population", sep = " ")
    
    plot_data <- plot_data %>% 
      mutate(Health_service_type = factor(Health_service_type, 
                                          levels = c("ED attendances","Planned admissions",
                                                     "Emergency admissions")))
    scale_x_labels <- c("ED attendances" = "ED\nattendances",
                        "Planned admissions" = "Planned\nadmissions",
                        "Emergency admissions" = "Emergency\nadmissions")
    
    
  } else if(analysis_group == "gp"){
    scale_y_label <- "Crude rates per person per year" 
    plot_title <- paste("GP interaction rates among \nLA- and GP-identified unpaid carers in ", la_name,"  per person per year", sep = " ")
    
    
    
  }else if(analysis_group == "op"){
    scale_y_label <- "Crude rates per 1,000"  
    plot_title <- paste("Outpatient attendances among \nLA- and GP-identified unpaid carers in ", la_name," using \ncrude prevalence rates per 1,000 population", sep = " ")
    
    
    
  }
  
  
  
  plot_out <- ggplot() +
    geom_col(data = plot_data, 
             aes(x = Health_service_type, 
                 y = rate,
                 fill = identifier),
             position = "dodge") +
    scale_y_continuous(name = scale_y_label,
                       expand = expansion(mult = c(0,0.05))
    ) +
    scale_fill_manual(values = lagpcarers_palette) +
    scale_x_discrete(labels = scale_x_labels) +
    
    geom_text(data = plot_data, 
              aes(x = Health_service_type,
                  y = rate,
                  label = sprintf("%.1f", rate),
                  group = factor(identifier, levels = c("LA-identified", "GP-identified"))),
              position = position_dodge(width = 1),
              vjust = -0.5) +
    
    labs(
      title = plot_title,
      x = "Health service use measures",
      fill = fill_label
    ) + 
    theme(
      panel.background = element_rect(fill = "white", color = "grey50"),
      strip.background = element_rect(fill = "grey93", linetype = "solid", color = "black"),
      axis.line = element_line(colour = "black"),
      
      text = element_text(size = 12)
    ) 
  
  if(!is.null(anno)){
    plot_out <- plot_out +
      geom_text(data = anno, 
                aes(x = xstar,  y = ystar, label = lab),
                size = 10/.pt,
                vjust= -.5) +
      
      geom_segment(data = anno, aes(x = x1, xend = x1, 
                                    y = y1, yend = y2),
                   colour = "black")+
      geom_segment(data = anno, aes(x = x2, xend = x2, 
                                    y = y1, yend = y2),
                   colour = "black") +
      geom_segment(data = anno, aes(x = x1, xend = x2, 
                                    y = y2, yend = y2),
                   colour = "black")
  }
  
  return(plot_out)
  
}
