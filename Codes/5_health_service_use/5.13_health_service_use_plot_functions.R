# function plots health service use for lagp unpaid carers for ED department attendances, inpatient hospitalisations (planned and emergency) and outpatient attendances
plot_hsu_signif <- function(ratio_df, analysis_group, anno){
  plot_data <- ratio_df %>%  mutate(identifier = factor(identifier, levels = c("LA-identified", "GP-identified")))
  fill_label <- "Unpaid carers cohort"
  
  if(analysis_group == "ed_pedw_op"){
    scale_y_label <- "Crude rates per 1,000"
    plot_title <- paste("Emergency Department attendances, planned and emergency admissions, and outpatient attendances \namong LA- and GP-identified unpaid carers by local Authority \nusing crude prevalence rates per 1,000 population", sep=" ")
    
    plot_data <- plot_data %>%
      mutate(Health_service_type = factor(Health_service_type,
                                          levels = c("ED attendances",
                                                     "Planned admissions",
                                                     "Emergency admissions",
                                                     "Outpatient attendances")))
    scale_x_labels <- c("ED attendances" = "ED\nattendances",
                        "Planned admissions" = "Planned\nadmissions",
                        "Emergency admissions" = "Emergency\nadmissions",
                        "Outpatient attendances" = "Outpatient\nattendances")
    y_max <- 550
    
  } else {
    scale_y_label <- "Crude rates per person per year"
    plot_title <- paste("GP interactions rates \namong LA- and GP-identified unpaid carers by Local Authority \nper person per year", sep=" ")
    
    plot_data <- plot_data %>%
      mutate(Health_service_type = factor(Health_service_type,
                                          levels = c(
                                            "GP interactions",
                                            "Outpatient attendances")))
    scale_x_labels <- c("GP interactions" = "GP\ninteractions",
                        "Outpatient attendances" = "Outpatient\nattendances")
    y_max <- 0
  }
  
  
  plot_out <- ggplot() +
    geom_col(data = plot_data,
             aes(x = Health_service_type,
                 y = rate,
                 fill = identifier),
             position = "dodge") +
    scale_y_continuous(name = scale_y_label,
                       limits = c(0, y_max),
                       expand = expansion(mult = c(0, 0.05))) +
    scale_fill_manual(values = lagpcarers_palette) +
    scale_x_discrete(labels = scale_x_labels) + 
    
    
    geom_text(data = plot_data,
              aes(x = Health_service_type,
                  y = rate,
                  label = sprintf("%.1f", rate),
                  group = factor(identifier, levels = c("LA-identified", "GP-identified"))),
              position = position_dodge(width = 1),
              vjust = -.5,
              size = 5) +
    
    
    
    geom_text(data = anno, 
              aes(x = xstar, y = ystar, label = lab),
              size = 10.5/.pt,
              vjust = -.5) +
    geom_segment(data = anno, aes(x = x1, xend = x1,
                                  y = y1, yend = y2),
                 color = "black") +
    geom_segment(data = anno, aes(x = x2, xend = x2,
                                  y = y1, yend = y2),
                 color = "black") +
    geom_segment(data = anno, aes(x = x1, xend = x2,
                                  y = y2, yend = y2),
                 color = "black") + 
    labs(
      title = plot_title,
      x = "Health service use measures",
      fill = fill_label
    ) +
    theme(
      panel.background = element_rect(fill = "white", color = "grey50"),
      strip.background = element_rect(fill = "grey93", linetype = "solid", color = "black"),
      axis.line = element_line(color = "black"),
      legend.position = c(.9, .9),
      text = element_text(size = 18)
    ) 
  
  if(analysis_group == "ed_pedw_op"){
    plot_out <- plot_out +
      facet_wrap(~factor(LA, levels = c("Neath Port Talbot", "Swansea", "Denbighshire")), nrow = 1)
  }
  
  
  return(plot_out)
}


# function plots health service use for lagp unpaid carers for GP visits
plot_gp_signif <- function(ratio_df, analysis_group, anno){
  plot_data <- ratio_df %>%  mutate(identifier = factor(identifier, levels = c("LA-identified", "GP-identified")))
  fill_label <- "Unpaid carers cohort"
  
  scale_y_label <- "Crude rates per person per year"
  plot_title <- paste("GP interactions rates among LA- and GP-identified unpaid carers by Local Authority \nper person per year", sep=" ")
  
  plot_data <- plot_data %>%
    mutate(LA = factor(LA, 
                       levels = c("Neath Port Talbot", "Swansea", "Denbighshire"))
    )
  
  y_max <- 40
  
  
  plot_out <- ggplot() +
    geom_col(data = plot_data,
             aes(x = LA,
                 y = rate,
                 fill = identifier),
             position = "dodge") +
    scale_y_continuous(name = scale_y_label,
                       limits = c(0, y_max),
                       expand = expansion(mult = c(0, 0.05))) +
    scale_fill_manual(values = lagpcarers_palette) +
    geom_text(data = plot_data,
              aes(x = LA,
                  y = rate,
                  label = sprintf("%.1f", rate),
                  group = factor(identifier, levels = c("LA-identified", "GP-identified"))),
              position = position_dodge(width = 1),
              vjust = -.5,
              size = 5) +
    geom_text(data = anno, 
              aes(x = xstar, y = ystar, label = lab),
              size = 10.5/.pt,
              vjust = -.5) +
    geom_segment(data = anno, aes(x = x1, xend = x1,
                                  y = y1, yend = y2),
                 color = "black") +
    geom_segment(data = anno, aes(x = x2, xend = x2,
                                  y = y1, yend = y2),
                 color = "black") +
    geom_segment(data = anno, aes(x = x1, xend = x2,
                                  y = y2, yend = y2),
                 color = "black") + 
    labs(
      title = plot_title,
      x = "",
      fill = fill_label
    ) +
    theme(
      panel.background = element_rect(fill = "white", color = "grey50"),
      strip.background = element_rect(fill = "grey93", linetype = "solid", color = "black"),
      axis.line = element_line(color = "black"),
      legend.position = c(.9, .9),
      text = element_text(size = 18)
    ) 
  
  
  return(plot_out)
}