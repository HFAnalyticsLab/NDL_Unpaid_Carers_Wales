## ---- lsoa_data_prep ----

lsoa_data_prep <- function(lsoa_raw){
  
  la_names <- c("Gwynedd", "Denbighshire", "Swansea", "Neath Port Talbot") # add LAs of interest to vector
  
  df_ons_lsoa <- lsoa_raw %>% # imported ons lsoa estimates
    clean_names() %>% # clean the column names so they aren't numbers
    filter(la_name_2018_boundaries %in% la_names) %>% # filter by LAs of interest
    select(la_name_2018_boundaries, lsoa_code, "x18":"x90") %>% # remove unnecessary columns
    rowwise() %>%
    mutate(count = sum(c_across("x18":"x90"))) %>%
    select(la_name_2018_boundaries, lsoa_code, count) %>%
    ungroup()
  
  return(df_ons_lsoa)
  
}


## ---- carer_count ----

carer_demog_count <- function(df_la, la_name, demog){
  
  df_dedup <- df_la %>% # demographics for npt from Peje's script
    # filter(variable == demog) %>% # filter to demog
    select(-sex, -wimd) %>%
    mutate(name = la_name) %>% # add la name column for merge with ons estimates
    group_by(name, level) %>% # group by lsoa name and demog level
    mutate(count = sum(count)) %>% # sum LA/GP counts to get overall
    ungroup() %>%
    select(-identifiedby, -total, -percentage) %>%
    distinct() %>%
    arrange(name, demog)
  
  names(df_dedup)[grep("\\bfactor_levels\\b", colnames(df_dedup))] <- demog

  return(df_dedup)
  
}

carer_demog_count_lagp <- function(df_la, la_name, demog){
  
  df_dedup <- df_la %>% # demographics for npt from Peje's script
    # filter(variable == demog) %>% # filter to demog
    select(-sex, -wimd, -total, -percentage) %>%
    mutate(name = la_name) %>% # add la name column for merge with ons estimates
    distinct() %>%
    arrange(name, demog)
  
  names(df_dedup)[grep("\\bfactor_levels\\b", colnames(df_dedup))] <- demog
  
  return(df_dedup)
  
}


## ---- carer_percentage_ons ----

carer_percentage_ons <- function(df_dedup, df_ons, demog){
  
  df_la_proportion_ons <- df_dedup %>%
    left_join(df_ons, by = c(demog, "name")) %>%
    mutate(carer_proportion = as.numeric(formatC(count / count_ons * 100, format = "f", digits = 1))) # calculate percentage of ons estimates identified as carers to 1 dp
  
  return(df_la_proportion_ons)
  
}


## ---- plot_population_ons_percentages ----

plot_ons <- function(df_ons, la, demog, demog_name, palette){ # function to plot ons estimates
  
  top_value <- df_ons %>%
    filter(name == la) %>%
    group_by(get(demog)) %>%
    filter(percentage_ons == max(percentage_ons))
  
  df_ons %>%
    filter(name == la) %>%
    ggplot(aes(x = get(demog),
               y = percentage_ons,
               fill = get(demog))) +
    geom_col() +
    geom_blank(data = top_value, aes(x = get(demog),
                                     y = percentage_ons * 1.1,
                                     label = percentage_ons)) + # plot blank geom above highest y value so geom_text is not cut off
    labs(title = paste(demog_name, 'distribution of general population in', la, '(based on \n ONS 2020 adult MYE)', sep = " "),
         x = demog_name,
         y = 'Percentage (%)') +
    scale_y_continuous(expand = c(0, 0)) +
    geom_text(aes(x = get(demog),
                  y = percentage_ons,
                  label = sprintf("%.1f", percentage_ons),
                  group = get(demog)),
              vjust = -0.5) +
    scale_fill_manual(values = rev(palette)) + # reversed so consistent with other plots
    theme(
      legend.position = 'none',
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black")
    )
  
}

## ---- plot_carer_ons_percentages ----

plot_carer_ons <- function(df_la_proportion_ons, la, demog, demog_title, demog_axis, palette){ # function to plot carer proportions of ons sex estimates
  
  top_value <- df_la_proportion_ons %>%
    filter(name == la) %>%
    group_by(get(demog)) %>%
    filter(carer_proportion == max(carer_proportion))
  
  df_la_proportion_ons %>%
    filter(name == la) %>%
    ggplot(aes(x = get(demog), 
               y = carer_proportion, 
               fill = get(demog))) +
    geom_col() + 
    geom_blank(data = top_value, aes(x = get(demog),
                                     y = carer_proportion * 1.1,
                                     label = carer_proportion)) + # plot blank geom above highest y value so geom_text is not cut off 
    labs(title = paste('Percentage of general population in', la, 'identified as \n unpaid carers, by', demog_title, '(based on ONS 2020 adult MYE)', sep = " "),
         x = demog_axis,
         y = 'Percentage (%)') +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_discrete(limits = rev) + # underlying data is in reverse order for stack plots, so revert back
    geom_text(aes(x = get(demog),  
                  y = carer_proportion, 
                  label = sprintf("%.1f", carer_proportion), 
                  group = get(demog)), 
              vjust = -0.5) +
    scale_fill_manual(values = palette) +
    theme( 
      legend.position = 'none',
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.background = element_blank(), 
      axis.line = element_line(colour = "black")
    )
  
}

plot_carer_ons_lagp <- function(df_la_proportion_ons, la, demog, demog_title, demog_axis, palette){ # function to plot carer proportions of ons sex estimates
  
  top_value <- df_la_proportion_ons %>%
    filter(name == la) %>%
    group_by(get(demog)) %>%
    filter(carer_proportion == max(carer_proportion))
  
  df_la_proportion_ons %>%
    filter(name == la) %>%
    ggplot(aes(x = get(demog), 
               y = carer_proportion, 
               fill = identifiedby)) +
    geom_col(position = "dodge") + 
    geom_blank(data = top_value, aes(x = get(demog),
                                     y = carer_proportion * 1.1,
                                     label = carer_proportion)) + # plot blank geom above highest y value so geom_text is not cut off 
    labs(title = paste('Percentage of general population in', la, 'identified as \n unpaid carers, by', demog_title, 'and LA/GP (based on ONS 2020 adult MYE)', sep = " "),
         x = demog_axis,
         y = 'Percentage (%)',
         fill = "Unpaid carers cohort") +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_discrete(limits = rev) + # underlying data is in reverse order for stack plots, so revert back
    geom_text(aes(x = get(demog),  
                  y = carer_proportion, 
                  label = sprintf("%.1f", carer_proportion), 
                  group = identifiedby),
              position = position_dodge(width = 1),
              vjust = -0.5) +
    scale_fill_manual(values = palette) +
    theme( 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.background = element_blank(), 
      axis.line = element_line(colour = "black")
    )
  
}
