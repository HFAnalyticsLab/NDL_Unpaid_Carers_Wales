## ---- data_prep_functions ----

# Function for general cleaning
data_prep <- function(df, group_breakdown) {
  
  if(group_breakdown == "carer_type"){
    df <- df[ , .(ALF_PE, TREATED, SEX, AGE_INDEXDATE, WIMD, RUC11_DESC, conds)]
  }
  
  if(group_breakdown == "lagp"){
    df <- df[ , .(ALF_PE, TREATED, SEX, AGE_INDEXDATE, WIMD, RUC11_DESC, FIRST_IDENTIFIED_BY, conds)]
  }
   
  names(df) <- tolower(names(df)) # names to lower case
  nrow(df) == length(unique(df$alf_pe)) # TRUE - one row per alf
  df[, age_group := cut(age_indexdate,
                        breaks = c(17, 39, 49, 59, 69, 79, Inf),
                        labels = c("Under 40", "40-49", "50-59", "60-69", "70-79", "80+"))]
  df <- df[, overall := 'overall'] # add new variable to allow "trait" in calc_perc function to work for the overall counts
  
  return(df)
}


## Count number of conditions

count_n_cond <- function(df_cms, group_breakdown, la_name){
  
  if(group_breakdown == "carer_type"){
    count_overall_cms <- df_cms[, .N, by = .(treated, conds)] %>% # count number of conditions by treatment group (carers vs controls) 
      .[, vartype := 'overall'] %>% .[, varlvl := 'overall'] # add type and typelvl columns
    count_overall_cms <- setnames(count_overall_cms, 'N', 'count') # rename count column
    
    df_count_cond_list <- list()
    df_count_cond_list[[paste0("count_overall_cms_", la_name)]] <- count_overall_cms
  }
  
  if(group_breakdown == "lagp"){
    count_overall_cms_lagp <- df_cms[, .N, by = .(treated, conds, first_identified_by)] %>% # count number of conditions by treatment group (carers vs controls) 
      .[, vartype := 'overall'] %>% .[, varlvl := 'overall'] # add type and typelvl columns
    count_overall_cms_lagp <- setnames(count_overall_cms_lagp, 'N', 'count') # rename count column
    
    count_sex_age_cms_lagp <- df_cms[, .(count = .N),  by = .(treated, sex, age_group, first_identified_by, conds)] # count number of conditions by treatment group, first identified by, sex and age (carers vs controls)
    
    df_count_cond_list <- list()
    df_count_cond_list[[paste0("count_overall_cms_", la_name, "_lagp")]] <- count_overall_cms_lagp
    df_count_cond_list[[paste0("count_sex_age_cms_", la_name, "_lagp")]] <- count_sex_age_cms_lagp
  }
  
  return(df_count_cond_list)
  
}

# Create function to calculate percentages

# NB: aware this is now slightly convoluted, was constructed this way to allow breakdown by demographics (since removed)
calc_rate <- function(df, dfla, group_breakdown, rate_per, report_type){
  
  trait <- df$vartype[1] # assign variable type (demographic type) to trait
  
  if(group_breakdown == "carer_type"){
    rows <- dfla[, .N, by = .(treated, eval(as.name(trait)))] # group by treated and demographic type, count to get total per group
    df <- df[rows, on = .(varlvl = as.name, treated = treated)] # join total column back into counts table
  }
  
  if(group_breakdown == "lagp"){
    rows <- dfla[, .N, by = .(treated, first_identified_by, eval(as.name(trait)))] # group by treated and demographic type, count to get total per group
    df <- df[rows, on = .(varlvl = as.name, treated = treated, first_identified_by = first_identified_by)] # join total column back into counts table
  } 
  
  df <- setnames(df, 'N', 'total') # rename as 'total' for clarity
  df[, percentage := count / total * 100] # calculate percentage per condition count
  
  if(report_type == "hf"){
    df[ , conds_group := ifelse(conds < 3, conds, "3+")] # add column for hf condition groups (to avoid SDC issues)
  }
  
  if(report_type == "internal"){
  df[ , conds_group := ifelse(conds < 2, conds, "2+")] # add column for internal condition groups (to avoid SDC issues)
  }
    
  if(group_breakdown == "carer_type"){
    df[ , count_group := sum(count), by = .(treated, varlvl, conds_group)] # count people per condition count group
  }
  
  if(group_breakdown == "lagp"){
    df[ , count_group := sum(count), by = .(treated, first_identified_by, varlvl, conds_group)] # count people per condition count group
  }
  
    df[ , rate_group := count_group / total * rate_per] # calculate rate by condition count group
    df$rate_group <- as.numeric(formatC(df$rate_group, format = "f", digits = 1)) # round rate group to 1 dp 
  
  return(df)
}

# Create function to reduce columns for SDC
reduce_cols <- function(df){
  df[ , c('conds', 'count', 'percentage') := NULL]# remove columns relating to single counts
  df <- unique(df) 
  
  return(df)
}

## ---- age_sex_standardisation ----

# NB: this is only for LAGP carers data 
 standardise <- function(df_lagp, la_name, count_sex_age_cms, df_standardpop_lkup){
   
   # create values for each stratifying variable
   treated <- c(0, 1)
   first_identified_by <- c("GP", "LA")
   sex <- c(1, 2)
   age_group <- c("Under 40", "40-49", "50-59", "60-69", "70-79", "80+")
   # create value for conds_group
   conds_group <- c("0", "1", "2+")
   
   # create a dummy table containing every combination of stratifying variables by conds_group
   dummy <- as.data.table(expand.grid(treated, first_identified_by, sex, age_group, conds_group))
   setnames(dummy, new = c("treated", "first_identified_by", "sex", "age_group", "conds_group")) # name columns
   
   # study pop totals for each combination of stratifying variables
   totals <- df_lagp[, .(total = .N), by = .(treated, first_identified_by, sex, age_group)][, name := la_name] # group by treated, first identified by, age and sex. Count to get total per strata. This will act as a "dummy" table, providing all strata that need to be included in the final table.
   
   # study pop counts per conds group by stratifying variables
   group_counts <- count_sex_age_cms[, conds_group := ifelse(conds >= 2, "2+", conds)][, .(count_group = sum(count)), by = .(treated, sex, age_group, first_identified_by, conds_group)][(treated == 1), ] # change to group cond counts, sum counts per conds group (this acts like "summarise" - removes single count columns and uniques. Filtered to treated only for now.
   
   group_counts[, name := la_name] # add column for LA name
   
   # join totals table to dummy table to get one row per conds_group by stratifying variables
   conds_group_dummy <- totals[dummy, on = .(treated, first_identified_by, sex, age_group)]
   
   # join conds_group_dummy (dummy table with study pop totals) to standard population totals. This provides all the strata needed.
   populations <- as.data.table(df_standardpop_lkup)[conds_group_dummy, on = .(age = age_group, sex_code = sex, name)][(treated ==1)]
   
   dsr_input <- group_counts[populations, on = .(treated, first_identified_by, age_group = age, sex = sex_code, conds_group)] # take populations table (which has the total per age/sex/first_identified_by for our cohort and standard pop) and join on conds group counts
   
   dsr_input$count_group[is.na(dsr_input$count_group)] <- 0 # replace NA count group values with 0s
   
   # Using PHEindicatormethods package
   std <- as.data.frame(dsr_input) %>%
     group_by(first_identified_by, conds_group) %>%
     phe_dsr(
       x = count_group,
       n = total,
       stdpop = pop,
       stdpoptype = "field",
       multiplier = 1000
     )
   
   return(std)
 }

## ----rate_ratios----
ratio_calc <- function(df_std_matched, comparison, rate_per){
  
  if(comparison == "lagp_std"){
    
    wide_cols <- colnames(df_std_matched) %>% tail(-2) %>% head(-3) # NB: this relies on column order
    
    wide_ratios <- as.data.table(pivot_wider(df_std_matched,
                                             id_cols = conds_group, # row per conds_group
                                             names_from = first_identified_by, # widen by GP/LA
                                             values_from = all_of(wide_cols), # across all value columns
                                             names_glue = "{first_identified_by}_{.value}")) # name columns LA/GP_crude_rate 
    
    wide_ratios[, `:=` (ratio = LA_value/GP_value, # calculate ratios
                        ratio_lowercl = LA_lowercl/GP_uppercl,
                        ratio_uppercl = LA_uppercl/GP_lowercl)] %>%
      select(starts_with("cond"), starts_with("LA"), everything()) # order columns
  }
  
  if(comparison == "carer_type"){
    
    wide_ratios <- df_std_matched %>%
      mutate(treated = factor(treated, levels = c(1, 0), labels = c("unpaid_carer", "non_carer"))) %>% # factorise
      pivot_wider(id_cols = conds_group, # row per conds_group
                  names_from = treated, # widen by carer/non-carer
                  values_from = c("total", "count_group", "rate_group"),
                  names_glue = "{treated}_{.value}") %>% # across all value columns
      arrange(conds_group) %>% # order by conds_group
      mutate(non_carer_lowercl = ((rate_per/non_carer_total)*(non_carer_count_group - (sqrt(non_carer_count_group) * 1.96))),
             non_carer_uppercl = ((rate_per/non_carer_total)*(non_carer_count_group + (sqrt(non_carer_count_group) * 1.96))),
             unpaid_carer_lowercl = ((rate_per/unpaid_carer_total)*(unpaid_carer_count_group - (sqrt(unpaid_carer_count_group) * 1.96))),
             unpaid_carer_uppercl = ((rate_per/unpaid_carer_total)*(unpaid_carer_count_group + (sqrt(unpaid_carer_count_group) * 1.96))),
             ratio = unpaid_carer_rate_group / non_carer_rate_group,
             ratio_lowercl = unpaid_carer_lowercl / non_carer_uppercl,
             ratio_uppercl = unpaid_carer_uppercl / non_carer_lowercl) %>%
      select(starts_with("cond"), starts_with("unpaid"), everything()) # order columns
    
  }
  
  if(comparison == "lagp_crude"){
    
    wide_ratios <- df_std_matched %>%
      filter(treated == 1) %>% # limit to unpaid carers only
      pivot_wider(id_cols = conds_group, # row per conds_group
                  names_from = first_identified_by, # widen by carer/non-carer
                  values_from = c("total", "count_group", "rate_group"),
                  names_glue = "{first_identified_by}_{.value}") %>% # across all value columns
      arrange(conds_group) %>% # order by conds_group
      mutate(GP_lowercl = ((rate_per/GP_total)*(GP_count_group - (sqrt(GP_count_group) * 1.96))),
             GP_uppercl = ((rate_per/GP_total)*(GP_count_group + (sqrt(GP_count_group) * 1.96))),
             LA_lowercl = ((rate_per/LA_total)*(LA_count_group - (sqrt(LA_count_group) * 1.96))),
             LA_uppercl = ((rate_per/LA_total)*(LA_count_group + (sqrt(LA_count_group) * 1.96))),
             ratio = LA_rate_group / GP_rate_group,
             ratio_lowercl = LA_lowercl / GP_uppercl,
             ratio_uppercl = LA_uppercl / GP_lowercl) %>%
      select(starts_with("cond"), starts_with("LA"), everything()) # order columns
    
  }
  
  return(wide_ratios)
}



## ---- binned_cond_count_plot ----

# Create function to plot percentages per bin (matched cohort) for HF only so 0, 1, 2, 3+ conds
plot_percentage_bins <- function(plot_data, group_breakdown, la_name, palette){

  # vartype <- plot_data$vartype[1] # assign 1st row contents of vartype variable to 'vartype'
  
  if(group_breakdown == "carer_type"){ # & vartype == "overall"
    
    plot_data2 <- plot_data %>% filter(treated == 1)
    
    plot_out <- ggplot(plot_data2, aes(x = as.character(treated),
                                      y = rate_group,
                                      fill = factor(conds_group, levels = rev(c('0', '1', '2', '3+'))))) +
      geom_col(position = 'stack') + # stack the columns +
      scale_fill_manual(values = rev(palette)) +
      # coord_flip() +
      geom_text(aes(x = as.character(treated), 
                    y = rate_group, 
                    label=rate_group, 
                    group = factor(conds_group, levels = rev(c('0', '1', '2', '3+')))), 
                position= position_stack(vjust=0.5)) +
      labs(title = paste("Distribution of number of LTCs in unpaid carers in", la_name, sep = " "), # add title using vartype value - , "by:", vartype,
           x = 'Unpaid carers', # add labels
           y = 'Percentage (%)',
           fill = 'Number of LTCs'
      ) +
      scale_y_continuous(expand = c(0, 0)) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        axis.text.x = element_blank(),
        # axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line(colour = "black")
      )
  }
  
  if(group_breakdown == "lagp"){
    
    plot_data <- plot_data %>% filter(treated == 1)
    
    plot_out <- ggplot(plot_data, aes(x = factor(first_identified_by, levels = c("LA", "GP")), # set aesthetics
                          y = rate_group,
                          fill = factor(conds_group, levels = c('3+', '2', '1', '0')))) +
      geom_col(position = 'stack') + # stack the columns
      facet_grid( ~ factor(varlvl)) + # facet by variable level
      # coord_flip() +
      geom_text(aes(x = first_identified_by,  y = rate_group, label=rate_group, group = factor(conds_group, levels = rev(c('0', '1', '2', '3+')))), position= position_stack(vjust=0.5)) +
      scale_x_discrete(limits = rev) +
      scale_fill_manual(values = rev(palette), guide = guide_legend(reverse = TRUE)) +
      labs(title = paste("Distribution of number of LTCs in unpaid carers in", la_name, "LA and GP \n identified cohorts", sep = " "),
           x = 'Identified by', # add labels
           y = 'Percentage (%)',
           fill = 'Number of LTCs'
      ) +
      scale_y_continuous(expand = c(0, 0)) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.line = element_line(colour = "black")
      )
  }
  return(plot_out)
}



# Rate ratio plot

plot_rr <- function(ratio_df, group_breakdown, la_name, palette){
  
  if(group_breakdown == "carer_type"){
    
    plot_data <- ratio_df %>% # pivot longer - treated column to allow dodge plot
      select(conds_group, unpaid_carer_rate_group, non_carer_rate_group) %>%
      pivot_longer(cols = unpaid_carer_rate_group:non_carer_rate_group,
                   names_to = "treated",
                   values_to = "crude_rate") %>%
      mutate(treated = substring(treated, 1, 2),
             treated = factor(treated, levels = c("un", "no"), labels = c("Unpaid carers", "Non-carers")))
    
    plot_out <- ggplot() +
      geom_col(data = plot_data, 
               aes(x = conds_group, 
                   y = crude_rate,
                   fill = treated),
               position = "dodge") +
      scale_y_continuous(expand = c(0, 0), 
                         limits = c(0, ceiling((max(plot_data$crude_rate) + 35)/100)*100)) + # make y axis max at nearest 100 above highest data point (+20 is added to account for room needed for any significance markers)
      scale_fill_manual(values = palette) +
      geom_text(data = plot_data,
                aes(x = conds_group,  
                    y = crude_rate, 
                    label = format(round(crude_rate, digits = 1), nsmall = 1), 
                    group = treated), 
                position = position_dodge(width = 1),
                vjust = -.5) +
      labs(
        title = paste("Number of LTCs among unpaid carers and non-carers in", la_name, "using \nprevalence per 1,000 population", sep = " "),
        x = "Number of LTCs",
        y = "Crude rate per 1,000",
        fill = "Carer type"
      )
  }
  
  if(group_breakdown == "lagp_crude"){
    
    plot_data <- ratio_df %>% # pivot longer - first identified column to allow dodge plot
      select(conds_group, LA_rate_group, GP_rate_group) %>%
      pivot_longer(cols = LA_rate_group:GP_rate_group,
                   names_to = "first_identified_by",
                   values_to = "crude_rate") %>%
      mutate(first_identified_by = substring(first_identified_by, 1, 2)) %>%
      group_by(factor(first_identified_by, levels = c("LA", "GP"))) %>%
      arrange(conds_group)
    
    plot_out <- ggplot() +
      geom_col(data = plot_data, 
               aes(x = conds_group, 
                   y = crude_rate,
                   fill = factor(first_identified_by, 
                                 levels = c("LA", "GP"),
                                 labels = c("LA-identified", "GP-identified"))),
               position = "dodge") +
      scale_y_continuous(expand = c(0, 0), 
                         limits = c(0, ceiling((max(plot_data$crude_rate) + 35)/100)*100)) + # make y axis max at nearest 100 above highest data point (+20 is added to account for room needed for any significance markers)
      scale_fill_manual(values = palette) +
      geom_text(data = plot_data,
                aes(x = conds_group,  
                    y = crude_rate, 
                    label = format(round(crude_rate, digits = 1), nsmall = 1), 
                    group = factor(first_identified_by, 
                                   levels = c("LA", "GP"),
                                   labels = c("LA-identified", "GP-identified"))), 
                position = position_dodge(width = 1),
                vjust = -.5) +
      labs(
        title = paste("Number of LTCs among LA and GP identified unpaid carers in", la_name, "using crude \nprevalence rate per 1,000 population", sep = " "),
        x = "Number of LTCs",
        y = "Crude rate per 1,000",
        fill = "Unpaid carers cohort"
      )
  }
  
  if(group_breakdown == "lagp_std"){
    
    plot_data <- ratio_df %>% # pivot longer - first identified column to allow dodge plot
      select(conds_group, LA_value, GP_value) %>%
      pivot_longer(cols = LA_value:GP_value,
                   names_to = "first_identified_by",
                   values_to = "standardised_rate") %>%
      mutate(first_identified_by = substring(first_identified_by, 1, 2)) %>%
      group_by(factor(first_identified_by, levels = c("LA", "GP"))) %>%
      arrange(conds_group)
    
    plot_out <- ggplot() +
      geom_col(data = plot_data, 
               aes(x = conds_group, 
                   y = standardised_rate,
                   fill = factor(first_identified_by, 
                                 levels = c("LA", "GP"),
                                 labels = c("LA-identified", "GP-identified"))),
               position = "dodge") +
      scale_y_continuous(expand = c(0, 0), 
                         limits = c(0, ceiling((max(plot_data$standardised_rate) + 35)/100)*100)) + # make y axis max at nearest 100 above highest data point (+20 is added to account for room needed for any significance markers)
      scale_fill_manual(values = palette) +
      geom_text(data = plot_data,
                aes(x = conds_group,  
                    y = standardised_rate, 
                    label = format(round(standardised_rate, digits = 1), nsmall = 1), 
                    group = factor(first_identified_by, 
                                   levels = c("LA", "GP"),
                                   labels = c("LA-identified", "GP-identified"))), 
                position = position_dodge(width = 1),
                vjust = -.5) +
      labs(
        title = paste("Number of LTCs among LA and GP identified unpaid carers in", la_name, "using age-sex standardised \nprevalence per 1,000 population", sep = " "),
        x = "Number of LTCs",
        y = "Age-sex standardised rate per 1,000",
        fill = "Unpaid carers cohort"
      )
  }
  
  plot_out <- plot_out +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      axis.line = element_line(colour = "black")
    )
  
  return(plot_out)
}

# Function to create 4 pager rate plot for all three LAs

prep_all_rates <- function(ratio_df_npt, ratio_df_swansea, ratio_df_den){
  
  # mutate each LA table with LA_name column
  ratio_df_npt <- ratio_df_npt %>%
    mutate(LA_name = "NPT")
  
  ratio_df_swansea <- ratio_df_swansea %>%
    mutate(LA_name = "Swansea")
  
  ratio_df_den <- ratio_df_den %>%
    mutate(LA_name = "Denbighshire")
  
  # merge all three tables
  ratio_df_all <- ratio_df_npt %>%
    merge(ratio_df_swansea, all = TRUE) %>%
    merge(ratio_df_den, all = TRUE)
  
  return(ratio_df_all)
}

plot_all_rates <- function(ratio_df_all, palette){
  
  plot_data <- ratio_df_all %>% # pivot longer - first identified column to allow dodge plot
    select(LA_name, conds_group, LA_rate_group, GP_rate_group) %>%
    filter(conds_group == "2+") %>%
    pivot_longer(cols = LA_rate_group:GP_rate_group,
                 names_to = "first_identified_by",
                 values_to = "crude_rate") %>%
    mutate(first_identified_by = substring(first_identified_by, 1, 2),
           first_identified_by = paste0(first_identified_by, "-identified")) %>%
    group_by(factor(first_identified_by, levels = c("LA-identified", "GP-identified"))) %>%
    arrange(conds_group)
  
  anno <- data.frame(x1 = c(0.75, 2.75), 
                     x2 = c(1.25, 3.25),
                     y1 = c(475, 610), 
                     y2 = c(480, 615), 
                     xstar = c(1, 3),
                     ystar = c(490, 625), 
                     lab = c("*", "*"), 
                     LA_name = c("NPT", "Denbighshire"))
  
  plot_out <- ggplot() +
    geom_col(data = plot_data, 
             aes(x = factor(LA_name, levels = c("NPT", "Swansea", "Denbighshire")), 
                 y = crude_rate,
                 fill = factor(first_identified_by, levels = c("LA-identified", "GP-identified"))),
             position = "dodge") +
    geom_text(data = plot_data, 
              aes(x = factor(LA_name, levels = c("NPT", "Swansea", "Denbighshire")),
                  y = crude_rate,
                  label = sprintf("%.1f", crude_rate),
                  group = factor(first_identified_by, levels = c("LA-identified", "GP-identified"))),
              position = position_dodge(width = 1),
              vjust = -0.5) +
    geom_text(data = anno, aes(x = xstar, y = ystar, label = lab)) +
    geom_segment(data = anno, aes(x = x1, xend = x1, y = y1, yend = y2), colour = "black") +
    geom_segment(data = anno, aes(x = x2, xend = x2, y = y1, yend = y2), colour = "black") +
    geom_segment(data = anno, aes(x = x1, xend = x2, y = y2, yend = y2), colour = "black") +
    scale_y_continuous(name = "Crude rate per 1,000",
                       expand = expansion(mult = c(0, 0.05))) +
    scale_fill_manual(values = palette) +
    labs(
      title = "Multimorbidity among GP- and LA-identified unpaid carers by Local Authority using crude \nprevalence rate per 1,000 population",
      x = element_blank(),
      fill = "Unpaid carer cohort"
      ) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.background = element_rect(fill = "white", color = "grey50"),
          strip.background = element_rect(fill = "grey93", linetype = "solid", color = "black"))
  
  return(plot_out)

}
  

ltc_mw <- function(la_df, comparison){
  
  lagp_counts <- la_df[la_df$treated == 1,] # filter unpaid carers only

  stats_df <- wilcox.test(conds ~ first_identified_by, data = lagp_counts, exact= FALSE, paired=FALSE)
  
  return(stats_df)
}

