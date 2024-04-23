# Set rate denominator
rate_per <- 1000

# Function to count controls/carers with each ltc and calculate percentage

bycondition <- function(df_cms, group_breakdown, cms_desc) {
  
  cond_counts <- data.frame() # create blank df to populate with output
  
  list_all_conditions <- names(df_cms[ , .SD, .SDcols = ALC:THY]) # list condition names and assign to list_all_conditions value
  
  # count all cohort
  if(group_breakdown == "carer_type"){
    for (cond in list_all_conditions) {
      print(paste0("Condition ID:", cond))
      
      count <- df_cms[ , .N, by = .(TREATED, eval(as.name(cond)))] # group by condition and sum to get count
      count <- setnames(count, "as.name", cond)
      count[, cond_name := cond] # mutate a condition column and populate with condition name 
      count_colnames_old <- names(count)[2:3] # assign names of first 2 columns to 'old' names value
      count_colnames_new <- c('cond_flag', 'count') # assign replacement names of first 2 columns to 'new' names value
      count <- setnames(count, count_colnames_old, count_colnames_new) # update column names
      
      cond_counts <- rbind(cond_counts, count) # bind to ouput from previous loops
    }
  }
  
  # count by lagp
  if(group_breakdown == "lagp"){
    for (cond in list_all_conditions) {
      print(paste0("Condition ID:", cond))
      
      count <- df_cms[ , .N, by = .(TREATED, FIRST_IDENTIFIED_BY, eval(as.name(cond)))] # group by condition and sum to get count
      count <- setnames(count, "as.name", cond)
      count[, cond_name := cond] # mutate a condition column and populate with condition name 
      count_colnames_old <- names(count)[3:4] # assign names of first 2 columns to 'old' names value
      count_colnames_new <- c('cond_flag', 'count') # assign replacement names of first 2 columns to 'new' names value
      count <- setnames(count, count_colnames_old, count_colnames_new) # update column names
      
      cond_counts <- rbind(cond_counts, count) # bind to ouput from previous loops
      
    }
  }
  
  # count by age sex
  if(group_breakdown == "age_sex"){
    
    for (cond in list_all_conditions) {
      print(paste0("Condition ID:", cond))
      
      count <- df_cms[ , .N, by = .(TREATED, FIRST_IDENTIFIED_BY, age_group, SEX, eval(as.name(cond)))] # group by treated, identified by, age, sex and condition - sum to get count
      count <- setnames(count, "as.name", cond)
      count[, cond_name := cond] # mutate a condition column and populate with condition name 
      count_colnames_old <- names(count)[5:6] # assign names of first 2 columns to 'old' names value
      count_colnames_new <- c('cond_flag', 'count') # assign replacement names of first 2 columns to 'new' names value
      count <- setnames(count, count_colnames_old, count_colnames_new) # update column names
      
      cond_counts <- rbind(cond_counts, count) # bind to ouput from previous loops
      
    }
  }
  
  cond_counts <- cond_counts[cms_desc, on = .(cond_name = cond)] # join to cms condition descriptions

  if(group_breakdown == "carer_type"){ # add totals column for all cohort
    cond_counts[, total := sum(count), by = .(TREATED, cond_name)]
  }

  if(group_breakdown == "lagp"){ # add totals column for lagp
    cond_counts[, total := sum(count), by = .(TREATED, FIRST_IDENTIFIED_BY, cond_name)]
  }

  if(group_breakdown == "age_sex"){ # add totals column for lagp
    cond_counts[, total := sum(count), by = .(TREATED, FIRST_IDENTIFIED_BY, age_group, SEX, cond_name)]
  }

  cond_counts[, rate := count / total * rate_per] # calculate rate
  cond_counts$rate <- as.numeric(format(round(cond_counts$rate, digits = 1), nsmall = 1)) # round rate group to 1 dp

  cond_counts <- cond_counts[cond_flag == 1, ] %>% # filter to only those who have the named condition
    .[, cond_flag := NULL] # remove cond_flag column
  
  return(cond_counts)
}

# Standardise - NB: this is only for LAGP carers data 

standardise <- function(df_lagp, la_name, count_sex_age_cms, df_standardpop_lkup){

  # create values for each stratifying variable
  treated <- c(0, 1)
  first_identified_by <- c("GP", "LA")
  sex <- c(1, 2)
  age_group <- c("Under 40", "40-49", "50-59", "60-69", "70-79", "80+")
  # create value for conds_group
  cond_name <- names(df_lagp[ , .SD, .SDcols = ALC:THY])
  # cond_flag <- c(0, 1)
  
  # create a dummy table containing every combination of stratifying variables by conds_name
  dummy <- as.data.table(expand.grid(treated, first_identified_by, sex, age_group, cond_name)) # , cond_flag
  setnames(dummy, new = c("treated", "first_identified_by", "sex", "age_group", "cond_name")) # name columns # , "cond_flag"
  
  # study pop totals for each combination of stratifying variables
  totals <- df_lagp[, .(total = .N), by = .(TREATED, FIRST_IDENTIFIED_BY, SEX, age_group)][, name := la_name] # group by treated, first identified by, age and sex. Count to get total per strata. This will act as a "dummy" table, providing all strata that need to be included in the final table.
  
  # join totals table to dummy table to get one row per conds_group by stratifying variables; filter to treated only 
  conds_group_dummy <- totals[dummy, on = .(TREATED = treated, FIRST_IDENTIFIED_BY = first_identified_by, SEX = sex, age_group)][(TREATED == 1)]
  
  # study pop counts per conds group by stratifying variables
  group_counts <- as.data.table(count_sex_age_cms)[(TREATED == 1), ] # change to group cond counts, sum counts per conds group (this acts like "summarise" - removes single count columns and uniques. Filtered to treated only for now.
  
  group_counts[, name := la_name] # add column for LA name
  
  # join conds_group_dummy (dummy table with study pop totals) to standard population totals. This provides all the strata needed.
  populations <- as.data.table(df_standardpop_lkup)[conds_group_dummy, on = .(age = age_group, sex_code = SEX, name)]
  
  dsr_input <- group_counts[, `:=` (total = NULL)][populations, on = .(TREATED, FIRST_IDENTIFIED_BY, age_group = age, SEX = sex_code, cond_name)] # take populations table (which has the total per age/sex/first_identified_by for our cohort and standard pop) and join on conds group counts # , cond_flag
  
  dsr_input$count[is.na(dsr_input$count)] <- 0 # replace NA count group values with 0s
  
  # Using PHEindicatormethods package
  std <- as.data.frame(dsr_input) %>%
    group_by(FIRST_IDENTIFIED_BY, cond_name) %>%
    phe_dsr(
      x = count,
      n = total,
      stdpop = pop,
      stdpoptype = "field",
      multiplier = 1000
    )
  
  return(std)
}

# Calculate prevalence rate ratios 

calc_rr <- function(df_crude_rates){
  
  wide_id_cols <- c("cond_name", "cond_desc") # assign column names to widen by la/gp
  
  rm_cols <- c("total", "count")
  
  df_crude_rates_wide <- df_crude_rates %>%
    rename(crude_rate = rate) %>% # rename rate to crude_rate
    mutate(TREATED = factor(TREATED, levels = c(1, 0), labels = c("unpaid_carer", "non_carer"))) %>% # factorise
    pivot_wider(id_cols = all_of(wide_id_cols), # row per cond (keeping description for joining)
                names_from = TREATED,
                values_from = c("total", "count", "crude_rate"), # widen crude_rate - column per la/gp
                names_glue = "{TREATED}_{.value}") %>% # name columns LA/GP_crude_rate
    mutate(non_carer_lowercl = ((1000/non_carer_total)*(non_carer_count - (sqrt(non_carer_count) * 1.96))),
           non_carer_uppercl = ((1000/non_carer_total)*(non_carer_count + (sqrt(non_carer_count) * 1.96))),
           unpaid_carer_lowercl = ((1000/unpaid_carer_total)*(unpaid_carer_count - (sqrt(unpaid_carer_count) * 1.96))),
           unpaid_carer_uppercl = ((1000/unpaid_carer_total)*(unpaid_carer_count + (sqrt(unpaid_carer_count) * 1.96))),
           ratio = unpaid_carer_crude_rate / non_carer_crude_rate,
           ratio_lowercl = unpaid_carer_lowercl / non_carer_uppercl,
           ratio_uppercl = unpaid_carer_uppercl / non_carer_lowercl) # %>%
    # select(-unpaid_carer_total, -unpaid_carer_count, -non_carer_total, -non_carer_count)
  
  df_crude_rates_wide <- df_crude_rates_wide %>% select(cond_name, cond_desc, grep("unpaid", names(df_crude_rates_wide)), everything()) 
  
  
  df_crude_rates_wide[is.na(df_crude_rates_wide)] <- 0 # Replace NAs with 0
  
  
  return(df_crude_rates_wide)
}

## Crude LA/GP unpaid carers
calc_rr_lagp_crude <- function(df_crude_rates){
  
  wide_id_cols <- c("cond_name", "cond_desc") # assign column names to widen by la/gp
  
  rm_cols <- c("total", "count")
  
  df_crude_rates_wide <- df_crude_rates %>%
    filter(TREATED == 1) %>% # carers only
    rename(crude_rate = rate) %>% # rename rate to crude_rate
    pivot_wider(id_cols = all_of(wide_id_cols), # row per cond (keeping description for joining)
                names_from = FIRST_IDENTIFIED_BY,
                values_from = c("total", "count", "crude_rate"), # widen crude_rate - column per la/gp
                names_glue = "{FIRST_IDENTIFIED_BY}_{.value}") %>% # name columns LA/GP_crude_rate
    mutate(GP_lowercl = ((1000/GP_total)*(GP_count - (sqrt(GP_count) * 1.96))),
           GP_uppercl = ((1000/GP_total)*(GP_count + (sqrt(GP_count) * 1.96))),
           LA_lowercl = ((1000/LA_total)*(LA_count - (sqrt(LA_count) * 1.96))),
           LA_uppercl = ((1000/LA_total)*(LA_count + (sqrt(LA_count) * 1.96))),
           ratio = LA_crude_rate / GP_crude_rate,
           ratio_lowercl = LA_lowercl / GP_uppercl,
           ratio_uppercl = LA_uppercl / GP_lowercl) %>%
    #select(-LA_total, -LA_count, -GP_total, -GP_count) %>%
    select(starts_with("cond"), starts_with("LA"), everything()) # reorder: cond_name/desc, LA rates, GP rates
  
  df_crude_rates_wide <- df_crude_rates_wide %>% select(cond_name, cond_desc, grep("unpaid", names(df_crude_rates_wide)), everything()) 
  
  
  df_crude_rates_wide[is.na(df_crude_rates_wide)] <- 0 # Replace NAs with 0
  
  
  return(df_crude_rates_wide)
}


## Standardised LAGP unpaid carers
calc_rr_std <- function(df_std_rates, df_crude_rates){
  
  # Format standardised rates 
  df_std_rates <- df_std_rates %>%
    select(FIRST_IDENTIFIED_BY, cond_name, value, lowercl, uppercl) %>% # select needed columns from phe_dsr output
    rename(standardised_rate = value) # rename value column to "standardised_rate"
  
  wide_val_cols <- c("standardised_rate", "lowercl", "uppercl") # assign column names to widen by la/gp
  
  df_std_rates_wide <- df_std_rates  %>%
    pivot_wider(id_cols = cond_name, # row per cond
                names_from = FIRST_IDENTIFIED_BY,  
                values_from = all_of(wide_val_cols), # widen value columns for LA and GP 
                names_glue = "{FIRST_IDENTIFIED_BY}_{.value}") # name columns LA/GP_valuename
  
  # Format crude rates
  wide_id_cols <- c("cond_name", "cond_desc") # assign column names to widen by la/gp
  
  df_crude_rates_wide <- df_crude_rates %>%
    filter(TREATED == 1) %>% # carers only
    select(FIRST_IDENTIFIED_BY, cond_name, cond_desc, rate) %>% # select necessary cols
    rename(crude_rate = rate) %>% # rename rate to crude_rate
    pivot_wider(id_cols = all_of(wide_id_cols), # row per cond (keeping description for joining)
                names_from = FIRST_IDENTIFIED_BY,
                values_from = crude_rate, # widen crude_rate - column per la/gp
                names_glue = "{FIRST_IDENTIFIED_BY}_crude_rate") # name columns LA/GP_crude_rate
  
  # Combine std and crude rates
  df_crude_std <- df_crude_rates_wide %>% # take crude rates table
    left_join(df_std_rates_wide) %>% # left join std rates table (to keep cond_desc)
    select(starts_with("cond"), starts_with("LA"), everything()) # reorder: cond_name/desc, LA rates, GP rates
  
  
  df_crude_std[is.na(df_crude_std)] <- 0 # Replace NAs with 0
  
  # Calculate ratios
  df_rr <- df_crude_std %>%
    mutate(ratio = LA_standardised_rate/GP_standardised_rate, # calculate ratios
           ratio_lowercl = LA_lowercl/GP_uppercl,
           ratio_uppercl = LA_uppercl/GP_lowercl)
  
  df_rr[is.na(df_rr)] <- 0 # Replace NAs with 0
  
  return(df_rr)
}

# filter to conditions with sig differences

# sig_conds <- function(df, la_name){
# 
#   out_df <- df %>% filter(ratio_lowercl < 1 & ratio_uppercl < 1 | ratio_lowercl > 1 & ratio_uppercl > 1) # filter ratios to only conditions where difference between la/gp is significant
#   
#   out_df <- out_df %>%
#     mutate(name = la_name)
#   
#   return(out_df)
# }


# Function to select top 5 conditions for carers/controls

top5 <- function(cond_counts, group_breakdown) {
  
    if(group_breakdown == "hf"){ # prep top 5 for hf carers only
      
      top_5 <- cond_counts %>% 
        filter(TREATED == 1) %>% # filter to carers only for HF
        mutate(percentage = rate/10) %>% # calculate percentage for HF plots 
        arrange(-percentage) %>% 
        top_n(5)
    } 
    
    if(group_breakdown == "carer_type"){ # prep top 5 for all cohort
      
      carer <- cond_counts %>% 
        select(cond_name:unpaid_carer_uppercl) %>%
        arrange(-unpaid_carer_crude_rate) %>%
        head(5) %>%
        left_join(cond_counts)
      
      non_carer <- cond_counts %>% 
        select(cond_name, cond_desc, non_carer_crude_rate:non_carer_uppercl) %>%
        arrange(-non_carer_crude_rate) %>%
        head(5) %>%
        left_join(cond_counts)
      
      top_5 <- full_join(carer, non_carer)
    } 
    
    if(group_breakdown == "lagp_crude"){ # prep top 5 by lagp
      
      la <- cond_counts %>% 
        select(cond_name:LA_uppercl) %>%
        arrange(-LA_crude_rate) %>%
        head(5) %>%
        left_join(cond_counts)
      
      gp <- cond_counts %>% 
        select(cond_name, cond_desc, GP_crude_rate:GP_uppercl) %>%
        arrange(-GP_crude_rate) %>%
        head(5) %>%
        left_join(cond_counts)
      
      top_5 <- full_join(la, gp)
    }
  
  if(group_breakdown == "lagp_std"){ # prep top 5 by lagp
    
    la <- cond_counts %>% 
      select(cond_name:LA_uppercl) %>%
      arrange(-LA_standardised_rate) %>%
      head(5) %>%
      left_join(cond_counts)
    
    gp <- cond_counts %>% 
      select(cond_name, cond_desc, GP_crude_rate:GP_uppercl) %>%
      arrange(-GP_standardised_rate) %>%
      head(5) %>%
      left_join(cond_counts)
    
    top_5 <- full_join(la, gp)
  }
  
  return(top_5)
}

# Plot top 5 

plot_top_5 <- function(df_top5, group_breakdown, la_name, palette){
  
  if(group_breakdown == "carer_type"){
    plot_data <- df_top5 %>% # pivot longer - treated column to allow dodge plot
      select(cond_name, cond_desc, unpaid_carer_crude_rate, non_carer_crude_rate) %>%
      pivot_longer(cols = unpaid_carer_crude_rate:non_carer_crude_rate,
                   names_to = "treated",
                   values_to = "crude_rate") %>%
      mutate(treated = substring(treated, 1, 2),
             treated = factor(treated, levels = c("un", "no"), labels = c("Unpaid carers", "Non-carers"))) %>%
      group_by(treated) %>%
      arrange(-crude_rate)
    
    out_plot <- plot_data %>% 
      ggplot() +
      geom_col(aes(x = cond_name, 
                   y = crude_rate, 
                   fill = treated), position = "dodge") +
      scale_y_continuous(name = "Crude rate per 1,000", sec.axis = sec_axis(~./200, name = "Prevalence rate ratio with 95% confidence intervals")) +
      geom_hline(yintercept = 200, linetype = "dashed", color = "gray") +
      geom_point(data = df_top5, aes(x = cond_name, y = ratio * 200)) +
      geom_errorbar(data = df_top5, aes(x = cond_name, ymin = ratio_lowercl * 200, ymax = ratio_uppercl * 200), width = .1) +
      scale_fill_manual(values = palette) +
      labs(
        title = paste("Five most prevalent LTCs among unpaid carers and non-carers in", la_name, "\nper 1,000 population and rate ratio", sep = " "),
        x = "LTC",
        fill = "Cohort"
      )
  }
  
  if(group_breakdown == "lagp_crude"){
    plot_data <- df_top5 %>% # pivot longer - first identified column to allow dodge plot
      select(cond_name, cond_desc, LA_crude_rate, GP_crude_rate) %>%
      pivot_longer(cols = LA_crude_rate:GP_crude_rate,
                   names_to = "first_identified_by",
                   values_to = "crude_rate") %>%
      mutate(first_identified_by = substring(first_identified_by, 1, 2)) %>%
      group_by(factor(first_identified_by, levels = c("LA", "GP"))) %>%
      arrange(-crude_rate)
    
    out_plot <- plot_data %>% 
      ggplot() +
      geom_col(aes(x = cond_name, 
                   y = crude_rate, 
                   fill = factor(first_identified_by, levels = c("LA", "GP"))), position = "dodge") +
      scale_y_continuous(name = "Crude rate per 1,000", sec.axis = sec_axis(~./200, name = "Prevalence rate ratio with 95% confidence intervals")) +
      geom_hline(yintercept = 200, linetype = "dashed", color = "gray") +
      geom_point(data = df_top5, aes(x = cond_name, y = ratio * 200)) +
      geom_errorbar(data = df_top5, aes(x = cond_name, ymin = ratio_lowercl * 200, ymax = ratio_uppercl * 200), width = .1) +
      scale_fill_manual(values = palette) +
      labs(
        title = paste("Top 5 LTCs among GP and LA identified unpaid carers in", la_name, "using crude \nprevalence per 1,000 population and rate ratio", sep = " "),
        x = "LTC",
        fill = "Identified by"
      )
  }
  
  if(group_breakdown == "lagp_std"){
    plot_data <- df_top5 %>% # pivot longer - first identified column to allow dodge plot
      select(cond_name, cond_desc, LA_standardised_rate, GP_standardised_rate) %>%
      pivot_longer(cols = LA_standardised_rate:GP_standardised_rate,
                   names_to = "first_identified_by",
                   values_to = "standardised_rate") %>%
      mutate(first_identified_by = substring(first_identified_by, 1, 2)) %>%
      group_by(factor(first_identified_by, levels = c("LA", "GP"))) %>%
      arrange(-standardised_rate)
    
    out_plot <- plot_data %>% 
      ggplot() +
      geom_col(aes(x = cond_name, 
                   y = standardised_rate, 
                   fill = factor(first_identified_by, levels = c("LA", "GP"))), position = "dodge") +
      scale_y_continuous(name = "Age-sex standardised rate per 1,000", sec.axis = sec_axis(~./200, name = "Prevalence rate ratio with 95% confidence intervals")) +
      geom_hline(yintercept = 200, linetype = "dashed", color = "gray") +
      geom_point(data = df_top5, aes(x = cond_name, y = ratio * 200)) +
      geom_errorbar(data = df_top5, aes(x = cond_name, ymin = ratio_lowercl * 200, ymax = ratio_uppercl * 200), width = .1) +
      scale_fill_manual(values = palette) +
      labs(
        title = paste("Top 5 LTCs among GP and LA identified unpaid carers in", la_name, "using age-sex standardised \nprevalence per 1,000 population and rate ratio", sep = " "),
        x = "LTC",
        fill = "Identified by"
      )
  }
  
  return(out_plot)
}



# Plot top 5 hf

plot_top_5_hf <- function(df_top5_carers, top5_color_scheme, group_breakdown, la_name){
  
  # join top 5 cond to colour lookup (ensures same colour scheme per condition across plots)
  df_top5_carers <- df_top5_carers %>% 
    left_join(top5_color_scheme, by = c('cond_name' = 'top5_cond'))
  
  # order from highest to lowest for plotting
  if(group_breakdown == "all"){ 
    df_top5_carers <- df_top5_carers %>% 
      mutate(cond_name_order = reorder(cond_desc, -percentage))
  }
  
  if(group_breakdown == "lagp"){
    df_top5_carers <- df_top5_carers %>% 
      mutate(cond_name_order = reorder_within(cond_desc, -percentage, FIRST_IDENTIFIED_BY))
  }
    
  colour <- as.character(df_top5_carers$top5_colour) # get colour codes for plotting
  
  out_plot <- df_top5_carers %>%
    ggplot(aes(x = cond_name_order, 
               y = percentage, 
               fill = cond_name_order)) +
    geom_col() +
    labs( 
      title = paste("Most prevalent LTCs in unpaid carers in", la_name, sep = " "),
      y = "Percentage (%)",
      x = "LTC"
    ) +
    geom_text(aes(x = cond_name_order,  
                  y = percentage, 
                  label = percentage, 
                  group = cond_name_order), 
              vjust = -0.5) +
    scale_y_continuous(expand = c(0, 0), 
                       limits = c(0, 40), 
                       breaks = seq(0, 40, 10)) +
    scale_fill_manual(values = colour) +
    theme(  
      legend.position = 'none',
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.background = element_blank(), 
      axis.line = element_line(colour = "black")
    )
  
  # for all unpaid carers: add labels 
  if(group_breakdown == "all"){
    out_plot <- out_plot +
      labs( 
        title = paste("Most prevalent LTCs in unpaid carers in", la_name, sep = " "),
        y = "Percentage (%)",
        x = "LTC"
      )  
  }
  
  # for lagp: facet by la/gp, add labels & rotate x axis text
  if(group_breakdown == "lagp"){
    out_plot <- out_plot +
      facet_wrap(~ FIRST_IDENTIFIED_BY, scales = "free_x") +
      scale_x_reordered() +
      labs(
        title = paste("Most prevalent LTCs in unpaid carers in", la_name, "by LA and GP identified cohorts", sep = " "),
        y = "Percentage (%)",
        x = "LTC"
      ) +
      theme(  
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  }
  
  return(out_plot)
}



