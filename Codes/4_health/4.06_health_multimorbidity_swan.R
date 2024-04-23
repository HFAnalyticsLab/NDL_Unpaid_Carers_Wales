## ---- swansea_ltc_set_up ----

# Load libraries
pkgs <- c("dplyr", "ggplot2", "data.table", "tidyr") # package list
# lapply(pkgs, install.packages) # install packages
lapply(pkgs, library, character.only = T) # load packages

# Run file_paths_lb.R

source(paste0(main_path, "1429_color_palette.R"))


## ---- swansea_ltc_load_data ----

cms_swansea_raw <- readRDS(paste0(lb_ltc_path, "data/processed/1429_cms_swansea_lb_20221108.rds")) # read in Swansea matched cohort cms person level data

## ---- swansea_ltc_functions -----

source(paste0(lb_ltc_path, "functions/1429_camcode_analysis_functions_lb_20221109.R"))

source(paste0(main_path, "1429_general_functions.R")) # source general functions

## ---- swansea_ltc_data_prep -----

df_cms_swansea <- cms_swansea_raw # create copy of raw data

df_cms_swansea <- data_prep(df_cms_swansea, "carer_type") # select columns, lower case, factorise age groups

## ---- swansea_ltc_descriptives ----

# count number of conditions by carer status and demographic, output as list of tables
swansea_count_cond_out_list <- count_n_cond(df_cms_swansea, "carer_type", "swansea") #(df_cms, group_breakdown, la_name)

list2env(swansea_count_cond_out_list, envir = .GlobalEnv) # move tables to environment


## ---- wilcox ----

# stats comparison btwn carers / non-carers for total number of conditions
#  Mann-Whitney U test
swansea_carer_type_ltcs_stats_data <- wilcox.test(conds ~ treated, data = df_cms_swansea, exact = FALSE, paired = FALSE) # comparison of counts for exact number conditions (not grouped)


## ---- calculate_rates ----

# number of conditions as a percentage of carer status and vartype (demographic)
count_overall_cms_swansea_hf <- calc_rate(count_overall_cms_swansea, df_cms_swansea, "carer_type", 100, "hf") # input: (df = condition count df, df_la = individual level df for calculating total individuals, group breakdown, rate_per, report_type)

# number of conditions as a rate per 1000 of carer status and vartype (demographic)
count_overall_cms_swansea <- calc_rate(count_overall_cms_swansea, df_cms_swansea, "carer_type", 1000, "internal") # input: (df = condition count df, df_la = individual level df for calculating total individuals, group breakdown, rate_per, report_type)

# remove columns relating to single counts and distinct
count_overall_cms_swansea_hf <- reduce_cols(count_overall_cms_swansea_hf) 
count_overall_cms_swansea <- reduce_cols(count_overall_cms_swansea)


## ---- ratios ----

ratio_cms_swansea_carer_status <- ratio_calc(count_overall_cms_swansea, "carer_type", 1000) 


## ---- swansea_ltc_save_data_tables ----

initials <- "lb"

# carer/non-carer counts
save_csv_rds(count_overall_cms_swansea_hf, lb_ltc_path) # save hf count & rate long table

# carer/non-carer ratios
save_csv_rds(ratio_cms_swansea_carer_status, lb_ltc_path) # save internal count, rate and ratio wide table


## ---- swansea_ltc_plots_overall_bins ----
plot_overall_cms_swansea_hf <- plot_percentage_bins(count_overall_cms_swansea_hf, "carer_type", 'Swansea', ltc_palette) # plot: (data, group_breakdown, LA name, palette)


## ---- swansea_ltc_plots_overall_rr_bins ----
plot_overall_cms_swansea <- plot_rr(ratio_cms_swansea_carer_status, "carer_type", 'Swansea', swansea_carertype_palette) # plot: (data, group_breakdown, LA name, palette)

anno <- data.frame(x1 = c(0.75, 2.75), 
                   x2 = c(1.25, 3.25),
                   y1 = c(ceiling(ratio_cms_swansea_carer_status$non_carer_rate_group[1]) + 20, ceiling(ratio_cms_swansea_carer_status$unpaid_carer_rate_group[3]) + 20), 
                   y2 = c(ceiling(ratio_cms_swansea_carer_status$non_carer_rate_group[1]) + 25, ceiling(ratio_cms_swansea_carer_status$unpaid_carer_rate_group[3]) + 25), 
                   xstar = c(1, 3),
                   ystar = c(ceiling(ratio_cms_swansea_carer_status$non_carer_rate_group[1]) + 30, ceiling(ratio_cms_swansea_carer_status$unpaid_carer_rate_group[3]) + 30), 
                   lab = c("*", "*"), 
                   conds_group = c("0", "2+"))

plot_overall_cms_swansea <- plot_overall_cms_swansea +
  geom_text(data = anno, aes(x = xstar, y = ystar, label = lab)) +
  geom_segment(data = anno, aes(x = x1, xend = x1, y = y1, yend = y2), colour = "black") +
  geom_segment(data = anno, aes(x = x2, xend = x2, y = y1, yend = y2), colour = "black") +
  geom_segment(data = anno, aes(x = x1, xend = x2, y = y2, yend = y2), colour = "black")


## ---- swansea_ltc_save_plots ----
ggsave(paste0(lb_ltc_path, "outputs/1429_plot_overall_cms_swansea_ucnc_hf_lb_20221109.png"), plot_overall_cms_swansea_hf, width = 9.0, height=9.0, units = "in") # save hf plot

ggsave(paste0(lb_ltc_path, "outputs/1429_plot_overall_cms_swansea_ucnc_lb_20221109.png"), plot_overall_cms_swansea, width = 9.0, height=9.0, units = "in") # save internal plot

