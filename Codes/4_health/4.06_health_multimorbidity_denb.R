## ---- denbighshire_ltc_set_up ----

# Load libraries
pkgs <- c("dplyr", "ggplot2", "data.table", "tidyr") # package list
# lapply(pkgs, install.packages) # install packages
lapply(pkgs, library, character.only = T) # load packages

# Run file_paths_lb.R

source(paste0(main_path, "1429_color_palette.R"))


## ---- denbighshire_ltc_load_data ----

cms_denbighshire_raw <- readRDS(paste0(lb_ltc_path, "data/processed/1429_cms_denbighshire_lb_20230421.rds")) # read in Denbighshire matched cohort cms person level data

## ---- denbighshire_ltc_functions -----

source(paste0(lb_ltc_path, "functions/1429_camcode_analysis_functions_lb_20221109.R")) # source ltc functions

source(paste0(main_path, "1429_general_functions.R")) # source general functions

## ---- denbighshire_ltc_data_prep -----

df_cms_denbighshire <- cms_denbighshire_raw # create copy of raw data

df_cms_denbighshire <- data_prep(df_cms_denbighshire, "carer_type") # select columns, lower case, factorise age groups

## ---- denbighshire_ltc_descriptives ----

# count number of conditions by carer status and demographic, output as list of tables
denbighshire_count_cond_out_list <- count_n_cond(df_cms_denbighshire, "carer_type", "denbighshire") #(df_cms, group_breakdown, la_name)

list2env(denbighshire_count_cond_out_list, envir = .GlobalEnv) # move tables to environment


## ---- wilcox ----

# stats comparison btwn carers / non-carers for total number of conditions
#  Mann-Whitney U test
denbighshire_carer_type_ltcs_stats_data <- wilcox.test(conds ~ treated, data = df_cms_denbighshire, exact = FALSE, paired = FALSE) # comparison of counts for exact number conditions (not grouped)


## ---- calculate_rates ----

# number of conditions as a percentage of carer status and vartype (demographic)
count_overall_cms_denbighshire_hf <- calc_rate(count_overall_cms_denbighshire, df_cms_denbighshire, "carer_type", 100, "hf") # input: (df = condition count df, df_la = individual level df for calculating total individuals, group breakdown, rate_per, report_type)

# number of conditions as a rate per 1000 of carer status and vartype (demographic)
count_overall_cms_denbighshire <- calc_rate(count_overall_cms_denbighshire, df_cms_denbighshire, "carer_type", 1000, "internal") # input: (df = condition count df, df_la = individual level df for calculating total individuals, group breakdown, rate_per, report_type)

# remove columns relating to single counts and distinct
count_overall_cms_denbighshire_hf <- reduce_cols(count_overall_cms_denbighshire_hf) 
count_overall_cms_denbighshire <- reduce_cols(count_overall_cms_denbighshire) 


## ---- ratios ----

ratio_cms_denbighshire_carer_status <- ratio_calc(count_overall_cms_denbighshire, "carer_type", 1000) 


## ---- denbighshire_ltc_save_data_tables ----

initials <- "lb"

# carer/non-carer counts
save_csv_rds(count_overall_cms_denbighshire_hf, lb_ltc_path) # save hf count & rate long table

# carer/non-carer ratios
save_csv_rds(ratio_cms_denbighshire_carer_status, lb_ltc_path) # save internal count, rate and ratio wide table


## ---- denbighshire_ltc_plots_overall_bins ----
plot_overall_cms_denbighshire_hf <- plot_percentage_bins(count_overall_cms_denbighshire_hf, "carer_type", 'Denbighshire', ltc_palette) # plot: (data, group_breakdown, LA name, palette)


## ---- denbighshire_ltc_plots_overall_rr_bins ----
plot_overall_cms_denbighshire <- plot_rr(ratio_cms_denbighshire_carer_status, "carer_type", 'Denbighshire', denbighshire_carertype_palette) # plot: (data, group_breakdown, LA name, palette)

anno <- data.frame(x1 = c(0.75, 2.75), 
                   x2 = c(1.25, 3.25),
                   y1 = c(ceiling(ratio_cms_denbighshire_carer_status$non_carer_rate_group[1]) + 20, ceiling(ratio_cms_denbighshire_carer_status$unpaid_carer_rate_group[3]) + 20), 
                   y2 = c(ceiling(ratio_cms_denbighshire_carer_status$non_carer_rate_group[1]) + 25, ceiling(ratio_cms_denbighshire_carer_status$unpaid_carer_rate_group[3]) + 25), 
                   xstar = c(1, 3),
                   ystar = c(ceiling(ratio_cms_denbighshire_carer_status$non_carer_rate_group[1]) + 30, ceiling(ratio_cms_denbighshire_carer_status$unpaid_carer_rate_group[3]) + 30), 
                   lab = c("*", "*"), 
                   conds_group = c("0", "2+"))

plot_overall_cms_denbighshire <- plot_overall_cms_denbighshire +
  geom_text(data = anno, aes(x = xstar, y = ystar, label = lab)) +
  geom_segment(data = anno, aes(x = x1, xend = x1, y = y1, yend = y2), colour = "black") +
  geom_segment(data = anno, aes(x = x2, xend = x2, y = y1, yend = y2), colour = "black") +
  geom_segment(data = anno, aes(x = x1, xend = x2, y = y2, yend = y2), colour = "black")


## ---- denbighshire_ltc_save_plots ----
ggsave(paste0(lb_ltc_path, "outputs/1429_plot_overall_cms_denbighshire_ucnc_hf_lb_20230421.png"), plot_overall_cms_denbighshire_hf, width = 9.0, height=9.0, units = "in")

ggsave(paste0(lb_ltc_path, "outputs/1429_plot_overall_cms_denbighshire_ucnc_lb_20230421.png"), plot_overall_cms_denbighshire, width = 9.0, height=9.0, units = "in")

