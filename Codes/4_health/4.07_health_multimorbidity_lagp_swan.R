## ---- swansea_ltc_lagp_set_up ----

# Load libraries
pkgs <- c("dplyr", "ggplot2", "data.table", "gtsummary", "stats", "purrr") # package list
# lapply(pkgs, install.packages) # install packages
lapply(pkgs, library, character.only = T) # load packages

# Run file_paths_lb.R

source(paste0(main_path, "1429_color_palette.R"))


## ---- swansea_ltc_lagp_load_data ----

cms_swansea_raw <- readRDS(paste0(lb_ltc_path, "data/processed/1429_cms_swansea_lb_20221108.rds")) # read in swansea matched cohort cms person level data

df_standardpop_lkup <- readRDS(paste0(look_up_path, "1429_standardpop_lkup_lb_20230228.rds")) # read in standard population look up

## ---- swansea_ltc_lagp_functions -----

source(paste0(lb_ltc_path, "functions/1429_camcode_analysis_functions_lb_20221109.R")) # ltc functions

source(paste0(main_path, "1429_general_functions.R")) # source general functions

## ---- swansea_ltc_lagp_data_prep -----
df_cms_swansea_lagp <- cms_swansea_raw # create copy of raw data

df_cms_swansea_lagp <- data_prep(df_cms_swansea_lagp, "lagp") # select columns, lower case, factorise age groups

## ---- swansea_ltc_lagp_descriptives ----

# count number of conditions by carer status, demographic and first identified, output as list of tables
swansea_count_cond_out_list_lagp <- count_n_cond(df_cms_swansea_lagp, "lagp", "swansea") #(df_cms, group_breakdown, la_name)

list2env(swansea_count_cond_out_list_lagp, envir = .GlobalEnv) # move tables to environment


## ---- swansea_ltc_lagp_mann_whitney ----

# Mann-Whitney U tests on exact conds count
swansea_lagp_ltcs_lagp_stats_data <- ltc_mw(df_cms_swansea_lagp, "lagp")


## ---- swansea_ltc_lagp_calculate_rates ----

# number of conditions as a percentage of carer status, vartype and first identified
count_overall_cms_swansea_lagp_hf <- calc_rate(count_overall_cms_swansea_lagp, df_cms_swansea_lagp, "lagp", 100, "hf") # (df, dfla, group_breakdown, rate_per, report_type)

# number of conditions as a rate per 1000 of carer status, vartype and first identified
count_overall_cms_swansea_lagp <- calc_rate(count_overall_cms_swansea_lagp, df_cms_swansea_lagp, "lagp", 1000, "internal") # (df, dfla, group_breakdown, rate_per, report_type)

# reduce columns to bins only and distinct rows
count_overall_cms_swansea_lagp_hf <- reduce_cols(count_overall_cms_swansea_lagp_hf) # percentage for hf
count_overall_cms_swansea_lagp <- reduce_cols(count_overall_cms_swansea_lagp) # rate per 1000 for internal


## ---- standardisation ----

count_overall_cms_swansea_lagp_std <- standardise(df_cms_swansea_lagp, "Swansea", count_sex_age_cms_swansea_lagp, df_standardpop_lkup)


## ---- ratios ----

ratio_cms_swansea_lagp_crude <- ratio_calc(count_overall_cms_swansea_lagp, "lagp_crude", 1000) # crude LA/GP per 1000

ratio_cms_swansea_lagp_std <- ratio_calc(count_overall_cms_swansea_lagp_std, "lagp_std", 1000) # standardised LA/GP per 1000


# ## ---- swansea_ltc_lagp_save_data ----
initials <- "lb"

# HF LA/GP
save_csv_rds(count_overall_cms_swansea_lagp_hf, lb_ltc_path)

# Internal LA/GP
save_csv_rds(ratio_cms_swansea_lagp_crude, lb_ltc_path) # carers only crude rate ratios
save_csv_rds(ratio_cms_swansea_lagp_std, lb_ltc_path) # carer only standardised ratios


## ---- swansea_ltc_lagp_plots_overall_bins ----

plot_overall_cms_swansea_lagp_hf <- plot_percentage_bins(count_overall_cms_swansea_lagp_hf, "lagp", 'Swansea', ltc_palette) # stacked % plot for HF central analysis


## ---- swansea_ltc_lagp_plots_overall_rr_bins_crude ----

plot_overall_cms_swansea_lagp_crude <- plot_rr(ratio_cms_swansea_lagp_crude, "lagp_crude", 'Swansea', lagpcarers_palette) # Dodged rr plot for internal analysis
# No sig. diffs


## ---- swansea_ltc_lagp_plots_overall_rr_bins_std ----

plot_overall_cms_swansea_lagp_std <- plot_rr(ratio_cms_swansea_lagp_std, "lagp_std", 'Swansea', lagpcarers_palette) # Dodged rr plot for internal analysis
# No sig. diffs

## ---- swansea_ltc_lagp_save_plots ----

## HF central analysis
ggsave(paste0(lb_ltc_path, "outputs/1429_plot_overall_cms_swansea_lagp_hf_lb_20221109.png"), plot_overall_cms_swansea_lagp_hf, width = 9.0, height=9.0, units = "in")

## internal report
ggsave(paste0(lb_ltc_path, "outputs/1429_plot_overall_cms_swansea_lagp_crude_lb.png"), plot_overall_cms_swansea_lagp_crude, width = 9.0, height=9.0, units = "in")
ggsave(paste0(lb_ltc_path, "outputs/1429_plot_overall_cms_swansea_lagp_std_lb.png"), plot_overall_cms_swansea_lagp_std, width = 9.0, height=9.0, units = "in")

