## ---- npt_ltc_lagp_set_up ----

# Load libraries
pkgs <- c("dplyr", "ggplot2", "data.table", "gtsummary", "PHEindicatormethods", "tidyr", "stats", "gtsummary", "ggsignif") # package list
# lapply(pkgs, install.packages) # install packages
lapply(pkgs, library, character.only = T) # load packages

# Run file_paths_lb.R

source(paste0(main_path, "1429_color_palette.R"))


## ---- npt_ltc_lagp_load_data ----

cms_npt_raw <- readRDS(paste0(lb_ltc_path, "data/processed/1429_cms_npt_lb_20221108.rds")) # read in npt matched cohort cms person level data

df_standardpop_lkup <- readRDS(paste0(look_up_path, "1429_standardpop_lkup_lb_20230228.rds")) # read in npt standard population look up

## ---- npt_ltc_lagp_functions -----

source(paste0(lb_ltc_path, "functions/1429_camcode_analysis_functions_lb_20221109.R")) # ltc functions

source(paste0(main_path, "1429_general_functions.R")) # source general functions

## ---- npt_ltc_lagp_data_prep -----
df_cms_npt_lagp <- cms_npt_raw # create copy of raw data

df_cms_npt_lagp <- data_prep(df_cms_npt_lagp, "lagp") # select columns, lower case, factorise age groups

## ---- npt_ltc_lagp_descriptives ----

# count number of conditions by carer status, demographic and first identified, output as list of tables
npt_count_cond_out_list_lagp <- count_n_cond(df_cms_npt_lagp, "lagp", "npt") #(df_cms, group_breakdown, la_name)

list2env(npt_count_cond_out_list_lagp, envir = .GlobalEnv) # move tables to environment


## ---- npt_ltc_lagp_mann_whitney ----

# Mann-Whitney U tests on exact conds count
npt_lagp_ltcs_lagp_stats_data <- ltc_mw(df_cms_npt_lagp, "lagp")


## ---- npt_ltc_lagp_calculate_rates ----

# number of conditions as a percentage of carer status, vartype and first identified
count_overall_cms_npt_lagp_hf <- calc_rate(count_overall_cms_npt_lagp, df_cms_npt_lagp, "lagp", 100, "hf") # (df, dfla, group_breakdown, rate_per, report_type)

# number of conditions as a rate per 1000 of carer status, vartype and first identified
count_overall_cms_npt_lagp <- calc_rate(count_overall_cms_npt_lagp, df_cms_npt_lagp, "lagp", 1000, "internal") # (df, dfla, group_breakdown, rate_per, report_type)

# reduce columns to bins only and distinct rows
count_overall_cms_npt_lagp_hf <- reduce_cols(count_overall_cms_npt_lagp_hf) # percentage for hf
count_overall_cms_npt_lagp <- reduce_cols(count_overall_cms_npt_lagp) # rate per 1000 for internal


## ---- standardisation ----

count_overall_cms_npt_lagp_std <- standardise(df_cms_npt_lagp, "Neath Port Talbot", count_sex_age_cms_npt_lagp, df_standardpop_lkup)


## ---- ratios ----

ratio_cms_npt_lagp_crude <- ratio_calc(count_overall_cms_npt_lagp, "lagp_crude", 1000) # crude LA/GP per 1000

ratio_cms_npt_lagp_std <- ratio_calc(count_overall_cms_npt_lagp_std, "lagp_std", 1000) # standardised LA/GP per 1000


## ---- npt_ltc_lagp_save_data ----
initials <- "lb"

# HF LA/GP
save_csv_rds(count_overall_cms_npt_lagp_hf, lb_ltc_path)

# Internal LA/GP
save_csv_rds(ratio_cms_npt_lagp_crude, lb_ltc_path) # carers only crude rate ratios
save_csv_rds(ratio_cms_npt_lagp_std, lb_ltc_path) # carers only standardised rate ratios


## ---- npt_ltc_lagp_plots_overall_bins ----

plot_overall_cms_npt_lagp_hf <- plot_percentage_bins(count_overall_cms_npt_lagp_hf, "lagp", 'NPT', ltc_palette) # stacked % plot for HF central analysis


## ---- npt_ltc_lagp_plots_overall_rr_bins_crude ----
plot_overall_cms_npt_lagp_crude <- plot_rr(ratio_cms_npt_lagp_crude, "lagp_crude", 'NPT', lagpcarers_palette) # Dodged rr plot for internal analysis

anno <- data.frame(x1 = 0.75, 
                   x2 = 1.25,
                   y1 = ceiling(ratio_cms_npt_lagp_crude$GP_rate_group[1]) + 20,  
                   y2 = ceiling(ratio_cms_npt_lagp_crude$GP_rate_group[1]) + 25,
                   xstar = 1,
                   ystar = ceiling(ratio_cms_npt_lagp_crude$GP_rate_group[1]) + 30,
                   lab = "*", 
                   conds_group = "0")

plot_overall_cms_npt_lagp_crude <- plot_overall_cms_npt_lagp_crude +
  geom_text(data = anno, aes(x = xstar, y = ystar, label = lab)) +
  geom_segment(data = anno, aes(x = x1, xend = x1, y = y1, yend = y2), colour = "black") +
  geom_segment(data = anno, aes(x = x2, xend = x2, y = y1, yend = y2), colour = "black") +
  geom_segment(data = anno, aes(x = x1, xend = x2, y = y2, yend = y2), colour = "black")

## ---- npt_ltc_lagp_plots_overall_rr_bins_std ----
plot_overall_cms_npt_lagp_std <- plot_rr(ratio_cms_npt_lagp_std, "lagp_std", 'NPT', lagpcarers_palette) # Dodged rr plot for internal analysis

## ---- npt_ltc_lagp_save_plots ----

## HF central analysis
ggsave(paste0(lb_ltc_path, "outputs/1429_plot_overall_cms_npt_lagp_hf_lb_20221109.png"), plot_overall_cms_npt_lagp_hf, width = 9.0, height=9.0, units = "in")

## internal report
ggsave(paste0(lb_ltc_path, "outputs/1429_plot_overall_cms_npt_lagp_crude_lb.png"), plot_overall_cms_npt_lagp_crude, width = 9.0, height=9.0, units = "in")
ggsave(paste0(lb_ltc_path, "outputs/1429_plot_overall_cms_npt_lagp_std_lb.png"), plot_overall_cms_npt_lagp_std, width = 9.0, height=9.0, units = "in")

