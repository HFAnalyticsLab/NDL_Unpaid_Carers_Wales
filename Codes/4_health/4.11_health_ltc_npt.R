## ---- npt_ltc_bycond_set_up ----

# Load libraries
pkgs <- c("dplyr", "ggplot2", "data.table", "tidytext", "reshape2", "tidyr") # package list
# lapply(pkgs, install.packages) # install packages
lapply(pkgs, library, character.only = T) # load packages

# Run file_paths_lb.R

source(paste0(main_path, "1429_color_palette.R"))

source(paste0(main_path, "1429_general_functions.R")) # source general functions

source(paste0(lb_ltc_path, "functions/1429_camcode_analysis_by_cond_functions_lb_20221109.R")) # source ltc functions

## ---- npt_ltc_bycond_load_data ----

cms_npt_raw <- readRDS(paste0(lb_ltc_path, "data/processed/1429_cms_npt_lb_20221108.rds")) # ltc data

cms_desc <- readRDS(paste0(lb_ltc_path, "data/processed/1429_ltc_desc_lookup_lb_20230112.rds")) # ltc descriptions for graphing

top5_color_scheme <- readRDS(paste0(lb_ltc_path, "data/processed/1429_top5_color_scheme_lb_20221114.rds")) # colour scheme for HF plot


## ---- npt_ltc_bycond_data_prep ----

df_cms_npt <- cms_npt_raw # create copy of raw data


## ---- npt_ltc_bycond_descriptives -----

df_cms_npt_cond_counts <- bycondition(df_cms_npt, "carer_type", cms_desc) # count controls/carers with each ltc and calculate percentages

df_cms_npt_cond_rr <- calc_rr(df_cms_npt_cond_counts) # calculate rr & CI per condition for uc/nc

df_cms_npt_top5 <- top5(df_cms_npt_cond_rr, "carer_type") # top 5 for uc/nc

df_cms_npt_top5_carers <- top5(df_cms_npt_cond_counts, "hf") # top 5 for uc only with rate per 1000 and percentage


## ---- npt_ltc_bycond_save_data ----

initials <- "lb"

save_csv_rds(df_cms_npt_cond_rr, lb_ltc_path) # save full condition counts & rates data table

save_csv_rds(df_cms_npt_top5, lb_ltc_path) #  save carer/control top 5 data table as csv

save_csv_rds(df_cms_npt_top5_carers, lb_ltc_path) # save carer only top 5 data table as csv


## ---- npt_ltc_bycond_plot_top5_carers_controls ----

plot_top_5_npt <- plot_top_5(df_cms_npt_top5, "carer_type", "NPT", npt_carertype_palette) # plot top 5 in uc and nc (total 6 conds) with rr and ci


## ---- npt_ltc_bycond_plot_top5_carers ----

plot_top_5_carers_npt <- plot_top_5_hf(df_cms_npt_top5_carers, top5_color_scheme, "all", "NPT") # HF plot top 5 carers only


## ---- npt_ltc_bycond_save_plots ----

ggsave(paste0(lb_ltc_path, "outputs/1429_plot_top_5_ltc_npt_lb_20221114.png"), plot_top_5_npt)
ggsave(paste0(lb_ltc_path, "outputs/1429_plot_top_5_ltc_carers_npt_lb_20221114.png"), plot_top_5_carers_npt)