## ---- npt_ltc_bycond_lagp_set_up ----

# Load libraries
pkgs <- c("dplyr", "ggplot2", "data.table", "tidytext", "reshape2", "PHEindicatormethods", "tidyr") # package list
# lapply(pkgs, install.packages) # install packages
lapply(pkgs, library, character.only = T) # load packages

## Run file_paths_lb.R

source(paste0(main_path, "1429_color_palette.R")) # colour palette for internal plot

source(paste0(main_path, "1429_general_functions.R")) # source general functions

source(paste0(lb_ltc_path, "functions/1429_camcode_analysis_by_cond_functions_lb_20221109.R")) # ltc by condition functions

## ---- npt_ltc_bycond_lagp_load_data ----

cms_npt_raw <- readRDS(paste0(lb_ltc_path, "data/processed/1429_cms_npt_lb_20221108.rds")) # npt ltc data

cms_desc <- readRDS(paste0(lb_ltc_path, "data/processed/1429_ltc_desc_lookup_lb_20230112.rds")) # ltc descriptions for labelling plots

df_standardpop_lkup <- readRDS(paste0(look_up_path, "1429_standardpop_lkup_lb_20230228.rds")) # read in npt standard population look up - age-sex counts

top5_color_scheme <- readRDS(paste0(lb_ltc_path, "data/processed/1429_top5_color_scheme_lb_20221114.rds")) # colour scheme for HF plot


## ---- npt_ltc_bycond_lagp_data_prep ----

df_cms_npt <- cms_npt_raw # create copy of raw data

df_cms_npt[, age_group := cut(AGE_INDEXDATE,
                                 breaks = c(17, 39, 49, 59, 69, 79, Inf),
                                 labels = c("Under 40", "40-49", "50-59", "60-69", "70-79", "80+"))]


## ---- npt_ltc_bycond_lagp_crude ----

df_cms_npt_cond_counts_lagp <- bycondition(df_cms_npt, "lagp", cms_desc) # count controls/carers by la/gp with each ltc and calculate crude rates

df_cms_npt_cond_lagp_crude_rr <- calc_rr_lagp_crude(df_cms_npt_cond_counts_lagp) # calculate crude rate ratios and CIs for LA/GP-identified

df_cms_npt_top5_lagp_crude <- top5(df_cms_npt_cond_lagp_crude_rr, "lagp_crude") # get top 5 conditions for LA/GP-identified crude rates

## ---- npt_ltc_bycond_lagp_standardised ---- 

df_cms_npt_cond_counts_lagp_agesex <- bycondition(df_cms_npt, "age_sex", cms_desc) # count controls/carers per LTC by LA/GP-identified, age group and sex and calculate rates

df_cms_npt_cond_counts_lagp_agesex_std <- standardise(df_cms_npt, "Neath Port Talbot", df_cms_npt_cond_counts_lagp_agesex, df_standardpop_lkup) # calculate age-sex standardised rates for LA/GP-identified

df_cms_npt_cond_lagp_std_rr <- calc_rr_std(df_cms_npt_cond_counts_lagp_agesex_std, df_cms_npt_cond_counts_lagp) # calculate standardised rate ratios and CIs for LA/GP-identified.

# Top 5
df_cms_npt_top5_lagp_std <- top5(df_cms_npt_cond_lagp_std_rr, "lagp_std") # get top 5 conditions for LA/GP-identified using standardised rates


## ---- npt_ltc_bycond_lagp_save_data ----

initials <- "lb"

save_csv_rds(df_cms_npt_cond_lagp_crude_rr, lb_ltc_path) # save LA/GP identified crude rates per condition

save_csv_rds(df_cms_npt_top5_lagp_crude, lb_ltc_path) # save LA/GP top 5 crude rates

save_csv_rds(df_cms_npt_cond_lagp_std_rr, lb_ltc_path) # save LA/GP identified standardised rates per condition

save_csv_rds(df_cms_npt_top5_lagp_std, lb_ltc_path) # save LA/GP top 5 standardised rates


## ---- npt_ltc_bycond_lagp_plot_top5_carers_crude ----

plot_top_5_carers_npt_lagp_crude <- plot_top_5(df_cms_npt_top5_lagp_crude, "lagp_crude", "NPT", npt_lagpcarers_palette)


## ---- npt_ltc_bycond_lagp_plot_top5_carers_std ----

plot_top_5_carers_npt_lagp_std <- plot_top_5(df_cms_npt_top5_lagp_std, "lagp_std", "NPT", npt_lagpcarers_palette) 


## ---- npt_ltc_bycond_lagp_save_plots ----

ggsave(paste0(lb_ltc_path, "outputs/1429_plot_top_5_ltc_npt_lagp_crude_lb_20221114.png"), plot_top_5_carers_npt_lagp_crude, width = 9.0, height=9.0, units = "in", scale = 1.5)
ggsave(paste0(lb_ltc_path, "outputs/1429_plot_top_5_ltc_npt_lagp_std_lb_20221114.png"), plot_top_5_carers_npt_lagp_std)
