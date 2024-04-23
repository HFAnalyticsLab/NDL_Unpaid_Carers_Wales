## ---- all_la_ltc_set_up ----

# Load libraries
pkgs_lb <- c("dplyr", "ggplot2", "data.table", "gtsummary", "PHEindicatormethods", "tidyr", "stats", "gtsummary", "ggsignif") # package list
# lapply(pkgs_lb, install.packages) # install packages
lapply(pkgs_lb, library, character.only = T) # load packages

# Run file_paths_lb.R

source(paste0(main_path, "1429_color_palette.R")) # source colour palette

source(paste0(main_path, "1429_general_functions.R")) # source general functions

source(paste0(lb_ltc_path, "functions/1429_camcode_analysis_functions_lb_20221109.R")) # source ltc functions

## ---- all_la_ltc_load_data ----

# Crude rates carer/non-carer data
ratio_cms_npt_carer_status <- read.csv(paste0(lb_ltc_path, "data/processed/1429_ratio_cms_npt_carer_status_lb.csv"))
ratio_cms_swansea_carer_status <- read.csv(paste0(lb_ltc_path, "data/processed/1429_ratio_cms_swansea_carer_status_lb.csv"))
ratio_cms_denbighshire_carer_status <- read.csv(paste0(lb_ltc_path, "data/processed/1429_ratio_cms_denbighshire_carer_status_lb.csv"))

# Crude rates lagp data
ratio_cms_npt_lagp_crude <- read.csv(paste0(lb_ltc_path, "data/processed/1429_ratio_cms_npt_lagp_crude_lb.csv"))
ratio_cms_swansea_lagp_crude <- read.csv(paste0(lb_ltc_path, "data/processed/1429_ratio_cms_swansea_lagp_crude_lb.csv"))
ratio_cms_denbighshire_lagp_crude <- read.csv(paste0(lb_ltc_path, "data/processed/1429_ratio_cms_denbighshire_lagp_crude_lb.csv"))

# Top 5 crude rates carer/non-carer data
df_cms_npt_top5_lb <- read.csv(paste0(lb_ltc_path, "data/processed/1429_top5_ltc_npt_lb_20221114.csv"))
df_cms_swansea_top5_lb <- read.csv(paste0(lb_ltc_path, "data/processed/1429_top5_ltc_swansea_lb_20221114.csv"))
df_cms_denbighshire_top5_lb <- read.csv(paste0(lb_ltc_path, "data/processed/1429_top5_ltc_denbighshire_lb_20230421.csv"))


## ---- all_la_ltc_data_prep ----

# Crude rates carer/non-carer combine
ratio_df_all_carer_status <- prep_all_rates(ratio_cms_npt_carer_status, ratio_cms_swansea_carer_status, ratio_cms_denbighshire_carer_status)

ratio_df_all_carer_status <- ratio_df_all_carer_status %>%
  select(starts_with("cond"), starts_with("un"), everything()) # order columns

initials <- "lb"
save_csv_rds(ratio_df_all_carer_status, lb_ltc_path)


# Crude rates lagp data combine
ratio_df_all_lagp <- prep_all_rates(ratio_cms_npt_lagp_crude, ratio_cms_swansea_lagp_crude, ratio_cms_denbighshire_lagp_crude)

initials <- "gbh"
save_csv_rds(ratio_df_all_lagp, lb_ltc_path)


# Top 5 crude rates carer/non-carer combine
top5_df_all_carer_status <- prep_all_rates(df_cms_npt_top5_lb, df_cms_swansea_top5_lb, df_cms_denbighshire_top5_lb) %>%
  arrange(LA_name, -unpaid_carer_crude_rate)

initials <- "lb"
save_csv_rds(top5_df_all_carer_status, lb_ltc_path)

## ---- all_la_ltc_plot ----

# crude rates lagp plot
plot_overall_cms_all_lagp_crude_gbh <- plot_all_rates(ratio_df_all_lagp, lagpcarers_palette)

ggsave(paste0(lb_ltc_path, "outputs/1429_plot_overall_cms_all_lagp_crude_gbh.png"), plot_overall_cms_all_lagp_crude_gbh, width = 9.0, height = 9.0, units = "in")
