## ---- ltc_cond_desc_set_up ----

# Load libraries
pkgs_lb <- c("dplyr", "ggplot2", "data.table") # package list
# lapply(pkgs_lb, install.packages) # install packages
lapply(pkgs_lb, library, character.only = T) # load packages

# Run file_paths_lb.R


## ---- ltc_cond_desc_load_data ----

cam_codelist_wales <- readRDS(paste0(look_up_path, "cam_codelist_wales.rds"))


## ---- ltc_cond_desc_data_prep ----
df_ltc_desc <- unique(cam_codelist_wales[, c('cond', 'cond_desc')])


## ---- ltc_cond_desc_save_data_tables ----
saveRDS(df_ltc_desc, paste0(lb_ltc_path, "data/processed/1429_ltc_desc_lookup_lb_20230112.rds"))
