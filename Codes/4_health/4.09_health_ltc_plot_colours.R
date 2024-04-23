## This relies on all "by condition" cms scripts being run first

top5_cond <- c(df_cms_npt_top5_carers_lb$cond_name, df_cms_npt_top5_carers_lagp_lb$cond_name, df_cms_swansea_top5_carers_lb$cond_name, df_cms_swansea_top5_carers_lagp_lb$cond_name)
top5_cond <- unique(top5_cond)
top5_colour <- top5_palette

top5_color_scheme <- data.frame(top5_cond, top5_colour)

saveRDS(top5_color_scheme, paste0(lb_ltc_path, "data/processed/1429_top5_color_scheme_lb_20221114.rds")) # save 
