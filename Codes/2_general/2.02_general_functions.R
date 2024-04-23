## This file contains functions used across multiple scripts
# created by PEJE - 31/01/2023


calc_percentage <- function(num, denom){
  
  
  return(round(num / denom * 100, digits = 3))
  #return(num / denom * 100)
  #return(as.numeric(formatC(num / denom * 100, format = "f", digits=1)))
}

save_csv_rds <- function(df, p){
  
  # saveRDS(df,paste0(p,'data/processed/1429_',deparse(substitute(df)), "_", initials, '.rds'))
  write.csv(df,paste0(p,'data/processed/1429_',deparse(substitute(df)),"_", initials, '.csv'),row.names = FALSE)
  
}


save_ggplot <- function(plot,p){
  #ggsave(paste0(p,'outputs/1429_',deparse(substitute(plot)),"_", initials, '.png') , plot, width = 7, height=7, units = "in")
  ggsave(paste0(p,'outputs/1429_',deparse(substitute(plot)),"_", initials, '.png'),  plot, width = 15.0, height=15.0, units = "in")
}

# for list of dataframes is using
listdf_save_csv_rds <- function(df_list, p, n, i){
  
  # saveRDS(df_list[[i]],paste0(p,'data/processed/1429_',n[[i]], "_", initials, '.rds'))
  write.csv(df_list[[i]],paste0(p,'data/processed/1429_',n[[i]], "_", initials, '.csv'),row.names = FALSE)
  
}