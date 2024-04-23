# This is adapted from code written by Dr Will Parry, sourced from HFAnalyticsLab GitHub
# Original code: https://github.com/HFAnalyticsLab/CMD_multimorbidity/blob/master/R/03_Create_Cambridge_Score.R

## ---- swansea_camcode_load_libraries ---- 

pkgs <- c('here', 'dplyr', 'data.table')
lapply(pkgs, library, character.only = T)


## ---- swansea_camcode_login ----

## Run file_paths_lb.R

source(paste0(main_path, "1429_RODBC_login_20221031.R"))

## ---- swansea_camcode_load_data ---- 

lb_cms_cancer_swansea_raw <- sqlQuery(sql, "SELECT * FROM SAILW1429V.LB_CMS_CANCER_LKUP_SWANSEA") # look up for first recorded cancer Read code within 5 years of index date
lb_cms_ckd_swansea_raw <- sqlQuery(sql, "SELECT * FROM SAILW1429V.LB_CMS_CKD_LKUP_SWANSEA_2") # look up for highest of 2 most recent kidney function tests <60ml/min
lb_cms_medcode_swansea_raw <- sqlQuery(sql, "SELECT * FROM SAILW1429V.LB_CMS_MEDCODE_LKUP_SWANSEA") # look up for all other 'medcode' Read codes within specified time
lb_cms_prodcode_swansea_raw <- sqlQuery(sql, "SELECT * FROM SAILW1429V.LB_CMS_PRODCODE_LKUP_SWANSEA") # look up for all 'prodcode' Read codes within specified period of time with prescription count (pcount) greater than or equal to Rx

# Read in SWANSEA cohort
lb_swansea_cohort_raw <- sqlQuery(sql, "SELECT * FROM SAILW1429V.PEJE_MATCHED_COHORT_SWANSEA_2")


## ---- swansea_camcode_data_prep ---- 

df_cms_swansea_lb <- rbindlist(list(lb_cms_cancer_swansea_raw, lb_cms_ckd_swansea_raw, lb_cms_medcode_swansea_raw, lb_cms_prodcode_swansea_raw), fill = TRUE)[ # bind lookup tables into one df
  , .(ALF_PE, REF, FLAG = 1)] %>% # restrict to required variables and add a flag (to indicate the ref exists)
  dcast(... ~ REF, value.var = 'FLAG', fill = 0) %>% # cast table wide (where patients do not have condition, fill with 0)
  .[, `:=` (ANX = ifelse(ANX140 != 0 | ANX141 != 0, 1, 0), # include logic where necessary and flag condition in new column
            AST = ifelse(AST127 != 0 & AST142 != 0, 1, 0),
            DEP = ifelse(DEP152 != 0 | DEP153 != 0, 1, 0),
            EPI = ifelse(EPI155 != 0 & EPI156 != 0, 1, 0),
            IBS = ifelse(IBS161 != 0 | IBS162 != 0, 1, 0),
            PNC = ifelse(PNC166 != 0 | (PNC167 != 0 & EPI155 == 0), 1, 0),
            PSO = ifelse(PSO171 != 0 & PSO172 != 0, 1, 0),
            SCZ = ifelse(SCZ175 != 0 | SCZ176 != 0, 1, 0),
            ANX140 = NULL, ANX141 = NULL, AST127 = NULL, AST142 = NULL, # remove all ref columns used with logic rules
            DEP152 = NULL, DEP153 = NULL, EPI155 = NULL, EPI156 = NULL, 
            IBS161 = NULL, IBS162 = NULL, PNC166 = NULL, PNC167 = NULL, 
            PSO171 = NULL, PSO172 = NULL, SCZ175 = NULL, SCZ176 = NULL
  )]

condrefs_lb <- names(df_cms_swansea_lb) %>% tail(-1) %>% sort() # get column names, remove ALF and order alphabetically
condcodes_lb <- substr(condrefs_lb, 1, 3) # create new column names using only the condition letters
setnames(df_cms_swansea_lb, condrefs_lb, condcodes_lb) # rename condition columns using reference vector to use first 3 letters
setcolorder(df_cms_swansea_lb, c('ALF_PE', condcodes_lb)) # set column order to alphabetical

df_cms_swansea_lb[, conds := rowSums(.SD), .SDcols = 2:length(condcodes_lb)] # add a count of how many conditions are flagged (sum all columns exc ALF_PE)

df_full_cms_swansea_lb <- df_cms_swansea_lb # copy full cms info to a new data frame so it's not lost in the next step.
condnames_lb <- names(df_full_cms_swansea_lb) %>% tail(-1) # get column names, remove ALF 

df_cms_swansea_lb <- df_cms_swansea_lb[lb_swansea_cohort_raw, on = .(ALF_PE = ALF_PE)] # join to full cohort
#df_cms_swansea_lb <- df_cms_swansea_lb[ , .SD, .SDcols = -(1:length(condcodes_lb) + 1)] # Remove all individual condition variables
cohortcols_lb <- names(lb_swansea_cohort_raw) # create vector with cohort column names
setcolorder(df_cms_swansea_lb, c(cohortcols_lb, condnames_lb)) # reorder so conditions column is at end

df_cms_swansea_lb[, (condnames_lb) := lapply(.SD, nafill, fill = 0), .SDcols = condnames_lb] # replace NAs with 0 in condition columns

df_cms_swansea_lb_dist <- df_cms_swansea_lb %>% distinct() # Distinct is required for Swansea.

## ---- swansea_camcode_save_data ---- 
saveRDS(df_cms_swansea_lb_dist, paste0(lb_ltc_path,"data/processed/1429_cms_swansea_lb_20221108.rds")) # save data frame