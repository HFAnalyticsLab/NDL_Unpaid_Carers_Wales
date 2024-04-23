#'This script calculates the breakdown of ethnicity in percentages, including percentage with missing ethnicity date, per LA'#

#---- load libraries ----
library(dplyr)
library(ggplot2)
library(AMR)
library(RODBC)
library(sqldf)
library(stats)
library(tigerstats)

source('../../filepaths.R') # read in filepaths
source(paste0(main_project_path, '1429_RODBC_login_20221031.R'))

## ---- Demog_eth_dataprep----
# import NPT's unpaid carers data
npt_cohort <- sqlQuery(sql, "select ALF_PE,ETHNICITY_DESC,FIRST_IDENTIFIED_BY as IDENTIFIED_BY from SAILW1429V.PEJE_MATCHED_COHORT_NPT_2 WHERE TREATED = 1")
npt_cohort[1] <- lapply(npt_cohort[1], addNA) # allows NAs to be included as a factor in xtabs
nrow(npt_cohort) 

# count missing
summary(as.factor(npt_cohort$ETHNICITY_DESC))
# perc missing
data.frame(rowPerc(xtabs( ~ ETHNICITY_DESC, data=npt_cohort)))
data.frame(rowPerc(xtabs( ~ IDENTIFIED_BY+as.factor(ETHNICITY_DESC), data=npt_cohort)))

# import Swansea's unpaid carers data
swansea_cohort <- sqlQuery(sql, "select DISTINCT ALF_PE, ETHNICITY_DESC, FIRST_IDENTIFIED_BY as IDENTIFIED_BY from SAILW1429V.PEJE_MATCHED_COHORT_SWANSEA_2 WHERE TREATED = 1")
swansea_cohort[1] <- lapply(swansea_cohort[1], addNA)
nrow(swansea_cohort) 

summary(as.factor(swansea_cohort$ETHNICITY_DESC))
data.frame(rowPerc(xtabs( ~ ETHNICITY_DESC, data=swansea_cohort)))
data.frame(rowPerc(xtabs( ~ IDENTIFIED_BY+as.factor(ETHNICITY_DESC), data=swansea_cohort)))

# import Denbighshire's unpaid carers data
denbighshire_cohort <- sqlQuery(sql, "select ETHNICITY_DESC,FIRST_IDENTIFIED_BY as IDENTIFIED_BY from SAILW1429V.PEJE_MATCHED_COHORT_DENBIGHSHIRE_2 WHERE TREATED = 1")
denbighshire_cohort[1] <- lapply(denbighshire_cohort[1], addNA)
nrow(denbighshire_cohort) 

summary(as.factor(denbighshire_cohort$ETHNICITY_DESC))
data.frame(rowPerc(xtabs( ~ ETHNICITY_DESC, data=denbighshire_cohort)))
data.frame(rowPerc(xtabs( ~ IDENTIFIED_BY+as.factor(ETHNICITY_DESC), data=denbighshire_cohort)))