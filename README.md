![plot](https://github.com/tom-prendergast-thf/NDL_Unpaid_Carers_Wales/blob/main/ndlbanner.png)

# Networked Data Lab: NDL Wales analysis on unpaid carers at a local authority level in Wales

#### Project Status: In-progress

## Project Description

- This Networked Data Lab analysis by the NDL lab in Wales focusses on providing new insights into the demographics, health and health service use of unpaid carers at a local authority level in Wales.
- Please note that these research outputs have not yet been peer-reviewed and should be treated as preliminary.

## Data sources

This analysis used the following data sources, accessed via the SAIL Databank, Project 1429:
- Annual District Death Extract (ADDE) (as at 01/07/2022) - used to determine individuals who meet the study criteria for inclusion in the cohort.
- Emergency Department Data Set (EDDS) (as at 01/06/2022) - used to identify Emergency Department attendances.
- Outpatient Database Wales (OPDW) (as at 01/05/2022) - used to identify outpatient attendances. 
- Patient Episode Database Wales (PEDW) (as at 04/07/2022) - used to identify inpatient admissions.
- Unpaid carer data for Denbighshire Local Authority (as at 22/12/2022) - used to identify individuals in Denbighshire who received a carer assessment during the study period.
- Unpaid carer data for Neath Port Talbot Local Authority (as at 27/06/2022) - used to identify individuals in Neath Port Talbot who received a carer assessment during the study period.
- Unpaid carer data for Swansea Local Authority (as at 10/08/2022) - used to identify individuals in Swansea who received a carer assessment during the study period.
- Welsh Demographic Service Dataset (WDSD) (as at 04/07/2022) - used to determine individuals that meet the study criteria for inclusion in the cohort and their demographic information.
- Wales Longitudinal General Practice (WLGP) (as at 01/03/2021) - used to identify individuals who had an unpaid carer Read code during the study period.
  
## Requirements

SQL was written to query a DB2 database in SAIL Project 1429. R scripts were written in R 4.1.3 and run in RStudio.

## Getting started

- Section 1: Cohort creation 
  * 1.01_cohort_creation_gp.sql: Creates GP unpaid carer cohort using inclusion criteria
  * 1.02_cohort_creation_la.sql: Creates LA unpaid carer cohort using inclusion criteria
  * 1.03_cohort_creation_deduplication.sql: Combines GP and LA carers into one cohort and removes duplicated individuals
  * 1.04_cohort_creation_overlap.sql: Extracts duplicated individuals (i.e. in both GP and LA cohort)
  * 1.05_cohort_creation_matching_pool.sql: Creates matching pool of potential 'non-carer' matches
  * 1.06_cohort_creation_matching_functions.R: Functions for creating matched non-carer cohort
  * 1.07_cohort_creation_matched_cohort.R: Creates matched non-carer cohort
  * 1.08_cohort_creation_exclusion_criteria.R: Logic to ascertain matched non-carers are matched to specific unpaid carer demographics

- Section 2: General
  * 2.01_general_colour_palette.R: Colour palette variables for use in plotting
  * 2.02_general_functions.R: General analysis functions to calculate percentages, save data tables and save plots
 
- Section 3: Demographics
  * 3.01_demographics_timeline_functions.R: Functions for identification timeline
  * 3.02_demographics_timeline.R: Produces counts for identification timelines and exploratory timeline plots
  * 3.03_demographics_euler_timeline.R: Produces Euler diagrams and identification timelines
  * 3.04_demographics_functions.R: Functions for analysis of demographics (sex, age, WIMD, RUC)
  * 3.05_demographics_plot_functions.R: Functions for plots of demographics (sex, age, WIMD, RUC)
  * 3.06_demographics.R: Analysis of demographics (sex, age, WIMD, RUC)
  * 3.07_demographics_ethnicity.R: Analysis of ethnicity
  * 3.08_demographics_ons_population.R: Calculates percentage of general adult population identified in unpaid carer cohort in 2021/22 financial year
  * 3.09_demographics_ons_functions.R: Functions for analysis using ONS population estimates
  * 3.10_demographics_ons_sex.R: Anaysis of sex using ONS 2020 mid-year population estimates
  * 3.11_demographics_ons_age.R: Anaysis of age using ONS 2020 mid-year population estimates
  * 3.12_demographics_ons_wimd.R: Anaysis of WIMD using ONS 2020 mid-year population estimates
 
- Section 4: Health
  * 4.01_health_codelist.R: Creates Read code lookup for long-term conditions from Cambridge Primary Care Unit and Hanlon et. al. (2022). **Adapted from code written by Dr Will Parry, sourced from HFAnalyticsLab GitHub. Original code: https://github.com/HFAnalyticsLab/CMD_multimorbidity/blob/master/R/02_Create_CamCodeList.R**
  * 4.02_health_data_extract.sql: Extracts long-term conditions from WLGP
  * 4.03_health_cohort_ltc.R: Combines long-term conditions data extract with cohort data. **Adapted from code written by Dr Will Parry, sourced from HFAnalyticsLab GitHub. Original code: https://github.com/HFAnalyticsLab/CMD_multimorbidity/blob/master/R/03_Create_Cambridge_Score.R**
  * 4.04_health_sex_age_standardisation.R: Creates sex-age population lookup using ONS 2020-mid-year population estimates for standardisation of health analysis
  * 4.05_health_multimorbidity_functions.R: Functions for analysis of multimorditiy
  * 4.06_health_multimorbidity.R: Analysis of multimorbidity by unpaid carer/non-carer status
  * 4.07_health_multimorbidity_lagp.R: Analysis of multimorbidity in unpaid carers by LA/GP-identified status
  * 4.08_health_ltc_description_lookup.R: Creates lookup for full names of long-term conditions
  * 4.09_health_ltc_plot_colours.R: Creates colour lookup for long-term condition plots
  * 4.10_health_ltc_functions.R: Functions for anyalsis of most common long-term conditions
  * 4.11_health_ltc.R: Analysis of most common long-term conditions by unpaid carer/non-carer status
  * 4.12_health_ltc_lagp.R: Analysis of most common long-term conditions in unpaid carers by LA/GP-identified status
  * 4.13_health_multimorbidity_ltc_combined.R: Combines health outputs from all LAs into one table/plot
 
- Section 5: Health service use
  * 5.01_health_service_use_gp_data.sql: Extracts GP interaction data for analysis
  * 5.02_health_service_use_gp_functions.R: Functions for analysis of GP interactions
  * 5.03_health_service_use_gp.R: Analysis of GP interactions
  * 5.04_health_service_use_ed_data.sql: Extracts ED attendance data for analysis
  * 5.05_health_service_use_ed_functions.R: Functions for analysis of ED attendances
  * 5.06_health_service_use_ed.R: Analysis of ED attendances
  * 5.07_health_service_use_inpatient_data.sql: Extracts inpatient admission data for analysis
  * 5.08_health_service_use_inpatient_functions.R: Functions for anylsis of inpatient admissions
  * 5.09_health_service_use_inpatient.R: Analysis of inpatient admissions
  * 5.10_health_service_use_opdw_data.sql: Extracts Outpatient attendance data for analysis
  * 5.11_health_service_use_opdw_functions.R: Functions for analysis of Outpatient attendances
  * 5.12_health_service_use_opdw.R: Analysis of Outpatient attendances
  * 5.13_health_service_use_plot_functions.R: Functions to support creating health service plots across 3 Local Authorities
  * 5.14_health_service_use_plots.R: Script that calls health service analysis function to create plots used in final HTML output
  * 5.15_health_service_use_plot_la_functions.R: Functions to support creating health service plots for each LA
  * 5.16_health_service_use_plots_la.R: Script that calls health service analysis function to create plots used in LA-specific slide decks


