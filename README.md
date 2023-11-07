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

- Cohort creation (SQL codes provided per local authority)
  * 'cohort_creation_step1_la_unpaid_carers.sql' identifies unpaid carers in the local authority data source who meet the study criteria.
  * 'cohort_creation_step2_gp_unpaid_carers.sql' identifies unpaid carers in the WLGP data source who meet the study criteria.
  * 'cohort_creation_step3_la_deduplicated_full_cohort.sql' combines outputs from Step 1 and Step 2 to determine index date (earliest date of identification within study criteria) and assigns to "GP-identified" or "LA-identified" cohort based on data source.
  * 'cohort_creation_step4_lagp_overlap.sql' combines outputs from Step 1 and Step 2 to determine overlap between data sources.
