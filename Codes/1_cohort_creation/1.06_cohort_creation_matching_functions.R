#' This script contains functions relating to creating 1-1 match for NPT and Swansea's unpaid carers '#

# function prepares dataset by adding sex description and age groups at study start date
dataprep <- function(raw_df){
  
  # add sex description
  raw_df$SEX_Desc <- ifelse(raw_df$SEX == 1, 'Male', 'Female')
  
  # add age group at study start
  # install.packages("AMR")
  library(AMR)
  raw_df$AGEGROUP_STUDYSTART <- age_groups(raw_df$AGE_STUDYSTART, split_at = "tens")
    
  return(raw_df)
}


run_exactmatch <- function(UnpaidCarers_df, MatchingPool_df){
  
  library(MatchIt)
  
  
  #Step 1 join Carers and Matching cohort into a single dataset
  # Flag 1 as Unpaid carer and 0 for general population
  UnpaidCarers_df$treat <- 1
  MatchingPool_df$treat <- 0
  
  # WIMD criteria will be sorted after matching by age and sex
  #AGEGROUP_STUDYSTART
  UnpaidCarers_df2 <- UnpaidCarers_df %>% select(treat, SEX_Desc, AGEGROUP_STUDYSTART,  ALF_PE, WOB, DEATH_DT)
  MatchingPool_df2 <- MatchingPool_df %>% select(treat, SEX_Desc, AGEGROUP_STUDYSTART,  ALF_PE, WOB, DEATH_DT)
  
  
  # Step 2 join both into 1 dataset
  Merged_Matching_Unpaid <- rbind(UnpaidCarers_df2, MatchingPool_df2)
  
  # Step 3 match!
  
  #exact matching does not allow ratio to be specified, only nearest allows
  mout1 <- matchit(treat ~ SEX_Desc + AGEGROUP_STUDYSTART,
                                data = Merged_Matching_Unpaid, 
                                method = "exact")

  return(mout1)
  
}

# function checks which subclass has insufficient matches (0 matches)
view_empty_subclass_streatSeq <- function(cases_df, control_df){
  
    count_by_classnseq <- control_df %>%  
    select(subclass, treatSeq, ALF_PE) %>% 
    group_by(subclass, treatSeq) %>% 
    summarise(total_controls = n()) %>% arrange(subclass, treatSeq, total_controls)
  
   counts_df1 <- cases_df %>% select(subclass, treatSeq) %>%  merge(count_by_classnseq, by = c("subclass", "treatSeq"), all.x=TRUE)
    
   # View(counts_df1)
   
   
   if (is.na(nrow(counts_df1[counts_df1$total_controls <1 ,]) ))
   {print("All subclass and treatSeq has >= 1 matched control.")}
   else{
     print(paste0(nrow(counts_df1[counts_df1$total_controls < 1 ,]), " treatSeq has no matched controls"))
     # View(View(counts_df1[counts_df1$total_controls <1 ,]))
   }
  
   return(counts_df1)
}


# function extracts 1-1 match for each unpaid carer based on inclusion criteria on imputed pseudo-index date
extract_right_controls <- function(cases_df, controls_df){
  
  # Nested for-loops for imputation
  matched_pairs_df <- data.frame()
  
  # create a stop flag to break out of inner for-loops
  stop <- FALSE 
  # loop through each subclass
  subclass_list <- sort(unique(as.integer(cases_df$subclass)), decreasing = TRUE)
  for (sc in subclass_list){
    print(paste0("Subclass ID: " , sc))
    
    # get all cases in the subclass
    cases_list <- cases_df %>% filter(subclass == sc) %>% arrange(treatSeq)
    # get all controls in the subclass
    controls_list <- controls_df %>% filter(subclass == sc) %>% select(-treatSeq)
    
    
    # loop through each case within the subclass
    for(cid in cases_list$treatSeq){
      
      #give controls treatSeq of current case
      controls_list$treatSeq <- cid
      
      #subset case info; only one case (i.e carer) selected
      cid_info <- cases_list %>% filter(subclass == sc & treatSeq == cid)
     
      
      #get relevant index info for the case
      assmt_info <- cid_info  %>% select(subclass, treatSeq, IDENTIFIEDBY, FIRST_IDENTIFIED_DATE, FIRST_CARER_ASSMT, LATEST_CARER_ASSMT, TOTAL_NUM_ASSESSMENTS)
      
      
      #impute index details to each control using merge
      controls_imp1 <- controls_list %>% merge(assmt_info, by = c("subclass", "treatSeq"), all.x=TRUE)
      
      #run through exclusion criteria
      excl_1 <- death_excl(controls_imp1)
      excl_2 <- Age18_excl(excl_1)
      excl_3 <- washout_excl(excl_2)
      excl_4 <- welshaddr_excl(excl_3)
      excl_5 <- wimd_excl(excl_4, cid_info)
      excl_6 <- gpreg_excl(excl_5)
      
      # match at index age where applicable, otherwise match at study start age
      excl_7 <- sqldf("SELECT * FROM excl_6 e WHERE AGE_INDEXDATE IN (SELECT AGE_INDEXDATE FROM cid_info) ")
      if(nrow(excl_7) == 0){ excl_7 <- sqldf("SELECT * FROM excl_6 e WHERE AGE_STUDYSTART  IN (SELECT AGE_STUDYSTART  FROM cid_info) ")}
      
      # browser() ####
      
      # check if enough matches remain after applying all exclusion criteria
      if(nrow(excl_7) < 1){
        
        print(paste0("TreatID: " ,cid, " in subclass: ", sc, " has insufficient matches!!"))
        # set the stop flag to TRUE 
        stop <- TRUE
        # break out of the loop here
        break
      }else{
        # select columns needed
        cnames <- c("subclass", "treatSeq", "ALF_PE", "treat", "SEX", "SEX_Desc", "WOB", "AGE_INDEXDATE", "AGEGROUP_STUDYSTART","AGE_STUDYSTART","LSOA2011_CD", "WIMD", "WIMD_DESC", "RUC11_DESC", "IDENTIFIEDBY", "FIRST_IDENTIFIED_DATE", "FIRST_CARER_ASSMT", "LATEST_CARER_ASSMT", "TOTAL_NUM_ASSESSMENTS")
        
        # randomly select 1 matched control to be matched with the case
        set.seed(1429)
        mcontrols  <- excl_7 %>% sample_n(1) %>%  select(all_of(cnames))
		   
        # append matched pairs to list 
        matched_pairs_df <- rbind(matched_pairs_df, cid_info[,cnames] , mcontrols)
        
        #remove controls from control_list
        controls_list <- controls_list %>% filter(!(ALF_PE %in% mcontrols$ALF_PE))
        print(paste0("TreatID: " ,cid, " in subclass: ", sc, " found a match."))
        # uncomment below for debugging
        # browser() 
        
      }
	  # checks stop flag value. Breaks out of loop if stop == TRUE
      if(stop){break}
    }
    if(stop){break}
  }
  
  return(matched_pairs_df)
  
}

