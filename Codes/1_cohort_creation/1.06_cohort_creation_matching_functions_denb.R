#' This script contains functions relating to creating 1-1 match for Denbighshire's unpaid carers '#

# this function ascertains non-carer status of potential matches by ensuring they don't have a unpaid carer Read code 2 yrs prior to the index date.
washout_excl_den <- function(matched_0_df_2){
  
  #remove if any had unpaid carer read code 2 years before first assessment date
  matched_0_df_3 <- matched_0_df_2 %>% merge(mp_unpaidcarerevent, by="ALF_PE", all.x=TRUE)
  matched_0_df_3 <- matched_0_df_3 %>%  mutate(keepflag = ifelse(is.na(EVENT_DT) | EVENT_DT <= (REFERRAL_DATE -years(2)) ,1, 0))
  matched_0_df_3 <- matched_0_df_3 %>%  filter(keepflag == 1) %>% select(-EVENT_DT, - keepflag)
  return(matched_0_df_3)
}

# function extracts 1-1 match for each unpaid carer based on inclusion criteria on imputed pseudo-index date
extract_right_controls_den <- function(cases_df, controls_df){
  # Nested for-loops for imputation
  matched_pairs_df <- data.frame()
  
  # create a stop flag to break out of inner for-loops
  stop <- FALSE 
  
  # Create list of subclasses in descending order (reason for descending order being, no matches were found in the last few subclasses.
  # Arranging in descending order allows quicker debugging)
  subclass_list <- sort(unique(as.integer(cases_df$subclass)), decreasing = TRUE)
  
  no_match_count <- 0 # initialise variable to count subclassess with no matches
  
  for (sc in subclass_list){
    print(paste0("Subclass ID: " , sc))
    
    # get all cases in the subclass
    cases_list <- cases_df %>% filter(subclass == sc) %>% arrange(treatSeq)
    # get all controls in the subclass
    controls_list <- controls_df %>% filter(subclass == sc) %>% select(-treatSeq)
    
    
    # loop through each case within the subclass
    for(cid in cases_list$treatSeq){
      print(paste0("case ID: " , cid))
      #give controls treatSeq of current case
      controls_list$treatSeq <- cid
      
      #subset case info; only one case (i.e carer) selected
      cid_info <- cases_list %>% filter(subclass == sc & treatSeq == cid)
      
      
      #get relevant index info for the case
      assmt_info <- cid_info  %>% select(subclass, treatSeq, IDENTIFIEDBY, FIRST_IDENTIFIED_DATE, REFERRAL_DATE)
      
      
      #impute index details to each control using merge
      controls_imp1 <- controls_list %>% merge(assmt_info, by = c("subclass", "treatSeq"), all.x=TRUE)
      
      #run through exclusion criteria
      excl_1 <- death_excl(controls_imp1)
      excl_2 <- Age18_excl(excl_1)
      excl_3 <- washout_excl_den(excl_2)
      excl_4 <- welshaddr_excl(excl_3)
      excl_5 <- wimd_excl(excl_4, cid_info)
      excl_6 <- gpreg_excl(excl_5)
      
      # match at index age where applicable, otherwise match at study start age
      excl_7 <- sqldf("SELECT * FROM excl_6 e WHERE AGE_INDEXDATE IN (SELECT AGE_INDEXDATE FROM cid_info) ")
      # if none found, match on age at study start
      if(nrow(excl_7) == 0){ excl_7 <- sqldf("SELECT * FROM excl_6 e WHERE AGE_STUDYSTART  IN (SELECT AGE_STUDYSTART  FROM cid_info) ")}
      # if none found, match on age group at study start
      if(nrow(excl_7) == 0){ excl_7 <- sqldf("SELECT * FROM excl_6 e WHERE  AGEGROUP_STUDYSTART IN (SELECT AGEGROUP_STUDYSTART  FROM cid_info)")}
      
      # browser() ####
      
      # check if enough matches remain after applying all exclusion criteria
      if(nrow(excl_7) < 1){
        
        print(paste0("cid: " ,cid, " in subclass: ", sc, " has insufficient matches!!"))
        # set the stop flag to TRUE 
        stop <- TRUE
        
      
      }else{
        # select columns needed
        cnames <- c("subclass", "treatSeq", "ALF_PE", "treat", "SEX", "SEX_Desc", "WOB", "AGE_INDEXDATE", "AGEGROUP_STUDYSTART","AGE_STUDYSTART","LSOA2011_CD", "WIMD", "WIMD_DESC", "RUC11_DESC", "IDENTIFIEDBY", "FIRST_IDENTIFIED_DATE", "REFERRAL_DATE")
        
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
  
  print(paste0("total no matches: ", no_match_count))
  
  return(matched_pairs_df)
  
}
