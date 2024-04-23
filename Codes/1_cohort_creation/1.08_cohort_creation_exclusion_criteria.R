#' This script contains functions that assess validity of matches based on each inclusion criteria'#

death_excl <- function(matched_0_df){
  
  
  matched_0_df_1 <- matched_0_df %>% filter(is.na(DEATH_DT) | DEATH_DT > FIRST_IDENTIFIED_DATE  ) 

  return(matched_0_df_1)
}


# Check if cohort meets >= 18 inclusion criteria
Age18_excl <- function(matched_0_df){
  library(lubridate)
  
  # calculate age at Index date
  matched_0_df_1 <- matched_0_df %>% mutate(AGE_INDEXDATE = floor(time_length( difftime(FIRST_IDENTIFIED_DATE , WOB ), "years" ))) 
  
  # drop any that are < 18
  matched_0_df_2 <- matched_0_df_1[matched_0_df_1$AGE_INDEXDATE >= 18,]

  return(matched_0_df_2)
  
}




# Check if any within 2 year wash out period before index date 
washout_excl <- function(matched_0_df_2){
  
  #remove if any had unpaid carer read code 2 years before first assessment date
  matched_0_df_3 <- matched_0_df_2 %>% merge(mp_unpaidcarerevent, by="ALF_PE", all.x=TRUE)
  matched_0_df_3 <- matched_0_df_3 %>%  mutate(keepflag = ifelse(is.na(EVENT_DT) | EVENT_DT <= (FIRST_CARER_ASSMT -years(2)) ,1, 0))
  matched_0_df_3 <- matched_0_df_3 %>%  filter(keepflag == 1) %>% select(-EVENT_DT, - keepflag)
  
  return(matched_0_df_3)
}




# Check if 1 year Welsh address from first assessment date 
welshaddr_excl <- function(matched_0_df_3){
  matched0_addr <- matched_0_df_3 %>% merge(matchingpool_LSOALkup, by = "ALF_PE", all.x=TRUE)
  matched0_addr <- matched0_addr %>% mutate(keepflag =  ifelse(FIRST_IDENTIFIED_DATE - years(1) >= WELSH_START_DT &  FIRST_IDENTIFIED_DATE<=WELSH_END_DT,1,0))

  
  #remove those who does not meet criteria
  matched0_addr2 <- matched0_addr %>% filter(keepflag == 1) 

  matched0_addr3 <- subset(matched0_addr2, select=-(keepflag))
  
  # select address nearest to the index date to limit 1 LSOA per ALF
  matched0_addr4 <- matched0_addr3 %>%  mutate(keepflag = ifelse( FIRST_IDENTIFIED_DATE >= LSOA_START_DATE & LSOA_END_DATE > FIRST_IDENTIFIED_DATE,1,0) )
  matched0_addr4 <- matched0_addr4 %>% filter(keepflag == 1) %>% distinct_all()
  matched_0_df_4 <- matched0_addr4 %>% select(-keepflag, -weights)

  return(matched_0_df_4)
}




# Impute WIMD of unpaid carer, remove if WIMD is different
wimd_excl <- function(matched_0_df_4, matched_1_df){
  uc_wimd <- matched_1_df %>% select(WIMD, subclass, treatSeq) 
  names(uc_wimd)[1] <- "treat1_WIMD"
  
  #impute WIMD for filtering
  matched0_tr1wimd <- matched_0_df_4 %>% merge(uc_wimd, by=c("subclass", "treatSeq") )

  
  # filter to keep where wimd of treat and controls are the same
  df <- matched0_tr1wimd[matched0_tr1wimd$WIMD != matched0_tr1wimd$treat1_WIMD,]

  matched0_tr1wimd <- matched0_tr1wimd %>% filter(treat1_WIMD==WIMD) 
  matched_0_df_5 <- matched0_tr1wimd %>% select(-treat1_WIMD)

  remove(matched0_tr1wimd, uc_wimd)
  
  return(matched_0_df_5)
}





# Check if 1 year of GP registration


gpreg_excl <- function(matched_0_df_5){
  # nrow(matchingpool_GPRegLkup)#145442
  
  matched0_GP <- matched_0_df_5 %>% merge(matchingpool_GPRegLkup, by = "ALF_PE", all.x=TRUE)
  # nrow(matched0_GP) #28908
  
  # View(matched0_GP[order(matched0_GP$ALF_PE, matched0_GP$GPREG_STARTDATE),])
  
  
  
  #flag where GP start date < first identification date -1 and gp registration end date is after
  matched0_GP <- matched0_GP %>% mutate(GPReg1yrflag = ifelse( FIRST_IDENTIFIED_DATE - years(1) >= GPREG_STARTDATE &    FIRST_IDENTIFIED_DATE <= GPREG_ENDDATE, 1 ,0 ))
  
  # xtabs(~GPReg1yrflag, data=matched0_GP) #0:5254   1:22318  
  
  
  # remove those that dont meet inclusion criteria
  matched0_GP2 <- matched0_GP %>% filter(GPReg1yrflag ==1)
  # nrow(matched0_GP2)#22318
  
  
  #check how many per ALF
  
  df <- sqldf("select count(*) n, ALF_PE from matched0_GP2 GROUP BY ALF_PE")
  # nrow(df[df$n > 1,]) #0
  # View(df[df$n > 1,])
  
  
  matched_0_df_6 <- matched0_GP2 %>% select(-ISLAND_ID, -GPReg1yrflag, -GPREG_STARTDATE, -GPREG_ENDDATE, -WELSH_ADDRESS, -WELSH_START_DT, -WELSH_END_DT)
  # nrow(matched_0_df_6) #22318
  
  
  rcount <- matched0_GP %>% filter(GPReg1yrflag ==0) %>% select(ALF_PE) %>%  distinct() %>%  nrow()
  # print(paste0("\n ALFs excluded using 1 year GP registration filter: ", rcount ))
  # print(paste0("ALFs remaining after exclusion: ", nrow(matched_0_df_6)))
  # 
  remove(df, matched0_GP, matched0_GP2)
  
  return(matched_0_df_6)
}




