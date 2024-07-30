clean <- function(df){
  df$Enroll  <- ifelse(df$Enroll  == "Y", 1,0)
  df$IL_IND  <- ifelse(df$USSTATE  == "IL", 1,0)
  df$KY_IND  <- ifelse(df$USSTATE  == "KY", 1,0)
  df$AR_IND  <- ifelse(df$USSTATE  == "AR", 1,0)
  df$TN_IND  <- ifelse(df$USSTATE  == "TN", 1,0)
  
  # DATA TYPE
  df$Enroll  <- as.factor(df$Enroll)
  df$WHITE_IND  <- as.factor(df$WHITE_IND)
  df$H_EXEMPT  <- as.factor(df$H_EXEMPT)
  df$MERIT_IND  <- as.factor(df$MERIT_IND)
  df$HOUSE_RCVD_IND  <- as.factor(df$HOUSE_RCVD_IND)
  #df$EFC <- as.numeric(df$EFC)
  df$DUAL_IND  <- as.factor(df$DUAL_IND)
  df$IL_IND  <- as.factor(df$IL_IND)
  df$KY_IND  <- as.factor(df$KY_IND)
  df$AR_IND  <- as.factor(df$AR_IND)
  df$TN_IND  <- as.factor(df$TN_IND)
  df$MO_IND  <- as.factor(df$MO_IND)
  df$Need_Level <- as.factor(df$Need_Level)
  df$INST_RANK <- as.factor(df$INST_RANK)
  df$HIGH_SCHOOL_GPA  <- as.numeric(df$HIGH_SCHOOL_GPA)
  df$A05  <- as.numeric(df$A05)
  #df$YEAR  <- as.numeric(df$YEAR)

  df$HSRI <- (df$HIGH_SCHOOL_GPA*10)+(df$A05/2)
 df %>% mutate(
    df$Need_Level == case_when(
    df$Need_Level == "1" ~ 0,
    df$Need_Level == "2" ~ 1,
    df$Need_Level == "3" ~ 2,
    df$Need_Level == "4" ~ 3,
    df$Need_Level == "5" ~ 4,
    df$Need_Level == "6" ~ 5
    )
  )
 
 df %>% mutate(
   df$INST_RANK == case_when(
     df$INST_RANK == "1" ~ 0,
     df$INST_RANK == "2" ~ 1,
     df$INST_RANK == "3" ~ 2,
     df$INST_RANK == "4" ~ 3,
     df$INST_RANK == "5" ~ 4,
     df$INST_RANK == "6" ~ 5
   )
 )
  
  # Remove variables 
  library(dplyr)
  clean <- df %>%
    dplyr::select(-c("PIDM_KEY", "HIGH_SCHOOL_GPA", "A05", "SBGI_DESC_HIGH_SCHOOL", "USSTATE", "YEAR"))
  
  # Exclude rows that have missing value in ANY variable
  clean <- na.omit(clean)
  
  return(clean)
}



