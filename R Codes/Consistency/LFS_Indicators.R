
LFS_Indicators_S  <-  tibble(
  Year                = numeric(),
  Season              = character(),
  Male_Employment     = numeric(),
  Female_Employment   = numeric(),
  Total_Employment    = numeric(),
  Male_Unemployment   = numeric(),
  Female_Unemployment = numeric(),
  Total_Unemployment  = numeric(),
  Male_Participation  = numeric(),
  Female_Participation= numeric(),
  Total_Participation = numeric(),
  Rural_Employment    = numeric(),
  Rural_Unemployment  = numeric(),
  Rural_Participation = numeric(),
  Urban_Employment    = numeric(),
  Urban_Unemployment  = numeric(),
  Urban_Participation = numeric()
)
LFS_Indicators_Y  <-  tibble(
  Year                = numeric(),
  Male_Employment     = numeric(),
  Female_Employment   = numeric(),
  Total_Employment    = numeric(),
  Male_Unemployment   = numeric(),
  Female_Unemployment = numeric(),
  Total_Unemployment  = numeric(),
  Male_Participation  = numeric(),
  Female_Participation= numeric(),
  Total_Participation = numeric(),
  Rural_Employment    = numeric(),
  Rural_Unemployment  = numeric(),
  Rural_Participation = numeric(),
  Urban_Employment    = numeric(),
  Urban_Unemployment  = numeric(),
  Urban_Participation = numeric()
)

for (year in First_year:Last_year) {
  
  FORM3 <- readRDS(paste0(Processd_data_Path,"/",year,"/","FORM3.RDS"))
  FORM3 <- FORM3%>%
    select(Pkey,F3_D01,F3_D02,F3_D03,F3_D06,F3_D08,F3_D31,F3_D33,F3_D34)
  
  FORM2 <- readRDS(paste0(Processd_data_Path,"/",year,"/","FORM2JOZ.RDS"))
  FORM2 <- FORM2%>%
    select(Pkey,Age,Gender)
    
  W3 <- readRDS(paste0(Processd_data_Path,"/",year,"/","W3.RDS"))
  W3 <- W3 %>%
    select(Pkey,IW_Seasonly,Rural,Season)
  
  F3 <- FORM3%>%
    left_join(FORM2,by="Pkey")%>%
    left_join(W3,by = "Pkey")%>%
    mutate(Employed = case_when(
      F3_D01 == "Yes"|F3_D02 == "Yes"|F3_D03 == "Yes"|F3_D06 == "Yes"|F3_D08 == "Yes" ~ 1,
      F3_D01 == "No"|F3_D02 == "No"|F3_D03 == "No"|F3_D06 == "No"|F3_D08 == "No" ~ 0))%>%
    mutate(Unemployed = case_when(
      Employed == 0 & F3_D31 == "Yes" & F3_D34 == "Yes" ~ 1,
      Employed == 0 & F3_D31 == "No"  & F3_D33 %in% c("First","Secound") & F3_D34 == "Yes" ~ 1
    ))
  rm(FORM3,FORM2,W3)

  
  
  

    for (k in c("01","02","03","04")) {
      LFS_Indicators_S <- LFS_Indicators_S %>% add_row(
        Year                = year,
        Season              = k,
        Male_Employment     = sum(F3$IW_Seasonly[F3$Season == k & F3$Gender == "Male" & F3$Employed == 1],na.rm = true),
        Female_Employment   = sum(F3$IW_Seasonly[F3$Season == k & F3$Gender == "Female" & F3$Employed == 1],na.rm = true),
        Male_Unemployment   = sum(F3$IW_Seasonly[F3$Season == k & F3$Gender == "Male" & F3$Unemployed == 1],na.rm = true),
        Female_Unemployment = sum(F3$IW_Seasonly[F3$Season == k & F3$Gender == "Female" & F3$Unemployed == 1],na.rm = true),
        Rural_Employment    = sum(F3$IW_Seasonly[F3$Season == k & F3$Rural == 2 & F3$Employed == 1],na.rm = true),
        Rural_Unemployment  = sum(F3$IW_Seasonly[F3$Season == k & F3$Rural == 2 & F3$Unemployed == 1],na.rm = true),
        Urban_Employment    = sum(F3$IW_Seasonly[F3$Season == k & F3$Rural == 1 & F3$Employed == 1],na.rm = true),
        Urban_Unemployment  = sum(F3$IW_Seasonly[F3$Season == k & F3$Rural == 1 & F3$Unemployed == 1],na.rm = true),
      )
    }
  LFS_Indicators_Y <- LFS_Indicators_Y %>% add_row(
    Year                = year,
    Male_Employment     = mean(LFS_Indicators_S$Male_Employment[LFS_Indicators_S$Year==year]), 
    Female_Employment   = mean(LFS_Indicators_S$Female_Employment[LFS_Indicators_S$Year==year]), 
    Male_Unemployment   = mean(LFS_Indicators_S$Male_Unemployment[LFS_Indicators_S$Year==year]), 
    Female_Unemployment = mean(LFS_Indicators_S$Female_Unemployment[LFS_Indicators_S$Year==year]), 
    Rural_Employment    = mean(LFS_Indicators_S$Rural_Employment[LFS_Indicators_S$Year==year]), 
    Rural_Unemployment  = mean(LFS_Indicators_S$Rural_Unemployment[LFS_Indicators_S$Year==year]),
    Urban_Employment    = mean(LFS_Indicators_S$Urban_Employment[LFS_Indicators_S$Year==year]), 
    Urban_Unemployment  = mean(LFS_Indicators_S$Urban_Unemployment[LFS_Indicators_S$Year==year])
  )
}



LFS_Indicators_S <-  LFS_Indicators_S %>%
  mutate(Total_Employment = Female_Employment + Male_Employment)%>%
  mutate(Total_Unemployment = Female_Unemployment + Male_Unemployment)%>%
  mutate(Total_Participation = Total_Employment + Total_Unemployment)%>%
  mutate(Rural_Participation = Rural_Employment + Rural_Unemployment)%>%
  mutate(Urban_Participation = Urban_Employment + Urban_Unemployment) %>%
  mutate(Male_Participation = Male_Employment + Male_Unemployment)%>%
  mutate(Female_Participation = Female_Employment + Female_Unemployment)

LFS_Indicators_Y <-  LFS_Indicators_Y %>%
  mutate(Total_Employment = Female_Employment + Male_Employment)%>%
  mutate(Total_Unemployment = Female_Unemployment + Male_Unemployment)%>%
  mutate(Total_Participation = Total_Employment + Total_Unemployment)%>%
  mutate(Rural_Participation = Rural_Employment + Rural_Unemployment)%>%
  mutate(Urban_Participation = Urban_Employment + Urban_Unemployment) %>%
  mutate(Male_Participation = Male_Employment + Male_Unemployment)%>%
  mutate(Female_Participation = Female_Employment + Female_Unemployment)


saveRDS(LFS_Indicators_S,"C:/Users/ali/Dropbox/LFS Cleaning/GitHub/R Codes/Consistency/LFS_Indicators_S.RDS")
saveRDS(LFS_Indicators_Y,"C:/Users/ali/Dropbox/LFS Cleaning/GitHub/R Codes/Consistency/LFS_Indicators_Y.RDS")

