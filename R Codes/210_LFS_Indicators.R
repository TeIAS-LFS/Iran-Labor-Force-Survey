
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

for (i in First_year:Last_year) {
    FORM3 <- readRDS(paste0(Processd_data_Path,"/",i,"/","FORM3.RDS"))
    FORM2 <- readRDS(paste0(Processd_data_Path,"/",i,"/","FORM2JOZ.RDS"))
    W3 <- readRDS(paste0(Processd_data_Path,"/",i,"/","W3.RDS"))
    
    W3 <- W3 %>%select(Pkey,IW_Seasonly,Rural,Season)
    
    FORM2 <- FORM2%>%
      select(Pkey,Age,Gender)
    F3 <- left_join(FORM3,W3,key = "Pkey")
    F3 <- left_join(F3,FORM2, key="Pkey")
    rm(FORM2,FORM3,W3)

    
    
    F3$SHAGHEL <- NULL
    F3$SHAGHEL[F3$F3_D01== "Yes" | F3$F3_D02== "Yes" | F3$F3_D03== "Yes"|F3$F3_D06== "Yes"  |F3$F3_D08== "Yes"] <- 1
    F3$SHAGHEL[is.na(F3$SHAGHEL)] <- 0
    F3$SHAGHEL_Pop <- NULL
    F3$SHAGHEL_Pop  <-  F3$SHAGHEL*F3$IW_Seasonly
    
    F3$BIKAR <- NULL
    F3$BIKAR[(F3$SHAGHEL!=1 & F3$F3_D31 == "Yes" & F3$F3_D34 == "Yes")|(F3$SHAGHEL!=1 & F3$F3_D31 == "No"  & F3$F3_D34 == "Yes")] <- 1
    F3$BIKAR_Pop <- NULL
    F3$BIKAR_Pop  <-  F3$BIKAR*F3$IW_Seasonly

    if (i < 98) {
      for (k in c("01","02","03","04")) {
        LFS_Indicators_S <- LFS_Indicators_S %>% add_row(
          Year                = i,
          Season              = k,
          Male_Employment     = sum(F3$SHAGHEL_Pop[F3$Season == k & F3$Gender == "Male"],na.rm = true),
          Female_Employment   = sum(F3$SHAGHEL_Pop[F3$Season == k & F3$Gender == "Female"],na.rm = true),
          Male_Unemployment   = sum(F3$BIKAR_Pop[F3$Season == k & F3$Gender == "Male"],na.rm = true),
          Female_Unemployment = sum(F3$BIKAR_Pop[F3$Season == k & F3$Gender == "Female"],na.rm = true),
          Rural_Employment    = sum(F3$SHAGHEL_Pop[F3$Season == k & F3$Rural == 2],na.rm = true),
          Rural_Unemployment  = sum(F3$BIKAR_Pop[F3$Season == k & F3$Rural == 2],na.rm = true),
          Urban_Employment    = sum(F3$SHAGHEL_Pop[F3$Season == k & F3$Rural == 1],na.rm = true),
          Urban_Unemployment  = sum(F3$BIKAR_Pop[F3$Season == k & F3$Rural == 1],na.rm = true),
        )
      }
    }else{
      
      for (k in c("01","02","03","04")) {
        LFS_Indicators_S <- LFS_Indicators_S %>% add_row(
          Year                = i,
          Season              = k,
          Male_Employment     = sum(F3$SHAGHEL_Pop[F3$Season == k & F3$Gender == "Male"],na.rm = true),
          Female_Employment   = sum(F3$SHAGHEL_Pop[F3$Season == k & F3$Gender == "Female"],na.rm = true),
          Male_Unemployment   = sum(F3$BIKAR_Pop[F3$Season == k & F3$Gender == "Male"],na.rm = true),
          Female_Unemployment = sum(F3$BIKAR_Pop[F3$Season == k & F3$Gender == "Female"],na.rm = true),
          Rural_Employment    = sum(F3$SHAGHEL_Pop[F3$Season == k & F3$Rural == 2],na.rm = true),
          Rural_Unemployment  = sum(F3$BIKAR_Pop[F3$Season == k & F3$Rural == 2],na.rm = true),
          Urban_Employment    = sum(F3$SHAGHEL_Pop[F3$Season == k & F3$Rural == 1],na.rm = true),
          Urban_Unemployment  = sum(F3$BIKAR_Pop[F3$Season == k & F3$Rural == 1],na.rm = true),
        )
      }
    }
    
  }
  


for (i in First_year:Last_year) {

    LFS_Indicators_Y <- LFS_Indicators_Y %>% add_row(
      Year                = i,
      Male_Employment     = mean(LFS_Indicators_S$Male_Employment[LFS_Indicators_S$Year==i]), 
      Female_Employment   = mean(LFS_Indicators_S$Female_Employment[LFS_Indicators_S$Year==i]), 
      Male_Unemployment   = mean(LFS_Indicators_S$Male_Unemployment[LFS_Indicators_S$Year==i]), 
      Female_Unemployment = mean(LFS_Indicators_S$Female_Unemployment[LFS_Indicators_S$Year==i]), 
      Rural_Employment    = mean(LFS_Indicators_S$Rural_Employment[LFS_Indicators_S$Year==i]), 
      Rural_Unemployment  = mean(LFS_Indicators_S$Rural_Unemployment[LFS_Indicators_S$Year==i]),
      Urban_Employment    = mean(LFS_Indicators_S$Urban_Employment[LFS_Indicators_S$Year==i]), 
      Urban_Unemployment  = mean(LFS_Indicators_S$Urban_Unemployment[LFS_Indicators_S$Year==i])
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


saveRDS(LFS_Indicators_S,"F:/LFS/Code/Consistency/LFS_Indicators_S.RDS")
saveRDS(LFS_Indicators_Y,"F:/LFS/Code/Consistency/LFS_Indicators_Y.RDS")

& (F3$F3_D33 == "First"|F3$F3_D33 == "Second")
