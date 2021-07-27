Total_Pop_C  <-  tibble(
  Year                = numeric(),
  Total_population    = numeric(),
  Male_T              = numeric(),
  Female_T            = numeric(),
  Urban_T             = numeric(),
  Urban_M             = numeric(),
  Urban_F             = numeric(),
  Rural_T             = numeric(),
  Rural_M             = numeric(),
  Rural_F             = numeric(),
  
)

Age10_Pop_C  <-  tibble(
  Year                = numeric(),
  Age10_population    = numeric(),
  Age_10_Male         = numeric(),
  Age_10_Female       = numeric(),
  Age_10_Urban_T      = numeric(),
  Age_10_Urban_M      = numeric(),
  Age_10_Urban_F      = numeric(),
  Age_10_Rural_T      = numeric(),
  Age_10_Rural_M      = numeric(),
  Age_10_Rural_F      = numeric(),
  
)

for (i in First_year :Last_year) {
  
  FORM2 <- readRDS(paste0(Processd_data_Path,"/",i,"/","FORM2JOZ.RDS"))
  FORM2 <- FORM2%>%
    select(Pkey,Age,Gender)
  
  W3 <- readRDS(paste0(Processd_data_Path,"/",i,"/","W3.RDS"))
  
  W3 <- W3%>%
    select(Pkey,Rural,IW_Seasonly)%>%
    left_join(FORM2,by = "Pkey")
  
  Total_Pop_C  <-  Total_Pop_C %>% add_row(
    Year                = i,
    Total_population    = 0.25*sum(W3$IW_Seasonly,na.rm = true),
    Male_T              = 0.25*sum(W3$IW_Seasonly[W3$Gender=="Male"],na.rm = true),
    Female_T            = 0.25*sum(W3$IW_Seasonly[W3$Gender=="Female"],na.rm = true),
    Urban_T             = 0.25*sum(W3$IW_Seasonly[W3$Rural=="1"],na.rm = true),
    Urban_M             = 0.25*sum(W3$IW_Seasonly[W3$Rural=="1" & W3$Gender=="Male" ],na.rm = true),
    Urban_F             = 0.25*sum(W3$IW_Seasonly[W3$Rural=="1" & W3$Gender=="Female" ],na.rm = true),
    Rural_T             = 0.25*sum(W3$IW_Seasonly[W3$Rural=="2"],na.rm = true),
    Rural_M             = 0.25*sum(W3$IW_Seasonly[W3$Rural=="2" & W3$Gender=="Male" ],na.rm = true),
    Rural_F             = 0.25*sum(W3$IW_Seasonly[W3$Rural=="2" & W3$Gender=="Female" ],na.rm = true)
    
  )
  
  Age10_Pop_C <- Age10_Pop_C %>% add_row(
    Year                = i,
    Age10_population    = 0.25*sum(W3$IW_Seasonly[W3$Age >= 10],na.rm = true),
    Age_10_Male         = 0.25*sum(W3$IW_Seasonly[W3$Age >= 10 & W3$Gender == "Male"],na.rm = true),
    Age_10_Female       = 0.25*sum(W3$IW_Seasonly[W3$Age >= 10 & W3$Gender == "Female"],na.rm = true),
    Age_10_Urban_T      = 0.25*sum(W3$IW_Seasonly[W3$Age >= 10 & W3$Rural == "1"],na.rm = true),
    Age_10_Urban_M      = 0.25*sum(W3$IW_Seasonly[W3$Age >= 10 & W3$Rural == "1" & W3$Gender == "Male"],na.rm = true),
    Age_10_Urban_F      = 0.25*sum(W3$IW_Seasonly[W3$Age >= 10 & W3$Rural == "1" & W3$Gender == "Female"],na.rm = true),
    Age_10_Rural_T      = 0.25*sum(W3$IW_Seasonly[W3$Age >= 10 & W3$Rural == "2"],na.rm = true),
    Age_10_Rural_M      = 0.25*sum(W3$IW_Seasonly[W3$Age >= 10 & W3$Rural == "2" & W3$Gender == "Male"],na.rm = true),
    Age_10_Rural_F      = 0.25*sum(W3$IW_Seasonly[W3$Age >= 10 & W3$Rural == "2" & W3$Gender == "Female"],na.rm = true),
  )
  
}


saveRDS(Age10_Pop_C,"C:/Users/ali/Dropbox/LFS Cleaning/GitHub/R Codes/Consistency/Age10_Pop_C.RDS")
saveRDS(Total_Pop_C,"C:/Users/ali/Dropbox/LFS Cleaning/GitHub/R Codes/Consistency/Total_Pop_C.RDS")

