F2K_Jaygozin_T <- tibble()
for (year in First_year:Last_year) {
  FORM2KOL <-  readRDS(paste0(Processd_data_Path,"/",year,"/FORM2KOL.RDS"))
  FORM2KOL <-  FORM2KOL%>%
    mutate(Year = str_sub(YSHHID,1,2))%>%
    mutate(Season = str_sub(YSHHID,3,4))%>%
    mutate(HHID = case_when(
      year %in% 84:87 ~ paste0("01",str_sub(YSHHID,5,12)),
      year %in% 88:91 ~ paste0("02",str_sub(YSHHID,5,12)),
      year %in% 92:96 ~ paste0("03",str_sub(YSHHID,5,12)),
      year %in% 97:98 ~ paste0("04",str_sub(YSHHID,5,12))
      ))%>%
    select(YSHHID,Year,Season,HHID,F2Kol_D20,F2Kol_DJAYGOZIN)
  
  F2K_Jaygozin_T <-bind_rows(F2K_Jaygozin_T,FORM2KOL)
}

A <- F2K_Jaygozin_T%>%
  filter(F2Kol_DJAYGOZIN=="1")

B <- F2K_Jaygozin_T%>%
  filter(HHID %in% A$HHID)%>%
  arrange(HHID)



E <- B %>% 
  distinct(HHID,.keep_all = TRUE)%>%
  filter(F2Kol_DJAYGOZIN=="1")

F <- B%>%
  filter(!HHID %in% E$HHID)


W3_T_F_J <- W3_T_F%>%
  filter(HHID %in% F$HHID)

W3_T_F_NJ <- W3_T_F %>%
  filter(!HHID %in% W3_T_F_J$HHID)

W3_T_F <- readRDS("F:/LFS/Code/Consistency/W3_T_F.RDS")

D <- W3_T_F%>%
 filter(HHID %in% C$HHID)


W3_T_F <- W3_T_F %>%
  mutate(Season = str_sub(Pkey,3,4))

A <- left_join(W3_T_F,F2K_Jaygozin_T, by =c("HHID","Year","Season"))

A <- A%>%
  select(Pkey,HHID,IID,Relation,Age,Gender,F2Kol_DJAYGOZIN,F2Kol_D20)
