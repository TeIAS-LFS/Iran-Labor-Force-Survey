
W3_T  <- tibble()


for (year in 84L:98L) {
  W <- readRDS(paste0(Processd_data_Path,"/",year,"/W3.RDS"))
  F2 <- readRDS(paste0(Processd_data_Path,"/",year,"/FORM2JOZ.RDS"))
  F2K <- readRDS(paste0(Processd_data_Path,"/",year,"/FORM2KOL.RDS"))
  F2K <- F2K%>%
    select(YSHHID,F2Kol_DJAYGOZIN,F2Kol_D20,F2Kol_D21)
  W3_F2 <- left_join(W,F2)
  W3_F2<- W3_F2%>%
    select(Pkey,Year,HHID,IID,Province_ID,Rural,Relation,Birth_Year,Age,Gender,Marriage_Status)%>%
    mutate(YSHHID = str_sub(Pkey,1,12))
  
  W3_F2 <- left_join(W3_F2,F2K,key="YSHHID")
  W3_T <- bind_rows(W3_T,W3_F2)
}

#saveRDS(W3_T,"F:/LFS/Code/Consistency/W3_T.RDS")
