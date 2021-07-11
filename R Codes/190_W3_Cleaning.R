
W3_colnames <- read_xlsx(Col_name_path,sheet = "W3")

W3_RDS_Path <- RDS_path[str_detect(RDS_path,"W3")]
W3_RDS_Path <- c(W3_RDS_Path,RDS_path[str_detect(RDS_path,"9802")|str_detect(RDS_path,"9803")|str_detect(RDS_path,"9804")])

Intercensal <- read_xlsx(paste0(Files_paths,"/","Cencus75_95.xlsx"),sheet = "Intercensal_pop")


for (year in First_year:Last_year) {
  index <- match(W3_colnames$Year_Season[str_detect(W3_colnames$Year_Season,paste0("^",year))],W3_colnames$Year_Season)
  W3 <- tibble()
  for (j in index) {
    ww <- readRDS(W3_RDS_Path[str_detect(W3_RDS_Path, as.character(W3_colnames$Year_Season[j]))])
    for (i in 2:length(colnames(W3_colnames))) {
      names(ww)[names(ww) == eval(parse(text=paste0("W3_colnames","$",colnames(W3_colnames)[i])))[j]] <- colnames(W3_colnames)[i]
    }
    ww <- ww %>%
      mutate_all(as.character)
    W3 <- bind_rows(W3,ww)
  }

  if (year <= 95) {
    W3$AGE[W3$AGE == "**" | W3$AGE == "-2"] <- "101"
    W3 <- W3 %>% 
      mutate_at(vars(AGE,IW_Seasonly),as.double)
    
    if (year == 84) {
      W3$Month  <-  str_sub(W3$PKEY,3,4)
      W3$Season[W3$Month == "01" |W3$Month == "02" |W3$Month == "03"]  <- "01"
      W3$Season[W3$Month == "04" |W3$Month == "05" |W3$Month == "06"]  <- "02"
      W3$Season[W3$Month == "07" |W3$Month == "08" |W3$Month == "09"]  <- "03"
      W3$Season[W3$Month == "10" |W3$Month == "11" |W3$Month == "12"]  <- "04"
      W3$PKEY <- paste0(str_sub(W3$PKEY,1,2),W3$Season,str_sub(W3$PKEY,5,12))
    }
    
    W3<- W3%>%
      mutate(Year = str_sub(PKEY,1,2))%>%
      mutate(Season = str_sub(PKEY,3,4))%>%
      mutate(Province_ID = str_sub(PKEY,5,6))%>%
      mutate(Rural = str_sub(PKEY ,7,7))%>%
      mutate(Pkey = case_when(
        nchar(F2_D01)== 1 ~ paste0(PKEY,"0",F2_D01),
        nchar(F2_D01)== 2 ~ paste0(PKEY, F2_D01)
      ))%>%
      mutate(HHID = str_sub(Pkey,5,12))%>%
      mutate(IID  = str_sub(Pkey,5,14))%>%
      mutate(Adj_IW_Seasonly = IW_Seasonly)%>%
      select(-PKEY)
    
    W3$C_Sub_Pop <- NULL
    W3$C_Sub_Pop[W3$GENDER == "1" & W3$Rural == "1" & W3$AGE <=9] <- 0.25*sum(W3$IW_Seasonly[W3$GENDER == "1" & W3$Rural == "1" & W3$AGE <=9],na.rm = true)
    W3$C_Sub_Pop[W3$GENDER == "2" & W3$Rural == "1" & W3$AGE <=9] <- 0.25*sum(W3$IW_Seasonly[W3$GENDER == "2" & W3$Rural == "1" & W3$AGE <=9],na.rm = true)
    W3$C_Sub_Pop[W3$GENDER == "1" & W3$Rural == "2" & W3$AGE <=9] <- 0.25*sum(W3$IW_Seasonly[W3$GENDER == "1" & W3$Rural == "2" & W3$AGE <=9],na.rm = true)
    W3$C_Sub_Pop[W3$GENDER == "2" & W3$Rural == "2" & W3$AGE <=9] <- 0.25*sum(W3$IW_Seasonly[W3$GENDER == "2" & W3$Rural == "2" & W3$AGE <=9],na.rm = true)
    W3$C_Sub_Pop[W3$GENDER == "1" & W3$Rural == "1" & (W3$AGE >= 10 & W3$AGE <= 64)] <- 0.25*sum(W3$IW_Seasonly[W3$GENDER == "1" & W3$Rural == "1" & (W3$AGE >= 10 & W3$AGE <= 64)],na.rm = true)
    W3$C_Sub_Pop[W3$GENDER == "2" & W3$Rural == "1" & (W3$AGE >= 10 & W3$AGE <= 64)] <- 0.25*sum(W3$IW_Seasonly[W3$GENDER == '2' & W3$Rural == "1" & (W3$AGE >= 10 & W3$AGE <= 64)],na.rm = true)
    W3$C_Sub_Pop[W3$GENDER == "1" & W3$Rural == "2" & (W3$AGE >= 10 & W3$AGE <= 64)] <- 0.25*sum(W3$IW_Seasonly[W3$GENDER == "1" & W3$Rural == "2" & (W3$AGE >= 10 & W3$AGE <= 64)],na.rm = true)
    W3$C_Sub_Pop[W3$GENDER == "2" & W3$Rural == "2" & (W3$AGE >= 10 & W3$AGE <= 64)] <- 0.25*sum(W3$IW_Seasonly[W3$GENDER == "2" & W3$Rural == "2" & (W3$AGE >= 10 & W3$AGE <= 64)],na.rm = true)
    W3$C_Sub_Pop[W3$GENDER == "1" & W3$Rural == "1" & W3$AGE >=65] <- 0.25*sum(W3$IW_Seasonly[W3$GENDER == "1" & W3$Rural == "1" & W3$AGE >=65],na.rm = true)
    W3$C_Sub_Pop[W3$GENDER == "2" & W3$Rural == "1" & W3$AGE >=65] <- 0.25*sum(W3$IW_Seasonly[W3$GENDER == "2" & W3$Rural == "1" & W3$AGE >=65],na.rm = true)
    W3$C_Sub_Pop[W3$GENDER == "1" & W3$Rural == "2" & W3$AGE >=65] <- 0.25*sum(W3$IW_Seasonly[W3$GENDER == "1" & W3$Rural == '2' & W3$AGE >=65],na.rm = true)
    W3$C_Sub_Pop[W3$GENDER == "2" & W3$Rural == "2" & W3$AGE >=65] <- 0.25*sum(W3$IW_Seasonly[W3$GENDER == "2" & W3$Rural == "2" & W3$AGE >=65],na.rm = true)
    
    W3_A<- W3%>%
      select(Pkey,Year,Season,HHID,IID,Province_ID, Rural,IW_Seasonly,Adj_IW_Seasonly)
    W3_EX <- W3%>%
      select(-colnames(W3_A),Pkey)%>%
      select(Pkey,everything())
  
    if (year >=85) {
      W3$R_Sub_Pop <- NULL
      W3$R_Sub_Pop[W3$GENDER == "1" & W3$Rural == "1" & W3$AGE <=9] <- Intercensal$Male_0_9_U[Intercensal$Year == year]
      W3$R_Sub_Pop[W3$GENDER == "2" & W3$Rural == "1" & W3$AGE <=9] <- Intercensal$Female_0_9_U[Intercensal$Year == year]
      W3$R_Sub_Pop[W3$GENDER == "1" & W3$Rural == "2" & W3$AGE <=9] <- Intercensal$Male_0_9_R[Intercensal$Year == year ]
      W3$R_Sub_Pop[W3$GENDER == "2" & W3$Rural == "2" & W3$AGE <=9] <- Intercensal$Female_0_9_R[Intercensal$Year == year ]
      W3$R_Sub_Pop[W3$GENDER == "1" & W3$Rural == "1" & (W3$AGE >= 10 & W3$AGE <= 64)] <- Intercensal$Male_10_64_U[Intercensal$Year== year]
      W3$R_Sub_Pop[W3$GENDER == "2" & W3$Rural == "1" & (W3$AGE >= 10 & W3$AGE <= 64)] <- Intercensal$Female_10_64_U[Intercensal$Year== year]
      W3$R_Sub_Pop[W3$GENDER == "1" & W3$Rural == "2" & (W3$AGE >= 10 & W3$AGE <= 64)] <- Intercensal$Male_10_64_R[Intercensal$Year== year]
      W3$R_Sub_Pop[W3$GENDER == "2" & W3$Rural == "2" & (W3$AGE >= 10 & W3$AGE <= 64)] <- Intercensal$Female_10_64_R[Intercensal$Year== year]
      W3$R_Sub_Pop[W3$GENDER == "1" & W3$Rural == "1" & W3$AGE >=65] <- Intercensal$Male_65_U[Intercensal$Year== year]
      W3$R_Sub_Pop[W3$GENDER == "2" & W3$Rural == "1" & W3$AGE >=65] <- Intercensal$Female_65_U[Intercensal$Year== year]
      W3$R_Sub_Pop[W3$GENDER == "1" & W3$Rural == "2" & W3$AGE >=65] <- Intercensal$Male_65_R[Intercensal$Year== year]
      W3$R_Sub_Pop[W3$GENDER == "2" & W3$Rural == "2" & W3$AGE >=65] <- Intercensal$Female_65_R[Intercensal$Year== year]   
      
      W3_A<- W3 %>%
        mutate(Adj_Coef = R_Sub_Pop/C_Sub_Pop)%>%
        mutate(Adj_IW_Seasonly = Adj_Coef*IW_Seasonly)%>%
        select(Pkey,Year,Season,HHID,IID,Province_ID, Rural,IW_Seasonly,Adj_IW_Seasonly)
      
      W3_EX <- W3 %>%
        select(-IW_Seasonly)%>%
        select(Pkey,everything())
    }
    saveRDS(W3_A,paste0(Processd_data_Path,"/",year,"/","W3.RDS"))
    saveRDS(W3_EX,paste0(Processd_data_Path,"/",year,"/","W3_EX.RDS"))
    
  }else{
    W3<- W3%>%
      mutate(Year = str_sub(PKEY,1,2))%>%
      mutate(Season = str_sub(PKEY,3,4))%>%
      mutate(Province_ID = str_sub(PKEY,5,6))%>%
      mutate(Rural = str_sub(PKEY ,7,7))%>%
      mutate(Pkey = case_when(
        nchar(F2_D01)== 1 ~ paste0(PKEY,"0",F2_D01),
        nchar(F2_D01)== 2 ~ paste0(PKEY, F2_D01)
      ))%>%
      mutate(HHID = str_sub(Pkey,5,12))%>%
      mutate(IID  = str_sub(Pkey,5,14))%>%
      mutate(Adj_IW_Seasonly = IW_Seasonly)%>%
      mutate_at(vars(IW_Seasonly ,Adj_IW_Seasonly),as.double)%>%
      select(Pkey,Year,Season,Province_ID,HHID,IID,Rural,IW_Seasonly,Adj_IW_Seasonly)

    saveRDS(W3,paste0(Processd_data_Path,"/",year,"/","W3.RDS"))
  }
}

for (year in N) {
  W3 <- readRDS(paste0(Processd_data_Path,"/",year,"/W3.RDS"))
  if (year %in% 84:87) {
   W3 <- W3%>%
     mutate(HHID = paste0("01",HHID))%>%
     mutate(IID  = paste0("01",IID))
  }else if (year %in% 88:91) {
    W3 <- W3%>%
      mutate(HHID = paste0("02",HHID))%>%
      mutate(IID  = paste0("02",IID))    
    
  }else if (year %in% 92:96) {
    W3 <- W3%>%
      mutate(HHID = paste0("03",HHID))%>%
      mutate(IID  = paste0("03",IID))    
    
  }else{
    W3 <- W3%>%
      mutate(HHID = paste0("04",HHID))%>%
      mutate(IID  = paste0("04",IID))    
  }
  saveRDS(W3,paste0(Processd_data_Path,"/",year,"/W3.RDS"))
}

C_HHID_IID <- readRDS(paste0(Files_paths,"/C_HHID_IID.RDS"))
County_ID <- readRDS(paste0(Files_paths,"/County_ID.RDS"))

for (year in First_year:Last_year) {
  W3 <- readRDS(paste0(Processd_data_Path,"/",year,"/W3.RDS"))
  
  W3 <- W3%>%
    left_join(C_HHID_IID,key = "Pkey")%>%
    left_join(County_ID,key = "Pkey")%>%
    select(Pkey,Year,Season,Province_ID,Shahrestan,C_HHID,C_IID,Rural,IW_Seasonly,Adj_IW_Seasonly)
  
  saveRDS(W3,paste0(Processd_data_Path,"/",year,"/W3.RDS"))
}


