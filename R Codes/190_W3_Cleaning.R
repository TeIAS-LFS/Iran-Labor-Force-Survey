
W3_colnames <- read_xlsx(Col_name_path,sheet = "W3")
W3_RDS_Path <- RDS_path[str_detect(RDS_path,"W3.RDS|9802|9803|9804")]
Intercensal <- read_xlsx(paste0(Files_paths,"/","Cencus75_95.xlsx"),sheet = "Intercensal_pop")
County_ID <- readRDS(paste0(Files_paths,"/County_ID.RDS"))

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
  
  
    if (year == 84) {
      W3 <- W3%>%
        mutate(Season = ifelse(str_sub(PKEY,3,4) %in% c("01","02","03"),"01",
                               ifelse(str_sub(PKEY,3,4) %in% c("04","05","06"),"02",
                                      ifelse(str_sub(PKEY,3,4) %in% c("07","08","09"),"03","04"))))%>%
        mutate(PKEY = paste0(str_sub(PKEY,1,2),Season,str_sub(PKEY,5,12)))%>%
        select(-Season)
    }
  
  
  if (year == 97) {
    FORM2JOZ <- readRDS(paste0(Processd_data_Path,"/",year,"/FORM2JOZ.RDS"))
    
    FORM2JOZ <- FORM2JOZ%>%
      dplyr::rename("AGE" = "Age",
                    "GENDER" = "Gender")%>%
      mutate(GENDER = ifelse(GENDER =="Male","1","2"))%>%
      select(Pkey,GENDER,AGE)
    
    W3 <- W3%>%
      mutate(Pkey = case_when(
        nchar(F2_D01)==1~ paste0(PKEY,"0",F2_D01),
        nchar(F2_D01)==2~ paste0(PKEY,F2_D01)))%>%
      left_join(FORM2JOZ,by = "Pkey")
    
    rm(FORM2JOZ)
  }
  
  W3 <- W3%>%
    mutate(AGE =ifelse(AGE %in% c("**","-2"),"101",AGE),
           F2_D01 = ifelse(nchar(F2_D01)==1,paste0("0",F2_D01),F2_D01),
           Year = str_sub(PKEY,1,2),
           Season = str_sub(PKEY,3,4),
           Province_ID = str_sub(PKEY,5,6),
           Rural = str_sub(PKEY ,7,7),
           Pkey = paste0(PKEY,F2_D01),
           HHID = case_when(
             year %in% c("84","85","86","87") ~ paste0("01",str_sub(Pkey,5,12)),
             year %in% c("88","89","90","91") ~ paste0("02",str_sub(Pkey,5,12)),
             year %in% c("92","93","94","95","96") ~ paste0("03",str_sub(Pkey,5,12)),
             year %in% c("97","98") ~ paste0("04",str_sub(Pkey,5,12))))%>%
    mutate(IID = paste0(HHID,F2_D01),
           Adj_IW_Seasonly = IW_Seasonly)%>%
    select(-PKEY)%>%
    mutate_at(vars(AGE,IW_Seasonly,Adj_IW_Seasonly),as.numeric)
  
  
  W3 <- W3%>%
    mutate(Age_Cat = case_when(
      AGE <= 9 ~ "0-9",
      AGE >=10 & AGE<=64 ~"10-64",
      AGE >=65 ~ "65+"
    ))%>%
    mutate_at(vars(GENDER,Rural,Age_Cat),as.factor)%>%
    group_by(Age_Cat,GENDER,Rural)%>%
    dplyr::mutate(CSP = 0.25*sum(IW_Seasonly,na.rm = T))%>%
    ungroup()%>%
    mutate(R_Sub_Pop = NA)
    
    if (year>=85 & year <=95) {
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
      
    }

    W3<- W3 %>%
      left_join(County_ID,by = "HHID")%>%
      mutate(Adj_Coef = R_Sub_Pop/CSP)%>%
      mutate(Adj_IW_Seasonly = ifelse(!is.na(Adj_Coef),Adj_Coef*IW_Seasonly,Adj_IW_Seasonly))


    
    W3_A<- W3%>%
      select(Pkey,Year,Season,HHID,IID,Province_ID,Shahrestan, Rural,IW_Seasonly,Adj_IW_Seasonly)
    
    W3_EX <- W3%>%
      select(-colnames(W3_A),Pkey)%>%
      select(Pkey,everything())
    
    saveRDS(W3_A,paste0(Processd_data_Path,"/",year,"/","W3.RDS"))
    saveRDS(W3_EX,paste0(Processd_data_Path,"/",year,"/","W3_EX.RDS"))
    

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


