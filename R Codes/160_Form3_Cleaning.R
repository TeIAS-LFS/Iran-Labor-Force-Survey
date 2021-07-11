
library("tidyverse")
library("stringi")
library("readxl")
library("foreign")
library("RODBC")

F3_colnames <- read_xlsx(Col_name_path,sheet = "FORM3")

F3_RDS_Path <- RDS_path[str_detect(RDS_path,"FORM3.RDS")]
F3_RDS_Path <- c(F3_RDS_Path,RDS_path[str_detect(RDS_path,"9802")|str_detect(RDS_path,"9803")|str_detect(RDS_path,"9804")])

for (year in First_year:Last_year) {
  index <- match(F3_colnames$Year_Season[str_detect(F3_colnames$Year_Season,paste0("^",year))],F3_colnames$Year_Season)
  FORM3 <- tibble()
  for (j in index) {
    F3 <- readRDS(F3_RDS_Path[str_detect(F3_RDS_Path, as.character(F3_colnames$Year_Season[j]))])
    for (i in 2:length(colnames(F3_colnames))) {
      names(F3)[names(F3) == eval(parse(text=paste0("F3_colnames","$",colnames(F3_colnames)[i])))[j]] <- colnames(F3_colnames)[i]
    }
    F3 <- F3 %>%
      mutate_all(as.character)
    
    FORM3 <- bind_rows(FORM3,F3)
  }
  if(year== 84){
    FORM3$Month  <-  str_sub(FORM3$PKEY,3,4)
    
    FORM3$Season[FORM3$Month == "01" |FORM3$Month == "02" |FORM3$Month == "03"]  <- "01"
    FORM3$Season[FORM3$Month == "04" |FORM3$Month == "05" |FORM3$Month == "06"]  <- "02"
    FORM3$Season[FORM3$Month == "07" |FORM3$Month == "08" |FORM3$Month == "09"]  <- "03"
    FORM3$Season[FORM3$Month == "10" |FORM3$Month == "11" |FORM3$Month == "12"]  <- "04"
    
    FORM3$PKEY <- paste0(str_sub(FORM3$PKEY,1,2),FORM3$Season,str_sub(FORM3$PKEY,5,14))
    
    FORM3<- FORM3 %>%
      mutate(F3_D45 = F3_D34)%>%
      mutate_at(vars(F3_D02),as.factor)%>%
      select(-Month,Season)
  }
  if (year %in% c(85,86)) {
    FORM3<- FORM3 %>%
      mutate(F3_D45 = F3_D34)%>%
      mutate_at(vars(F3_D02),as.factor)
  }
  FORM3 <- FORM3 %>%
    mutate_at(vars(F3_D14SAL,F3_D14MAH,F3_D15SAL,F3_D15MAH,F3_D16SHASLIR,
                   F3_D16SHASLIS,F3_D16SHHAMRO,F3_D16SHHAMSA,F3_D17,F3_D18SHANBEH,
                   F3_D18YEKSHAN,F3_D18DOSHANB,F3_D18SESHANB,F3_D18CHARSHA,
                   F3_D18PANSHAN,F3_D18JOMEH,F3_D18JAM,F3_D27ROZ,F3_D27SAAT,
                   F3_D35SAL,F3_D35MAH,F3_D37SAL,F3_D37MAH,F3_D40SAL,F3_D40MAH),
              as.integer)%>%
    mutate_at(vars(PKEY),as.character)%>%
    mutate_at(vars(F3_D01,F3_D03,F3_D06,F3_D07,F3_D08,F3_D09,F3_D10,
                   F3_D11,F3_D12,F3_D13,F3_D17,F3_D19,F3_D20,F3_D21,F3_D22,
                   F3_D23,F3_D24,F3_D25,F3_D26,F3_D28,F3_D29,F3_D30,F3_D31,F3_D1_32,F3_D2_32,
                   F3_D3_32,F3_D4_32,F3_D5_32,F3_D6_32,F3_D7_32,F3_D8_32,
                   F3_D33,F3_D34,F3_D36,F3_D38,F3_D39,F3_D41,F3_D42,F3_D43,F3_D44,F3_D45,
                   F3_D46,F3_D47,F3_D49,F3_D50),as.factor)
  if(year >= 87){
    FORM3$F3_D02[FORM3$F3_D02_87 == 1 | FORM3$F3_D04_87 == 1 | FORM3$F3_D05_87 == 1] <- 1
    FORM3$F3_D02[FORM3$F3_D02_87 == 2 & FORM3$F3_D04_87 == 2 & FORM3$F3_D05_87 == 2] <- 2
    FORM3 <- FORM3%>%
      mutate_at(vars(F3_D02,F3_D02_87,F3_D04_87,F3_D05_87,F3_D9_32),as.factor)%>%
      mutate_at(vars(F3_D48SAAT,F3_D48ROZ),as.integer)
  }
  Fac_vars <- excel_sheets(F3_Labels_path)
  Fac_vars <- Fac_vars[excel_sheets(F3_Labels_path) %in% colnames(FORM3)]
  for (i in 1:length(Fac_vars)) {
    Levels <- read_xlsx(F3_Labels_path,sheet = Fac_vars[i])
    Levels[Levels == "space"] <- " "
    Levels[Levels == "2space"] <- "  "
    Levels[Levels == "0space"] <- ""
    Levels_Value <- colnames(Levels)[!str_detect(colnames(Levels),"\\...")]
    for (j in 2:length(Levels_Value)) {
      L <- levels(eval(parse(text = paste0("FORM3$",Fac_vars[i])))) == eval(parse(text = paste0("Levels$","`",Levels_Value[j],"`")))[Levels$Year == year]
      eval(parse(text = paste0("levels(","FORM3$",Fac_vars[i],")[L]"," <- ","Levels$",Levels_Value[j],"[1]")))
    }
  }
  F_Vars <- colnames(FORM3)[sapply(FORM3,is.factor)]
  
  for (i in F_Vars[2:length(F_Vars)]) {
    eval(parse(text = paste0("FORM3$",i," <- ","factor(FORM3$",i,",","levels = c(levels(FORM3$",i,")",",","\"","NR","\"","))"))) 
    
  }
  FORM3$F3_D02[FORM3$F3_D01 == "Yes"] <- "NR"
  FORM3$F3_D03[FORM3$F3_D02 == "NR"|FORM3$F3_D02=="Yes"] <- "NR"
  FORM3$F3_D06[FORM3$F3_D03 == "NR"|FORM3$F3_D03=="Yes"] <- "NR"
  FORM3$F3_D07[FORM3$F3_D06 == "NR" |FORM3$F3_D06 == "No"] <- "NR"
  FORM3$F3_D08[FORM3$F3_D06 == "No"] <- "NR"
  FORM3$F3_D09[FORM3$F3_D06 == "No"] <- "NR"
  FORM3$F3_D10[FORM3$F3_D06 == "No"] <- "NR"
  FORM3$F3_D11[FORM3$F3_D06 == "No"] <- "NR"
  FORM3$F3_D12[FORM3$F3_D06 == "No"] <- "NR"
  FORM3$F3_D13[FORM3$F3_D06 == "No"] <- "NR"
  FORM3$F3_D14SAL[FORM3$F3_D06 == "No"] <- -1
  FORM3$F3_D14MAH[FORM3$F3_D06 == "No"] <- -1
  FORM3$F3_D15SAL[FORM3$F3_D06 == "No"] <- -1
  FORM3$F3_D15MAH[FORM3$F3_D06 == "No"] <- -1
  FORM3$F3_D16SHASLIR[FORM3$F3_D06 == "No"] <- -1
  FORM3$F3_D16SHASLIS[FORM3$F3_D06 == "No"] <- -1
  FORM3$F3_D16SHHAMRO[FORM3$F3_D06 == "No"] <- -1
  FORM3$F3_D16SHHAMSA[FORM3$F3_D06 == "No"] <- -1
  FORM3$F3_D17[FORM3$F3_D06 == "No" |FORM3$F3_D16SHHAMSA >=44] <- "NR"
  FORM3$F3_D18SHANBEH[FORM3$F3_D06 == "No" |FORM3$F3_D16SHHAMSA < 44] <- -1
  FORM3$F3_D18SHANBEH[FORM3$F3_D06 == "No" |FORM3$F3_D16SHHAMSA < 44] <- -1
  FORM3$F3_D18YEKSHAN[FORM3$F3_D06 == "No" |FORM3$F3_D16SHHAMSA < 44] <- -1
  FORM3$F3_D18DOSHANB[FORM3$F3_D06 == "No" |FORM3$F3_D16SHHAMSA < 44] <- -1
  FORM3$F3_D18SESHANB[FORM3$F3_D06 == "No" |FORM3$F3_D16SHHAMSA < 44] <- -1
  FORM3$F3_D18CHARSHA[FORM3$F3_D06 == "No" |FORM3$F3_D16SHHAMSA < 44] <- -1
  FORM3$F3_D18PANSHAN[FORM3$F3_D06 == "No" |FORM3$F3_D16SHHAMSA < 44] <- -1
  FORM3$F3_D18JOMEH[FORM3$F3_D06 == "No" |FORM3$F3_D16SHHAMSA < 44] <- -1
  FORM3$F3_D18JAM[FORM3$F3_D06 == "No" |FORM3$F3_D16SHHAMSA < 44] <- -1
  FORM3$F3_D19[FORM3$F3_D06 == "No" |FORM3$F3_D18JAM >= FORM3$F3_D16SHHAMSA] <- "NR"
  FORM3$F3_D20[FORM3$F3_D06 == "No" |FORM3$F3_D18JAM <= FORM3$F3_D16SHHAMSA] <- "NR"
  FORM3$F3_D21[FORM3$F3_D06 == "No"] <- "NR"
  FORM3$F3_D22[FORM3$F3_D06 == "No"|FORM3$F3_D21=="No"] <- "NR"
  FORM3$F3_D23[FORM3$F3_D06 == "No"|FORM3$F3_D21=="No"] <- "NR"
  FORM3$F3_D24[FORM3$F3_D06 == "No"] <- "NR"
  FORM3$F3_D25[FORM3$F3_D06 == "No"|FORM3$F3_D24=="No"] <- "NR"
  FORM3$F3_D26[FORM3$F3_D06 == "No"|FORM3$F3_D24=="No"] <- "NR"
  FORM3$F3_D27ROZ[FORM3$F3_D06 == "No"] <- -1
  FORM3$F3_D27SAAT[FORM3$F3_D06 == "No"] <- -1
  FORM3$F3_D28[FORM3$F3_D06 == "No"|FORM3$F3_D08 == "No"] <- "NR"
  FORM3$F3_D29[FORM3$F3_D06 == "No"|FORM3$F3_D08 == "No"] <- "NR"
  FORM3$F3_D30[FORM3$F3_D06 == "No"|FORM3$F3_D08 == "No"] <- "NR"
  FORM3$F3_D31[FORM3$F3_D06 == "NR"|FORM3$F3_D06 == "Yes"] <- "NR"
  FORM3$F3_D1_32[FORM3$F3_D06 == "NR"|FORM3$F3_D06 == "Yes"|FORM3$F3_D31=="No"] <- "NR"
  FORM3$F3_D2_32[FORM3$F3_D06 == "NR"|FORM3$F3_D06 == "Yes"|FORM3$F3_D31=="No"] <- "NR"
  FORM3$F3_D3_32[FORM3$F3_D06 == "NR"|FORM3$F3_D06 == "Yes"|FORM3$F3_D31=="No"] <- "NR"
  FORM3$F3_D4_32[FORM3$F3_D06 == "NR"|FORM3$F3_D06 == "Yes"|FORM3$F3_D31=="No"] <- "NR"
  FORM3$F3_D5_32[FORM3$F3_D06 == "NR"|FORM3$F3_D06 == "Yes"|FORM3$F3_D31=="No"] <- "NR"
  FORM3$F3_D6_32[FORM3$F3_D06 == "NR"|FORM3$F3_D06 == "Yes"|FORM3$F3_D31=="No"] <- "NR"
  FORM3$F3_D7_32[FORM3$F3_D06 == "NR"|FORM3$F3_D06 == "Yes"|FORM3$F3_D31=="No"] <- "NR"
  FORM3$F3_D8_32[FORM3$F3_D06 == "NR"|FORM3$F3_D06 == "Yes"|FORM3$F3_D31=="No"] <- "NR"
  FORM3$F3_D33[FORM3$F3_D06 == "NR"|FORM3$F3_D06 == "Yes"|FORM3$F3_D31=="Yes"] <- "NR"
  FORM3$F3_D34[FORM3$F3_D06 == "NR"|FORM3$F3_D06 == "Yes"|FORM3$F3_D33 %in% c("Third","Fourth","Fifth","Sixth","Seventh","Eighth","Ninth","Tenth","Eleventh","Twelfth")] <- "NR"
  FORM3$F3_D35SAL[FORM3$F3_D34=="NR"|FORM3$F3_D34 == "No"] <- -1
  FORM3$F3_D35MAH[FORM3$F3_D34=="NR"|FORM3$F3_D34 == "No"] <- -1
  FORM3$F3_D36[FORM3$F3_D34=="NR"|FORM3$F3_D34 == "No"] <- "NR"
  FORM3$F3_D37SAL[FORM3$F3_D34=="NR"|FORM3$F3_D34 == "No"|FORM3$F3_D36 %in% c("Studying","Housewife","Retired","Non-retired","Others")] <- -1
  FORM3$F3_D37MAH[FORM3$F3_D34=="NR"|FORM3$F3_D34 == "No"|FORM3$F3_D36 %in% c("Studying","Housewife","Retired","Non-retired","Others")] <- -1
  FORM3$F3_D38[FORM3$F3_D34=="NR"|FORM3$F3_D34 == "No"|FORM3$F3_D36 %in% c("Studying","Housewife","Retired","Non-retired","Others")] <- "NR"
  FORM3$F3_D39[FORM3$F3_D34=="NR"|FORM3$F3_D34== "No"] <- "NR"
  FORM3$F3_D40SAL[FORM3$F3_D39=="NR"|FORM3$F3_D39== "No"] <- -1
  FORM3$F3_D40MAH[FORM3$F3_D39=="NR"|FORM3$F3_D39== "No"] <- -1
  FORM3$F3_D41[FORM3$F3_D39=="NR"|FORM3$F3_D39== "No"] <- "NR"
  FORM3$F3_D42[FORM3$F3_D39=="NR"|FORM3$F3_D39== "No"] <- "NR"
  FORM3$F3_D43[FORM3$F3_D39=="NR"|FORM3$F3_D39== "No"] <- "NR"
  FORM3$F3_D44[FORM3$F3_D39=="NR"|FORM3$F3_D39== "No"] <- "NR"
  FORM3$F3_D45[FORM3$F3_D31=="NR"|FORM3$F3_D33 %in% c("Seventh","Eighth","Ninth","Tenth","Eleventh","Twelfth")|FORM3$F3_D34=="No"|FORM3$F3_D39%in%c("Yes","No")] <- "NR"
  FORM3$F3_D46[FORM3$F3_D31=="NR"|FORM3$F3_D33 %in% c("Seventh","Eighth","Ninth","Tenth","Eleventh","Twelfth")|FORM3$F3_D39=="No"|FORM3$F3_D45== "Yes"] <- "NR"
  FORM3$F3_D47[FORM3$F3_D31=="NR"|FORM3$F3_D39 %in% c("Yes","No")] <- "NR"
  FORM3$F3_D49[FORM3$F3_D06=="NR"|FORM3$F3_D06== "Yes"] <- "NR"
  FORM3$F3_D50[FORM3$F3_D06=="NR"|FORM3$F3_D06== "Yes"] <- "NR"
  
  F3_L <- read_xlsx(paste0(Files_paths,"/","Labels.xlsx"),sheet = "FORM3")
  F_Vars <- colnames(F3_L)
  
  for (vars in F_Vars) {
    eval(parse(text = paste0("FORM3$",vars, " <- factor(","FORM3$",vars,",","levels =","F3_L$",vars,"[!is.na(F3_L$",vars,")])")))
  }

  if (year <= 97) {
    
    FORM3<- FORM3 %>%
      mutate(Pkey = PKEY) %>%
      select(-PKEY)
    
    FORM3_A <- FORM3 %>%
      select(Pkey,F3_D01,F3_D02,F3_D03,F3_D06,F3_D07,F3_D08,F3_D09,F3_D10,
             F3_D11,F3_D12,F3_D13,F3_D14SAL,F3_D14MAH,F3_D15SAL,F3_D15MAH,
             F3_D16SHASLIR,F3_D16SHASLIS,F3_D16SHHAMRO,F3_D16SHHAMSA,
             F3_D17,F3_D18SHANBEH,F3_D18YEKSHAN,F3_D18DOSHANB,F3_D18SESHANB,
             F3_D18CHARSHA,F3_D18PANSHAN,F3_D18JOMEH,F3_D18JAM,F3_D19,F3_D20,
             F3_D21,F3_D22,F3_D23,F3_D24,F3_D25,F3_D26,F3_D27ROZ,F3_D27SAAT,
             F3_D28,F3_D29,F3_D30,F3_D31,F3_D1_32,F3_D2_32,F3_D3_32,F3_D4_32,
             F3_D5_32,F3_D6_32,F3_D7_32,F3_D8_32,F3_D33,F3_D34,F3_D35SAL,
             F3_D35MAH,F3_D36,F3_D37SAL,F3_D37MAH,F3_D38,F3_D39,F3_D40SAL,
             F3_D40MAH,F3_D41,F3_D42,F3_D43,F3_D44,F3_D45,F3_D46,F3_D47,F3_D49,F3_D50)

    FORM3_EX <- FORM3 %>%
      select(-colnames(FORM3_A),Pkey)%>%
      select(Pkey,everything())

    saveRDS(FORM3_A,paste0(Processd_data_Path,"/",year,"/","FORM3.RDS"))
    saveRDS(FORM3_EX,paste0(Processd_data_Path,"/",year,"/","FORM3_EX.RDS"))
  }else{
    FORM3$F2_D07[FORM3$F2_D07=="**"] <- 101
    FORM3<- FORM3%>%
      mutate(Pkey = case_when(
        str_sub(PKEY,3,4) == "01" ~ PKEY,
        str_sub(PKEY,3,4) %in% c("02","03","04") & nchar(F2_D01) == 1 ~ paste0(PKEY,"0",F2_D01),
        str_sub(PKEY,3,4) %in% c("02","03","04") & nchar(F2_D01) == 2 ~ paste0(PKEY,F2_D01)
      ))%>%
      select(-PKEY)%>%
      mutate_at(vars(F2_D07),as.integer)%>%
      filter(F2_D07 >=10|is.na(F2_D07))
    FORM3 <- FORM3%>%select(colnames(FORM3)[colnames(FORM3)%in% c(colnames(F3_colnames),"Pkey")])
      
    
    FORM3_A <- FORM3%>%
      select(Pkey,F3_D01,F3_D02,F3_D03,F3_D06,F3_D07,F3_D08,F3_D09,F3_D10,
             F3_D11,F3_D12,F3_D13,F3_D14SAL,F3_D14MAH,F3_D15SAL,F3_D15MAH,
             F3_D16SHASLIR,F3_D16SHASLIS,F3_D16SHHAMRO,F3_D16SHHAMSA,
             F3_D17,F3_D18SHANBEH,F3_D18YEKSHAN,F3_D18DOSHANB,F3_D18SESHANB,
             F3_D18CHARSHA,F3_D18PANSHAN,F3_D18JOMEH,F3_D18JAM,F3_D19,F3_D20,
             F3_D21,F3_D22,F3_D23,F3_D24,F3_D25,F3_D26,F3_D27ROZ,F3_D27SAAT,
             F3_D28,F3_D29,F3_D30,F3_D31,F3_D1_32,F3_D2_32,F3_D3_32,F3_D4_32,
             F3_D5_32,F3_D6_32,F3_D7_32,F3_D8_32,F3_D33,F3_D34,F3_D35SAL,
             F3_D35MAH,F3_D36,F3_D37SAL,F3_D37MAH,F3_D38,F3_D39,F3_D40SAL,
             F3_D40MAH,F3_D41,F3_D42,F3_D43,F3_D44,F3_D45,F3_D46,F3_D47,F3_D49,F3_D50)
      

    FORM3_EX <- FORM3%>%
      select(-colnames(FORM3_A),Pkey)%>%
      select(Pkey,everything())
    
    saveRDS(FORM3_A,paste0(Processd_data_Path,"/",year,"/","FORM3.RDS"))
    saveRDS(FORM3_EX,paste0(Processd_data_Path,"/",year,"/","FORM3_EX.RDS"))
  }
}


#ISIC_Main <- read_xlsx(paste0(Files_paths,"/ISIC.xlsx"),sheet = "REV3.1_Main_Group")
#ISIC_Detail <- read_xlsx(paste0(Files_paths,"/ISIC.xlsx"),sheet = "REV4 to REV3.1")
#for (year in 87:98) {
#  FORM3 <- readRDS(paste0(Processd_data_Path,"/",year,"/FORM3.RDS"))%>%
#    mutate_at(vars(F3_D10),as.character)
#  if(year %in% 87:91){
#    for (i in 1:length(ISIC_Main$Code_3.1)) {
#      FORM3$F3_D10[str_detect(FORM3$F3_D10,paste0("^",ISIC_Main$Code_3.1[i]))] <- ISIC_Main$Main_Group[i]
#    }
#    FORM3 <- FORM3%>%
#      mutate_at(vars(F3_D10),as.factor)
#    saveRDS(FORM3,paste0(Processd_data_Path,"/",year,"/FORM3.RDS"))
#  }else if (year %in% 92:98) {
#    
#    for (j in 1:length(ISIC_Detail$ISIC4code)) {
#      FORM3$F3_D10[str_detect(FORM3$F3_D10,paste0("^",ISIC_Detail$ISIC4code[j]))] <- ISIC_Detail$Main_Group[j]  
#    } 
#    for (k in 1:length(ISIC_Main$Code_4)) {
#      FORM3$F3_D10[str_detect(FORM3$F3_D10,paste0("^",ISIC_Main$Code_4[k]))] <- ISIC_Main$Main_Group[k]
#    }
#    FORM3 <- FORM3%>%
#      mutate_at(vars(F3_D10),as.factor)
#    saveRDS(FORM3,paste0(Processd_data_Path,"/",year,"/FORM3.RDS"))
#  }
#}


