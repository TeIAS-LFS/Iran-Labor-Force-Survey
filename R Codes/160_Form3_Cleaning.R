
library("tidyverse")
library("stringi")
library("readxl")
library("foreign")
library("RODBC")

F3_colnames <- read_xlsx(Col_name_path,sheet = "FORM3")

F3_RDS_Path <- RDS_path[str_detect(RDS_path,"FORM3.RDS|9802|9803|9804")]

Factor_vars <- c("F3_D01","F3_D02","F3_D02_87","F3_D03","F3_D04","F3_D04_87",
                 "F3_D05","F3_D05_87","F3_D06","F3_D07","F3_D08","F3_D09","F3_D10",
                 "F3_D11","F3_D12","F3_D13","F3_D17","F3_D19","F3_D20",
                 "F3_D21","F3_D22","F3_D23","F3_D24","F3_D25","F3_D26",
                 "F3_D28","F3_D29","F3_D30","F3_D31","F3_D1_32","F3_D2_32",
                 "F3_D3_32","F3_D4_32","F3_D5_32","F3_D6_32","F3_D7_32",
                 "F3_D8_32","F3_D9_32","F3_D33","F3_D34","F3_D36","F3_D38",
                 "F3_D39","F3_D41","F3_D42","F3_D43","F3_D44","F3_D45",
                 "F3_D46","F3_D47","F3_D49","F3_D50")

Numeric_vars <- c("F3_D14SAL","F3_D14MAH","F3_D15SAL","F3_D15MAH","F3_D16SHASLIR",
                  "F3_D16SHASLIS","F3_D16SHHAMRO","F3_D16SHHAMSA",
                  "F3_D18SHANBEH","F3_D18YEKSHAN","F3_D18DOSHANB","F3_D18SESHANB",
                  "F3_D18CHARSHA","F3_D18PANSHAN","F3_D18JOMEH","F3_D18JAM",
                  "F3_D27ROZ","F3_D27SAAT","F3_D35SAL","F3_D35MAH","F3_D37SAL",
                  "F3_D37MAH","F3_D40SAL","F3_D40MAH","F3_D48SAAT","F3_D48ROZ")

Character_vars <- c("Pkey")


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
    
    FORM3 <- FORM3%>%
      mutate(Season = ifelse(str_sub(Pkey,3,4) %in% c("01","02","03"),"01",
                             ifelse(str_sub(Pkey,3,4) %in% c("04","05","06"),"02",
                                    ifelse(str_sub(Pkey,3,4) %in% c("07","08","09"),"03","04"))),
             F3_D45 = F3_D34)%>%
      mutate(Pkey = paste0(str_sub(Pkey,1,2),Season,str_sub(Pkey,5,14)))%>%
      select(-Season)
    
  }else if(year %in% c(85,86)){
    FORM3<- FORM3 %>%
      mutate(F3_D45 = F3_D34)
  }else{
    FORM3 <- FORM3%>%
      mutate(F3_D02 = case_when(
        F3_D02_87 == "1" | F3_D04_87 == "1" | F3_D05_87 == "1" ~ "1",
        F3_D02_87 == "2" & F3_D04_87 == "2" & F3_D05_87 == "2" ~ "2"))
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
      L <-  eval(parse(text = paste0("FORM3$",Fac_vars[i],"==","Levels$",Levels_Value[j],"[Levels$Year ==",year,"]")))          
      eval(parse(text = paste0("FORM3$",Fac_vars[i],"[L]"," <- ","Levels$",Levels_Value[j],"[1]")))
    }
  }
  
  if(year < 87){
    FORM3 <- FORM3%>%
      mutate(F3_D02 = ifelse(is.na(F3_D02) & (F3_D01 == "Yes"),"NR",F3_D02),
             F3_D03 = ifelse(is.na(F3_D03) & (F3_D02 !="No"),"NR",F3_D03),
             F3_D06 = ifelse(is.na(F3_D06) & (F3_D03 !="No"),"NR",F3_D06),
             F3_D07 = ifelse(is.na(F3_D07) & (F3_D06 !="No"),"NR",F3_D07),
             F3_D08 = ifelse(is.na(F3_D08) & (F3_D06 == "No"),"NR",F3_D08),
             F3_D09 = ifelse(is.na(F3_D09) & (F3_D08 == "NR"),"NR",F3_D09),
             F3_D10 = ifelse(is.na(F3_D10) & (F3_D08 == "NR"),"NR",F3_D10),
             F3_D11 = ifelse(is.na(F3_D11) & (F3_D08 == "NR"),"NR",F3_D11),
             F3_D12 = ifelse(is.na(F3_D12) & (F3_D08 == "NR"),"NR",F3_D12),
             F3_D13 = ifelse(is.na(F3_D13) & (F3_D08 == "NR"),"NR",F3_D13),
             F3_D14SAL = ifelse(is.na(F3_D14SAL) & (F3_D08 == "NR"),-1,F3_D14SAL),
             F3_D14MAH = ifelse(is.na(F3_D14MAH) & (F3_D08 == "NR"),-1,F3_D14MAH),
             F3_D15SAL = ifelse(is.na(F3_D15SAL) & (F3_D08 == "NR"),-1,F3_D15SAL),
             F3_D15MAH = ifelse(is.na(F3_D15MAH) & (F3_D08 == "NR"),-1,F3_D15MAH),
             F3_D16SHASLIR = ifelse(is.na(F3_D16SHASLIR) & (F3_D08 == "NR"),-1,F3_D16SHASLIR),
             F3_D16SHASLIS = ifelse(is.na(F3_D16SHASLIS) & (F3_D08 == "NR"),-1,F3_D16SHASLIS),
             F3_D16SHHAMRO = ifelse(is.na(F3_D16SHHAMRO) & (F3_D08 == "NR"),-1,F3_D16SHHAMRO),
             F3_D16SHHAMSA = ifelse(is.na(F3_D16SHHAMSA) & (F3_D08 == "NR"),-1,F3_D16SHHAMSA),
             F3_D17 = ifelse(is.na(F3_D17) & (F3_D08 == "NR"|F3_D16SHHAMSA >=44),"NR",F3_D17),
             F3_D18SHANBEH = ifelse(is.na(F3_D18SHANBEH) & (F3_D08 == "NR"),-1,F3_D18SHANBEH),
             F3_D18YEKSHAN = ifelse(is.na(F3_D18YEKSHAN) & (F3_D08 == "NR"),-1,F3_D18YEKSHAN),
             F3_D18DOSHANB = ifelse(is.na(F3_D18DOSHANB) & (F3_D08 == "NR"),-1,F3_D18DOSHANB),
             F3_D18SESHANB = ifelse(is.na(F3_D18SESHANB) & (F3_D08 == "NR"),-1,F3_D18SESHANB),
             F3_D18CHARSHA = ifelse(is.na(F3_D18CHARSHA) & (F3_D08 == "NR"),-1,F3_D18CHARSHA),
             F3_D18PANSHAN = ifelse(is.na(F3_D18PANSHAN) & (F3_D08 == "NR"),-1,F3_D18PANSHAN),
             F3_D18JOMEH = ifelse(is.na(F3_D18JOMEH) & (F3_D08 == "NR"),-1,F3_D18JOMEH),
             F3_D18JAM = ifelse(is.na(F3_D18JAM) & (F3_D08 == "NR"),-1,F3_D18JAM),
             F3_D19 = ifelse(is.na(F3_D19) & (F3_D08 == "NR"|F3_D16SHHAMSA >= F3_D18JAM),"NR",F3_D19),
             F3_D20 = ifelse(is.na(F3_D20) & (F3_D08 == "NR"|F3_D16SHHAMSA <= F3_D18JAM),"NR",F3_D20),
             F3_D21 = ifelse(is.na(F3_D21) & (F3_D08 == "NR"),"NR",F3_D21),
             F3_D22 = ifelse(is.na(F3_D22) & (F3_D08 == "NR"|F3_D21 == "No"),"NR",F3_D22),
             F3_D23 = ifelse(is.na(F3_D23) & (F3_D08 == "NR"|F3_D21 == "No"|F3_D22 == "No"),"NR",F3_D23),
             F3_D24 = ifelse(is.na(F3_D24) & (F3_D08 == "NR"),"NR",F3_D24),
             F3_D25 = ifelse(is.na(F3_D25) & (F3_D08 == "NR"|F3_D24 == "No"),"NR",F3_D25),
             F3_D26 = ifelse(is.na(F3_D26) & (F3_D08 == "NR"|F3_D24 == "No"),"NR",F3_D26),
             F3_D27ROZ = ifelse(is.na(F3_D27ROZ) & (F3_D08 == "NR"),-1,F3_D27ROZ),
             F3_D27SAAT = ifelse(is.na(F3_D27SAAT) & (F3_D08 == "NR"),-1,F3_D27SAAT),
             F3_D28 = ifelse(is.na(F3_D28) & (F3_D08 == "NR"|F3_D08 == "No"),"NR",F3_D28),
             F3_D29 = ifelse(is.na(F3_D29) & (F3_D08 == "NR"|F3_D08 == "No"),"NR",F3_D29),
             F3_D30 = ifelse(is.na(F3_D30) & (F3_D08 == "NR"|F3_D08 == "No"),"NR",F3_D30),
             F3_D31 = ifelse(is.na(F3_D31) & (F3_D06 != "No"),"NR",F3_D31),
             F3_D1_32 = ifelse(is.na(F3_D1_32) & (F3_D31 == "NR"|F3_D31 == "No"),"NR",F3_D1_32),
             F3_D2_32 = ifelse(is.na(F3_D2_32) & (F3_D31 == "NR"|F3_D31 == "No"),"NR",F3_D2_32),
             F3_D3_32 = ifelse(is.na(F3_D3_32) & (F3_D31 == "NR"|F3_D31 == "No"),"NR",F3_D3_32),
             F3_D4_32 = ifelse(is.na(F3_D4_32) & (F3_D31 == "NR"|F3_D31 == "No"),"NR",F3_D4_32),
             F3_D5_32 = ifelse(is.na(F3_D5_32) & (F3_D31 == "NR"|F3_D31 == "No"),"NR",F3_D5_32),
             F3_D6_32 = ifelse(is.na(F3_D6_32) & (F3_D31 == "NR"|F3_D31 == "No"),"NR",F3_D6_32),
             F3_D7_32 = ifelse(is.na(F3_D7_32) & (F3_D31 == "NR"|F3_D31 == "No"),"NR",F3_D7_32),
             F3_D8_32 = ifelse(is.na(F3_D8_32) & (F3_D31 == "NR"|F3_D31 == "No"),"NR",F3_D8_32),
             F3_D33 = ifelse(is.na(F3_D33) & (F3_D31 == "NR"|F3_D31 == "Yes"),"NR",F3_D33),
             EX_D36 = ifelse(is.na(EX_D36) & (F3_D33 == "NR"|!(F3_D33 %in% c("First" , "Secound"))),"NR",EX_D36),
             EX_D37SAL = ifelse(is.na(EX_D37SAL) & (EX_D36 == "NR"|EX_D36 == "No"),-1,EX_D37SAL),
             EX_D37MAH = ifelse(is.na(EX_D37MAH) & (EX_D36 == "NR"|EX_D36 == "No"),-1,EX_D37MAH),
             F3_D34 = ifelse(is.na(F3_D34) & (F3_D08 == "NR"),"NR",F3_D34),
             F3_D35SAL = ifelse(is.na(F3_D35SAL) & (F3_D31 != "Yes"),-1,F3_D35SAL),
             F3_D35MAH = ifelse(is.na(F3_D35MAH) & (F3_D31 != "Yes"),-1,F3_D35MAH),
             F3_D36 = ifelse(is.na(F3_D36) & (F3_D31 == "NR"|EX_D36 == "No"|!(F3_D33 %in% c("First" , "Secound"))),"NR",F3_D36),
             F3_D37SAL = ifelse(is.na(F3_D37SAL) & (F3_D36 == "NR"|F3_D36 != "Working"),-1,F3_D37SAL),
             F3_D37MAH = ifelse(is.na(F3_D37MAH) & (F3_D36 == "NR"|F3_D36 != "Working"),-1,F3_D37MAH),
             F3_D38 = ifelse(is.na(F3_D38) & (F3_D31 == "NR"|EX_D36 == "No"|!(F3_D33 %in% c("First" , "Secound"))),"NR",F3_D38),
             F3_D39 = ifelse(is.na(F3_D39) & (F3_D06 != "No"),"NR",F3_D39),
             F3_D40SAL = ifelse(is.na(F3_D40SAL) & (F3_D39 == "NR"|F3_D39 == "No"),-1,F3_D40SAL),
             F3_D40MAH = ifelse(is.na(F3_D40MAH) & (F3_D39 == "NR"|F3_D39 == "No"),-1,F3_D40MAH),
             F3_D41 = ifelse(is.na(F3_D41) & (F3_D39 == "NR"|F3_D39 == "No"),"NR",F3_D41),
             F3_D42 = ifelse(is.na(F3_D42) & (F3_D39 == "NR"|F3_D39 == "No"),"NR",F3_D42),
             F3_D43 = ifelse(is.na(F3_D43) & (F3_D39 == "NR"|F3_D39 == "No"),"NR",F3_D43),
             F3_D44 = ifelse(is.na(F3_D44) & (F3_D39 == "NR"|F3_D39 == "No"),"NR",F3_D44),
             F3_D45 = ifelse(is.na(F3_D45) & (F3_D38 == "NR"),"NR",F3_D45),
             F3_D46 = ifelse(is.na(F3_D46) & (F3_D45 == "NR"|F3_D45 == "Yes"),"NR",F3_D46),
             F3_D47 = ifelse(is.na(F3_D47) & (F3_D31 == "NR"|F3_D45 == "Yes"),"NR",F3_D47),
             F3_D49 = ifelse(is.na(F3_D49) & (F3_D06=="NR"|F3_D06== "Yes"),"NR",F3_D49),
             F3_D50 = ifelse(is.na(F3_D50) & (F3_D06=="NR"|F3_D06== "Yes"),"NR",F3_D50))
  }
  
  
  if (year >= 87) {
    
    FORM3 <- FORM3%>%
      mutate(F3_D02_87 = ifelse(is.na(F3_D02_87) & (F3_D01 == "Yes") ,"NR",F3_D02_87),
             F3_D03 = ifelse(is.na(F3_D03) & (F3_D02_87 == "NR"|F3_D02_87=="Yes")  ,"NR",F3_D03),
             F3_D04_87 = ifelse(is.na(F3_D04_87) & (F3_D03 == "NR" | F3_D03 == "Yes")  ,"NR",F3_D04_87),
             F3_D05_87 = ifelse(is.na(F3_D05_87) & (F3_D04_87 == "NR" | F3_D04_87 == "Yes")  ,"NR",F3_D05_87),
             F3_D06 = ifelse(is.na(F3_D06) & (F3_D05_87 == "NR"|F3_D05_87=="Yes")  ,"NR",F3_D06),
             F3_D07 = ifelse(is.na(F3_D07) & (F3_D06 == "NR" |F3_D06 == "No" ) ,"NR",F3_D07),
             F3_D08 = ifelse(is.na(F3_D08) & (F3_D06 == "No" ) ,"NR",F3_D08),
             F3_D09 = ifelse(is.na(F3_D09) & (F3_D06 == "No" ) ,"NR",F3_D09),
             F3_D10 = ifelse(is.na(F3_D10) &(F3_D06 == "No" ) ,"NR",F3_D10),
             F3_D11 = ifelse(is.na(F3_D11) & (F3_D06 == "No")  ,"NR",F3_D11),
             F3_D12 = ifelse(is.na(F3_D12) & (F3_D06 == "No" ) ,"NR",F3_D12),
             F3_D13 = ifelse(is.na(F3_D13) & (F3_D06 == "No" ) ,"NR",F3_D13),
             F3_D14SAL = ifelse(is.na(F3_D14SAL) & (F3_D06 == "No" ) ,-1,F3_D14SAL),
             F3_D14MAH = ifelse(is.na(F3_D14MAH) & (F3_D06 == "No") ,-1,F3_D14MAH),
             F3_D15SAL = ifelse(is.na(F3_D15SAL) & (F3_D06 == "No" ) ,-1,F3_D15SAL),
             F3_D15MAH = ifelse(is.na(F3_D15MAH) & (F3_D06 == "No" ) ,-1,F3_D15MAH),
             F3_D16SHASLIR = ifelse(is.na(F3_D16SHASLIR) & (F3_D06 == "No" ) ,-1,F3_D16SHASLIR),
             F3_D16SHASLIS = ifelse(is.na(F3_D16SHASLIS) & (F3_D06 == "No")  ,-1,F3_D16SHASLIS),
             F3_D16SHHAMRO = ifelse(is.na(F3_D16SHHAMRO) & (F3_D06 == "No" ) ,-1,F3_D16SHHAMRO),
             F3_D16SHHAMSA = ifelse(is.na(F3_D16SHHAMSA) & (F3_D06 == "No" ) ,-1,F3_D16SHHAMSA),
             F3_D17 = ifelse(is.na(F3_D17) & (F3_D06 == "No" |F3_D16SHHAMSA >=44 ) ,"NR",F3_D17 ),
             F3_D18SHANBEH = ifelse(is.na(F3_D18SHANBEH) & (F3_D06 == "No")  ,-1 ,F3_D18SHANBEH ),
             F3_D18YEKSHAN = ifelse(is.na(F3_D18YEKSHAN) & (F3_D06 == "No" )  ,-1 ,F3_D18YEKSHAN ),
             F3_D18DOSHANB = ifelse(is.na(F3_D18DOSHANB) & (F3_D06 == "No" ) ,-1 ,F3_D18DOSHANB ),
             F3_D18SESHANB = ifelse(is.na(F3_D18SESHANB) & (F3_D06 == "No" ) ,-1 ,F3_D18SESHANB ),
             F3_D18CHARSHA = ifelse(is.na(F3_D18CHARSHA) & (F3_D06 == "No" ) ,-1 ,F3_D18CHARSHA ),
             F3_D18PANSHAN = ifelse(is.na(F3_D18PANSHAN) & (F3_D06 == "No" ) ,-1 ,F3_D18PANSHAN ),
             F3_D18JOMEH = ifelse(is.na(F3_D18JOMEH) & (F3_D06 == "No") ,-1 ,F3_D18JOMEH ),
             F3_D18JAM = ifelse(is.na(F3_D18JAM) & (F3_D06 == "No" ) ,-1 ,F3_D18JAM ),
             F3_D19 = ifelse(is.na(F3_D19) & (F3_D06 == "No" |F3_D18JAM >= F3_D16SHHAMSA)  ,"NR",F3_D19),
             F3_D20 = ifelse(is.na(F3_D20) & (F3_D06 == "No" |F3_D18JAM <= F3_D16SHHAMSA)  ,"NR",F3_D20),
             F3_D21 = ifelse(is.na(F3_D21) & (F3_D06 == "No" ) ,"NR",F3_D21),
             F3_D22 = ifelse(is.na(F3_D22) & (F3_D06 == "No"|F3_D21=="No") ,"NR",F3_D22),
             F3_D23 = ifelse(is.na(F3_D23) & (F3_D06 == "No"|F3_D21=="No" ) ,"NR",F3_D23),
             F3_D24 = ifelse(is.na(F3_D24) & (FORM3$F3_D06 == "No")  ,"NR",F3_D24),
             F3_D25 = ifelse(is.na(F3_D25) & (F3_D06 == "No"|F3_D24=="No" ) ,"NR",F3_D25),
             F3_D26 = ifelse(is.na(F3_D26) & (F3_D06 == "No"|F3_D24=="No" ) ,"NR",F3_D26),
             F3_D27ROZ = ifelse(is.na(F3_D27ROZ) & (F3_D06 == "No" ) ,-1,F3_D27ROZ),
             F3_D27SAAT = ifelse(is.na(F3_D27SAAT) & (F3_D06 == "No" ) ,-1,F3_D27SAAT),
             F3_D28 = ifelse(is.na(F3_D28) & (F3_D06 == "No"|F3_D08 == "No" ) ,"NR", F3_D28),
             F3_D29 = ifelse(is.na(F3_D29) & (F3_D06 == "No"|F3_D08 == "No" ) ,"NR", F3_D29),
             F3_D30 = ifelse(is.na(F3_D30) & (F3_D06 == "No"|F3_D08 == "No" ) ,"NR",F3_D30),
             F3_D31 = ifelse(is.na(F3_D31) & (F3_D06 == "NR"|F3_D06 == "Yes" ) ,"NR",F3_D31),
             F3_D1_32 = ifelse(is.na(F3_D1_32) & (F3_D06 == "NR"|F3_D06 == "Yes"|F3_D31=="No" ) ,"NR",F3_D1_32 ),
             F3_D2_32 = ifelse(is.na(F3_D2_32) & (F3_D06 == "NR"|F3_D06 == "Yes"|F3_D31=="No" ) ,"NR",F3_D2_32 ),
             F3_D3_32 = ifelse(is.na(F3_D3_32) & (F3_D06 == "NR"|F3_D06 == "Yes"|F3_D31=="No" ) ,"NR",F3_D3_32 ),
             F3_D4_32 = ifelse(is.na(F3_D4_32) & (F3_D06 == "NR"|F3_D06 == "Yes"|F3_D31=="No" ) ,"NR",F3_D4_32 ),
             F3_D5_32 = ifelse(is.na(F3_D5_32) & (F3_D06 == "NR"|F3_D06 == "Yes"|F3_D31=="No" ) ,"NR",F3_D5_32 ),
             F3_D6_32 = ifelse(is.na(F3_D6_32) & (F3_D06 == "NR"|F3_D06 == "Yes"|F3_D31=="No" ), "NR",F3_D6_32 ),
             F3_D7_32 = ifelse(is.na(F3_D7_32) & (F3_D06 == "NR"|F3_D06 == "Yes"|F3_D31=="No" ) ,"NR",F3_D7_32 ),
             F3_D8_32 = ifelse(is.na(F3_D8_32) & (F3_D06 == "NR"|F3_D06 == "Yes"|F3_D31=="No" ) ,"NR",F3_D8_32 ),
             F3_D33 = ifelse(is.na(F3_D33) & (F3_D06 == "NR"|F3_D06 == "Yes"|F3_D31=="Yes" ) ,"NR",F3_D33),
             F3_D34 = ifelse(is.na(F3_D34) & (F3_D06 == "NR"|F3_D06 == "Yes"|!(F3_D33 %in% c("First","Secound"))),"NR",F3_D34),
             F3_D35SAL = ifelse(is.na(F3_D35SAL) & (F3_D34=="NR"|F3_D34 == "No" ), -1,F3_D35SAL),
             F3_D35MAH = ifelse(is.na(F3_D35MAH) & (F3_D34=="NR"|F3_D34 == "No" ), -1,F3_D35MAH),
             F3_D36 = ifelse(is.na(F3_D36) & (F3_D34=="NR"|F3_D34 == "No" ), "NR",F3_D36),
             F3_D37SAL = ifelse(is.na(F3_D37SAL) & (F3_D34=="NR"|F3_D34 == "No"|!(F3_D36 %in% c("Working"))),-1,F3_D37SAL),
             F3_D37MAH = ifelse(is.na(F3_D37MAH) & (F3_D34=="NR"|F3_D34 == "No"|!(F3_D36 %in% c("Working"))),-1,F3_D37MAH),
             F3_D38 = ifelse(is.na(F3_D38) & (F3_D34=="NR"|F3_D34 == "No"|!(F3_D36 %in% c("Working"))),"NR",F3_D38),
             F3_D39 = ifelse(is.na(F3_D39) & (F3_D34=="NR"|F3_D34== "No" ),"NR",F3_D39),
             F3_D40SAL = ifelse(is.na(F3_D40SAL) & (F3_D39=="NR"|F3_D39== "No" ), -1,F3_D40SAL),
             F3_D40MAH = ifelse(is.na(F3_D40MAH) & (F3_D39=="NR"|F3_D39== "No" ),-1,F3_D40MAH),
             F3_D41 = ifelse(is.na(F3_D41) & (F3_D39=="NR"|F3_D39== "No" ) ,"NR",F3_D41),
             F3_D42 = ifelse(is.na(F3_D42) & (F3_D39=="NR"|F3_D39== "No" ),"NR",F3_D42),
             F3_D43 = ifelse(is.na(F3_D43) & (F3_D39=="NR"|F3_D39== "No" ), -1,F3_D43),
             F3_D44 = ifelse(is.na(F3_D44) & (F3_D39=="NR"|F3_D39== "No" ) ,-1,F3_D44),
             F3_D45 = ifelse(is.na(F3_D45) & (F3_D31=="NR"|F3_D33 %in% c("Seventh","Eighth","Ninth","Tenth","Eleventh","Twelfth")|F3_D34=="No"|F3_D39%in%c("Yes","No")),"NR",F3_D45),
             F3_D46 = ifelse(is.na(F3_D46) & (F3_D31=="NR"|F3_D33 %in% c("Seventh","Eighth","Ninth","Tenth","Eleventh","Twelfth")|F3_D39=="No"|F3_D45== "Yes" ),"NR",F3_D46),
             F3_D47 = ifelse(is.na(F3_D47) & (F3_D31=="NR"|F3_D39 %in% c("Yes","No") ) ,"NR",F3_D47),
             F3_D49 = ifelse(is.na(F3_D49) & (F3_D06=="NR"|F3_D06== "Yes"), "NR",F3_D49),
             F3_D50 = ifelse(is.na(F3_D50) & (F3_D06=="NR"|F3_D06== "Yes"),"NR",F3_D50))
    FORM3 <- FORM3%>%
      mutate(F3_D02 = case_when(
        F3_D02_87 == "Yes" | F3_D04_87 == "Yes" | F3_D05_87 == "Yes" ~ "Yes",
        F3_D02_87 == "No" & F3_D04_87 == "No" & F3_D05_87 == "No" ~ "No",
        F3_D02_87 == "NR" & F3_D04_87 == "NR" & F3_D05_87 == "NR" ~ "NR"))
  }
  



  FORM3 <- FORM3%>%
    mutate_at(vars(Factor_vars[Factor_vars %in% colnames(FORM3)]),as.factor)%>%
    mutate_at(vars(Numeric_vars[Numeric_vars %in% colnames(FORM3)]),as.integer)%>%
    mutate_at(vars(Character_vars[Character_vars %in% colnames(FORM3)]),as.character)
  
  
  
  F3_L <- read_xlsx(paste0(Files_paths,"/","Labels.xlsx"),sheet = "FORM3")
  F_Vars <- colnames(F3_L)
  
  for (vars in F_Vars) {
    eval(parse(text = paste0("FORM3$",vars, " <- factor(","FORM3$",vars,",","levels =","F3_L$",vars,"[!is.na(F3_L$",vars,")])")))
  }
  
  FORM3 <- FORM3%>%
    mutate(F3_D14MAH     = ifelse(F3_D14MAH > 12,NA,F3_D14MAH),
           F3_D15MAH    = ifelse(F3_D15MAH > 12,NA,F3_D15MAH),
           F3_D16SHASLIR = ifelse(F3_D16SHASLIR > 7,NA,F3_D16SHASLIR),
           F3_D16SHASLIS = ifelse((F3_D16SHASLIS/F3_D16SHASLIR) > 24,NA,F3_D16SHASLIS),
           F3_D16SHHAMRO = ifelse(F3_D16SHHAMRO > 7,NA,F3_D16SHHAMRO),
           F3_D16SHHAMSA = ifelse((F3_D16SHHAMSA/F3_D16SHHAMRO) > 24,NA,F3_D16SHHAMSA),
           F3_D27ROZ = ifelse(F3_D27ROZ > 7,NA,F3_D27ROZ),
           F3_D27SAAT = ifelse((F3_D27SAAT/F3_D27ROZ) >24,NA,F3_D27SAAT),
           F3_D35MAH = ifelse(F3_D35MAH > 12,NA,F3_D35MAH),
           F3_D37MAH = ifelse(F3_D37MAH > 12,NA,F3_D37MAH),
           F3_D40MAH = ifelse(F3_D40MAH > 12,NA,F3_D40MAH))
  


  
  if (year <= 97) {
    
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
        str_sub(Pkey,3,4) == "01" ~ Pkey,
        str_sub(Pkey,3,4) %in% c("02","03","04") & nchar(F2_D01) == 1 ~ paste0(Pkey,"0",F2_D01),
        str_sub(Pkey,3,4) %in% c("02","03","04") & nchar(F2_D01) == 2 ~ paste0(Pkey,F2_D01)
      ))%>%
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

