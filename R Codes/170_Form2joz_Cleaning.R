library("tidyverse")
library("stringi")
library("readxl")
library("foreign")
library("RODBC")

F2J_colnames <- read_xlsx(Col_name_path,sheet = "FORM2JOZ")
F2J_RDS_Path <- RDS_path[str_detect(RDS_path,"FORM2JOZ.RDS|9802|9803|9804")]

Factor_vars <- c("Relation","Gender","Birth_Month","Nationality","Residence_Status",
                 "Residence_Duration","F2_D11","F2_D12","F2_D13","F2_D14","Student",
                 "Literacy","Degree","Field_Study","Marriage_Status")

Numeric_vars <- c("Birth_Year","Age")

Character_vars <- c("Pkey","F2_D01")

for (year in First_year:Last_year) {
  index <- match(F2J_colnames$Year_Season[str_detect(F2J_colnames$Year_Season,paste0("^",year))],F2J_colnames$Year_Season)
  FORM2JOZ <- tibble()
  
  for (j in index) {
    F2J <- readRDS(F2J_RDS_Path[str_detect(F2J_RDS_Path, as.character(F2J_colnames$Year_Season[j]))])
    for (i in 2:length(colnames(F2J_colnames))) {
      names(F2J)[names(F2J) == eval(parse(text=paste0("F2J_colnames","$",colnames(F2J_colnames)[i])))[j]] <- colnames(F2J_colnames)[i]
    }
    F2J <- F2J %>%
      mutate_all(as.character)
    FORM2JOZ <- bind_rows(FORM2JOZ,F2J)
  }
  
  FORM2JOZ<- FORM2JOZ%>%
    mutate(Pkey = case_when(
      nchar(F2_D01)== 1 ~ paste0(PKEY,"0",F2_D01),
      nchar(F2_D01)== 2 ~ paste0(PKEY, F2_D01)
    )) %>% select(-PKEY)%>%
    mutate(Age = ifelse(Age %in% c("**","-2"),"101",Age))%>%
    mutate_at(vars(Age),as.integer)%>%
    mutate(Age = ifelse(Age >=101 , 101 , Age))
  

    if (year == 84) {
      
      FORM2JOZ <- FORM2JOZ%>%
        mutate(Season = ifelse((str_sub(Pkey,3,4) %in% c("01","02","03")),"01",
                               ifelse((str_sub(Pkey,3,4)%in% c("04","05","06")),"02",
                                      ifelse((str_sub(Pkey,3,4)%in% c("07","08","09")),
                                             "03","04"))))%>%
        mutate(Pkey = paste0(str_sub(Pkey,1,2),Season,str_sub(Pkey,5,14)))%>%
        select(-Season)
    }
    
  Fac_vars <- excel_sheets(F2J_Labels_path)
  Fac_vars <- Fac_vars[excel_sheets(F2J_Labels_path) %in% colnames(FORM2JOZ)]
  for (i in 1:length(Fac_vars)) {
    Levels <- read_xlsx(F2J_Labels_path,sheet = Fac_vars[i])
    Levels[Levels == "space"] <- " "
    Levels[Levels == "2space"] <- "  "
    Levels[Levels == "0space"] <- ""
    Levels_Value <- colnames(Levels)[!str_detect(colnames(Levels),"\\...")]
    for (j in 2:length(Levels_Value)) {
      L <-  eval(parse(text = paste0("FORM2JOZ$",Fac_vars[i],"==","Levels$",Levels_Value[j],"[Levels$Year ==",year,"]")))          
      eval(parse(text = paste0("FORM2JOZ$",Fac_vars[i],"[L]"," <- ","Levels$",Levels_Value[j],"[1]")))
    }
  }
  
  FORM2JOZ <- FORM2JOZ%>%
    mutate(Residence_Duration = ifelse(is.na(Residence_Duration) & (Residence_Status %in% c("Abroad","Others")|Age==0),"NR",
                                       Residence_Duration),
           F2_D11 = ifelse(is.na(F2_D11) & (Residence_Duration=="NR"|Residence_Duration=="1 year and more"),"NR",
                           F2_D11),
           F2_D12 = ifelse(is.na(F2_D12) & (F2_D11=="NR"|F2_D11== "Other places Same City"),"NR",
                           F2_D12),
           F2_D13 = ifelse(is.na(F2_D13) & (F2_D11=="NR"|F2_D11 %in% c("Other places Same City","Other City Same Province",
                                                      "Other village Same Province","Abroad")),"NR",
                           F2_D13),
           F2_D14 = ifelse(is.na(F2_D14) & (Residence_Status %in% c("Present","Temporary_Absent","Others")),"NR",
                           F2_D14),
           Student = ifelse(is.na(Student) & (Age < 6 ), "NR",
                            Age),
           Literacy = ifelse(is.na(Literacy) & (Age < 6), "NR",
                             Literacy),
           Degree = ifelse(is.na(Degree) & (Literacy == "NR"|Literacy=="No"),"NR",
                           Degree),
           Field_Study = ifelse(is.na(Field_Study) & (Degree=="NR"|Degree %in% c("Elementary","Junior","Others")),"NR",
                                Field_Study),
           Marriage_Status = ifelse(is.na(Marriage_Status) & (Age < 10) , "NR",Marriage_Status))
  
  FORM2JOZ <- FORM2JOZ%>%
    mutate_at(vars(Factor_vars[Factor_vars %in% colnames(FORM2JOZ)]),as.factor)%>%
    mutate_at(vars(Numeric_vars[Numeric_vars %in% colnames(FORM2JOZ)]),as.integer)%>%
    mutate_at(vars(Character_vars[Character_vars %in% colnames(FORM2JOZ)]),as.character)%>%
    select(colnames(FORM2JOZ)[colnames(FORM2JOZ)%in% c(colnames(F2J_colnames),"Pkey")])
  
  
  
  F2J_L <- read_xlsx(paste0(Files_paths,"/","Labels.xlsx"),sheet = "FORM2JOZ")
  F_Vars <- colnames(F2J_L)
  
  for (vars in F_Vars) {
    eval(parse(text = paste0("FORM2JOZ$",vars, " <- factor(","FORM2JOZ$",vars,",","levels =","F2J_L$",vars,"[!is.na(F2J_L$",vars,")])")))
  }

  
  saveRDS(FORM2JOZ,paste0(Processd_data_Path,"/",year,"/","FORM2JOZ.RDS"))
  

  
}

