F2J_colnames <- read_xlsx(Col_name_path,sheet = "FORM2JOZ")

F2J_RDS_Path <- RDS_path[str_detect(RDS_path,"FORM2JOZ.RDS")]
F2J_RDS_Path <- c(F2J_RDS_Path,RDS_path[str_detect(RDS_path,"9802")|str_detect(RDS_path,"9803")|str_detect(RDS_path,"9804")])


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
    mutate_at(vars(Age),as.character)   
  FORM2JOZ$Age[FORM2JOZ$Age == "**" | FORM2JOZ$Age == "-2"] <- 101
  
  if (year <= 97) {
    if (year == 84) {
      FORM2JOZ$Month  <-  str_sub(FORM2JOZ$PKEY,3,4)
      FORM2JOZ$Season[FORM2JOZ$Month == "01" |FORM2JOZ$Month == "02" |FORM2JOZ$Month == "03"]  <- "01"
      FORM2JOZ$Season[FORM2JOZ$Month == "04" |FORM2JOZ$Month == "05" |FORM2JOZ$Month == "06"]  <- "02"
      FORM2JOZ$Season[FORM2JOZ$Month == "07" |FORM2JOZ$Month == "08" |FORM2JOZ$Month == "09"]  <- "03"
      FORM2JOZ$Season[FORM2JOZ$Month == "10" |FORM2JOZ$Month == "11" |FORM2JOZ$Month == "12"]  <- "04"
      FORM2JOZ$PKEY <- paste0(str_sub(FORM2JOZ$PKEY,1,2),FORM2JOZ$Season,str_sub(FORM2JOZ$PKEY,5,14))
    }
    FORM2JOZ<- FORM2JOZ%>%
      mutate(Pkey = case_when(
        nchar(F2_D01)== 1 ~ paste0(PKEY,"0",F2_D01),
        nchar(F2_D01)== 2 ~ paste0(PKEY, F2_D01)
      ))%>%
      select(-PKEY)
    
    FORM2JOZ <- FORM2JOZ %>%
      mutate_at(vars(Birth_Year,Age),as.integer)%>%
      mutate_at(vars(Pkey,F2_D01),as.character)%>%
      mutate_at(vars(Relation,Gender,Birth_Month,Nationality,Residence_Status,
                     Residence_Duration,F2_D11,F2_D12,F2_D13,F2_D14,Student,
                     Literacy,Degree,Field_Study,Marriage_Status),as.factor)
    
  }else{
    FORM2JOZ<- FORM2JOZ%>%
      mutate(Pkey  = case_when(
        nchar(F2_D01)== 1 ~ paste0(PKEY,"0",F2_D01),
        nchar(F2_D01)== 2 ~ paste0(PKEY, F2_D01)
      ))%>%
      select(-PKEY)
    
    FORM2JOZ <- FORM2JOZ %>%
      mutate_at(vars(Birth_Year,Age),as.integer)%>%
      mutate_at(vars(F2_D01),as.character)%>%
      mutate_at(vars(Relation,Gender,Birth_Month,Nationality,Residence_Status,
                     Residence_Duration,F2_D11,F2_D12,F2_D13,F2_D14,Student,
                     Literacy,Degree,Field_Study,Marriage_Status),as.factor)
    FORM2JOZ <- FORM2JOZ%>%
      select(colnames(FORM2JOZ)[colnames(FORM2JOZ)%in% c(colnames(F2J_colnames),"Pkey")])
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
      L <- levels(eval(parse(text = paste0("FORM2JOZ$",Fac_vars[i])))) == eval(parse(text = paste0("Levels$",Levels_Value[j])))[Levels$Year == year]
      eval(parse(text =paste0("levels(","FORM2JOZ$",Fac_vars[i],")[L]"," <- ","Levels$",Levels_Value[j],"[1]")))
    }
  }
  
  F_Vars <- colnames(FORM2JOZ)[sapply(FORM2JOZ,is.factor)]
  
  for (i in F_Vars[2:length(F_Vars)]) {
    eval(parse(text = paste0("FORM2JOZ$",i," <- ","factor(FORM2JOZ$",i,",","levels = c(levels(FORM2JOZ$",i,")",",","\"","NR","\"","))"))) 
    
  }
  FORM2JOZ$Residence_Duration[FORM2JOZ$Residence_Status %in% c("Abroad","Others")|FORM2JOZ$Age==0] <- "NR"
  FORM2JOZ$F2_D11[FORM2JOZ$Residence_Duration=="NR"|FORM2JOZ$Residence_Duration=="1 year and more"] <- "NR"
  FORM2JOZ$F2_D12[FORM2JOZ$F2_D11=="NR"|FORM2JOZ$F2_D11== "Other places Same City"] <- "NR"
  FORM2JOZ$F2_D13[FORM2JOZ$F2_D11=="NR"|FORM2JOZ$F2_D11 %in% c("Other places Same City","Other City Same Province","Other village Same Province","Abroad")] <-"NR" 
  FORM2JOZ$F2_D14[FORM2JOZ$Residence_Status %in% c("Present","Temporary_Absent","Others")] <- "NR"
  FORM2JOZ$Student[FORM2JOZ$Age < 6] <- "NR"
  FORM2JOZ$Literacy[FORM2JOZ$Age < 6] <- "NR"
  FORM2JOZ$Degree[FORM2JOZ$Literacy == "NR"|FORM2JOZ$Literacy=="No"] <- "NR"
  FORM2JOZ$Field_Study[FORM2JOZ$Degree=="NR"|FORM2JOZ$Degree %in% c("Elementary","Junior","Others")] <- "NR"
  FORM2JOZ$Marriage_Status[FORM2JOZ$Age < 10] <- "NR"
  
  
  FORM2JOZ <-FORM2JOZ%>%
    select(Pkey,F2_D01,Relation,Gender,Birth_Month,Birth_Year,Age,Nationality,
           Residence_Status,Residence_Duration,F2_D11,F2_D12,F2_D13,F2_D14,
           Student,Literacy,Degree,Field_Study,Marriage_Status)
  
  F2J_L <- read_xlsx(paste0(Files_paths,"/","Labels.xlsx"),sheet = "FORM2JOZ")
  F_Vars <- colnames(F2J_L)
  
  for (vars in F_Vars) {
    eval(parse(text = paste0("FORM2JOZ$",vars, " <- factor(","FORM2JOZ$",vars,",","levels =","F2J_L$",vars,"[!is.na(F2J_L$",vars,")])")))
  }
  FORM2JOZ$Birth_Month[FORM2JOZ$Birth_Month == "--"|FORM2JOZ$Birth_Month == "&&"] <- NA
  FORM2JOZ$F2_D13[FORM2JOZ$F2_D13 =="&&"] <- NA
  
  saveRDS(FORM2JOZ,paste0(Processd_data_Path,"/",year,"/","FORM2JOZ.RDS"))
  

  
}

