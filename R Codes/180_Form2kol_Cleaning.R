
F2K_colnames <- read_xlsx(Col_name_path,sheet = "FORM2KOL")

F2K_RDS_Path <- RDS_path[str_detect(RDS_path,"FORM2KOL.RDS")]
F2K_RDS_Path <- c(F2K_RDS_Path,RDS_path[str_detect(RDS_path,"9802")|str_detect(RDS_path,"9803")|str_detect(RDS_path,"9804")])

for (year in First_year:Last_year) {
  index <- match(F2K_colnames$Year_Season[str_detect(F2K_colnames$Year_Season,paste0("^",year))],F2K_colnames$Year_Season)
  FORM2KOL <- tibble()
  for (j in index) {
    F2K <- readRDS(F2K_RDS_Path[str_detect(F2K_RDS_Path, as.character(F2K_colnames$Year_Season[j]))])
    for (i in 2:length(colnames(F2K_colnames))) {
      names(F2K)[names(F2K) == eval(parse(text=paste0("F2K_colnames","$",colnames(F2K_colnames)[i])))[j]] <- colnames(F2K_colnames)[i]
    }
    F2K <- F2K %>%
      mutate_all(as.character)
    FORM2KOL <- bind_rows(FORM2KOL,F2K)
  }
  if (year <= 97) {
    if (year == 84) {
      FORM2KOL$Month  <-  str_sub(FORM2KOL$YSHHID,3,4)
      
      FORM2KOL$Season[FORM2KOL$Month == "01" |FORM2KOL$Month == "02" |FORM2KOL$Month == "03"]  <- "01"
      FORM2KOL$Season[FORM2KOL$Month == "04" |FORM2KOL$Month == "05" |FORM2KOL$Month == "06"]  <- "02"
      FORM2KOL$Season[FORM2KOL$Month == "07" |FORM2KOL$Month == "08" |FORM2KOL$Month == "09"]  <- "03"
      FORM2KOL$Season[FORM2KOL$Month == "10" |FORM2KOL$Month == "11" |FORM2KOL$Month == "12"]  <- "04"
      
      FORM2KOL <- FORM2KOL%>%
        mutate(YSHHID = paste0(str_sub(YSHHID,1,2),Season,str_sub(YSHHID,5,12)))%>%
        select(-Month,-Season)
    }
    
    saveRDS(FORM2KOL,paste0(Processd_data_Path,"/",year,"/","FORM2KOL.RDS"))

    }else{
      FORM2KOL <- FORM2KOL%>%select(colnames(FORM2KOL)[colnames(FORM2KOL)%in% c(colnames(F2K_colnames))])%>%
        select(YSHHID,everything())
      FORM2KOL <- FORM2KOL%>%
        distinct(YSHHID,.keep_all = TRUE)
      saveRDS(FORM2KOL,paste0(Processd_data_Path,"/",year,"/","FORM2KOL.RDS"))
  }
}

  

