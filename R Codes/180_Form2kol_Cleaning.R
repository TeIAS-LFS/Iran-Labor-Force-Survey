
F2K_colnames <- read_xlsx(Col_name_path,sheet = "FORM2KOL")

F2K_RDS_Path <- RDS_path[str_detect(RDS_path,"FORM2KOL.RDS|9802|9803|9804")]

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
  

    if (year == 84) {
      FORM2KOL <- FORM2KOL%>%
        mutate(Season = ifelse((str_sub(YSHHID,3,4) %in% c("01","02","03")),"01",
                               ifelse((str_sub(YSHHID,3,4)%in% c("04","05","06")),"02",
                                      ifelse((str_sub(YSHHID,3,4)%in% c("07","08","09")),
                                             "03","04"))))%>%
        mutate(YSHHID = paste0(str_sub(YSHHID,1,2),Season,str_sub(YSHHID,5,12)))%>%
        select(-Season)
      
    }
  
  FORM2KOL <- FORM2KOL%>%
    select(colnames(FORM2KOL)[colnames(FORM2KOL)%in% c(colnames(F2K_colnames))])%>%
    select(YSHHID,everything())
  
    saveRDS(FORM2KOL,paste0(Processd_data_Path,"/",year,"/","FORM2KOL.RDS"))

  }


  

