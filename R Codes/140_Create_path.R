library("stringi")

Raw_path<- list.files(Raw_data_path,pattern = "FORM2JOZ|FORM2KOL|FORM3|W3|.mdb",
                  full.names = TRUE,recursive = TRUE)

RDS_path <- Raw_path%>%
  str_replace(".xlsx|.DBF|.mdb|.dta",".RDS")%>%
  str_replace("Raw data","Imported data")

