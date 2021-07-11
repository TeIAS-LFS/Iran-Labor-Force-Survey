#==============================================================================#
#                                                                              #
#              First we load creating path.R file to read data                 #                                    
#              Then we covert raw data to RDS file and store their path        #
#                                                                              #                                                                           #
#==============================================================================#
library("tidyverse")
library("stringi")
library("readxl")
library("foreign")
library("RODBC")
#==============================================================================#
for (i in 1:length(Raw_path)) {
  if (str_detect(Raw_path[i],".xlsx")) {
    s = read_excel(Raw_path[i],col_types = "text")
    saveRDS(s,RDS_path[i])
  }else if (str_detect(Raw_path[i],".DBF")) {
    s = read.dbf(Raw_path[i],as.is = TRUE)
    saveRDS(s,RDS_path[i])
  }else if (str_detect(Raw_path[i],".mdb")) {
      db <-paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",Raw_path[i])
      con <- odbcDriverConnect(db)
      a <- sqlTables(con)
      LFS <- as_tibble(sqlFetch (con ,a$TABLE_NAME[length(a$TABLE_NAME)],rownames=TRUE))
      saveRDS(LFS,RDS_path[i])
      odbcClose(con)
  }else if(str_detect(Raw_path[i],".dta")){
    s = read_dta(Raw_path[i])
    saveRDS(s,RDS_path[i])
  }
}


