
library("tidyverse")
library("stringi")
library("readxl")
library("foreign")
library("RODBC")
library("rio")


if (file.exists(Processd_data_Path_dta)) {
  for (year in N) {
    if(file.exists(paste0(Processd_data_Path_dta,"/",year))){
      NULL
    }else{
      dir.create(paste0(Processd_data_Path_dta,"/",year))
    }
  }
}else{
  dir.create(Processd_data_Path_dta)
  for (year in N) {
    dir.create(paste0(Processd_data_Path_dta,"/",year))
  }
}

RDS_Pro_P <- list.files(Processd_data_Path,pattern = ".RDS",
                full.names = TRUE,recursive = TRUE)
DTA_Pro_P <- str_replace(RDS_Pro_P,Processd_data,Processd_data_dta)
DTA_Pro_P <- str_replace(DTA_Pro_P,".RDS",".dta")


for (i in 1:length(RDS_Pro_P)) {
  A <- readRDS(RDS_Pro_P[i])
  export(A,DTA_Pro_P[i])
}
