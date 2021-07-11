library("stringi")
library("tidyverse")
#==============================================================================#
#PF2J : path FORM2JOZ
#PF2K : path FOrm2KOL
#PF3  : path FORM3
#PW3  : path W3
#==============================================================================#
#              creating Path for FORM2JOZ for year 84:95                       #
#==============================================================================#
PF2J <-list.files(Raw_data_path,pattern = "FORM2JOZ.xlsx",
                  full.names = TRUE,recursive = TRUE) 

PF2J <- c(PF2J,list.files(Raw_data_path,pattern = "FORM2JOZ.DBF",
                          full.names = TRUE,recursive = TRUE))

PF2J <- c(PF2J,list.files(Raw_data_path,pattern = "FORM2JOZ.dta",
                          full.names = TRUE ,recursive = TRUE))
#==============================================================================#
#              creating path for FORM2KOL for year 84:95                       #
#==============================================================================#
PF2K <- list.files(Raw_data_path,pattern="FORM2KOL.xlsx",
                   full.names = TRUE,recursive = TRUE)      
PF2K <- c(PF2K,list.files(Raw_data_path,pattern = "FORM2KOL.DBF",
                          full.names = TRUE,recursive = TRUE))
PF2K <- c(PF2K,list.files(Raw_data_path,pattern = "FORM2KOL.dta",
                          full.names = TRUE,recursive = TRUE))
#==============================================================================#
#                 creating path for FORM3L for year 84:95                      #
#==============================================================================#
PF3 <- list.files(Raw_data_path,pattern = "FORM3.xlsx",
                  full.names = TRUE,recursive = TRUE)
PF3 <- c(PF3,list.files(Raw_data_path,pattern = "FORM3.DBF",
                        full.names = TRUE,recursive = TRUE))
PF3 <- c(PF3,list.files(Raw_data_path,pattern = "FORM3.dta",
                        full.names = TRUE,recursive = TRUE))
#==============================================================================#
#                   creating path for W3 for year 84:95                        #
#==============================================================================#
PW3 <- list.files(Raw_data_path,pattern = "W3.xlsx",
                  full.names = TRUE,recursive = TRUE)
PW3<- c(PW3,list.files(Raw_data_path,pattern = "W3.DBF",
                      full.names = TRUE,recursive = TRUE))
PW3 <- c(PW3,list.files(Raw_data_path,pattern = "W3.dta",
                        full.names =TRUE ,recursive = TRUE))
#==============================================================================#
#                         creating path for year 96 , ...                      #
#==============================================================================#
mdb_files <- list.files(Raw_data_path,pattern = ".mdb",
                        full.names = TRUE,recursive = TRUE)
#==============================================================================#
Raw_path <- c(PF2J,PF2K,PF3,PW3,mdb_files)
RDS_path <- str_replace(Raw_path,".xlsx",".RDS")
RDS_path <- str_replace(RDS_path,".DBF",".RDS")
RDS_path <- str_replace(RDS_path,".mdb",".RDS")
RDS_path <- str_replace(RDS_path,".dta",".RDS")
RDS_path <- str_replace(RDS_path,Raw_data,Imported_data)
RDS_path
rm(PF2J,PF2K,PF3,PW3,package,pkglist,mdb_files)

