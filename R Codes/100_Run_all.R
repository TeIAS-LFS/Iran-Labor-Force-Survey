rm(list=ls())

# Before Running Code specify the directories in  which your code,
#             your files and your data stored.

#code_path = "C:/Users/pc/Dropbox/Research/Labor Market/LFS Cleaning/R Codes"
code_path  = "C:/Users/ali/Dropbox/LFS Cleaning/GitHub/R Codes"
Files_paths       = "C:/Users/ali/Dropbox/LFS Cleaning/GitHub/Files" #path of files expect raw data
Path              = "F:/LFS" 
First_year        = 84L
Last_year         = 98L


source(paste0(code_path,"/","101_Initializing.R"))
source(paste0(code_path,"/","110_Installing_packages.R"))
source(paste0(code_path,"/","120_Building_directory.R"))
#source(paste0(code_path,"/","130_Downloding_rawdata.R"))
source(paste0(code_path,"/","140_Create_path.R"))
#source(paste0(code_path,"/","150_Converting_RDS.R"))
source(paste0(code_path,"/","160_Form3_Cleaning.R"))
source(paste0(code_path,"/","170_Form2joz_Cleaning.R"))
source(paste0(code_path,"/","180_Form2kol_Cleaning.R"))
source(paste0(code_path,"/","190_W3_Cleaning.R"))
source(paste0(code_path,"/","200_Exporting_DTA.R"))



