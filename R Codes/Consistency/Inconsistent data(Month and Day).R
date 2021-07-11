Inconsistent_F3 <- tibble(
  Year = numeric(),
  F3_D14MAH =numeric(),
  F3_D15MAH =numeric(),
  F3_D16SHASLIR =numeric(),
  F3_D16SHASLIS =numeric(),
  F3_D16SHHAMRO =numeric(),
  F3_D16SHHAMSA =numeric(),
  F3_D27ROZ =numeric(),
  F3_D27SAAT =numeric(),
  F3_D35MAH =numeric(),

  F3_D37MAH =numeric(),
  F3_D40MAH =numeric(),
)

for (year in First_year:Last_year) {
  FORM3 <-  readRDS(paste0(Processd_data_Path,"/",year,"/FORM3.RDS"))
  
  Inconsistent_F3 <- Inconsistent_F3%>%add_row(
    Year = year,
    F3_D14MAH     = sum(FORM3$F3_D14MAH > 12 , na.rm = true),
    F3_D15MAH     = sum(FORM3$F3_D15MAH > 12 , na.rm =true),
    F3_D16SHASLIR = sum(FORM3$F3_D16SHASLIR > 7 , na.rm = true),
    F3_D16SHASLIS = sum((FORM3$F3_D16SHASLIS/FORM3$F3_D16SHASLIR) > 24 , na.rm = true),
    F3_D16SHHAMRO = sum(FORM3$F3_D16SHHAMRO > 7 , na.rm = true),
    F3_D16SHHAMSA = sum((FORM3$F3_D16SHHAMSA/FORM3$F3_D16SHHAMRO) > 24 , na.rm = true),
    F3_D27ROZ     = sum(FORM3$F3_D27ROZ > 7 ,na.rm = true),
    F3_D27SAAT    = sum((FORM3$F3_D27SAAT/FORM3$F3_D27ROZ) >24 ,na.rm = true),
    F3_D35MAH     = sum(FORM3$F3_D35MAH > 12 , na.rm = true),
    F3_D37MAH     = sum(FORM3$F3_D37MAH > 12 , na.rm = true),
    F3_D40MAH     = sum(FORM3$F3_D40MAH > 12 , na.rm = true)
  )
  
}

saveRDS(Inconsistent_F3,"F:/LFS/Code/Consistency/FORM3 Inconsistent data.RDS")
write_xlsx(Inconsistent_F3,"F:/LFS/Code/Consistency/FORM3 Inconsistent data.xlsx")


 

