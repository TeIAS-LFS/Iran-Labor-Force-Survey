
for (i in N) {
  if (i <= 95) {
    print(paste0("there is no link for year",i," data you should import them manually with the specific format"))
    
  }else{
    if (file.exists(paste0(Path,"/",Raw_data,"/",i,"/","LFS_",i,"RawData.zip"))) {
      print("file exist")
    }else{
      download.file(eval(parse(text = paste0("SRD$Raw_",i))),
                    destfile = paste0(Path,"/",Raw_data,i,"/","LFS_",i,"RawData.zip")
                    ,mode="wb")
    }
  }
}





