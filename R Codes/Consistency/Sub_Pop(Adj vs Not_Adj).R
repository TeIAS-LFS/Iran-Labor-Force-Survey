Sub_Pop <- tibble(
  Year            = character(),
  Total           = numeric(),
  Male_0_9_U      = numeric(),
  Male_10_64_U    = numeric(),
  Male_65_U       = numeric(),
  Female_0_9_U    = numeric(),
  Female_10_64_U  = numeric(),
  Female_65_U     = numeric(),
  Male_0_9_R      = numeric(),
  Male_10_64_R    = numeric(),
  Male_65_R       = numeric(),
  Female_0_9_R    = numeric(),
  Female_10_64_R  = numeric(),
  Female_65_R     = numeric(),
)

Adj_Sub_Pop <- tibble(
  Year            = character(),
  Total           = numeric(),
  Male_0_9_U      = numeric(),
  Male_10_64_U    = numeric(),
  Male_65_U       = numeric(),
  Female_0_9_U    = numeric(),
  Female_10_64_U  = numeric(),
  Female_65_U     = numeric(),
  Male_0_9_R      = numeric(),
  Male_10_64_R    = numeric(),
  Male_65_R       = numeric(),
  Female_0_9_R    = numeric(),
  Female_10_64_R  = numeric(),
  Female_65_R     = numeric(),
)

for (i in 85 :95) {
  W3 <- readRDS(paste0(Path,"/",Temperory_files,"/",i,"/","W3.RDS"))
  Sub_Pop  <-  Sub_Pop %>% add_row(
    Year           = paste0(i),
    Total           = 0.25*sum(W3$IW_Yearly,na.rm = true),
    Male_0_9_U      = 0.25*sum(W3$IW_Yearly[W3$GENDER == "1" & W3$Rural == "1" & W3$AGE <=9],na.rm = true),
    Male_10_64_U    = 0.25*sum(W3$IW_Yearly[W3$GENDER == "1" & W3$Rural == "1" & (W3$AGE >= 10 & W3$AGE <= 64)],na.rm = true),
    Male_65_U       = 0.25*sum(W3$IW_Yearly[W3$GENDER == "1" & W3$Rural == "1" & W3$AGE >=65],na.rm = true),
    Female_0_9_U    = 0.25*sum(W3$IW_Yearly[W3$GENDER == "2" & W3$Rural == "1" & W3$AGE <=9],na.rm = true),
    Female_10_64_U  = 0.25*sum(W3$IW_Yearly[W3$GENDER == '2' & W3$Rural == "1" & (W3$AGE >= 10 & W3$AGE <= 64)],na.rm = true),
    Female_65_U     = 0.25*sum(W3$IW_Yearly[W3$GENDER == "2" & W3$Rural == "1" & W3$AGE >=65],na.rm = true),
    Male_0_9_R      = 0.25*sum(W3$IW_Yearly[W3$GENDER == "1" & W3$Rural == "2" & W3$AGE <=9],na.rm = true),
    Male_10_64_R    = 0.25*sum(W3$IW_Yearly[W3$GENDER == "1" & W3$Rural == "2" & (W3$AGE >= 10 & W3$AGE <= 64)],na.rm = true),
    Male_65_R       = 0.25*sum(W3$IW_Yearly[W3$GENDER == "1" & W3$Rural == '2' & W3$AGE >=65],na.rm = true),
    Female_0_9_R    = 0.25*sum(W3$IW_Yearly[W3$GENDER == "2" & W3$Rural == "2" & W3$AGE <=9],na.rm = true),
    Female_10_64_R  = 0.25*sum(W3$IW_Yearly[W3$GENDER == "2" & W3$Rural == "2" & (W3$AGE >= 10 & W3$AGE <= 64)],na.rm = true),
    Female_65_R     = 0.25*sum(W3$IW_Yearly[W3$GENDER == "2" & W3$Rural == "2" & W3$AGE >=65],na.rm = true),
    
  )
  
  Adj_Sub_Pop  <-  Adj_Sub_Pop %>% add_row(
    Year           = paste0(i),
    Total           = 0.25*sum(W3$Adj_IW_Yearly,na.rm = true),
    Male_0_9_U      = 0.25*sum(W3$Adj_IW_Yearly[W3$GENDER == "1" & W3$Rural == "1" & W3$AGE <=9],na.rm = true),
    Male_10_64_U    = 0.25*sum(W3$Adj_IW_Yearly[W3$GENDER == "1" & W3$Rural == "1" & (W3$AGE >= 10 & W3$AGE <= 64)],na.rm = true),
    Male_65_U       = 0.25*sum(W3$Adj_IW_Yearly[W3$GENDER == "1" & W3$Rural == "1" & W3$AGE >=65],na.rm = true),
    Female_0_9_U    = 0.25*sum(W3$Adj_IW_Yearly[W3$GENDER == "2" & W3$Rural == "1" & W3$AGE <=9],na.rm = true),
    Female_10_64_U  = 0.25*sum(W3$Adj_IW_Yearly[W3$GENDER == '2' & W3$Rural == "1" & (W3$AGE >= 10 & W3$AGE <= 64)],na.rm = true),
    Female_65_U     = 0.25*sum(W3$Adj_IW_Yearly[W3$GENDER == "2" & W3$Rural == "1" & W3$AGE >=65],na.rm = true),
    Male_0_9_R      = 0.25*sum(W3$Adj_IW_Yearly[W3$GENDER == "1" & W3$Rural == "2" & W3$AGE <=9],na.rm = true),
    Male_10_64_R    = 0.25*sum(W3$Adj_IW_Yearly[W3$GENDER == "1" & W3$Rural == "2" & (W3$AGE >= 10 & W3$AGE <= 64)],na.rm = true),
    Male_65_R       = 0.25*sum(W3$Adj_IW_Yearly[W3$GENDER == "1" & W3$Rural == '2' & W3$AGE >=65],na.rm = true),
    Female_0_9_R    = 0.25*sum(W3$Adj_IW_Yearly[W3$GENDER == "2" & W3$Rural == "2" & W3$AGE <=9],na.rm = true),
    Female_10_64_R  = 0.25*sum(W3$Adj_IW_Yearly[W3$GENDER == "2" & W3$Rural == "2" & (W3$AGE >= 10 & W3$AGE <= 64)],na.rm = true),
    Female_65_R     = 0.25*sum(W3$Adj_IW_Yearly[W3$GENDER == "2" & W3$Rural == "2" & W3$AGE >=65],na.rm = true),
  )
}


colors <- c("Not_adjusted" = "black","Adjusted" = "red")
linetype  <-  c( "Not_adjusted" = 2 , "Adjusted" = 4 )
shapes <- c("Not_adjusted" = 15,"Adjusted" = 17)
for (i in colnames(Adj_Sub_Pop)[2:length(colnames(Adj_Sub_Pop))]) {
  ggplot(mapping = aes(x = Year ,y = eval(parse(text = i)) /1000000,group = 1))+
    geom_line(data = Sub_Pop, aes(color = "Not_adjusted" , lty = "Not_adjusted"))+
    geom_point(data = Sub_Pop,aes(color = "Not_adjusted" , shape = "Not_adjusted"))+
    geom_line(data = Adj_Sub_Pop, aes(color =  "Adjusted" , lty = "Adjusted"))+
    geom_point(data = Adj_Sub_Pop, aes(color =  "Adjusted" , shape = "Adjusted"))+
    theme(
      legend.position = "bottom",
      legend.justification = "center",
      legend.box.just = "bottom",
      legend.margin = margin(6, 6, 6, 6),
      axis.text.x = element_text( face = "bold",color = "black", size = 10),
      plot.title = element_text(hjust = 0.5))+
    labs( color = "Legend")+
    guides(x = guide_axis(angle = 90))+
    scale_linetype_manual("",values = linetype)+
    scale_color_manual("",values = colors)+
    scale_shape_manual("",values = shapes)+
    ylab(paste0(i,"(Million)"))+
    ggtitle(i)
  ggsave(paste0("F:/LFS/Results/Sub_Pop/",i,".png"),width=20, height=15,limitsize = FALSE,units = "cm")
}
