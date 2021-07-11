

Adj_LFS_Indicators_Y_C <- readRDS(paste0(code_path,"/Consistency/Adj_LFS_Indicators_Y.RDS"))
LFS_Indicators_Y_C <- readRDS(paste0(code_path,"/Consistency/LFS_Indicators_Y.RDS"))
LFS_Indicators_Y_R_F <- read_excel(paste0(Files_paths,"/","Reported_Indicators.xlsx"),sheet = "Full_yearly")






Adj_LFS_Indicators_Y_C<- Adj_LFS_Indicators_Y_C%>%
  mutate(EMPR_T = Total_Employment/Total_Participation)%>%
  mutate(EMPR_M = Male_Employment/Male_Participation)%>%
  mutate(EMPR_F  = Female_Employment / Female_Participation)%>%
  mutate(EMPR_U  = Urban_Employment / Urban_Participation)%>%
  mutate(EMPR_R  = Rural_Employment / Rural_Participation)%>%
  mutate(UMPR_T = Total_Unemployment/Total_Participation)%>%
  mutate(UMPR_M = Male_Unemployment/Male_Participation)%>%
  mutate(UMPR_F  = Female_Unemployment / Female_Participation)%>%
  mutate(UMPR_U  = Urban_Unemployment / Urban_Participation)%>%
  mutate(UMPR_R  = Rural_Unemployment / Rural_Participation)%>%
  filter(Year %in% c(85:95))%>%
  mutate_at(vars(Year),as.character)


LFS_Indicators_Y_C <- LFS_Indicators_Y_C%>%
  mutate(EMPR_T = Total_Employment/Total_Participation)%>%
  mutate(EMPR_M = Male_Employment/Male_Participation)%>%
  mutate(EMPR_F  = Female_Employment / Female_Participation)%>%
  mutate(EMPR_U  = Urban_Employment / Urban_Participation)%>%
  mutate(EMPR_R  = Rural_Employment / Rural_Participation)%>%
  mutate(UMPR_T = Total_Unemployment/Total_Participation)%>%
  mutate(UMPR_M = Male_Unemployment/Male_Participation)%>%
  mutate(UMPR_F  = Female_Unemployment / Female_Participation)%>%
  mutate(UMPR_U  = Urban_Unemployment / Urban_Participation)%>%
  mutate(UMPR_R  = Rural_Unemployment / Rural_Participation)%>%
  filter(Year %in% c(85:95))%>%
  mutate_at(vars(Year),as.character)




LFS_Indicators_Y_R_F  <- LFS_Indicators_Y_R_F %>%
  mutate(EMPR_T = Total_Employment/Total_Participation)%>%
  mutate(EMPR_M = Male_Employment/Male_Participation)%>%
  mutate(EMPR_F  = Female_Employment / Female_Participation)%>%
  mutate(EMPR_U  = Urban_Employment / Urban_Participation)%>%
  mutate(EMPR_R  = Rural_Employment / Rural_Participation)%>%
  mutate(UMPR_T = Total_Unemployment/Total_Participation)%>%
  mutate(UMPR_M = Male_Unemployment/Male_Participation)%>%
  mutate(UMPR_F  = Female_Unemployment / Female_Participation)%>%
  mutate(UMPR_U  = Urban_Unemployment / Urban_Participation)%>%
  mutate(UMPR_R  = Rural_Unemployment / Rural_Participation)%>%
  filter(Year %in% c(85:95))%>%
  mutate_at(vars(Year),as.character)




colors <- c("Calculated" = "black","Reported" = "red","Adjusted" = "blue")
linetype  <-  c("Calculated" = 1, "Reported" = 2 , "Adjusted" = 4 )
shapes <- c("Calculated" = 15, "Reported" = 16 , "Adjusted" = 17 )

##########################Yearly(Abstract vs calculated)########################
for (i in colnames(Adj_LFS_Indicators_Y_C)[2:16]) {
  ggplot(mapping = aes(x = Year ,y = eval(parse(text = i))/1000000,group =1 ))+
    geom_line(data = LFS_Indicators_Y_C,aes(color = "Calculated",lty ="Calculated"))+
    geom_point(data = LFS_Indicators_Y_C,aes(color = "Calculated",shape ="Calculated") )+
    geom_line(data = Adj_LFS_Indicators_Y_C,aes(color = "Adjusted" , lty = "Adjusted") )+
    geom_point(data = Adj_LFS_Indicators_Y_C,aes(color = "Adjusted" , shape = "Adjusted"))+
    geom_line(data = LFS_Indicators_Y_R_F,aes(color= "Reported" ,lty = "Reported"))+
    geom_point(data = LFS_Indicators_Y_R_F,aes(color= "Reported" ,shape = "Reported"))+
    theme(
      legend.position = "bottom",
      legend.justification = "bottom",
      axis.text.x = element_text( face = "bold",color = "black", size = 10),
      plot.title = element_text(hjust = 0.5))+
    labs( color = "Legend")+
    guides(x = guide_axis(angle = 90))+
    scale_color_manual("",values = colors)+
    scale_linetype_manual("",values = linetype)+
    scale_shape_manual("",values = shapes)+
    ylab(i)+
    ggtitle("Calculated vs Reported vs Adjusted Indicators")
  ggsave(paste0(Files_paths,"/Primary Results/Adjusted_LFS_indicators/Yearly/",i,".png"),width=20, height=15,limitsize = FALSE,units = "cm")
}

for (i in colnames(Adj_LFS_Indicators_Y_C)[17:26]) {
  ggplot(mapping = aes(x = Year ,y = eval(parse(text = i)),group =1 ))+
    geom_line(data = LFS_Indicators_Y_C,aes(color = "Calculated",lty ="Calculated"))+
    geom_point(data = LFS_Indicators_Y_C,aes(color = "Calculated",shape ="Calculated") )+
    geom_line(data = Adj_LFS_Indicators_Y_C,aes(color = "Adjusted" , lty = "Adjusted") )+
    geom_point(data = Adj_LFS_Indicators_Y_C,aes(color = "Adjusted" , shape = "Adjusted"))+
    geom_line(data = LFS_Indicators_Y_R_F,aes(color= "Reported" ,lty = "Reported"))+
    geom_point(data = LFS_Indicators_Y_R_F,aes(color= "Reported" ,shape = "Reported"))+
    theme(
      legend.position = "bottom",
      legend.justification = "bottom",
      axis.text.x = element_text( face = "bold",color = "black", size = 10),
      plot.title = element_text(hjust = 0.5))+
    labs( color = "Legend")+
    guides(x = guide_axis(angle = 90))+
    scale_color_manual("",values = colors)+
    scale_linetype_manual("",values = linetype)+
    scale_shape_manual("",values = shapes)+
    ylab(i)+
    ggtitle("Calculated vs Reported vs Adjusted Indicators")
  ggsave(paste0(Files_paths,"/Primary Results/Adjusted_LFS_indicators/Yearly/",i,".png"),width=20, height=15,limitsize = FALSE,units = "cm")
}
