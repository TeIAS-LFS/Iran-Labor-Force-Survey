rm(list=ls())

Age10_Pop_C <- readRDS("C:/Users/ali/Dropbox/LFS Cleaning/GitHub/R Codes/Consistency/Age10_Pop_C.RDS")
Total_Pop_C <- readRDS("C:/Users/ali/Dropbox/LFS Cleaning/GitHub/R Codes/Consistency/Total_Pop_C.RDS")

Age10_Pop_R <- read_excel("C:/Users/ali/Dropbox/LFS Cleaning/GitHub/Files/Pop_Reported_Indicatores.xlsx" , sheet = "Age10_Population")
Total_Pop_R <- read_excel("C:/Users/ali/Dropbox/LFS Cleaning/GitHub/Files/Pop_Reported_Indicatores.xlsx" , sheet = "Total_Population")




Demog <- function(Demografic_var) {
  
  colors <- c("Calculated" = "black","Reported" = "red")
  linetypes<- c("Calculated" = 2,"Reported" = 4)
  shapes <- c("Calculated" = 15,"Reported" = 17)
  
  P<- ggplot(mapping = aes(x = Year ,y = eval(parse(text = Demografic_var))/1000000 ,group =1 ))+
      geom_line(data = Total_Pop_C ,aes(color = "Calculated",lty = "Calculated") )+
      geom_point(data = Total_Pop_C ,aes(color = "Calculated",shape = "Calculated"))+
      geom_line(data = Total_Pop_R,aes(color="Reported",lty ="Reported" ))+
      geom_point(data = Total_Pop_R,aes(color="Reported",shape = "Reported"))+ 
      theme(
        legend.position = "bottom",
        legend.justification = "bottom",
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        axis.text.x = element_text( face = "bold",color = "black", size = 10),
        plot.title = element_text(hjust = 0.5))+
      labs( color = "Legend")+
      guides(x = guide_axis(angle = 90))+
      scale_color_manual("",values = colors)+
      scale_linetype_manual("",values = linetypes)+
      scale_shape_manual("",values = shapes)+
      ylab(paste0(Demografic_var,"(Million)"))
  return(P)
  
}
Demog("Male_T")

