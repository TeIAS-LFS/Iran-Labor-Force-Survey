rm(list=ls())
W3_T <- readRDS("F:/LFS/Code/Consistency/W3_T.RDS")

Mean_Age <- W3_T %>%
  group_by(IID) %>%
  dplyr::summarise(M_Age = mean(Age))

W3_T <- left_join(W3_T,Mean_Age,key="IID")

D <- W3_T%>%
  group_by(IID)%>%
  dplyr::summarise(n=n())


One_Iteration <- W3_T%>%
  group_by(IID)%>%
  dplyr::summarise(n=n())%>%
  filter(n==1)

Two_Iteration <- W3_T%>%
  group_by(IID)%>%
  dplyr::summarise(n=n())%>%
  filter(n==2)

Three_Iteration <-  W3_T%>%
  group_by(IID)%>%
  dplyr::summarise(n=n())%>%
  filter(n==3)

Four_Iteration <- W3_T%>%
  group_by(IID)%>%
  dplyr::summarise(n=n())%>%
  filter(n==4)

A <- W3_T%>%
  filter(IID %in% Two_Iteration$IID)%>%
  group_by(IID,Gender,Relation)%>%
  dplyr::summarise(n=n())%>%
  filter(n==1)

B <- W3_T%>%
  filter(IID %in% Three_Iteration$IID)%>%
  group_by(IID,Gender,Relation)%>%
  dplyr::summarise(n=n())%>%
  filter(n==1|n==2)

C <- W3_T%>%
  filter(IID %in% Four_Iteration$IID)%>%
  group_by(IID,Gender,Relation)%>%
  dplyr::summarise(n=n())%>%
  filter(n==1|n==2|n==3)

W3_T_F<- W3_T%>%
  filter(IID %in% A$IID |IID %in% B$IID|IID %in% C$IID|abs(Age-M_Age)>2)%>%
  arrange(IID)




saveRDS(W3_T_F,"F:/LFS/Code/Consistency/W3_T_F.RDS")


duplicated(B)|duplicated(B,fromLast = TRUE)



