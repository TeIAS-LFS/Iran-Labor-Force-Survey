
for (year in First_year:Last_year) {
  
  W3 <-  readRDS(paste0(Processd_data_Path,"/",year,"/W3.RDS"))
  
  if (year %in% 85:95) {
    
    W3$C_Sub_Pop <- NULL
    W3$C_Sub_Pop[W3$GENDER == "1" & W3$Rural == "1" & W3$AGE <=9] <- 0.25*sum(W3$IW_Yearly[W3$GENDER == "1" & W3$Rural == "1" & W3$AGE <=9],na.rm = true)
    W3$C_Sub_Pop[W3$GENDER == "2" & W3$Rural == "1" & W3$AGE <=9] <- 0.25*sum(W3$IW_Yearly[W3$GENDER == "2" & W3$Rural == "1" & W3$AGE <=9],na.rm = true)
    W3$C_Sub_Pop[W3$GENDER == "1" & W3$Rural == "2" & W3$AGE <=9] <- 0.25*sum(W3$IW_Yearly[W3$GENDER == "1" & W3$Rural == "2" & W3$AGE <=9],na.rm = true)
    W3$C_Sub_Pop[W3$GENDER == "2" & W3$Rural == "2" & W3$AGE <=9] <- 0.25*sum(W3$IW_Yearly[W3$GENDER == "2" & W3$Rural == "2" & W3$AGE <=9],na.rm = true)
    W3$C_Sub_Pop[W3$GENDER == "1" & W3$Rural == "1" & (W3$AGE >= 10 & W3$AGE <= 64)] <- 0.25*sum(W3$IW_Yearly[W3$GENDER == "1" & W3$Rural == "1" & (W3$AGE >= 10 & W3$AGE <= 64)],na.rm = true)
    W3$C_Sub_Pop[W3$GENDER == "2" & W3$Rural == "1" & (W3$AGE >= 10 & W3$AGE <= 64)] <- 0.25*sum(W3$IW_Yearly[W3$GENDER == '2' & W3$Rural == "1" & (W3$AGE >= 10 & W3$AGE <= 64)],na.rm = true)
    W3$C_Sub_Pop[W3$GENDER == "1" & W3$Rural == "2" & (W3$AGE >= 10 & W3$AGE <= 64)] <- 0.25*sum(W3$IW_Yearly[W3$GENDER == "1" & W3$Rural == "2" & (W3$AGE >= 10 & W3$AGE <= 64)],na.rm = true)
    W3$C_Sub_Pop[W3$GENDER == "2" & W3$Rural == "2" & (W3$AGE >= 10 & W3$AGE <= 64)] <- 0.25*sum(W3$IW_Yearly[W3$GENDER == "2" & W3$Rural == "2" & (W3$AGE >= 10 & W3$AGE <= 64)],na.rm = true)
    W3$C_Sub_Pop[W3$GENDER == "1" & W3$Rural == "1" & W3$AGE >=65] <- 0.25*sum(W3$IW_Yearly[W3$GENDER == "1" & W3$Rural == "1" & W3$AGE >=65],na.rm = true)
    W3$C_Sub_Pop[W3$GENDER == "2" & W3$Rural == "1" & W3$AGE >=65] <- 0.25*sum(W3$IW_Yearly[W3$GENDER == "2" & W3$Rural == "1" & W3$AGE >=65],na.rm = true)
    W3$C_Sub_Pop[W3$GENDER == "1" & W3$Rural == "2" & W3$AGE >=65] <- 0.25*sum(W3$IW_Yearly[W3$GENDER == "1" & W3$Rural == '2' & W3$AGE >=65],na.rm = true)
    W3$C_Sub_Pop[W3$GENDER == "2" & W3$Rural == "2" & W3$AGE >=65] <- 0.25*sum(W3$IW_Yearly[W3$GENDER == "2" & W3$Rural == "2" & W3$AGE >=65],na.rm = true)
    
    W3$R_Sub_Pop <- NULL
    W3$R_Sub_Pop[W3$GENDER == "1" & W3$Rural == "1" & W3$AGE <=9] <- Intercensal$Male_0_9_U[Intercensal$Year == year]
    W3$R_Sub_Pop[W3$GENDER == "2" & W3$Rural == "1" & W3$AGE <=9] <- Intercensal$Female_0_9_U[Intercensal$Year == year]
    W3$R_Sub_Pop[W3$GENDER == "1" & W3$Rural == "2" & W3$AGE <=9] <- Intercensal$Male_0_9_R[Intercensal$Year == year ]
    W3$R_Sub_Pop[W3$GENDER == "2" & W3$Rural == "2" & W3$AGE <=9] <- Intercensal$Female_0_9_R[Intercensal$Year == year ]
    W3$R_Sub_Pop[W3$GENDER == "1" & W3$Rural == "1" & (W3$AGE >= 10 & W3$AGE <= 64)] <- Intercensal$Male_10_64_U[Intercensal$Year== year]
    W3$R_Sub_Pop[W3$GENDER == "2" & W3$Rural == "1" & (W3$AGE >= 10 & W3$AGE <= 64)] <- Intercensal$Female_10_64_U[Intercensal$Year== year]
    W3$R_Sub_Pop[W3$GENDER == "1" & W3$Rural == "2" & (W3$AGE >= 10 & W3$AGE <= 64)] <- Intercensal$Male_10_64_R[Intercensal$Year== year]
    W3$R_Sub_Pop[W3$GENDER == "2" & W3$Rural == "2" & (W3$AGE >= 10 & W3$AGE <= 64)] <- Intercensal$Female_10_64_R[Intercensal$Year== year]
    W3$R_Sub_Pop[W3$GENDER == "1" & W3$Rural == "1" & W3$AGE >=65] <- Intercensal$Male_65_U[Intercensal$Year== year]
    W3$R_Sub_Pop[W3$GENDER == "2" & W3$Rural == "1" & W3$AGE >=65] <- Intercensal$Female_65_U[Intercensal$Year== year]
    W3$R_Sub_Pop[W3$GENDER == "1" & W3$Rural == "2" & W3$AGE >=65] <- Intercensal$Male_65_R[Intercensal$Year== year]
    W3$R_Sub_Pop[W3$GENDER == "2" & W3$Rural == "2" & W3$AGE >=65] <- Intercensal$Female_65_R[Intercensal$Year== year] 
    
    W3 <-  W3%>%
      mutate(Adj_IW_Yearly = (R_Sub_Pop/C_Sub_Pop)*IW_Yearly)%>%
      select(-AGE,-GENDER)
  }else{
    W3 <- W3%>%
      mutate(Adj_IW_Yearly = IW_Yearly)%>%
      select(-AGE,-GENDER)
  }

}

