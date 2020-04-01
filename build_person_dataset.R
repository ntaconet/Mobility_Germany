# Import person emissions
Person_emissions<-read.csv("Output/Person_emissions.csv")

# Import survey's data at the person's level
Personen<-read.csv("CSV/MiD2017_Personen.csv",sep=";",fileEncoding="UTF-8-BOM")

Person_dataset<-merge(Person_emissions,Personen,by="HP_ID",all=T)

# Correct emissions by the real number of Reise/Wege
Person_dataset<-Person_dataset%>%
  # if 0 reise, then 0 emissions 
  # otherwise, real nb of reise is P_ANZREISE
  mutate_at(.vars=vars(emissions_reise,emissions_RW,emissions_RL,emissions_flugzeug),
            .funs=list(~ifelse(P_ANZREISE!=0,.*P_ANZREISE/nb_reported_reisen,0)))%>%
  mutate_at(.vars=vars(emissions_wege,emissions_WL,emissions_WE,emissions_WW),
            .funs=list(~ifelse(W_ANZWW!=0 & (W_ANZWW %in% c(99,801,802,803,804,807,808))==F,.*(nb_reported_wege+W_ANZWW)/nb_reported_wege,.)))%>%
  # if no mobility am stichtag return 0
  mutate_at(.vars=vars(emissions_wege,emissions_WL,emissions_WE,emissions_WW),
          .funs=list(~ifelse(mobil==0,0,.)))

write.csv(Person_dataset,"Output/Person_dataset.csv",row.names = F)