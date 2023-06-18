# Import person emissions
Person_emissions<-read.csv("Output/Person_emissions.csv")

sum(!is.na(Person_emissions$emissions_RL))

# Import survey's data at the person's level
Personen<-read.csv("CSV/MiD2017_Personen.csv",sep=";",fileEncoding="UTF-8-BOM")

#Person_dataset<-merge(Person_emissions,Personen,by="HP_ID",all=T)

Person_dataset<-Person_emissions%>%
  full_join(Personen)

# Correct emissions by the real number of Reise/Wege
Person_dataset<-Person_dataset%>%
  # if 0 reise, then 0 emissions 
  # otherwise, real nb of reise is P_ANZREISE
  mutate_at(.vars=vars(emissions_reise,emissions_RW,emissions_RL,emissions_flugzeug),
            .funs=list(~ifelse(P_ANZREISE!=0,.*P_ANZREISE/nb_reported_reisen,0)))%>%
  mutate_at(.vars=vars(emissions_wege,emissions_WL,emissions_WE,emissions_WW),
            .funs=list(~ifelse(W_ANZWW!=0 & (W_ANZWW %in% c(99,801,802,803,804,807,808))==F,.*(nb_reported_wege+W_ANZWW)/nb_reported_wege,.)))%>%
  # if no mobility am stichtag return 0
  mutate_at(.vars=vars(emissions_wege,emissions_WL,emissions_WE,emissions_WW,emissions_WC),
          .funs=list(~ifelse(mobil==0,0,.)))


# Adjust emissions for Reisen and Wege
# emissions from wege should be multiplied by 365 (?)
factor_wege<-365
# emissions from Reisen should be multiplied by 4 (last three months)
factor_reisen<-4

Person_dataset<-Person_dataset%>%
  mutate(emissions_reise=emissions_reise*factor_reisen,
         emissions_RW=emissions_RW*factor_reisen,
         emissions_RL=emissions_RL*factor_reisen,
         emissions_flugzeug=emissions_flugzeug*factor_reisen,
         emissions_flugzeug_RL=emissions_flugzeug_RL*factor_reisen)%>%
  mutate(emissions_wege=emissions_wege*factor_wege,
         emissions_WL=emissions_WL*factor_wege,
         emissions_WE=emissions_WE*factor_wege,
         emissions_WW=emissions_WW*factor_wege)

sum(!is.na(Person_dataset$emissions_RL))

# currently: unit is gCO2, so we convert into kgCO2e
conversion_CO2_unit<-10**3

divide_by_conversion_factor<-function(x_emissions){
  return(x_emissions/conversion_CO2_unit)
}

Person_dataset<-Person_dataset%>%
  mutate_at(vars(contains("emissions")),divide_by_conversion_factor)


Person_dataset<-Person_dataset%>%
  mutate(Total_emissions=emissions_wege+emissions_reise,
         
         # exclude Reisen Work
         Total_emissions_wout_work=emissions_wege-emissions_WW+(emissions_reise-emissions_RW),
         emissions_wege_wout_work=emissions_wege-emissions_WW,
         emissions_flugzeug_wout_work=emissions_flugzeug_RL)

# Making the same thing with the question "Verkehrsmittel auf regelm berufl Wegen am Stichtag"
# Create a dummy variable that says if you take your bike for commute
Person_dataset<-Person_dataset%>%
  mutate(bike_to_work=NA)%>%
  mutate(bike_to_work=ifelse(P_RBW_VM %in% c(2,3),1,bike_to_work))%>%
  mutate(bike_to_work=ifelse(is.na(P_RBW_VM)==F & (P_RBW_VM %in% c(2,3))==F,0,bike_to_work))

#sum(!is.na(Person_dataset$Total_emissions_wout_RW))

write.csv(Person_dataset,"Output/Person_dataset.csv",row.names = F)

