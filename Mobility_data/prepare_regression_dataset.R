
# first let's choose independant variable and.

# emissions_RL stands for emissions from Reise Leisure
#Independant_variable<-"emissions_RL"

# If we want to look at total emissions wout Reise for work?
#Independant_variable<-"Total_emissions_wout_RW"

# emissions for everyday life outside leisure:
#Independant_variable<-"emissions_wege"

#If we want emissions from flugzeug
Independant_variable<-"emissions_flugzeug"

#suffix<-"without_emissions_commute"

################
# Import variable table ----
################

regression_type<-"basic"


# Whether to add control or not.
add_control_bool<-FALSE

add_control<-c("P_NUTZ_RAD","P_NUTZ_OPNV")
#add_control<-"emissions_wege"
#add_control<-c("P_NUTZ_RAD","P_NUTZ_OPNV","P_NUTZ_AUTO")
#add_control<-c(add_control,"P_EINVM_RAD","P_EINVM_AUTO","P_EINVM_OPNV","P_EINVM_FUSS")
#add_control<-c("P_ZUF_RAD","P_ZUF_AUTO","P_ZUF_OPNV","P_ZUF_FUSS")

suffix_add_control<-""
if (add_control_bool==T){
  #suffix_add_control<-add_control
  suffix_add_control<-"compensation"
}



Person_dataset<-read.csv("Output/Person_dataset.csv")


# making sure variable types are correct

Person_dataset<-Person_dataset%>%
  mutate(P_GEW_num=as.numeric(gsub(",",".",as.character(P_GEW))))%>% # weights should be numeric
  filter(alter_gr1 %in% c(5:9))# remove ppl below 18 years old
         #relevel(ST_MONAT,ref=10)) #  

# Lorenz curve ----

# 
data_lorenzcurve<-subset(Person_dataset,is.na(Total_emissions_wout_RW)==F)

#lorenz.curve(data=data_lorenzcurve[1:100,c("Total_emissions_wout_RW","P_GEW_num")])

plot(Lc(data_lorenzcurve$Total_emissions_wout_RW,n=data_lorenzcurve$P_GEW_num))

plot(Lc(data_lorenzcurve$emissions_reise,n=data_lorenzcurve$P_GEW_num))



unique(Person_dataset$ST_MONAT_grouped)

# Import a table, which contains the different variables to be used in the regression 
# In the table, variables are classified, and each line specifies values to remove (corresponding to no Answer, NA...).
table_variables<-read_excel(paste("Other_input/Table_Dependent_Variables_",regression_type,".xlsx",sep=""))

################
# Perform quantile regressions ----
################

Weights<-"P_GEW_num"

# Choose the dependent variables
Variables_tokeep<-subset(table_variables,type %in% c("main","control","attitude","accessibility","other") & include==1 | (add_control_bool==T & varname %in% add_control))
#Variables_tokeep<-subset(table_variables,type %in% c("main","control","accessibility","other"))


# Change the values for some
if (FALSE){
for (var0 in Variables_tokeep$change){
  legend_answers<-read_excel(paste("Other_input/legend_",varname[i],".xlsx",sep=""),col_names=c("Code","Meaning"))
  
  # Create variable containing meaning of answer
  Person_withoutoutliers<-Person_dataset%>%
    mutate(answer_meaning=factor(legend_answers$Meaning[match(get(varname),legend_answers$Code)],levels=legend_answers$Meaning))
  
}
}

Dependant_variables<-Variables_tokeep$label

Regression_dataset<-Person_dataset%>%
  select(all_of(Independant_variable), 
         Variables_tokeep$varname,
         Weights)%>%
  na.omit()

Regression_dataset<-Regression_dataset%>%
  rename_at(vars(as.character(Variables_tokeep$varname)),~as.character(Variables_tokeep$label))


# Remove answers corresponding to "No answer"
for (i in 1:nrow(Variables_tokeep)){
  print(Variables_tokeep$varname[i])
  Regression_dataset<-Regression_dataset%>%
    filter(get(Variables_tokeep$label[i])<Variables_tokeep$remove_above[i])
}

# Change a few labels:

Income_new_levels<-c("Less than 900e/m","900-2000e/m","2000-4000e/m","4000-6000e/m",">6000e/m")

Regression_dataset<-Regression_dataset%>%
  mutate(Income=case_when(Income %in% c(1,2) ~ Income_new_levels[1],
                          Income %in% c(3,4) ~ Income_new_levels[2],
                          Income %in% c(5,6) ~ Income_new_levels[3],
                          Income %in% c(7,8) ~ Income_new_levels[4],
                          Income %in% c(9,10) ~ Income_new_levels[5]))%>%
  mutate(Income=factor(Income,levels=Income_new_levels))

Education_new_levels<-c("Primary","Secondary","Tertiary")

Regression_dataset<-Regression_dataset%>%
  mutate(Education=case_when(Education %in% c(1,2,3) ~ Education_new_levels[1],
                             Education %in% c(4) ~ Education_new_levels[2],
                             Education %in% c(5,6) ~ Education_new_levels[3]))%>%
  mutate(Education=factor(Education,levels=Education_new_levels))


Location_new_levels<-c("Small city","Urban environment","Middle city","Big city","Metropole")

Regression_dataset<-Regression_dataset%>%
  mutate(Location=case_when(Location %in% c(55) ~ Location_new_levels[1],
                            Location %in% c(54) ~ Location_new_levels[2],
                            Location %in% c(53) ~ Location_new_levels[3],
                            Location %in% c(52) ~ Location_new_levels[4],
                            Location %in% c(51) ~ Location_new_levels[5]))%>%
  mutate(Location=factor(Location,levels=Location_new_levels))


HH_compo_new_levels<-Location_new_levels<-c("1 member","2 members","3 members","4+ members")

Regression_dataset<-Regression_dataset%>%
  mutate(HH_composition=case_when(HH_composition %in% c(1) ~ HH_compo_new_levels[1],
                            HH_composition %in% c(2) ~ HH_compo_new_levels[2],
                            HH_composition %in% c(3) ~ HH_compo_new_levels[3],
                            HH_composition %in% c(4,5) ~ HH_compo_new_levels[4]))%>%
  mutate(HH_composition=factor(HH_composition,levels=HH_compo_new_levels))

Enjoyment_levels<-data.frame(value=c("Strongly disagree","Disagree","Agree","Fully agree"),code=c("4","3","2","1"))

for (var0 in c("Enjoy_Biking","Enjoy_Car","Enjoy_PublicTransport")){
  Regression_dataset[var0][,1]<-Enjoyment_levels$value[match(Regression_dataset[var0][,1],Enjoyment_levels$code)]
  Regression_dataset[var0][,1]<-factor(Regression_dataset[var0][,1],levels=Enjoyment_levels$value)
}

if ("Frequency_Bike" %in% Dependant_variables){
  
  for (var0 in Dependant_variables[substr(Dependant_variables,1,4)=="Freq"])
  # rename levels: from 1 (daily) to 5 (almost never)
  Use_levels<-data.frame(value=c("Almost never","Less than monthly","1-3x by month","1-3x by week","Daily"),code=c("5","4","3","2","1"))
  
  Regression_dataset[var0][,1]<-Use_levels$value[match(Regression_dataset[var0][,1],Use_levels$code)]
  Regression_dataset[var0][,1]<-factor(Regression_dataset[var0][,1],levels=Use_levels$value)
  
}

Gender_levels<-c("Man","Woman")

Regression_dataset<-Regression_dataset%>%
  mutate(Gender=case_when(Gender %in% c(1) ~ Gender_levels[1],
                          Gender %in% c(2) ~ Gender_levels[2]))%>%
  mutate(Gender=factor(Gender,levels=Gender_levels))
                            
Car_ownership_levels<-c("No","Yes")

Regression_dataset<-Regression_dataset%>%
  mutate(Car_ownership=case_when(Car_ownership %in% c(0) ~ Car_ownership_levels[1],
                                 Car_ownership %in% c(1) ~ Car_ownership_levels[2]))%>%
  mutate(Car_ownership=factor(Car_ownership,levels=Car_ownership_levels))


Second_home_levels<-c("No","Yes")

Regression_dataset<-Regression_dataset%>%
  mutate(Second_Home=case_when(Second_Home %in% c(2) ~ Second_home_levels[1],
                               Second_Home %in% c(1) ~ Second_home_levels[2]))%>%
  mutate(Second_Home=factor(Second_Home,levels=Second_home_levels))


Car_sharing_levels<-c("No","Yes")

Regression_dataset<-Regression_dataset%>%
  mutate(Car_sharing=case_when(Car_sharing %in% c(3) ~ Car_sharing_levels[1],
                               Car_sharing %in% c(1,2) ~ Car_sharing_levels[2]))%>%
  mutate(Car_sharing=factor(Car_sharing,levels=Car_sharing_levels))

Employment_levels<-c("No","Yes")

Regression_dataset<-Regression_dataset%>%
  mutate(Employment=case_when(Employment %in% c(2) ~ Employment_levels[1],
                              Employment %in% c(1) ~ Employment_levels[2]))%>%
  mutate(Employment=factor(Employment,levels=Employment_levels))

Age_levels<-c("18-24","25-44","45-59","60-64","65+")

Regression_dataset<-Regression_dataset%>%
  mutate(Age=case_when(Age %in% c(5) ~ Age_levels[1],
                       Age %in% c(6) ~ Age_levels[2],
                       Age %in% c(7) ~ Age_levels[3],
                       Age %in% c(8) ~ Age_levels[4],
                       Age %in% c(9) ~ Age_levels[5]))%>%
  mutate(Age=factor(Age,levels=Age_levels))


write.csv(Regression_dataset,"Regression_dataset.csv",row.names = F)

# Descriptive graph for days:

if (FALSE){
ggplot(data=Regression_dataset,aes(x=factor(ST_WOTAG),y=emissions_wege/emissions_RL))+
  geom_boxplot(outlier.shape=NA)+
  coord_cartesian(ylim=c(0,30))

ggplot(data=Regression_dataset,aes(x=factor(ST_WOTAG),y=emissions_wege))+
  geom_boxplot(outlier.shape=NA)+
  coord_cartesian(ylim=c(0,6000))

ggplot(data=Regression_dataset,aes(x=factor(ST_MONAT),y=emissions_reise))+
  geom_boxplot(outlier.shape=NA)+
  coord_cartesian(ylim=c(0,10**3))
}

#dir.create("Descriptive_graphs")
if (FALSE){
Regression_dataset<-Regression_dataset%>%
  mutate(Month=case_when(Month %in% c(1,2,3,4) ~ "1_low",
                            Month %in% c(5,6,7,8,12) ~ "2_medium",
                            Month %in% c(9,10,11) ~ "3_high"))%>%
  mutate(Month=factor(Month))%>%
  mutate(Weekday=case_when(Weekday %in% c(1,2,3,4,5) ~ "1_weekday",
                            Weekday %in% c(6) ~ "2_Saturday",
                            Weekday %in% c(7) ~ "3_Sunday"))%>%
  mutate(Weekday=factor(Weekday))

}

#Regression_dataset<-Regression_dataset%>%
  #mutate_if(sapply(Regression_dataset, is.integer), as.factor)#%>%
  #mutate_if(sapply(Regression_dataset, is.character), as.factor)


#summary(subset(Regression_dataset,select=-c(P_GEW_num)),percent="column")
#stargazer(Regression_dataset)

if (FALSE){
Regression_Quantile<-rq(paste(Independant_variable,"~",paste(Dependant_variables,collapse=" + ")),
                        data=Regression_dataset,
                        tau=c(0.25,0.5,0.75,0.9),
                        weights=P_GEW_num)


Regression_Quantile
}