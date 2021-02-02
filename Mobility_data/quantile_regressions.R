
# first let's choose independant variable and.

# emissions_RL stands for emissions from Reise Leisure
Independant_variable<-"emissions_RL"

# If we want to look at total emissions wout Reise for work?
#Independant_variable<-"Total_emissions_wout_RW"

# emissions for everyday life outside leisure:
#Independant_variable<-"emissions_wege"


#If we want emissions from flugzeug
#Independant_variable<-"emissions_flugzeug"

#suffix<-"without_emissions_commute"

################
# Import variable table ----
################

regression_type<-"basic"

add_control_bool<-TRUE

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

Regression_dataset<-Regression_dataset%>%
  mutate(Income=case_when(Income %in% c(1,2) ~ "1. Less than 900e/m",
                          Income %in% c(3,4) ~ "2. 900-2000e/m",
                          Income %in% c(5,6) ~ "3. 2000-4000e/m",
                          Income %in% c(7,8) ~ "4. 4000-6000e/m",
                          Income %in% c(9,10) ~ "5. >6000e/m"))


Regression_dataset<-Regression_dataset%>%
  mutate(Education=case_when(Education %in% c(1,2,3) ~ "1. Primary",
                             Education %in% c(4) ~ "2. Secondary",
                             Education %in% c(5,6) ~ "3. Tertiary"))

Regression_dataset<-Regression_dataset%>%
  mutate(Location=case_when(Location %in% c(55) ~ "1. Small city",
                            Location %in% c(54) ~ "2. Staedtischer Raum",
                            Location %in% c(53) ~ "3. Middle city",
                            Location %in% c(52) ~ "4. Big city",
                            Location %in% c(51) ~ "5. Metropole"))

# Descriptive graph for days:


ggplot(data=Regression_dataset,aes(x=factor(ST_WOTAG),y=emissions_wege/emissions_RL))+
  geom_boxplot(outlier.shape=NA)+
  coord_cartesian(ylim=c(0,30))

ggplot(data=Regression_dataset,aes(x=factor(ST_WOTAG),y=emissions_wege))+
  geom_boxplot(outlier.shape=NA)+
  coord_cartesian(ylim=c(0,6000))

ggplot(data=Regression_dataset,aes(x=factor(ST_MONAT),y=emissions_reise))+
  geom_boxplot(outlier.shape=NA)+
  coord_cartesian(ylim=c(0,10**3))


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


Regression_dataset<-Regression_dataset%>%
  mutate(#Month=factor(Month),
         Age=factor(Age),
         Income=factor(Income),
         Education=factor(Education),
         Household_compo=factor(Household_compo),
         Location=factor(Location),
         Employment=factor(Employment),
         Gender=factor(Gender))
#         Income=factor(Income))




if (FALSE){
Regression_Quantile<-rq(paste(Independant_variable,"~",paste(Dependant_variables,collapse=" + ")),
                        data=Regression_dataset,
                        tau=c(0.25,0.5,0.75,0.9),
                        weights=P_GEW_num)


Regression_Quantile

}
# compare to OLS
Regression_OLS<-lm(paste(Independant_variable,"~",paste(Dependant_variables,collapse=" + ")),
                   data=Regression_dataset,
                   weights=P_GEW_num)


#Regression_Quantile


Regression_Quantile_tau90<-rq(paste(Independant_variable,"~",paste(Dependant_variables,collapse=" + ")),
                              data=Regression_dataset,
                              tau=c(0.9),
                              weights=P_GEW_num)

#summary.rq(Regression_Quantile_tau90)

Regression_Quantile_tau75<-rq(paste(Independant_variable,"~",paste(Dependant_variables,collapse=" + ")),
                              data=Regression_dataset,
                              tau=c(0.75),
                              weights=P_GEW_num)


Regression_Quantile_tau50<-rq(paste(Independant_variable,"~",paste(Dependant_variables,collapse=" + ")),
                              data=Regression_dataset,
                              tau=c(0.5),
                              weights=P_GEW_num)


Regression_OLS

export_summs(Regression_OLS,Regression_Quantile_tau50,Regression_Quantile_tau75,Regression_Quantile_tau90, 
              model.names = c("OLS","Quantile (50%)","Quantile (75%)","Quantile (90%)"),
              to.file = "docx", file.name = paste("Output_regressions/",as.character(Independant_variable),"_",regression_type,"_",suffix_add_control,".docx"),sep="")
              #to.file = "docx", file.name = paste("Output_regressions/",as.character(Independant_variable),"_without","_Comparing_OLS_to_Quantile.docx"),sep="")



# Let's try to look at the effect of not having a car:
# Create a variable that does that.



Regression_OLS<-lm(paste(Independant_variable,"~",paste(Dependant_variables,collapse=" + ")),
                   data=Regression_dataset,
                   weights=P_GEW_num)






#if (FALSE){

stargazer(Regression_OLS,Regression_Quantile_tau50,Regression_Quantile_tau75,Regression_Quantile_tau90, 
          intercept.bottom = FALSE,
          digits=0,
          rq.se="nid",
          #ci=TRUE,
          #ci.level=0.90,
          style = "aer",
          title="Quantile Regression Results",
          align=TRUE,
          dep.var.caption  = "",
          model.numbers = TRUE
          ,  t.auto = TRUE, p.auto = TRUE,
          font.size="scriptsize", keep.stat="aic",
          dep.var.labels   = "",
          multicolumn = TRUE,
          single.row = TRUE,
          #,omit.stat=c("f", "ser"),
          column.sep.width = "0.5pt",
          dep.var.labels.include = TRUE)


#}


#Regression_Quantile

#summary(Regression_Quantile,se="ker")
#summary(Regression_OLS)

# this doesn't work: do not use
#export_summs( Regression_OLS,Regression_Quantile, 
#              model.names = c("OLS","Quantile"),# (50%)","Quantile (75%)","Quantile (90%)"),
#              #to.file = "docx", file.name = paste("Output_regressions/",as.character(Independant_variable),"_Comparing_OLS_to_Quantile.docx"),sep="")
#              to.file = "docx", file.name = paste("Output_regressions/",as.character(Independant_variable),"_without","_Comparing_OLS_to_Quantile.docx"),sep="")


###########
# Quantile regression on total emissions
###########