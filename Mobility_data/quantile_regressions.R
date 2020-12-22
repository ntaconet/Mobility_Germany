
Person_dataset<-read.csv("Output/Person_dataset.csv")

# making sure variable types are correct

#which months have a higher?

Person_dataset<-Person_dataset%>%
  mutate(P_GEW_num=as.numeric(gsub(",",".",as.character(P_GEW))))%>% # weights should be numeric
  filter(alter_gr2 %in% c(3:9)) # remove ppl below 20
         #relevel(ST_MONAT,ref=10)) #  

#dir.create("Descriptive_graphs")


# first let's perform quantile regression for the main variables
#Independant_variable<-"emissions_RL"
Independant_variable<-"Total_emissions_wout_RW"

#suffix<-"without_emissions_commute"

################
# Import variable table ----
################

# Import a table, which contains the different variables to be used in the regression 
# In the table, variables are classified, and values to remove.
table_variables<-read_excel(paste("Other_input/Table_variables_",Independant_variable,".xlsx",sep=""))

################
# Perform quantile regressions ----
################

Weights<-"P_GEW_num"

# Choose the dependent variables
Variables_tokeep<-subset(table_variables,type %in% c("main","control","attitude","accessibility","other"))
#Variables_tokeep<-subset(table_variables,type %in% c("main","control","accessibility","other"))

Dependant_variables<-Variables_tokeep$label

Regression_dataset<-Person_dataset%>%
  select(Independant_variable,Variables_tokeep$varname,Weights)%>%
  na.omit()

Regression_dataset<-Regression_dataset%>%
  rename_at(vars(as.character(Variables_tokeep$varname)),~as.character(Variables_tokeep$label))


# Remove answers corresponding to "No answer"
for (i in 1:nrow(Variables_tokeep)){
  print(Variables_tokeep$varname[i])
  
  Regression_dataset<-Regression_dataset%>%
    filter(get(Variables_tokeep$label[i])<Variables_tokeep$remove_above[i])
}

Regression_dataset<-Regression_dataset%>%
  mutate(Month=factor(Month),
         Age=factor(Age))



Regression_Quantile<-rq(paste(Independant_variable,"~",paste(Dependant_variables,collapse=" + ")),
                        data=Regression_dataset,
                        tau=c(0.25,0.5,0.75,0.9),
                        weights=P_GEW_num)

# compare to OLS
Regression_OLS<-lm(paste(Independant_variable,"~",paste(Dependant_variables,collapse=" + ")),
                   data=Regression_dataset,
                   weights=P_GEW_num)

Regression_OLS_2<-lm(paste(Independant_variable,"~",paste(Dependant_variables,collapse=" + ")),
                   data=Regression_dataset,
                   weights=P_GEW_num)


Regression_Quantile_tau90<-rq(paste(Independant_variable,"~",paste(Dependant_variables,collapse=" + ")),
                              data=Regression_dataset,
                              tau=c(0.9),
                              weights=P_GEW_num)

Regression_Quantile_tau75<-rq(paste(Independant_variable,"~",paste(Dependant_variables,collapse=" + ")),
                              data=Regression_dataset,
                              tau=c(0.75),
                              weights=P_GEW_num)


Regression_Quantile_tau50<-rq(paste(Independant_variable,"~",paste(Dependant_variables,collapse=" + ")),
                              data=Regression_dataset,
                              tau=c(0.5),
                              weights=P_GEW_num)

#Regression_Quantile

#summary(Regression_Quantile,se="ker")
#summary(Regression_OLS)

export_summs( Regression_OLS,Regression_Quantile_tau90,Regression_Quantile_tau75,Regression_Quantile_tau50, 
              model.names = c("OLS","Quantile (50%)","Quantile (75%)","Quantile (90%)"),
              #to.file = "docx", file.name = paste("Output_regressions/",as.character(Independant_variable),"_Comparing_OLS_to_Quantile.docx"),sep="")
              to.file = "docx", file.name = paste("Output_regressions/",as.character(Independant_variable),"_without","_Comparing_OLS_to_Quantile.docx"),sep="")
  

###########
# Quantile regression on total emissions
###########