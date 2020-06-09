
Person_dataset<-read.csv("Output/Person_dataset.csv")

# making sure variable types are correct

#which months have a higher?

Person_dataset<-Person_dataset%>%
  mutate(P_GEW_num=as.numeric(gsub(",",".",as.character(P_GEW))), # weights should be numeric
         ST_MONAT=factor(ST_MONAT),
         alter_gr2=factor(alter_gr2))%>%
  filter(alter_gr2 %in% c(3:9)) # remove ppl below 20
         #relevel(ST_MONAT,ref=10)) #  

        
#dir.create("Descriptive_graphs")

################
# Import variable table ----
################

table_variables<-read_excel("Other_input/Table_variables.xlsx")

################
# Perform quantile regressions ----
################

# first let's perform quantile regression for the main variables

Independant_variable<-"emissions_RL"

# Choose the dependent variables
Variables_tokeep<-subset(table_variables,type %in% c("main","control","attitude","accessibility","other"))

Regression_dataset<-Person_dataset

# Remove answers corresponding to "No answer"
for (i in nrow(Variables_tokeep)){
  # except for MONTHS, which is a factor
  if ((class(Variables_tokeep$varname[i])=="factor")==F){
    Regression_dataset<-Regression_dataset%>%
      filter(get(Variables_tokeep$varname[i])<Variables_tokeep$remove_above[i])

  }
}

Dependant_variables<-Variables_tokeep$varname


Regression_Quantile<-rq(paste(Independant_variable,"~",paste(Dependant_variables,collapse=" + ")),
                        data=Person_dataset,
                        tau=c(0.25,0.5,0.75,0.9))

# compare to OLS
Regression_OLS<-lm(paste(Independant_variable,"~",paste(Dependant_variables,collapse=" + ")),
                   data=Person_dataset)


Regression_Quantile
#summary(Regression_Quantile,se="ker")

summary(Regression_OLS)


