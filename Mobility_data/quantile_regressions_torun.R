dir.create("Output_regressions")

Regression_dataset<-read.csv("Regression_dataset.csv")

Regression_dataset<-Regression_dataset%>%
  mutate(P_GEW_num=as.numeric(gsub(",",".",as.character(P_GEW)))) # weights should be numeric

#Independant_variables_torun<-c("emissions_RL","Total_emissions_wout_RW","emissions_wege")

# to specify a regression to run, need to add

Table_regressions_to_Run<-read_xlsx("Other_input/Table_Regressions_to_Run.xlsx")

for (reg_no in 1:nrow(Table_regressions_to_Run)){
#for (reg_no in 1:1){
  Independant_variable<-Table_regressions_to_Run$Independant_variable[reg_no]
  
  # Whether to add control or not.
  add_control_bool<-as.logical(Table_regressions_to_Run$add_control_bool[reg_no])
  add_control<-Table_regressions_to_Run$add_control[reg_no]
  
  add_control<-unlist(strsplit(add_control,","))
  
  Name_Regression<-Table_regressions_to_Run$Name_Regression[reg_no]
  
  Title_Table<-Table_regressions_to_Run$Title_Table[reg_no]
  
  # In the table, variables are classified, and each line specifies values to remove (corresponding to no Answer, NA...).
  regression_type<-"basic"
  
  source("quantile_regressions.R")
  #add_control<-c("P_NUTZ_RAD","P_NUTZ_OPNV")
  
  #add_control<-"emissions_wege"
  #add_control<-c("P_NUTZ_RAD","P_NUTZ_OPNV","P_NUTZ_AUTO")
  #add_control<-c(add_control,"P_EINVM_RAD","P_EINVM_AUTO","P_EINVM_OPNV","P_EINVM_FUSS")
  #add_control<-c("P_ZUF_RAD","P_ZUF_AUTO","P_ZUF_OPNV","P_ZUF_FUSS")
  
  #suffix_add_control<-""
  #if (add_control_bool==T){
    #suffix_add_control<-add_control
   # suffix_add_control<-"compensation"
  #}
  
  
}

# add later: emissions from air travel

# first let's choose independant variable and

# emissions_RL stands for emissions from Reise Leisure
#Independant_variable<-"emissions_RL"

# If we want to look at total emissions wout Reise for work?
#Independant_variable<-"Total_emissions_wout_RW"

# emissions for everyday life outside leisure:
#Independant_variable<-"emissions_wege"

#If we want emissions from flugzeug
#Independant_variable<-"emissions_flugzeug"

#suffix<-"without_emissions_commute"





