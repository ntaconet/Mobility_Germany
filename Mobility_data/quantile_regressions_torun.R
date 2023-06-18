dir.create("Output_regressions")

# Regressions -----

#Regression_dataset<-read.csv("Regression_dataset.csv")

#Regression_dataset<-Regression_dataset_wholesample
#Regression_dataset<-Regression_dataset_NoNA




#ggplot(data=Regression_dataset,aes(x=Total_emissions_wout_RW))+
 # geom_density()

# Dependent_variables_torun<-c("emissions_RL","Total_emissions_wout_RW","emissions_wege")

# to specify a regression to run, need to add

Table_regressions_to_Run<-read_xlsx("Other_input/Table_Regressions_to_Run.xlsx")

#for (reg_no in 1:1){
for (reg_no in nrow(Table_regressions_to_Run)){
#for (reg_no in 1:nrow(Table_regressions_to_Run)){
  
  if (Table_regressions_to_Run$To_run[reg_no]==1){
  
    Regression_dataset<-NULL
    
    #Regression_dataset<-read.csv(paste("Output/Regression_dataset_",Table_regressions_to_Run$Data[reg_no],".csv",sep=""))
    
    Regression_dataset<-get(paste("Regression_dataset_",Table_regressions_to_Run$Data[reg_no],sep=""))
    
    Regression_dataset<-Regression_dataset%>%
      mutate(P_GEW_num=as.numeric(gsub(",",".",as.character(P_GEW)))) # weights should be numeric
    
    Dependent_variable<-Table_regressions_to_Run$Dependent_variable[reg_no]
    
    # Whether to add control or not.
    add_control_bool<-as.logical(Table_regressions_to_Run$add_control_bool[reg_no])
    add_control<-Table_regressions_to_Run$add_control[reg_no]
    
    add_control<-unlist(strsplit(add_control,","))
    
    Name_Regression<-Table_regressions_to_Run$Name_Regression[reg_no]
    #Name_Regression<-"Fullsample"
    
    Title_Table<-Table_regressions_to_Run$Title_Table[reg_no]
    
    # In the table, variables are classified, and each line specifies values to remove (corresponding to no Answer, NA...).
    regression_type<-"basic"
    
    table_variables<-read_excel(paste("Other_input/Table_Independent_Variables_",regression_type,".xlsx",sep=""))
    
    Variables_tokeep<-subset(table_variables,type %in% c("main","control","attitude","accessibility","other") & include==1 | (add_control_bool==T & varname %in% add_control))
    
    
    additional_test<-""
    
    
    # to remove
    #Variables_tokeep<-subset(Variables_tokeep,!substr(label,1,5)=="Enjoy")
    
    Independent_variables<-Variables_tokeep$label
    
    # Remove unemployment variable
    
    if (Table_regressions_to_Run$employment[reg_no]==0){
      Independent_variables<-Independent_variables[Independent_variables!="Employment"]
    }
    
    #Independent_variables<-Independent_variables[Independent_variables!="Employment"]
    #additional_test<-"_noemployment"
    
    if (Table_regressions_to_Run$employment[reg_no]==0){
      Independent_variables<-Independent_variables[Independent_variables!="Employment"]
    }
    
    
    # Remove attitudinal variable
    
    if (Table_regressions_to_Run$noattitude[reg_no]==1){
      Variables_tokeep<-subset(table_variables,type %in% c("main","control","accessibility","other") & include==1 | (add_control_bool==T & varname %in% add_control))
      Independent_variables<-Variables_tokeep$label
      
    }
    
    if (Table_regressions_to_Run$spatial[reg_no]==1){
      Variables_tokeep<-subset(table_variables,type %in% c("main","control","attitude","accessibility","other") & include==1 | (add_control_bool==T & varname %in% add_control) | type=="geography")
      Independent_variables<-Variables_tokeep$label
      
    }
    
    #Variables_tokeep<-subset(table_variables,type %in% c("main","control","accessibility","other") & include==1 | (add_control_bool==T & varname %in% add_control))
    #Independent_variables<-Variables_tokeep$label
    #additional_test<-"_noattitude"
    
    source("quantile_regressions.R")
    
  }  
}

