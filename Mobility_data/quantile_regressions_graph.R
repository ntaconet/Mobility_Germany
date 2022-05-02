# This script is used to plot:
# 1/ Point estimate + confidence interval for different QR
# 2/ Characteristics of ppl owning electric bikes vs not,
# and of ppl liking or not.

Table_regressions_to_Run<-read_xlsx("Other_input/Table_Regressions_to_Run.xlsx")

reg_no<-3

Independant_variable<-Table_regressions_to_Run$Independant_variable[reg_no]

# Whether to add control or not.
add_control_bool<-as.logical(Table_regressions_to_Run$add_control_bool[reg_no])
add_control<-Table_regressions_to_Run$add_control[reg_no]

add_control<-unlist(strsplit(add_control,","))

Name_Regression<-Table_regressions_to_Run$Name_Regression[reg_no]

Title_Table<-Table_regressions_to_Run$Title_Table[reg_no]

# In the table, variables are classified, and each line specifies values to remove (corresponding to no Answer, NA...).
regression_type<-"basic"

table_variables<-read_excel(paste("Other_input/Table_Dependent_Variables_",regression_type,".xlsx",sep=""))

Variables_tokeep<-subset(table_variables,type %in% c("main","control","attitude","accessibility","other") & include==1 | (add_control_bool==T & varname %in% add_control))
Dependant_variables<-Variables_tokeep$label


#Regression_OLS<-lm(paste(Independant_variable,"~",paste(Dependant_variables,collapse=" + ")),
Regression_OLS<-lm(paste(Independant_variable,"~",paste(Dependant_variables,collapse=" + ")),
                   data=Regression_dataset,
                   weights=P_GEW_num)

stargazer(car::vif(Regression_OLS))

Regression_Quantile_all<-rq(paste(Independant_variable,"~",paste(Dependant_variables,collapse=" + ")),
                              data=Regression_dataset,
                              tau=c(0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1),
                              weights=P_GEW_num)

pdf("Descriptive_graphs/Quantile_regression_graph.pdf")
#plot.rqs(Regression_Quantile_all)
plot.summary.rqs(summary(Regression_Quantile_all,parm=c(2,10)))
dev.off()

pdf("Descriptive_graphs/Quantile_regression_graph.pdf")
plot(summary.rqs(Regression_Quantile_all),parm=c(2:15))
dev.off()

# 
# 
