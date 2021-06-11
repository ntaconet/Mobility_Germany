
table_variables<-read_excel(paste("Other_input/Table_Dependent_Variables_",regression_type,".xlsx",sep=""))

Variables_tokeep<-subset(table_variables,type %in% c("main","control","attitude","accessibility","other") & include==1 | (add_control_bool==T & varname %in% add_control))
Dependant_variables<-Variables_tokeep$label


if (FALSE){
# Make sure we add this later to 
# Lorenz curve ----

# 
data_lorenzcurve<-subset(Person_dataset,is.na(Total_emissions_wout_RW)==F)

#lorenz.curve(data=data_lorenzcurve[1:100,c("Total_emissions_wout_RW","P_GEW_num")])

plot(Lc(data_lorenzcurve$Total_emissions_wout_RW,n=data_lorenzcurve$P_GEW_num))

plot(Lc(data_lorenzcurve$emissions_reise,n=data_lorenzcurve$P_GEW_num))

unique(Person_dataset$ST_MONAT_grouped)
}


#Regression_OLS<-lm(paste(Independant_variable,"~",paste(Dependant_variables,collapse=" + ")),
Regression_OLS<-lm(paste(Independant_variable,"~",paste(Dependant_variables,collapse=" + ")),
                   data=Regression_dataset,
                   weights=P_GEW_num)

stargazer(car::vif(Regression_OLS))

pdf(paste("Robustness_graphs/Robustnesscheck_",Name_Regression,".pdf",sep=""))
par(mfrow = c(2, 2))
plot(Regression_OLS)
dev.off()

# plot normality
pdf(paste("Robustness_graphs/Normality_",Name_Regression,".pdf",sep=""))
plot(Regression_OLS,2)
dev.off()

# plot heteroscedasticity
pdf(paste("Robustness_graphs/Heteroscedasticity_",Name_Regression,".pdf",sep=""))
plot(Regression_OLS,3)
dev.off()

#Regression_Quantile


Regression_Quantile_tau90<-rq(paste(Independant_variable,"~",paste(Dependant_variables,collapse=" + ")),
                              data=Regression_dataset,
                              tau=c(0.9),
                              weights=P_GEW_num)

#summary.rq(Regression_Quantile_tau90)
if (Independant_variable!="emissions_flugzeug"){
  Regression_Quantile_tau75<-rq(paste(Independant_variable,"~",paste(Dependant_variables,collapse=" + ")),
                                data=Regression_dataset,
                                tau=c(0.75),
                                weights=P_GEW_num)
  
  
  Regression_Quantile_tau50<-rq(paste(Independant_variable,"~",paste(Dependant_variables,collapse=" + ")),
                                data=Regression_dataset,
                                tau=c(0.5),
                                weights=P_GEW_num)
  
}


# for flugzeug only show 90th percentile

# otherwise all:
if (Independant_variable=="emissions_flugzeug"){
  stargazer(Regression_OLS,Regression_Quantile_tau90, 
            intercept.bottom = FALSE,
            digits=0,
            rq.se="boot",
            label=Name_Regression,
            #rq.se="nid",
            #ci=TRUE,
            #ci.level=0.90,
            style = "aer",
            title=Title_Table,
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
            dep.var.labels.include = TRUE,
            out=paste("Output_regressions/regression_",Name_Regression,".tex",sep=""))
}
if (Independant_variable!="emissions_flugzeug"){
  
  stargazer(Regression_OLS,Regression_Quantile_tau50,Regression_Quantile_tau75,Regression_Quantile_tau90, 
            intercept.bottom = FALSE,
            digits=0,
            rq.se="boot",
            label=Name_Regression,
            #rq.se="nid",
            #ci=TRUE,
            #ci.level=0.90,
            style = "aer",
            title=Title_Table,
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
            dep.var.labels.include = TRUE,
            out=paste("Output_regressions/regression_",Name_Regression,".tex",sep=""))

}
# CART trees:

Regression_dataset$Top_10<-Regression_dataset[Independant_variable][,1]>wtd.quantile(Regression_dataset[Independant_variable][,1],0.90,na.rm=T,weight=Regression_dataset$P_GEW_num)


model_tree<-rpart(paste("Top_10","~",paste(Dependant_variables,collapse=" + ")),
                  data=Regression_dataset,
                  method="anova",
                  cp=10**-3,
                  minbucket=100,
                  weights=P_GEW_num)

pdf(paste("Trees/Model_tree_",Name_Regression,".pdf",sep=""))
rpart.plot(model_tree,extra=1)
dev.off()



# Trying PRIM analysis ----
if (FALSE){
PRIM_dataset<-Regression_dataset

PRIM_dataset$Top_10<-PRIM_dataset$Total_emissions_wout_RW>wtd.quantile(PRIM_dataset$Total_emissions_wout_RW,0.90,na.rm=T,weight=Regression_dataset$P_GEW_num)

#for (variable_name in colnames(Regression_dataset)){

for (variable_name in Dependant_variables){
    
  for (possible_result in unique(PRIM_dataset[variable_name])[,1]){
    var_name<-paste(variable_name,possible_result,sep="")
    PRIM_dataset<-PRIM_dataset%>%
      mutate(!!var_name := as.numeric(as.logical((get(variable_name)==possible_result))))
  }
  
}

PRIM_dataset<-PRIM_dataset%>%
  select(-Dependant_variables)%>%
  select(-P_GEW_num)%>%
  select(-Total_emissions_wout_RW)

PRIM_dataset<-PRIM_dataset%>%
  mutate(Top_10=as.numeric(Top_10))

write.csv(PRIM_dataset,file="PRIM_dataset.csv")

#sdprim(x=subset(PRIM_dataset,select=-c(Total_emissions_wout_RW,Top_10)),y=PRIM_dataset$Top_10)

sdprim(x=subset(PRIM_dataset,select=c(Age7,Age6,Age8)),
       y=PRIM_dataset$Top_10,
       thresh=0.5)


sd.start()

PRIM.prim<-prim.box(x=subset(PRIM_dataset,select=-c(Total_emissions_wout_RW,Top_10)),
                    y=PRIM_dataset$Top_10,
                    threshold=0.5,
                    threshold.type=1)

PRIM.prim

summary(PRIM.prim)

}
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