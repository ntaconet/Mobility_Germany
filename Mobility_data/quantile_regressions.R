

#Regression_OLS<-lm(paste(Independant_variable,"~",paste(Dependant_variables,collapse=" + ")),
Regression_OLS<-lm(paste(Independant_variable,"~",paste(Dependant_variables,collapse=" + ")),
                   data=Regression_dataset,
                   weights=P_GEW_num)

stargazer(car::vif(Regression_OLS))

pdf(paste("Robustness_graphs/Robustnesscheck_",Independant_variable,suffix_add_control,".pdf",sep=""))
par(mfrow = c(2, 2))
plot(Regression_OLS)
dev.off()

# plot normality
pdf(paste("Robustness_graphs/Normality_",Independant_variable,suffix_add_control,".pdf",sep=""))
plot(Regression_OLS,2)
dev.off()

# plot heteroscedasticity
pdf(paste("Robustness_graphs/Heteroscedasticity_",Independant_variable,suffix_add_control,".pdf",sep=""))
plot(Regression_OLS,3)
dev.off()

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


#Regression_OLS

#export_summs(Regression_OLS,Regression_Quantile_tau50,Regression_Quantile_tau75,Regression_Quantile_tau90, 
 #             model.names = c("OLS","Quantile (50%)","Quantile (75%)","Quantile (90%)"),
  #            to.file = "docx", file.name = paste("Output_regressions/",as.character(Independant_variable),"_",regression_type,"_",suffix_add_control,".docx"),sep="")
              #to.file = "docx", file.name = paste("Output_regressions/",as.character(Independant_variable),"_without","_Comparing_OLS_to_Quantile.docx"),sep="")



# Let's try to look at the effect of not having a car:
# Create a variable that does that.



#if (FALSE){

# for flugzeug only show 90th percentile
#stargazer(Regression_OLS,Regression_Quantile_tau90, 

# otherwise all:
stargazer(Regression_OLS,Regression_Quantile_tau50,Regression_Quantile_tau75,Regression_Quantile_tau90, 
          intercept.bottom = FALSE,
          digits=0,
          rq.se="boot",
          
          #rq.se="nid",
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

if (FALSE){

# remove car variables: 
Dependant_variables_without_car<-Dependant_variables[!(Dependant_variables %in% c("Enjoy_Auto","Car_ownership"))]

Regression_OLS_withinteraction<-lm(paste(Independant_variable,"~",paste(Dependant_variables_without_car,collapse=" + "),"+Car_ownership*Enjoy_Auto",sep=""),
                                    data=Regression_dataset,
                                    weights=P_GEW_num)



Regression_Quantile_tau50_withinteraction<-rq(paste(Independant_variable,"~",paste(Dependant_variables_without_car,collapse=" + "),"+Car_ownership*Enjoy_Auto",sep=""),
                                              data=Regression_dataset,
                                              tau=c(0.5),
                                              weights=P_GEW_num)


Regression_Quantile_tau75_withinteraction<-rq(paste(Independant_variable,"~",paste(Dependant_variables_without_car,collapse=" + "),"+Car_ownership*Enjoy_Auto",sep=""),
                                              data=Regression_dataset,
                                              tau=c(0.75),
                                              weights=P_GEW_num)


Regression_Quantile_tau90_withinteraction<-rq(paste(Independant_variable,"~",paste(Dependant_variables_without_car,collapse=" + "),"+Car_ownership*Enjoy_Auto",sep=""),
                                              data=Regression_dataset,
                                              tau=c(0.90),
                                              weights=P_GEW_num)



stargazer(Regression_OLS_withinteraction,Regression_Quantile_tau50_withinteraction,Regression_Quantile_tau75_withinteraction,Regression_Quantile_tau90_withinteraction, 
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

}


# CART trees:

Regression_dataset$Top_10<-Regression_dataset[Independant_variable][,1]>wtd.quantile(Regression_dataset[Independant_variable][,1],0.90,na.rm=T,weight=Regression_dataset$P_GEW_num)


model_tree<-rpart(paste("Top_10","~",paste(Dependant_variables,collapse=" + ")),
                  data=Regression_dataset,
                  method="anova",
                  cp=10**-3,
                  minbucket=100,
                  weights=P_GEW_num)

pdf(paste("Trees/Model_tree_",Independant_variable,suffix_add_control,".pdf",sep=""))
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