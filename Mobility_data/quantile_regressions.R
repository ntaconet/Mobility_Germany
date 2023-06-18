
for (variable_n in Independent_variables){
#  print(variable_n)
#  print(sum(is.na(Regression_dataset[variable_n])))
}

# Make sure we add this later to 
# Lorenz curve ----

# 
#library("gglorenz")


#unique(Person_dataset$ST_MONAT_grouped)


#Regression_OLS<-lm(paste(Dependent_variable,"~",paste(Independent_variables,collapse=" + ")),
Regression_OLS<-lm(paste(Dependent_variable,"~",paste(Independent_variables,collapse=" + ")),
                   data=Regression_dataset,
                   weights=P_GEW_num)

#summary(Regression_OLS)

stargazer(car::vif(Regression_OLS),
          out=paste("Robustness_graphs/VIF_",Name_Regression,".tex",sep=""),
          title=paste("VIF ",Name_Regression),
          label="vif_satisfaction")

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

# for plane emissions we want to show quantile 90th, 95th and 99th, as there as mostly zeros.
if (Dependent_variable %in% c("emissions_flugzeug","emissions_flugzeug_wout_work")){
  tau_list<-c(0.90,0.95,0.99)
  } else {
  tau_list<-c(0.5,0.75,0.9)
}

Regression_Quantile_tau1<-rq(paste(Dependent_variable,"~",paste(Independent_variables,collapse=" + ")),
                              data=Regression_dataset,
                              tau=tau_list[1],
                              weights=P_GEW_num)

Regression_Quantile_tau2<-rq(paste(Dependent_variable,"~",paste(Independent_variables,collapse=" + ")),
                             data=Regression_dataset,
                             tau=tau_list[2],
                             weights=P_GEW_num)

Regression_Quantile_tau3<-rq(paste(Dependent_variable,"~",paste(Independent_variables,collapse=" + ")),
                             data=Regression_dataset,
                             tau=tau_list[3],
                             weights=P_GEW_num)

summary.rq(Regression_Quantile_tau1,se="boot")

#stargazer(Regression_OLS,Regression_Quantile_tau1,Regression_Quantile_tau2,Regression_Quantile_tau3)


stargazer(Regression_OLS,Regression_Quantile_tau1,Regression_Quantile_tau2,Regression_Quantile_tau3, 
          intercept.bottom = FALSE,
          digits=1,
          rq.se="boot",
          label=Name_Regression,
          #rq.se="nid",
          #ci=TRUE,
          #ci.level=0.90,
          #style = "aer",
          style = "default",
          keep.stat=c("n","rsq"),
          #omit.stat=c("se"),
          title=Title_Table,
          align=TRUE,
          dep.var.caption  = "",
          model.numbers = TRUE
          ,  t.auto = TRUE, p.auto = TRUE,
          font.size="scriptsize", 
          dep.var.labels   = "",
          multicolumn = TRUE,
          single.row = TRUE,
          #,omit.stat=c("f", "ser"),
          column.sep.width = "-35pt",
          dep.var.labels.include = TRUE,
          out=paste("Output_regressions/regression_",Name_Regression,additional_test,".tex",sep=""))


