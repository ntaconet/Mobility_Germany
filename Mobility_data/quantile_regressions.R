
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

# how to add Machado Santos Silva test

Regression_Quantile_list<-c(Regression_Quantile_tau1,Regression_Quantile_tau2,Regression_Quantile_tau3)

list_MSS_test<-c()
list_pvalue<-c()

for (tau_n in c(1,2,3)){
  residuals_squared <- residuals(get(paste("Regression_Quantile_tau",
                                                              as.character(tau_n),
                                                              sep="")))**2

  weighted_residuals_squared <- residuals(get(paste("Regression_Quantile_tau",
                                            as.character(tau_n),
                                            sep="")))**2 * sqrt(Regression_dataset$P_GEW_num)
  
  
  # Extract the indices of non-missing weighted residuals
  non_missing_indices <- which(!is.na(weighted_residuals_squared))
  
  # Get the predicted values corresponding to the non-missing weighted residuals
  predicted_values <- predict(get(paste("Regression_Quantile_tau",
                                        as.character(tau_n),
                                        sep="")))[non_missing_indices]
  
  
  # Subset the weighted squared residuals to include only non-missing values
  weighted_residuals_squared <- weighted_residuals_squared[non_missing_indices]
  
  # Estimate the auxiliary regression model
  aux_model <- lm(weighted_residuals_squared ~ predicted_values)
  
  # Extract the test statistic and its associated p-value
  test_statistic <- summary(aux_model)$fstatistic[1]
  p_value <- summary(aux_model)$coef[2, 4]
  

  ms_statistic <- nobs(aux_model) * summary(aux_model)$r.squared
  ms_pvalue<- 1 - pchisq(ms_statistic, df = length(coef(aux_model)) - 1)
  
  print(ms_pvalue)
  
  list_pvalue<-c(list_pvalue,ms_pvalue)
  list_MSS_test<-c(list_MSS_test,ms_statistic)
  
}

ms_pvalue
# how to add ajusted Rsquared
pseudo_Rsquared_list<-c()

for (tau_n in c(1,2,3)){
    
  
    
    
  predicted_values <- fitted(get(paste("Regression_Quantile_tau",
                                     as.character(tau_n),
                                     sep="")))
  quantile_loss_observed <- mean(pmax(Regression_dataset[,Dependent_variable] - predicted_values, 0))
  quantile_loss_null <- mean(pmax(Regression_dataset[,Dependent_variable] - mean(Regression_dataset[,Dependent_variable]), 0))
  quantile_pseudo_R2 <- 1 - (quantile_loss_observed / quantile_loss_null)

  print(quantile_pseudo_R2)
  
  quantile_loss_observed <- sum(pmax(Regression_dataset[,Dependent_variable] - predicted_values, 0))
  quantile_loss_null <- sum(pmax(Regression_dataset[,Dependent_variable] - mean(Regression_dataset[,Dependent_variable]), 0))
  quantile_pseudo_R2 <- 1 - (quantile_loss_observed / quantile_loss_null)

  print(quantile_pseudo_R2)
  
  
  
  weighted_ssr <- sum((residuals(get(paste("Regression_Quantile_tau",
                                           as.character(tau_n),
                                           sep=""))) * sqrt(Regression_dataset$P_GEW_num))^2)
  
  weighted_tss <- sum((Regression_dataset[,Dependent_variable] - weighted.mean(Regression_dataset[,Dependent_variable], weights = Regression_dataset$P_GEW_num))^2 * Regression_dataset$P_GEW_num)
  quantile_pseudo_R2 <- 1 - (weighted_ssr / weighted_tss)
  
  pseudo_Rsquared_list<-c(pseudo_Rsquared_list,quantile_pseudo_R2)
  
  
}



#stargazer(Regression_OLS,Regression_Quantile_tau1,Regression_Quantile_tau2,Regression_Quantile_tau3)

stargazer(Regression_OLS,Regression_Quantile_tau1,Regression_Quantile_tau2,Regression_Quantile_tau3, 
          intercept.bottom = FALSE,
          digits=1,
          rq.se="boot",
          label=Name_Regression,
          add.lines=list(c("MSS test", list_MSS_test),
                         c("MSS test, pvalue", list_pvalue),
                         
                         c("Quantile Pseudo R-squared", pseudo_Rsquared_list)),

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


