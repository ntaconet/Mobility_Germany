
for (variable_n in Dependant_variables){
  print(variable_n)
  print(sum(is.na(Regression_dataset[variable_n])))
}

# Make sure we add this later to 
# Lorenz curve ----

# 
#library("gglorenz")

data_lorenzcurve<-subset(Person_dataset,is.na(Total_emissions_wout_RW)==F)

#lorenz.curve(data=data_lorenzcurve[1:100,c("Total_emissions_wout_RW","P_GEW_num")])

pdf(paste("Descriptive_graphs/Lorenz_curves.pdf",sep=""))
plot(Lc(data_lorenzcurve$Total_emissions_wout_RW,n=data_lorenzcurve$P_GEW_num),col=2)
lines(Lc(data_lorenzcurve$emissions_reise,n=data_lorenzcurve$P_GEW_num),col=3)
legend("topleft",col=c(2,3),lty=c(1,1),legend=c("Total emissions","Emissions from long-distance travels"))
dev.off()



#unique(Person_dataset$ST_MONAT_grouped)


#Regression_OLS<-lm(paste(Independant_variable,"~",paste(Dependant_variables,collapse=" + ")),
Regression_OLS<-lm(paste(Independant_variable,"~",paste(Dependant_variables,collapse=" + ")),
                   data=Regression_dataset,
                   weights=P_GEW_num)

summary(Regression_OLS)

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
if (Independant_variable=="emissions_flugzeug"){
  tau_list<-c(0.90,0.95,0.99)
  } else {
  tau_list<-c(0.5,0.75,0.9)
}

Regression_Quantile_tau1<-rq(paste(Independant_variable,"~",paste(Dependant_variables,collapse=" + ")),
                              data=Regression_dataset,
                              tau=tau_list[1],
                              weights=P_GEW_num)

Regression_Quantile_tau2<-rq(paste(Independant_variable,"~",paste(Dependant_variables,collapse=" + ")),
                             data=Regression_dataset,
                             tau=tau_list[2],
                             weights=P_GEW_num)

Regression_Quantile_tau3<-rq(paste(Independant_variable,"~",paste(Dependant_variables,collapse=" + ")),
                             data=Regression_dataset,
                             tau=tau_list[3],
                             weights=P_GEW_num)


stargazer(Regression_OLS,Regression_Quantile_tau1,Regression_Quantile_tau2,Regression_Quantile_tau3, 
          intercept.bottom = FALSE,
          digits=2,
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



if (FALSE){

if (Independant_variable=="emissions_flugzeug"){
  Regression_Quantile_tau99<-rq(paste(Independant_variable,"~",paste(Dependant_variables,collapse=" + ")),
                                data=Regression_dataset,
                                tau=c(0.99),
                                weights=P_GEW_num)
  
  
  Regression_Quantile_tau95<-rq(paste(Independant_variable,"~",paste(Dependant_variables,collapse=" + ")),
                                data=Regression_dataset,
                                tau=c(0.95),
                                weights=P_GEW_num)
}


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

summary_plot<-plot_summs(Regression_OLS,Regression_Quantile_tau50,Regression_Quantile_tau75,Regression_Quantile_tau90,
                         model.names=c("Linear","QR50","QR75","QR90"),
                         omit.coefs =c("Car_sharing") )
ggsave("Plot_quantile_coefficients.png",plot=summary_plot,height=12)


# for flugzeug only show 90th percentile

# otherwise all:
if (Independant_variable=="emissions_flugzeug"){
  stargazer(Regression_OLS,Regression_Quantile_tau90,Regression_Quantile_tau95, 
            intercept.bottom = FALSE,
            digits=0,
            rq.se="boot",
            label=Name_Regression,
            #rq.se="nid",
            #ci=TRUE,
            #ci.level=0.90,
            #style = "aer",
            style = "all2",
            keep.stat=c("n","rsq"),
            
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
            #style = "aer",
            style = "all2",
            #keep.stat="all",
            keep.stat=c("n","rsq"),
            
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
            column.sep.width = "0.5pt",
            dep.var.labels.include = TRUE,
            out=paste("Output_regressions/regression_",Name_Regression,".tex",sep=""))

}
}


# CART trees -----
if (FALSE){
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



model_tree<-rpart(paste("Top_10","~",paste(Dependant_variables,collapse=" + ")),
                  data=Regression_dataset,
                  method="anova",
                  cp=3*10**-3,
                  minbucket=100,
                  weights=P_GEW_num)

model_tree

pdf(paste("Trees/Model_tree_",Name_Regression,"_smalltree",".pdf",sep=""))
rpart.plot(model_tree,extra=1)
dev.off()

}
