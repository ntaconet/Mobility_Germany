# First some descriptive stats ----
table_variables<-read_excel(paste("Other_input/Table_Independent_Variables_basic.xlsx",sep=""))
variables0<-subset(table_variables,include==1)



# Plot Lorenz Curves -----

data_lorenzcurve<-subset(Person_dataset,is.na(Total_emissions_wout_work)==F)

pdf(paste("Descriptive_graphs/Lorenz_curves.pdf",sep=""))
plot(Lc(data_lorenzcurve$Total_emissions_wout_work,n=data_lorenzcurve$P_GEW_num),col="#55C667FF",
     xlab="Cumulative share of people",
     ylab="Cumulative share of emissions")
lines(Lc(data_lorenzcurve$emissions_RL,n=data_lorenzcurve$P_GEW_num),col="#FDE725FF")
lines(Lc(data_lorenzcurve$emissions_wege_wout_work,n=data_lorenzcurve$P_GEW_num),col="#482677FF")
legend("topleft",col=c("#55C667FF","#FDE725FF","#482677FF"),lty=c(1,1),
       legend=c("Total emissions",  
                "Emissions from long-distance travels",
                "Emissions from daily travels"),
)
dev.off()


# 1/ The whole sample ----
Regression_dataset_wholesample<-Regression_dataset
# Remove answers corresponding to "No answer"
for (i in 1:nrow(variables0)){
  
  print(variables0$label[i])
  if (!substr(variables0$label[i],1,5)=="Enjoy"){
    Regression_dataset_wholesample<-Regression_dataset_wholesample%>%
      #filter(get(Variables_tokeep$label[i])<Variables_tokeep$remove_above[i])
      filter(!is.na(get(variables0$label[i])))
    print(sum(!is.na(Regression_dataset_wholesample$emissions_RL)))
  }
}

Regression_dataset_wholesample<-Regression_dataset_wholesample%>%
  filter(!is.na(emissions_RL))%>%
  filter(!is.na(Total_emissions_wout_work))

write.csv(Regression_dataset_wholesample,"Output/Regression_dataset_wholesample.csv",row.names=F)



# 2/ The sample we'll be working with ----

Regression_dataset_NoNA<-Regression_dataset

# Remove answers corresponding to "No answer"
for (i in 1:nrow(variables0)){
  
  print(variables0$label[i])
  Regression_dataset_NoNA<-Regression_dataset_NoNA%>%
    #filter(get(Variables_tokeep$label[i])<Variables_tokeep$remove_above[i])
    filter(!is.na(get(variables0$label[i])))
  print(sum(!is.na(Regression_dataset_NoNA$emissions_RL)))
  
}
sum(!is.na(Regression_dataset$emissions_RL))

Regression_dataset_NoNA<-Regression_dataset_NoNA%>%
  filter(!is.na(emissions_RL))%>%
  filter(!is.na(Total_emissions_wout_work))

write.csv(Regression_dataset_NoNA,"Output/Regression_dataset_NoNA.csv",row.names=F)

Regression_dataset_NoZeros<-Regression_dataset_NoNA%>%
  filter(Total_emissions!=0)

write.csv(Regression_dataset_NoZeros,"Output/Regression_dataset_NoZeros.csv")

# 3/ Summary stats ----

summ_stats_Regression<-Regression_dataset_NoNA%>%
  select(variables0$label)

sumtable(summ_stats_Regression,out='latex',file="Descriptive_graphs/summary_table_Regression.tex",title="Summary Stat, regression sample",fit.page=9)

#variables0_wout_enjoy<-subset(variables0,)

summ_stats_wholesample<-Regression_dataset_wholesample%>%
  select(variables0$label)

sumtable(summ_stats_wholesample,out='latex',file="Descriptive_graphs/summary_table_Wholesample.tex",title="Summary Stat, whole sample",fit.page=9)


table_wholesample<-sumtable(summ_stats_wholesample,out='return')
table_regressionsample<-sumtable(summ_stats_Regression,out='return')

table_regressionsample<-table_regressionsample%>%
  rename(N_subsample=N,Percent_subsample=Percent)#%>%
  #select(-Variable)

table_wholesample<-table_wholesample%>%
  select(-Variable)

summ_stats_comparison<-cbind(table_wholesample,table_regressionsample)

stargazer(summ_stats_comparison,out="Descriptive_graphs/summary_table_comparison.tex")

#latex(summ_stats_comparison,rowlabel = "X", collabel =  "Y")

#print(summ_stats_comparison.to_latex())

#latex(summ_stats_comparison)

#sumtable(summ_stats_comparison,out='latex',file="Descriptive_graphs/summary_table.csv")


# do the Kolmogorov-Smirnov Test
ks.test(Regression_dataset_NoNA$Total_emissions_wout_work,
        Regression_dataset_wholesample$Total_emissions_wout_work)

# 4/ Other subsamples -----

summ_stats_electrobike<-Regression_dataset_NoNA%>%
  filter(P_VPED==1)%>%
  select(variables0$label)

sumtable(summ_stats_electrobike,out='latex',file="Descriptive_graphs/summary_table_electrobike.tex",fit.page=9)

Sumtable_electrobike<-sumtable(summ_stats_electrobike,out='return')

summ_stats_normalbike<-Regression_dataset_NoNA%>%
  filter(P_VRAD==1)%>%
  select(variables0$label)

Sumtable_normalbike<-sumtable(summ_stats_normalbike,out='return')#,out='latex',file="Descriptive_graphs/summary_table_electrobike.tex",fit.page=9)

sumtable(summ_stats_normalbike,out='latex',file="Descriptive_graphs/summary_table_electrobike.tex",fit.page=9)

Sumtable2<-Sumtable_normalbike%>%
  rename(N_normalbike=N,Percent_normalbike=Percent)%>%
  select(-Variable)

Sumtable3<-Sumtable_electrobike%>%
  rename(N_electrobike=N,Percent_electrobike=Percent)%>%
  select(-Variable)

Sumtable_all<-cbind(table_regressionsample,Sumtable2,Sumtable3)

typeof(Sumtable_all)

Sumtable_all
#Sumtable_all<-cbind(Sumtable,Sumtable3)

stargazer(Sumtable_all)

write.csv(Sumtable_all,"Descriptive_graphs/sumtable_electrobike.csv")


summ_stats_bikealot<-Regression_dataset_NoNA%>%
  filter(Enjoy_Biking=="Fully agree")%>%
  select(variables0$label)

Sumtable_bikealot<-sumtable(summ_stats_bikealot,out='return')#,out='latex',file="Descriptive_graphs/summary_table_electrobike.tex",fit.page=9)

Sumtable_bikealot<-Sumtable_bikealot%>%
  rename(N_bike=N,Percent_bike=Percent)%>%
  select(-Variable)

summ_stats_caralot<-Regression_dataset_NoNA%>%
  filter(Enjoy_Car=="Fully agree")%>%
  select(variables0$label)

Sumtable_caralot<-sumtable(summ_stats_caralot,out='return')#,out='latex',file="Descriptive_graphs/summary_table_electrobike.tex",fit.page=9)

Sumtable_caralot<-Sumtable_caralot%>%
  rename(N_car=N,Percent_car=Percent)%>%
  select(-Variable)

Sumtable_attitude<-cbind(table_regressionsample,Sumtable_caralot,Sumtable_bikealot)

write.csv(Sumtable_attitude,"Descriptive_graphs/sumtable_attitude.csv")

# How many car sharing?
nb_carsharing<-nrow(subset(Regression_dataset,Car_sharing=="Yes"))/nrow(Regression_dataset)

# What are the different deciles? ----- 


top_10<-wtd.quantile(Regression_dataset$Total_emissions_wout_work,0.90,na.rm=T,weight=Regression_dataset$P_GEW_num)

