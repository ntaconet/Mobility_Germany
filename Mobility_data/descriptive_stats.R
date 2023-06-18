
Person_dataset<-read.csv("Output/Person_dataset.csv")

Person_dataset<-Person_dataset%>%
  mutate(P_GEW_num=as.numeric(gsub(",",".",as.character(P_GEW))))

dir.create("Descriptive_graphs")


# how many ppl are responsible for the top 10% of emissions from long distance

Top_10_RL<-Person_dataset%>%
  filter(emissions_RL>wtd.quantile(Person_dataset$emissions_RL,0.90,na.rm=T,weight=Person_dataset$P_GEW_num))

emissions_top10<-sum(Top_10_RL$emissions_RL*Top_10_RL$P_GEW_num)
emissions_tot<-sum(Person_dataset$emissions_RL*Person_dataset$P_GEW_num,na.rm=T)
Share_of_emissions<-emissions_top10/emissions_tot
Share_of_emissions

# how many ppl are responsible for the top 10% of emissions in total
Top_10_total<-Person_dataset%>%
  filter(Total_emissions_wout_work>wtd.quantile(Person_dataset$Total_emissions_wout_work,0.90,na.rm=T,weight=Person_dataset$P_GEW_num))

emissions_top10<-sum(Top_10_total$Total_emissions_wout_work*Top_10_total$P_GEW_num)
emissions_tot<-sum(Person_dataset$Total_emissions_wout_work*Person_dataset$P_GEW_num,na.rm=T)
Share_of_emissions<-emissions_top10/emissions_tot
Share_of_emissions


# Basic density plots
# In this plot I will plot a few descriptive graphs to show relationship between long-distance Reise emissions and other variables


if (FALSE){
# First, emissions from long-distance scatterplots against other emissions
#remove observations from the 20 highest emissions?
percentile_remove<-0.90

# Emissions from everyday wege (outside work)
plot<-ggplot(data=subset(Person_dataset,is.na(emissions_RL)==F & is.na(emissions_WE)==F),
             aes(y=emissions_RL,x=emissions_WE))+
  geom_point(alpha=0.1)+
  #geom_hex()+
  #scale_fill_viridis_c() +
  #geom_point(shape = '.', col = 'white')+
  #ylim(0,quantile(Person_dataset$emissions_RL,percentile_remove,na.rm=T))+
  #xlim(0,quantile(Person_dataset$emissions_WE,percentile_remove,na.rm=T))+
  xlab("Emissions from everyday Wege")+
  ylab("Emissions from leisure Reise")
ggsave("Descriptive_graphs/everyday_wege.pdf",plot=plot)


# Emissions from leisure wege 
plot<-ggplot(data=subset(Person_dataset),
                         #emissions_RL < quantile(Person_dataset$emissions_RL,percentile_remove,na.rm=T) & 
                          # emissions_WL < quantile(Person_dataset$emissions_WL,percentile_remove,na.rm=T)),
             aes(y=emissions_RL,x=emissions_WL))+
  #geom_point(alpha=0.1)+
  geom_hex()+
  scale_fill_viridis_c() +
  geom_point(shape = '.', col = 'white')+
  ylim(0,quantile(Person_dataset$emissions_RL,percentile_remove,na.rm=T))+
  xlim(0,quantile(Person_dataset$emissions_WL,percentile_remove,na.rm=T))+
  xlab("Emissions from leisure Wege")+
  ylab("Emissions from leisure Reise")

ggsave("Descriptive_graphs/leisure_wege.pdf",plot=plot)


# Emissions from commute
plot<-ggplot(data=subset(Person_dataset),
                         #emissions_RL < quantile(Person_dataset$emissions_RL,percentile_remove,na.rm=T) & 
                          # emissions_WC < quantile(Person_dataset$emissions_WC,percentile_remove,na.rm=T)),
             aes(y=emissions_RL,x=emissions_WC))+
  #geom_point(alpha=0.1)+
  geom_hex()+
  scale_fill_viridis_c() +
  geom_point(shape = '.', col = 'white')+
  ylim(0,quantile(Person_dataset$emissions_RL,percentile_remove,na.rm=T))+
  xlim(0,quantile(Person_dataset$emissions_WC,percentile_remove,na.rm=T))+
  xlab("Emissions from everyday Wege")+
  ylab("Emissions from commute Wege")


ggsave("Descriptive_graphs/commute_wege.pdf",plot=plot)

}


# Then emissions from long-distance reise against:
name=c("Income","Education","Household_composition","Age","Enjoy_Biking","Gender","Rural_Urban","Month")
varname=c("hheink_gr2","P_BIL","hhgr_gr","alter_gr2","P_EINVM_RAD","HP_SEX","GEMTYP","ST_MONAT")

#Note that choose to display only values of emissions lower than:
#limit_emissions<-10**6

percentile_remove<-0.75


if (FALSE){

#Person_withoutoutliers<-subset(Person_dataset,emissions_RL < )

for (i in 1:length(name)){
  # import legend
  legend_answers<-read_excel(paste("Other_input/legend_",varname[i],".xlsx",sep=""),col_names=c("Code","Meaning"))

  # Create variable containing meaning of answer
  Person_withoutoutliers<-Person_dataset%>%
    mutate(answer_meaning=factor(legend_answers$Meaning[match(get(varname),legend_answers$Code)],levels=legend_answers$Meaning))
  
  # sum of the weights by categories:
  Sum_weights<-Person_withoutoutliers%>%
    group_by_at(varname[i])%>%
    summarise(sum_weights=sum(P_GEW_num))

  new_dataset<-Person_withoutoutliers%>%
    left_join(Sum_weights,by=varname[i])    
  
  pdf(paste("Descriptive_graphs/",name[i],".pdf",sep=""))
  plot<-ggplot(data=new_dataset,aes(x=answer_meaning,
                                    y=emissions_RL,weight=P_GEW_num/sum_weights))+
    #geom_boxplot(outlier.size=0.05)+
    geom_violin(draw_quantiles = c(0.5))+
    ylim(0,wtd.quantile(Person_dataset$emissions_RL,percentile_remove,na.rm=T,weight=Person_dataset$P_GEW_num))+
    xlab(name[i])+
    ylab("Emissions from leisure Reise")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  print(plot)
  dev.off()
  
  
  pdf(paste("Descriptive_graphs/",name[i],"_flugzeug.pdf",sep=""))
  
  plot<-ggplot(data=new_dataset,aes(x=answer_meaning,
                                    y=emissions_flugzeug,weight=as.numeric(P_GEW_num/sum_weights)))+
    geom_boxplot()+
    #geom_boxplot(draw_quantiles = c(0.5))+
    ylim(0,wtd.quantile(Person_dataset$emissions_flugzeug,percentile_remove,na.rm=T,weight=Person_dataset$P_GEW_num))+
    xlab(name[i])+
    ylab("Emissions from airtravel")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  print(plot)
  dev.off()
  
  plot<-ggplot(data=subset(new_dataset,is.na(emissions_flugzeug)==F),aes(x=answer_meaning,
                                    y=emissions_flugzeug))+
    geom_boxplot()+
    #geom_boxplot(draw_quantiles = c(0.5))+
    ylim(0,25)+
    xlab(name[i])+
    ylab("Emissions from airtravel")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  print(plot)
  dev.off()
  
  
}

}
# What is the emissions from?



