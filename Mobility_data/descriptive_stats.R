
Person_dataset<-read.csv("Output/Person_dataset.csv")

Person_dataset<-Person_dataset%>%
  mutate(P_GEW_num=as.numeric(gsub(",",".",as.character(P_GEW))))
                              
dir.create("Descriptive_graphs")

# In this plot I will plot a few descriptive graphs to show relationship between long-distance Reise emissions and other variables

# First, emissions from long-distance scatterplots against other emissions

#remove observations from the 20 highest emissions?
percentile_remove<-0.90

# Emissions from everyday wege (outside work)
pdf("Descriptive_graphs/everyday_wege.pdf")
plot<-ggplot(data=subset(Person_dataset,
                         emissions_RL < quantile(Person_dataset$emissions_RL,percentile_remove,na.rm=T) & 
                           emissions_WE < quantile(Person_dataset$emissions_WE,percentile_remove,na.rm=T)),
             aes(y=emissions_RL,x=emissions_WE))+
  #geom_point(alpha=0.1)+
  geom_hex()+
  scale_fill_viridis_c() +
  geom_point(shape = '.', col = 'white')+
  xlab("Emissions from everyday Wege")+
  ylab("Emissions from leisure Reise")
print(plot)
dev.off()

# Emissions from leisure wege 
pdf("Descriptive_graphs/leisure_wege.pdf")
plot<-ggplot(data=subset(Person_dataset,
                         emissions_RL < quantile(Person_dataset$emissions_RL,percentile_remove,na.rm=T) & 
                           emissions_WL < quantile(Person_dataset$emissions_WL,percentile_remove,na.rm=T)),
             aes(y=emissions_RL,x=emissions_WL))+
  #geom_point(alpha=0.1)+
  geom_hex()+
  scale_fill_viridis_c() +
  geom_point(shape = '.', col = 'white')+
  xlab("Emissions from leisure Wege")+
  ylab("Emissions from leisure Reise")
print(plot)
dev.off()


# Emissions from commute
pdf("Descriptive_graphs/commute_wege.pdf")
plot<-ggplot(data=subset(Person_dataset,
                         emissions_RL < quantile(Person_dataset$emissions_RL,percentile_remove,na.rm=T) & 
                           emissions_WC < quantile(Person_dataset$emissions_WC,percentile_remove,na.rm=T)),
             aes(y=emissions_RL,x=emissions_WC))+
  #geom_point(alpha=0.1)+
  geom_hex()+
  scale_fill_viridis_c() +
  geom_point(shape = '.', col = 'white')+
  xlab("Emissions from everyday Wege")+
  ylab("Emissions from commute Wege")
print(plot)
dev.off()


# link with km
pdf("Descriptive_graphs/everyday_wege.pdf")
plot<-ggplot(data=subset(Person_dataset,
                         emissions_RL < quantile(Person_dataset$emissions_RL,percentile_remove,na.rm=T) & 
                           emissions_WE < quantile(Person_dataset$emissions_WE,percentile_remove,na.rm=T)),
             aes(y=emissions_RL,x=emissions_WE))+
  #geom_point(alpha=0.1)+
  geom_hex()+
  scale_fill_viridis_c() +
  geom_point(shape = '.', col = 'white')+
  xlab("Emissions from everyday Wege")+
  ylab("Emissions from leisure Reise")
print(plot)
dev.off()


# correlation?
data_cor<-cor(Person_dataset[c("emissions_RL","emissions_WE","emissions_WC","emissions_WL")],use="complete.obs")


# Then emissions from long-distance reise against:

name=c("Income","Education","Household composition","Age","Enjoy Biking","Gender","Rural_Urban")
varname=c("hheink_gr2","P_BIL","hhgr_gr","alter_gr2","P_EINVM_RAD","HP_SEX","GEMTYP")

#Note that choose to display only values of emissions lower than:
#limit_emissions<-10**6


percentile_remove<-0.75

library("Hmisc")
Person_withoutoutliers<-subset(Person_dataset,emissions_RL < wtd.quantile(Person_dataset$emissions_RL,percentile_remove,na.rm=T,weight=Person_dataset$P_GEW_num))


for (i in 1:length(name)){
  
  # sum of the weights:
  
  #Sum_weights<-Person_withoutoutliers%>%
  #  group_by_at(varname[i])%>%
  #  summarise(sum_weights=sum(P_GEW_num))

  #new_dataset<-Person_withoutoutliers%>%
  #  left_join(Sum_weights,by=varname[i])    
  
  #essay_dataset<-new_dataset%>%
  #  group_by(hheink_gr2)%>%
  #  summarise(essay=sum(sum_weights),essay2=sum(P_GEW_num))
  
  # import legend
  legend_answers<-read_excel(paste("Other_input/legend_",varname[i],".xlsx",sep=""),col_names=c("Code","Meaning"))
  
  pdf(paste("Descriptive_graphs/",name[i],".pdf",sep=""))
  plot<-ggplot(data=Person_withoutoutliers,aes(x=factor(legend_answers$Meaning[match(get(varname),legend_answers$Code)]),
                                    y=emissions_RL,weight=P_GEW_num))+
    #geom_boxplot(outlier.size=0.05)+
    geom_violin(draw_quantiles = c(0.5))+
    #ylim(0,limit_emissions)+
    xlab(name[i])+
    ylab("Emissions from leisure Reise")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  print(plot)
  dev.off()
}


################
# Zone for tests ----
################

# who has highest data
Person_highRL<-subset(Person_dataset,emissions_RL==max(Person_dataset$emissions_RL,na.rm=T))$HP_ID
Reisen_personh<-subset(Reisen,HP_ID==Person_highRL)
Reisen_personh[c("hvm_r","R_ANZBEGL","R_ENTF")]

subset(Person_dataset,emissions_RL==max(Person_dataset$emissions_RL,na.rm=T))$P_ANZREISE

# the person has 3 reisen, which include a plane trip. But total 30 reisen (!!).
unique(Person_dataset$P_ANZREISE)

