Person_dataset<-read.csv("Output/Person_dataset.csv")

# making sure variable types are correct

Person_dataset<-Person_dataset%>%
  mutate(P_GEW_num=as.numeric(gsub(",",".",as.character(P_GEW))))%>% # weights should be numeric
  filter(alter_gr1 %in% c(5:9))# remove ppl below 18 years old
#relevel(ST_MONAT,ref=10)) #  

# Lorenz curve ----

data_lorenzcurve<-subset(Person_dataset,is.na(Total_emissions_wout_work)==F)

#lorenz.curve(data=data_lorenzcurve[1:100,c("Total_emissions_wout_RW","P_GEW_num")])

pdf("Descriptive_graphs/Lorenz_total_emissions.pdf")
plot(Lc(data_lorenzcurve$Total_emissions_wout_work,n=data_lorenzcurve$P_GEW_num),main="Total emissions",xlab="Share of population",ylab="Share of emissions")
grid(nx=10,ny=10)
dev.off()

pdf("Descriptive_graphs/Lorenz_reisen_emissions.pdf")
plot(Lc(data_lorenzcurve$emissions_RL,n=data_lorenzcurve$P_GEW_num),main="Long-distance emissions",xlab="Share of population",ylab="Share of emissions")
grid(nx=10,ny=10)
dev.off()


# Plot share for different quantiles of emissions.


################
# Bar charts ----
################

# What is the share of emissions for different categories of emitters
# 

Person_dataset_subset<-Person_dataset%>%
  filter(!is.na(Total_emissions_wout_work) & !is.na(emissions_wege))

Person_dataset_subset<-Person_dataset_subset%>%
  #mutate(Q_total_emissions = ntile(Total_emissions_wout_RW, 10))%>%
  mutate(Q_total_emissions = weighted_ntile(Total_emissions_wout_work, 10,weights=P_GEW_num))%>%
  mutate(share_emissions_from_wege=(emissions_wege_wout_work)/Total_emissions_wout_work)
#mutate(share_emissions_from_wege=(factor_wege*emissions_wege)/Total_emissions_wout_RW)

Qtile<-wtd.quantile(Person_dataset_subset$Total_emissions_wout_work,0.1*c(1:9),
                    weights=Person_dataset_subset$P_GEW_num)

Person_dataset_subset<-Person_dataset_subset%>%
  mutate(Q_cut=cut(Total_emissions_wout_work,c(-1,Qtile[3:9],10^9)))

subset_test<-subset(Person_dataset_subset,is.na(Q_cut))

data_mean<-Person_dataset_subset%>%
  group_by(Q_cut)%>%
  summarise(Total_emissions=weighted.mean(Total_emissions_wout_work,weights=P_GEW_num,na.rm=T),
            #Emissions_wege=weighted.mean(factor_wege*emissions_wege,weights=P_GEW_num,na.rm=T))
            Emissions_wege=weighted.mean(emissions_wege_wout_work,weights=P_GEW_num,na.rm=T))

plot<-ggplot(data=data_mean)+
  geom_bar(aes(Q_cut,Total_emissions/1000,fill="Long-distance emissions"),stat="identity")+
  geom_bar(aes(Q_cut,Emissions_wege/1000,fill="Daily emissions"),stat="identity")+
  scale_x_discrete(name="Quantile",labels=as.character(c(3:10)))+
  labs(y="Annual emissions (tCO2e)",fill=" ")+
  scale_fill_viridis_d()#+
  
  #scale_fill_manual(values=c("Red","Green"))#+
  #theme_bw()

plot

ggsave("Descriptive_graphs/Total_emissions.png",plot=plot)  


plot1<-ggplot(data=data_mean)+
  geom_bar(aes(Q_cut,Total_emissions/1000,fill="Long-distance emissions"),stat="identity")+
  geom_bar(aes(Q_cut,Emissions_wege/1000,fill="Daily emissions"),stat="identity")+
  scale_x_discrete(name="Quantile",labels=as.character(c(3:10)))+
  labs(y="Annual emissions (tCO2e)",fill=" ")+
  scale_fill_viridis_d()+
  theme_bw()+
  #theme(legend.position="none")
  theme(
        legend.position =c(0.4,0.8))

plot1

ggsave("Descriptive_graphs/Total_emissions_share.png",plot=plot1)  

plot2<-ggplot(data=data_mean)+
  geom_bar(aes(Q_cut,100,fill="Long-distance emissions"),stat="identity")+
  geom_bar(aes(Q_cut,100*Emissions_wege/Total_emissions,fill="Daily emissions"),stat="identity")+
  scale_x_discrete(name="Quantile",labels=as.character(c(3:10)))+
  labs(y="Percent (%)",fill=" ")+
  scale_fill_viridis_d()+
  theme_bw()+
  theme(legend.position="none")

plot2


plot3<-ggplot(data=data_mean)+
  geom_bar(aes(Q_cut,100,fill="Long-distance emissions"),stat="identity")+
  geom_bar(aes(Q_cut,100*Emissions_wege/Total_emissions,fill="Daily emissions"),stat="identity")+
  scale_x_discrete(name="Quantile",labels=as.character(c(3:10)))+
  labs(y="Percent (%)",fill=" ")+
  scale_fill_viridis_d()+
  theme_bw()+
  guides(fill = guide_legend(nrow = 1))
#  theme(legend.position="none")

plot3

legend<-get_legend(plot3)

plot_combined<-plot_grid(plot1,plot2)


pdf("Figures_article/Breakdown_emissions_quantile.pdf")
plot_combined
dev.off()

# old plot
#pdf("Figures_article/Breakdown_emissions_quantile.pdf")
#plot_grid(plot_combined,legend,ncol=1,rel_heights=c(1,0.2))
#dev.off()

# quantiles ----


# Quantiles -----

quantiles_values<-c(0.1*c(1:9),0.95,0.99)

quantiles_total_emissions<-data.frame(value=wtd.quantile(Person_dataset_subset$Total_emissions_wout_work,weights=Person_dataset_subset$P_GEW_num,probs=quantiles_values),
                                      quantile=quantiles_values,
                                      emission_type="Total emissions")
quantiles_reisen_emissions<-data.frame(value=wtd.quantile(Person_dataset_subset$emissions_RL,weights=Person_dataset_subset$P_GEW_num,probs=quantiles_values),
                                       quantile=quantiles_values,
                                       emission_type="Long-distance emissions")

quantiles_flugzeug_emissions<-data.frame(value=wtd.quantile(Person_dataset_subset$emissions_flugzeug_wout_work,weights=Person_dataset_subset$P_GEW_num,probs=quantiles_values),
                                         quantile=quantiles_values,
                                         emission_type="Plane emissions")

data_quantiles<-quantiles_total_emissions%>%
  bind_rows(quantiles_reisen_emissions)%>%
  bind_rows(quantiles_flugzeug_emissions)



pdf("Figures_article/Quantiles_values.pdf")
ggplot(data=data_quantiles,aes(x=as.character(quantile),y=value/1000))+
  #facet_wrap(emission_type~.,scales="free")+
  geom_bar(stat="identity",aes(fill=emission_type),position=position_dodge())+
  theme_bw()+
  labs(y="Emissions (tCO2eq)",x="Quantiles",fill="")
dev.off()





