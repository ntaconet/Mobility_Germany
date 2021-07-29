
Person_dataset<-read.csv("Output/Person_dataset.csv")

# making sure variable types are correct

Person_dataset<-Person_dataset%>%
  mutate(P_GEW_num=as.numeric(gsub(",",".",as.character(P_GEW))))%>% # weights should be numeric
  filter(alter_gr1 %in% c(5:9))# remove ppl below 18 years old
#relevel(ST_MONAT,ref=10)) #  

# Lorenz curve ----

# 
data_lorenzcurve<-subset(Person_dataset,is.na(Total_emissions_wout_RW)==F)

#lorenz.curve(data=data_lorenzcurve[1:100,c("Total_emissions_wout_RW","P_GEW_num")])

pdf("Descriptive_graphs/Lorenz_total_emissions.pdf")
plot(Lc(data_lorenzcurve$Total_emissions_wout_RW,n=data_lorenzcurve$P_GEW_num),main="Total emissions",xlab="Share of population",ylab="Share of emissions")
grid(nx=10,ny=10)
dev.off()

pdf("Descriptive_graphs/Lorenz_reisen_emissions.pdf")
plot(Lc(data_lorenzcurve$emissions_reise,n=data_lorenzcurve$P_GEW_num),main="Long-distance emissions",xlab="Share of population",ylab="Share of emissions")
grid(nx=10,ny=10)
dev.off()


# Plot share for different quantiles of emissions.

# Plot how much comes


################
# Bar charts ----
################

# What is the share of emissions for different categories of emitters
# 

Person_dataset_subset<-Person_dataset%>%
  filter(!is.na(Total_emissions_wout_RW) & !is.na(emissions_wege))

Person_dataset_subset<-Person_dataset_subset%>%
  #mutate(Q_total_emissions = ntile(Total_emissions_wout_RW, 10))%>%
  mutate(Q_total_emissions = weighted_ntile(Total_emissions_wout_RW, 10,weights=P_GEW_num))%>%
  mutate(share_emissions_from_wege=(emissions_wege)/Total_emissions_wout_RW)
#mutate(share_emissions_from_wege=(factor_wege*emissions_wege)/Total_emissions_wout_RW)

Qtile<-wtd.quantile(Person_dataset_subset$Total_emissions_wout_RW,0.1*c(1:9),
                    weights=Person_dataset_subset$P_GEW_num)

Person_dataset_subset<-Person_dataset_subset%>%
  mutate(Q_cut=cut(Total_emissions_wout_RW,Qtile[2:9]))

data_mean<-Person_dataset_subset%>%
  group_by(Q_cut)%>%
  summarise(Total_emissions=weighted.mean(Total_emissions_wout_RW,weights=P_GEW_num,na.rm=T),
            #Emissions_wege=weighted.mean(factor_wege*emissions_wege,weights=P_GEW_num,na.rm=T))
            Emissions_wege=weighted.mean(emissions_wege,weights=P_GEW_num,na.rm=T))

plot<-ggplot(data=data_mean)+
  geom_bar(aes(Q_cut,Total_emissions/1000,fill="Long-distance emissions"),stat="identity")+
  geom_bar(aes(Q_cut,Emissions_wege/1000,fill="Daily emissions"),stat="identity")+
  scale_x_discrete(name="Quantile",labels=as.character(c(3:10)))+
  labs(y="Annual emissions (tCO2e)",fill=" ")+
  scale_fill_viridis_d()+
  theme_bw()

plot
ggsave("Descriptive_graphs/Total_emissions.png",plot=plot)  


plot<-ggplot(data=data_mean)+
  geom_bar(aes(Q_cut,Total_emissions/1000,fill="Long-distance emissions"),stat="identity")+
  geom_bar(aes(Q_cut,Emissions_wege/1000,fill="Daily emissions"),stat="identity")+
  scale_x_discrete(name="Quantile",labels=as.character(c(3:10)))+
  labs(y="Annual emissions (tCO2e)",fill=" ")+
  scale_fill_viridis_d()
theme_bw()

plot
ggsave("Descriptive_graphs/Total_emissions_share.png",plot=plot)  


