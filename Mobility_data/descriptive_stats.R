
Person_dataset<-read.csv("Output/Person_dataset.csv")

dir.create("Descriptive_graphs")

# In this plot I will plot a few descriptive graphs to show relationship between long-distance Reise emissions and other variables

# First, emissions from long-distance scatterplots against other emissions

# Emissions from everyday wege (outside work)
pdf("Descriptive_graphs/everyday_wege.pdf")
plot<-ggplot(data=Person_dataset,aes(y=emissions_RL,x=emissions_WE))+
  geom_point(alpha=0.1)+
  xlab("Emissions from everyday Wege")+
  ylab("Emissions from leisure Reise")
print(plot)
dev.off()

pdf("Descriptive_graphs/everyday_wege_zoom.pdf")
plot<-ggplot(data=Person_dataset,aes(y=emissions_RL,x=emissions_WE))+
  geom_point(alpha=0.1)+
  xlab("Emissions from everyday Wege")+
  ylab("Emissions from leisure Reise")+
  ylim(0,0.5*10**7)+
  xlim(0,25000)
print(plot)
dev.off()


# Emissions from leisure wege 
pdf("Descriptive_graphs/leisure_wege.pdf")
plot<-ggplot(data=Person_dataset,aes(y=emissions_RL,x=emissions_WL))+
  geom_point(alpha=0.1)+
  xlab("Emissions from leisure Wege")+
  ylab("Emissions from leisure Reise")
print(plot)
dev.off()


# Emissions from commute
pdf("Descriptive_graphs/commute_wege.pdf")
plot<-ggplot(data=Person_dataset,aes(y=emissions_RL,x=emissions_WC))+
  geom_point(alpha=0.1)+
  xlab("Emissions from everyday Wege")+
  ylab("Emissions from commute Wege")
print(plot)
dev.off()

 
# correlation?
data_cor<-cor(Person_dataset[c("emissions_RL","emissions_WE","emissions_WC","emissions_WL")],use="complete.obs")


# Then emissions from long-distance reise against:

name=c("Income","Education","Household composition","Age","Enjoy Biking","Gender","Rural_Urban")
varname=c("hheink_gr2","P_BIL","H_GR","alter_gr2","P_EINVM_RAD","HP_SEX","GEMTYP")

#Note that choose to display only values of emissions lower than:
limit_emissions<-10**6

for (i in 1:length(name)){
  pdf(paste("Descriptive_graphs/",name[i],".pdf",sep=""))
  plot<-ggplot(data=subset(Person_dataset),aes(x=factor(get(varname[i])),y=emissions_RL))+
    #geom_boxplot(outlier.size=0.05)+
    geom_violin(draw_quantiles = c(0.5))+
    ylim(0,limit_emissions)+
    xlab(name[i])+
    ylab("Emissions from leisure Reise")
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

