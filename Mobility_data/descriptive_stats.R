
Person_dataset<-read.csv("Output/Person_dataset.csv")

Person_dataset<-Person_dataset%>%
  mutate(P_GEW_num=as.numeric(gsub(",",".",as.character(P_GEW))))
                              
dir.create("Descriptive_graphs")

# In this plot I will plot a few descriptive graphs to show relationship between long-distance Reise emissions and other variables

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


# correlation?
data_cor<-cor(Person_dataset[c("emissions_RL","emissions_WE","emissions_WC","emissions_WL")],use="complete.obs")



# how many ppl are responsible for the top 10% of emissions



Top_10_RL<-Person_dataset%>%
  filter(emissions_RL>wtd.quantile(Person_dataset$emissions_RL,0.90,na.rm=T,weight=Person_dataset$P_GEW_num))

emissions_top10<-sum(Top_10_RL$emissions_RL*Top_10_RL$P_GEW_num)
emissions_tot<-sum(Person_dataset$emissions_RL*Person_dataset$P_GEW_num,na.rm=T)
Share_of_emissions<-emissions_top10/emissions_tot


# Then emissions from long-distance reise against:

name=c("Income","Education","Household_composition","Age","Enjoy_Biking","Gender","Rural_Urban","Month")
varname=c("hheink_gr2","P_BIL","hhgr_gr","alter_gr2","P_EINVM_RAD","HP_SEX","GEMTYP","ST_MONAT")

#Note that choose to display only values of emissions lower than:
#limit_emissions<-10**6

percentile_remove<-0.75

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
}


################
# Regressions ----
################

# Important thing to note is that we only have 30k observations
#(Reise module is actually only for the official survey)

# for all, the R2 is very low...

# First: income
summary(lm(emissions_RL~hheink_gr2,data=Person_dataset,weights=P_GEW_num))
# sign makes sense!

# Then income and rural/urban
summary(lm(emissions_RL~hheink_gr2+GEMTYP,data=Person_dataset,weights=P_GEW_num))

# Then income, rural/urban and gender
summary(lm(emissions_RL~hheink_gr2+GEMTYP+HP_SEX,data=subset(Person_dataset,HP_SEX %in% c(1,2)),weights=P_GEW_num))

# Removing gender because found not significant.
# And adding question about whether you enjoy bike
summary(lm(emissions_RL~hheink_gr2+GEMTYP+P_EINVM_RAD,data=subset(Person_dataset,P_EINVM_RAD %in% c(1:4)),weights=P_GEW_num))

# Trying instead to use the "emissions_WC" (emissions for the commute)
summary(lm(emissions_RL~hheink_gr2+GEMTYP+emissions_WC,data=Person_dataset,weights=P_GEW_num))
# seems like it is not significant... same results for emissions_wege

Dataset_withoutNAs<-subset(Person_dataset,
                           is.na(emissions_RL)==F & HP_SEX %in% c(1,2) & alter_gr2!=99 & alter_gr2>2 & P_BIL<7 & auto!=9 & P_NUTZ_RAD<6)

# maybe age too, and delete ppl under 20
summary(lm(emissions_RL~hheink_gr2+GEMTYP+HP_SEX+factor(alter_gr2)+emissions_WC+P_BIL+hhgr_gr,
           data=subset(Dataset_withoutNAs,HP_SEX %in% c(1,2) & alter_gr2!=99 & alter_gr2>2 & P_BIL<7)),
           weights=P_GEW_num)

# check for how months (ST_MONAT plays a role)
summary(lm(emissions_RL~factor(ST_MONAT),
           data=subset(Dataset_withoutNAs,HP_SEX %in% c(1,2) & alter_gr2!=99 & alter_gr2>2 & P_BIL<7)),
        weights=P_GEW_num)
# very few are significant... maybe grouping months together?

summary(lm(emissions_RL~factor(ST_MONAT),
           data=subset(Dataset_withoutNAs,HP_SEX %in% c(1,2) & alter_gr2!=99 & alter_gr2>2 & P_BIL<7)),
        weights=P_GEW_num)


# log model

Dataset_withoutNAs<-Dataset_withoutNAs%>%
  mutate(zeroemissionRL=(emissions_RL==0))

mylogit <- glm(zeroemissionRL ~ hheink_gr2+GEMTYP+HP_SEX+factor(alter_gr2)+emissions_WC+P_BIL+hhgr_gr, data = Dataset_withoutNAs, family = "binomial")

summary(mylogit)


# Making the same thing with the question "Verkehrsmittel auf regelm berufl Wegen am Stichtag"
# Create a dummy variable that says if you take your bike for commute
Person_dataset<-Person_dataset%>%
  mutate(bike_to_work=NA)%>%
  mutate(bike_to_work=ifelse(P_RBW_VM %in% c(2,3),1,bike_to_work))%>%
  mutate(bike_to_work=ifelse(is.na(P_RBW_VM)==F & (P_RBW_VM %in% c(2,3))==F,0,bike_to_work))
  


summary(lm(emissions_RL~hheink_gr2+GEMTYP+bike_to_work,data=Person_dataset,weights=P_GEW_num))
# also not very significant... but each time the direction of the effect is known

# Also try to use the answer about bewusster verzicht of a car but i'm not sure how!

# Let's see if how much km_RL explains:
summary(lm(emissions_RL~km_RL+hheink_gr2+GEMTYP,data=Person_dataset,weights=P_GEW_num))

################
# Trying partitioning ----
################

table_variables<-data.frame(name=c("Income","Education","Household_composition","Age","Enjoy_Biking","Gender","Rural_Urban","Month","Emissions_commute","Car_ownership","Frequency_Bike"),
                varname=c("hheink_gr2","P_BIL","hhgr_gr","alter_gr2","P_EINVM_RAD","HP_SEX","GEMTYP","ST_MONAT","emissions_WC","auto","P_NUTZ_RAD"))


Dataset_withoutNAs<-Dataset_withoutNAs%>%
  mutate(top_10_RL=emissions_RL>wtd.quantile(Person_dataset$emissions_RL,0.90,na.rm=T,weight=Person_dataset$P_GEW_num))%>%
  mutate(top_20_RL=emissions_RL>wtd.quantile(Person_dataset$emissions_RL,0.80,na.rm=T,weight=Person_dataset$P_GEW_num))

  
Dataset_withoutNAs$ST_MONAT<-factor(Dataset_withoutNAs$ST_MONAT)

Dataset_newvariables<-Dataset_withoutNAs%>%
  rename_at(vars(as.character(table_variables$varname)),~as.character(table_variables$name))

Dataset_newvariables$Age<-factor(Dataset_newvariables$Age)

for (output_type in c("emissions_RL","emissions_reise","emissions_RW","emissions_flugzeug","top_10_RL","top_20_RL")){
  model_tree<-rpart(paste(output_type,"~",paste(name,collapse=" + ")),
                      data=Dataset_newvariables,
                      method="anova",
                      cp=10**-3,
                      minbucket=100,
                      weights=P_GEW_num)
  
  pdf(paste("Descriptive_graphs/Model_tree_",output_type,".pdf",sep=""))
  rpart.plot(model_tree,extra=1)
  dev.off()
  
}


# Note: I didn't add Nebenwohnsitz because it's not in the Person dataset... (in the Haushalt one)

#another thing would be to look at who is in the top 10% emitter


pdf("Descriptive_graphs/Model_binary.pdf")
rpart.plot(model_binary)
dev.off()

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

