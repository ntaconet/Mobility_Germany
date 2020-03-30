
# 

# Plot emissions for leisure reise against emissions for everyday life
ggplot(data=Person_dataset,aes(x=emissions_RL,y=emissions_WE))+
  geom_point(alpha=0.1)

# Correlation between RL and WE emissions
cor(Person_dataset$emissions_RL,Person_dataset$emissions_WE,use="complete.obs")
