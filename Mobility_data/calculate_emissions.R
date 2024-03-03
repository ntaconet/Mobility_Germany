
# Calculate emissions for each travel ---------

# Import emission factor data
Emission_factors_UBA <- read_excel("Other_input/Emission_Factors_UBA.xlsx")
Emission_factors_HBEFA <- read_excel("Other_input/Emission_Factors_HBEFA.xlsx")
Emission_factors_Flugzeug_UBA <- read_excel("Other_input/Emission_Factors_Flugzeug_UBA.xlsx")

# Retain UBA data
Emission_factors<-Emission_factors_UBA%>%
  rename(EF=EF_UBA,
         EF_Fzkm=EF_UBA_Fzkm)%>%
  # for Motos and AST Rufbus, use HBEFA estimates.
  mutate(EF_Fzkm=ifelse(varname %in% c("W_VM_D","W_VM_E","W_VM_F","W_VM_N"),
                        subset(Emission_factors_HBEFA,varname==varname & HBEFA_technology=="Benzin")$EF_HBEFA_2015, 
                        EF_Fzkm))%>%
  mutate(EF_Fzkm=ifelse(varname %in% c("W_VM_I"),
                        subset(Emission_factors_HBEFA,varname==varname & HBEFA_technology=="Diesel")$EF_HBEFA_2015, 
                        EF_Fzkm))%>%
  mutate(EF=as.numeric(EF),EF_Fzkm=as.numeric(EF_Fzkm))

# convert emissions factor from UBA into numeric values
Emission_factors_Flugzeug_UBA<-Emission_factors_Flugzeug_UBA%>%
  mutate(EF_Flugzeug_UBA=as.numeric(EF_Flugzeug_UBA))

# Emissions - Wege -----

# Associate the transportation mode with the labels in Emission_factors
# I'm using: hvm_diff2	(hauptverkehrsmittel, but rather detailed, while hvm contains too few categories)
# Associate each number of hvm_diff2 to corresponding emission factor
# matching with variables from Emission_factors sheet
hvm_diff2_matching<-read_excel("Other_input/hvm_diff2_matching.xlsx")

Wege<-read.csv("CSV/MiD2017_Wege.csv",sep=";",fileEncoding="UTF-8-BOM")

# 
Wege<-Wege%>%
  # Some variables are factors, with "," instead of dots: wegkm and W_GEW (weight)
  mutate(wegkm_num=as.numeric(gsub(",",".",as.character(wegkm))),
         W_GEW=as.numeric(gsub(",",".",as.character(W_GEW))))%>% #C factor, and has comas, not dots
  # repace coding for missing values by NA for the two variables we will use
  mutate(hvm_diff2=replace(hvm_diff2,hvm_diff2 %in% c(703,99),NA))%>% # 703: Weg ohne Detailerfassung (CAWI) / 99: No answer (only ~2000 obs)
  mutate(wegkm_num=replace(wegkm_num,wegkm_num %in% c(9994,9999,70703),NA))%>% # 9994: unplausibler Wert / 9999: keine Angabe / 70703: Weg ohne Detailerfassung (CAWI)
  # Compute emissions
  mutate(emissions=ifelse(hvm_diff2 %in% c(4,5,6,7,8,9,12,17,18),
                          # if Pkw or Lkw or Moto or Rufbus, then divide vehicule emissions by nb of person in vehicule:
                          (wegkm_num/(1+W_ANZBEGL))*Emission_factors$EF_Fzkm[match(hvm_diff2_matching$varname[hvm_diff2],Emission_factors$varname)],
                          # else, use Pkm
                          wegkm_num*Emission_factors$EF[match(hvm_diff2_matching$varname[hvm_diff2],Emission_factors$varname)]))%>%
  # For the answer "Ruckweg vom vorherigen Weg", assign Zweck (purpose) from previous travel 
  group_by(HP_ID)%>%
  arrange(W_ID,.by_group=TRUE)%>%
  mutate(W_ZWECK_filled=ifelse(W_ZWECK==9,lag(W_ZWECK),W_ZWECK))#%>%

# Emissions - Reise ----- 
hvm_r_matching<-read_excel("Other_input/hvm_r_matching.xlsx")

Reisen<-read.csv("CSV/MiD2017_Reisen.csv",sep=";",fileEncoding="UTF-8-BOM")

Reisen<-Reisen%>%
  # replace coding for missing values by NA for the two variables we will use
  mutate(hvm_r=replace(hvm_r,hvm_r==99,NA))%>% # 99: No answer
  mutate(R_ENTF=replace(R_ENTF,R_ENTF %in% c(99994,99999),NA))%>% # 99994: unplausibler Wert / 99999: keine Angabe
  # Calculate emissions
  mutate(
    emissions=case_when(
                        # For Pkw or Moto:
                        # if unplausible value for R_ANZBEGL, return NA
                        # else, divide vehicule emissions by nb of person in pkw:
                        hvm_r %in% c(1) & R_ANZBEGL %in% c(94,99) ~ NA_real_,
                        hvm_r %in% c(1) & !(R_ANZBEGL %in% c(94,99)) ~ (R_ENTF/(1+R_ANZBEGL))*Emission_factors$EF_Fzkm[match(hvm_r_matching$varname[hvm_r],Emission_factors$varname)],
                                               
                        # else, use Pkm
                        !(hvm_r %in% c(1)) ~ R_ENTF*Emission_factors$EF[match(hvm_r_matching$varname[hvm_r],Emission_factors$varname)]
    # special case for aviation, with factor depending on the distance
    # hvm_r=5, then use 
    # () & ()
    
    )
    )%>%
  filter(R_ZIEL!=9)%>%
  mutate(Destination=ifelse(R_ZIEL==1,"inland","ausland"))%>%
  mutate(EF_flugzeug=case_when(Destination=="inland" & R_ENTF<=500 ~ Emission_factors_Flugzeug_UBA$EF_Flugzeug_UBA[1],
                               Destination=="inland" & R_ENTF > 500 ~ Emission_factors_Flugzeug_UBA$EF_Flugzeug_UBA[2],
                               Destination=="ausland" & R_ENTF<=500 ~ Emission_factors_Flugzeug_UBA$EF_Flugzeug_UBA[3],
                               Destination=="ausland" & R_ENTF>500 & R_ENTF<=1000 ~ Emission_factors_Flugzeug_UBA$EF_Flugzeug_UBA[4],
                               Destination=="ausland" & R_ENTF>1000 & R_ENTF<=2000 ~ Emission_factors_Flugzeug_UBA$EF_Flugzeug_UBA[5],
                               Destination=="ausland" & R_ENTF>2000 & R_ENTF<=5000 ~ Emission_factors_Flugzeug_UBA$EF_Flugzeug_UBA[6],
                               Destination=="ausland" & R_ENTF>5000 & R_ENTF<=10000 ~ Emission_factors_Flugzeug_UBA$EF_Flugzeug_UBA[7],
                               Destination=="ausland" & R_ENTF>10000 ~ Emission_factors_Flugzeug_UBA$EF_Flugzeug_UBA[8]))%>%
  mutate(emissions=ifelse(hvm_r==5,
                          R_ENTF*EF_flugzeug,
                          emissions))

# How many NAs?
sum(is.na(Reisen$emissions))


# Plot the contribution of different transportation mode to total emissions ----
#to avoid double counting, we only keep wege that are less than 100 km, and Reisen that are above 100 km.

# emissions from wege should be multiplied by 365 days
factor_wege<-365
# emissions from Reisen should be multiplied by 4 (last three months)
factor_reisen<-4

Wege_tokeep<-Wege%>%
  # only those below 100 km
  filter(wegkm_num<100)%>%
  # select type of transportation hvm_diff2
  mutate(transportation_mode=case_when(hvm %in% c(1,2) ~ "Others",
                                       hvm %in% c(5) ~ "Public transportation",
                                       hvm %in% c(3,4) ~ "Car"))%>%
  mutate(emissions=emissions*factor_wege*as.numeric(gsub(",",".",as.character(W_GEW))))%>%
  select(transportation_mode,emissions)%>%
  mutate(type_travel="Daily mobility")
           
Reisen_tokeep<-Reisen%>%
  # only those that are greater than 100 km
  filter(R_ENTF>100)%>%
  mutate(transportation_mode=case_when(hvm_r %in% c(1) ~ "Car",
                                       hvm_r %in% c(2) ~ "Train",
                                       hvm_r %in% c(3,4) ~ "Long-distance bus",
                                       hvm_r %in% c(5) ~ "Plane",
                                       hvm_r %in% c(6,7,8) ~ "Others"))%>%
  mutate(emissions=emissions*factor_reisen*as.numeric(gsub(",",".",as.character(R_GEW))))%>%
  select(transportation_mode,emissions)%>%
  mutate(type_travel="Long distance")

all_travels<-rbind(Wege_tokeep,Reisen_tokeep)

all_travels_aggregated<-all_travels%>%
  group_by(transportation_mode)%>%
  summarise(total=sum(emissions,na.rm=T))

all_travels_aggregated<-all_travels_aggregated%>%
  mutate(total=total/sum(all_travels_aggregated$total))

#ggplot(data=all_travels,aes(x=" ",y=emissions,fill=transportation_mode))+
#  geom_bar(stat="identity",position="fill")

unique(all_travels$transportation_mode)

all_travels<-all_travels%>%
  filter(is.na(transportation_mode)==FALSE)%>%
  mutate(transportation_mode=factor(transportation_mode,
                                    levels=c("Car","Long-distance bus", "Plane","Public transportation","Train","Others")))

plot_share_by_category<-ggplot(data=all_travels,
                               aes(x=type_travel,
                                   y=emissions,
                                   fill=transportation_mode))+
  geom_bar(stat="identity",position="fill")+
  labs(y="Share of emissions",x="Type of travel",fill="Transportation mode")+
  theme_bw()+
  #scale_fill_brewer(palette="Dark2",type="qual")#+
  scale_fill_npg()

plot_share_by_category

ggsave("Descriptive_graphs/share_emissions_by_category.png",plot=plot_share_by_category)


plot_share_by_category_total<-ggplot(data=all_travels,
                               aes(x=" ",
                                   y=emissions,
                                   fill=transportation_mode))+
  geom_bar(stat="identity",position="fill")+
  labs(y="Share of emissions",x="",fill="Transportation mode")+
  theme_bw()#+
  #scale_fill_brewer(palette="Set1")

ggsave("Descriptive_graphs/share_emissions_by_category_total.png",plot=plot_share_by_category_total)


# Aggregate emissions at the person's level ---------

# depending on type of travel.

# R_ZWECK answers corresponding to:
# Work-related Reisen
RW=c(4)
# Leisure Reisen
RL=c(1,2,3)

# Now let's compile emissions:
Reisen_Person<-Reisen%>%
  group_by(HP_ID)%>%
  summarise(km_reise=sum(R_ENTF),
            km_RW=sum(R_ENTF[R_ZWECK %in% RW]),
            km_RL=sum(R_ENTF[R_ZWECK %in% RL]),
            emissions_reise=sum(emissions),
            emissions_RW=sum(emissions[R_ZWECK %in% RW]),
            emissions_RL=sum(emissions[R_ZWECK %in% RL]),
            # also adding total Flugzeug
            emissions_flugzeug=sum(emissions[hvm_r==5]),
            emissions_flugzeug_RL=sum(emissions_RL[hvm_r==5]),
            
            # nb of reported reisen (to correct if nb of Reise is >3)
            nb_reported_reisen=n()
            )

sum(is.na(Reisen_Person$emissions_RL))

# W_ZWECK CATEGORIES
# Everyday leisure travels
WL=c(7,14,15)

# Everyday other travel
WE=c(3,4,5,6,11,12,13,16)

# Everyday work travel
WW=c(2)

# Wege-commute
WC=c(1,8)

Wege_Person<-Wege%>%
  group_by(HP_ID)%>%
  summarise(km_weg=sum(wegkm_num),
            km_WL=sum(wegkm_num[W_ZWECK_filled %in% WL]),
            km_WE=sum(wegkm_num[W_ZWECK_filled %in% WE]),
            km_WW=sum(wegkm_num[W_ZWECK_filled %in% WW]),
            km_WC=sum(wegkm_num[W_ZWECK_filled %in% WC]),
            
            #emissions
            emissions_wege=sum(emissions),
            emissions_WL=sum(emissions[W_ZWECK_filled %in% WL]),
            emissions_WE=sum(emissions[W_ZWECK_filled %in% WE]),
            emissions_WW=sum(emissions[W_ZWECK_filled %in% WW]),
            emissions_WC=sum(emissions[W_ZWECK_filled %in% WC]),
            
            # nb of reported 
            nb_reported_wege=n()
            )

Person_emissions<-Reisen_Person%>%
  full_join(Wege_Person)

#Person_emissions<-merge(Reisen_Person,Wege_Person,all=T)
# Assign 0 value to NAs?

sum(!is.na(Person_emissions$emissions_RL))

write.csv(Person_emissions,"Output/Person_emissions.csv",row.names = F)

