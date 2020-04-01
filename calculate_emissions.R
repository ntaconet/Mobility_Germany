
# Calculate emissions for each travel ---------

# Import emission factor data
Emission_factors_UBA <- read_excel("Other_input/Emission_Factors_UBA.xlsx")
Emission_factors_HBEFA <- read_excel("Other_input/Emission_Factors_HBEFA.xlsx")

# Retain UBA data, and complete missing values with HBEFA estimates
Emission_factors<-Emission_factors_UBA%>%
  mutate(EF=EF_UBA,
         EF_Fzkm=EF_UBA_Fzkm)%>%
  # for Motos and AST Rufbus, use HBEFA estimates.
  mutate(EF_Fzkm=ifelse(varname %in% c("W_VM_D","W_VM_E","W_VM_F","W_VM_N"),
                        subset(Emission_factors_HBEFA,varname==varname & HBEFA_technology=="Benzin")$EF_HBEFA_2015, 
                        EF_Fzkm))%>%
  mutate(EF_Fzkm=ifelse(varname %in% c("W_VM_I"),
                        subset(Emission_factors_HBEFA,varname==varname & HBEFA_technology=="Diesel")$EF_HBEFA_2015, 
                        EF_Fzkm))%>%
  
  mutate(EF=as.numeric(EF),EF_Fzkm=as.numeric(EF_Fzkm))%>%
  select(Label,varname,EF,EF_Fzkm)#%>%


# Emissions - Wege -----

# Associate the transportation mode with the labels in Emission_factors
# I'm using: hvm_diff2	(hauptverkehrsmittel, but rather detailed, while hvm contains too few categories)
# Associate each number of hvm_diff2 to corresponding emission factor
# matching with variables from Emission_factors sheet
hvm_diff2_matching<-read_excel("Other_input/hvm_diff2_matching.xlsx")

Wege<-read.csv("CSV/MiD2017_Wege.csv",sep=";",fileEncoding="UTF-8-BOM")

# 
Wege<-Wege%>%
  mutate(wegkm_num=as.numeric(gsub(",",".",as.character(wegkm))))%>% #wegkm is factor, and has comas, not dots
  filter(hvm_diff2!=703)%>% # Weg ohne Detailerfassung (CAWI)
  filter(hvm_diff2!=99)%>% # No answer (only ~2000 obs)
  filter(wegkm!=9994)%>% # unplausibler Wert
  filter(wegkm!=9999)%>% # keine Angabe
  filter(wegkm!=70703)%>% # Weg ohne Detailerfassung (CAWI)
  mutate(emissions=ifelse(hvm_diff2 %in% c(4,5,6,7,8,9,12,17,18),
                          # if Pkw or Lkw or Moto or Rufbus, then divide vehicule emissions by nb of person in vehicule:
                          (wegkm_num/(1+W_ANZBEGL))*Emission_factors$EF_Fzkm[match(hvm_diff2_matching$varname[hvm_diff2],Emission_factors$varname)],
                          # else, use Pkm
                          wegkm_num*Emission_factors$EF[match(hvm_diff2_matching$varname[hvm_diff2],Emission_factors$varname)]))%>%
  # For the answer "Ruckweg vom vorherigen Weg", assign Zweck from previous travel 
  group_by(HP_ID)%>%
  arrange(W_ID,.by_group=TRUE)%>%
  mutate(W_ZWECK_filled=ifelse(W_ZWECK==9,lag(W_ZWECK),W_ZWECK))%>%
  ungroup()

# nb of NAs
#sum(is.na(Wege$emissions))

# Emissions - Reise ----- 

hvm_r_matching<-read_excel("Other_input/hvm_r_matching.xlsx")

Reisen<-read.csv("CSV/MiD2017_Reisen.csv",sep=";",fileEncoding="UTF-8-BOM")

Reisen<-Reisen%>%
  filter(hvm_r!=99)%>% # No answer
  filter(R_ENTF!=99994)%>% # unplausibler Wert
  filter(R_ENTF!=99999)%>% #keine Angabe
  mutate(
    emissions=case_when(
                        # For Pkw or Moto:
                        # if unplausible value for R_ANZBEGL, return NA
                        # else, divide vehicule emissions by nb of person in pkw:
                        hvm_r %in% c(1) & R_ANZBEGL %in% c(94,99) ~ NA_real_,
                        hvm_r %in% c(1) & !(R_ANZBEGL %in% c(94,99)) ~ (R_ENTF/(1+R_ANZBEGL))*Emission_factors$EF_Fzkm[match(hvm_r_matching$varname[hvm_r],Emission_factors$varname)],
                                               
                        # else, use Pkm
                        !(hvm_r %in% c(1)) ~ R_ENTF*Emission_factors$EF[match(hvm_r_matching$varname[hvm_r],Emission_factors$varname)]
    )
    )

# How many NAs?
#sum(is.na(Reisen$emissions))

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
  summarise(emissions_reise=sum(emissions),
            emissions_RW=sum(emissions[R_ZWECK %in% RW]),
            emissions_RL=sum(emissions[R_ZWECK %in% RL]),
            # also adding total Flugzeug
            emissions_flugzeug=sum(emissions[hvm_r==5]),
            
            # nb of reported reisen (to correct if nb of Reise is >3)
            #nb_reported_reisen=max(R_ID),
            nb_reported_reisen=n()
            )


# W_ZWECK CATEGORIES
# Everyday leisure travels
WL=c(7,14,15)

# Everyday other travel
WE=c(1,3,4,5,6,8,11,12,13,16)

# Everyday work travel
WW=c(2)

Wege_Person<-Wege%>%
  group_by(HP_ID)%>%
  summarise(emissions_wege=sum(emissions),
            emissions_WL=sum(emissions[W_ZWECK_filled %in% WL]),
            emissions_WE=sum(emissions[W_ZWECK_filled %in% WE]),
            emissions_WW=sum(emissions[W_ZWECK_filled %in% WW]),
            
            # nb of reported 
            #nb_reported_wege=max(W_ID),
            nb_reported_wege=n()
            )

Person_emissions<-merge(Reisen_Person,Wege_Person,all=T)

write.csv(Person_emissions,"Output/Person_emissions.csv",row.names = F)

