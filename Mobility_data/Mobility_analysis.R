# This is the script to analyse the data.
# Put it in a folder "Mobility_Germany", with 
# 1/ the folder "CSV" that contains Mobility data
# 2/ a folder "Other_input" that will receive the other input we will need (emission factors, ...)
rm(list = ls())

# Import necessary library
library(readxl)
library(tidyverse)


###########################
# STEP 1 : Calculate emissions for all travels ----
###########################


# Import emission factor data
Emission_factors_UBA <- read_excel("Other_input/Emission_Factors_UBA.xlsx")
Emission_factors_HBEFA <- read_excel("Other_input/Emission_Factors_HBEFA.xlsx")

# Retain UBA data
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

# How many observations have ?
Wege<-Wege%>%
  mutate(wegkm_num=as.numeric(gsub(",",".",as.character(wegkm))))%>% #wegkm is factor, and has comas, not dots
  filter(hvm_diff2!=703)%>% # Weg ohne Detailerfassung (CAWI)
  filter(hvm_diff2!=99)%>% # No answer (only ~2000 obs)
  filter(wegkm!=9994)%>% # unplausibler Wert
  filter(wegkm!=9999)%>% # keine Angabe
  filter(wegkm!=70703)%>% # Weg ohne Detailerfassung (CAWI)
  mutate(emissions=ifelse(hvm_diff2 %in% c(4,5,6,7,8,9,12,17,18),
                          # if Pkw or Lkw or Moto or Rufbus, then divide vehicule emissions by nb of person in pkw:
                          (wegkm_num/(1+W_ANZBEGL))*Emission_factors$EF_Fzkm[match(hvm_diff2_matching$varname[hvm_diff2],Emission_factors$varname)],
                          # else, use Pkm
                          wegkm_num*Emission_factors$EF[match(hvm_diff2_matching$varname[hvm_diff2],Emission_factors$varname)]))

# Only 3561 NAs :)
sum(is.na(Wege$emissions))


# Emissions - Reise ----- 

hvm_r_matching<-read_excel("Other_input/hvm_r_matching.xlsx")

Reisen<-read.csv("CSV/MiD2017_Reisen.csv",sep=";",fileEncoding="UTF-8-BOM")

Reisen<-Reisen%>%
  filter(hvm_r!=99)%>% # No answer
  filter(R_ENTF!=99994)%>% # unplausibler Wert
  filter(R_ENTF!=99999)%>% # keine Angabe
  mutate(emissions=ifelse(hvm_r %in% c(1),
                          # if Pkw or Moto, then divide vehicule emissions by nb of person in pkw:
                          (R_ENTF/(1+R_ANZBEGL))*Emission_factors$EF_Fzkm[match(hvm_r_matching$varname[hvm_r],Emission_factors$varname)],
                          # else, use Pkm
                          R_ENTF*Emission_factors$EF[match(hvm_r_matching$varname[hvm_r],Emission_factors$varname)]))

# How many Nas? Only 948
sum(is.na(Reisen$emissions))

