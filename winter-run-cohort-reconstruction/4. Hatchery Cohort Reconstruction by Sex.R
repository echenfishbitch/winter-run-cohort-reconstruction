###############################################
############## Hatchery Sex-specific###########
############## Reconstruction #################
###############################################
library(dplyr)
library(tidyr)
setwd("~/winter-run-cohort-reconstruction")

Escapement_SG_Bootstrap_M<-readRDS("CWTBootstraps_M.Rds") #Spawning Ground Escapement by Year and Age for Males
Escapement_SG_Bootstrap_F<-readRDS("CWTBootstraps_F.Rds") #Spawning Ground Escapement by Year and Age for Females

To_Hatchery_F<-read.csv("Escapement to Hatchery_F.csv") #Hatchery Escapement Females
To_Hatchery_M<-read.csv("Escapement to Hatchery_M.csv") #Hatchery Escapement Males

River_Harvest_F<-read.csv("River Harvest_F.csv")#River Harvest Proportion by SG Sex Ratio for each Age
River_Harvest_M<-read.csv("River Harvest_M.csv")

Recruits<-read.csv("Hatchery Release.csv") #Released
#Indicate male or female here, need to change code for males to females on
#lines 21, 22, 23, 29-30, 59, 111,116, 136,137, 138
Cohort<-Escapement_SG_Bootstrap_F
To_Hatchery<-To_Hatchery_F
River_Harvest<-River_Harvest_F
#####################

for(i in 1:1000){
  Cohort[[i]]<-Cohort[[i]] %>%
  left_join(Recruits,  by="brood_year") %>% #merging with recruits
  # select(brood_year, Individuals_Released, Age2Sp,Age3Sp,Age4Sp) %>% #Males
  select(brood_year, Individuals_Released, Age2Sp,Age3Sp,Age4Sp, Age5Sp) %>% #Females
  filter(brood_year > 2000)%>%
 left_join(To_Hatchery,by="brood_year") %>%
 left_join(River_Harvest,by="brood_year") %>%
 select(-run_year)
  Cohort[[i]][is.na(Cohort[[i]])]<-0
}
#Using Impact Rates estimated from CWT instead of absolute numbers
CWT<-readRDS("CWT Cohort Reconstruction_Bootstrap.Rds")
test<-CWT[[1]]
test<-Cohort[[1]]
################################################
#bootstrapping
for(k in 1:1000){
  # 5 Year Old Spawners (Females Only)
  Cohort[[k]]$Age6.2<-Cohort[[k]]$Age5Sp/(1-0.0184)
  Cohort[[k]]$Age6.1<-Cohort[[k]]$Age6.2/(1-0.0184)
  Cohort[[k]]$Age5.12<-Cohort[[k]]$Age6.1/(1-0.0184)
  Cohort[[k]]$Age5.11<-Cohort[[k]]$Age5.12/(1-0.0184)
  Cohort[[k]]$Age5.10<-Cohort[[k]]$Age5.11/(1-0.0184)
  Cohort[[k]]$Age5.9<-Cohort[[k]]$Age5.10/(1-0.0184)
  Cohort[[k]]$Age5.8<-Cohort[[k]]$Age5.9/(1-0.0184)
  Cohort[[k]]$Age5.7<-Cohort[[k]]$Age5.8/(1-0.0184)
  Cohort[[k]]$Age5.6<-Cohort[[k]]$Age5.7/(1-0.0184)
  Cohort[[k]]$Age5.5<-Cohort[[k]]$Age5.6/(1-0.0184)
  Cohort[[k]]$Age5.4<-Cohort[[k]]$Age5.5/(1-0.0184)
  Cohort[[k]]$Age5.3<-Cohort[[k]]$Age5.4/(1-0.0184)

  #4 Year Old Spawners
  Cohort[[k]]$Age5.2<-(Cohort[[k]]$Age4Sp+Cohort[[k]]$Age4Hat+Cohort[[k]]$InRiver4+Cohort[[k]]$Age5.3)/(1-0.0184)#
  
  #Third Year at Sea. 4 year olds are caught. 
  Cohort[[k]]$Age5.1<-Cohort[[k]]$Age5.2/(1-0.0184)
  Cohort[[k]]$Age4.12<-Cohort[[k]]$Age5.1/(1-0.0184)
  Cohort[[k]]$Age4.11<-(Cohort[[k]]$Age4.12/(1-0.0184))/(1-CWT[[k]]$Imp4_11)
  Cohort[[k]]$Age4.10<-(Cohort[[k]]$Age4.11/(1-0.0184))/(1-CWT[[k]]$Imp4_10)
  Cohort[[k]]$Age4.10[16]<-6 #No age 4 spawners, and no other fishing rate
  Cohort[[k]]$Age4.9<-(Cohort[[k]]$Age4.10/(1-0.0184))/(1-CWT[[k]]$Imp4_9)
  Cohort[[k]]$Age4.8<-(Cohort[[k]]$Age4.9/(1-0.0184))/(1-CWT[[k]]$Imp4_8)
  Cohort[[k]]$Age4.7<-(Cohort[[k]]$Age4.8/(1-0.0184))/(1-CWT[[k]]$Imp4_7)
  Cohort[[k]]$Age4.6<-(Cohort[[k]]$Age4.7/(1-0.0184))/(1-CWT[[k]]$Imp4_6)
  #4 year old Contact rate in May 2008 was 1 because no 4 year old spawners were recovered. Using the median fishing rate of years where fish were caught instead
  CWT[[k]]$Imp4_5[8]<-median(CWT[[k]]$Imp4_5[c(2,3,12,16)])
  Cohort[[k]]$Age4.5<-(Cohort[[k]]$Age4.6/(1-0.0184))/(1-CWT[[k]]$Imp4_5)
  Cohort[[k]]$Age4.4<-(Cohort[[k]]$Age4.5/(1-0.0184))/(1-CWT[[k]]$Imp4_4)
  Cohort[[k]]$Age4.3<-(Cohort[[k]]$Age4.4/(1-0.0184))
  
  #3 Year Old Spawners
  Cohort[[k]]$Age4.2<-(Cohort[[k]]$Age3Sp+Cohort[[k]]$Age3Hat+Cohort[[k]]$InRiver3+Cohort[[k]]$Age4.3)/(1-0.0184)
  
  #Second Year at Sea. 3 year olds are caught. 
  Cohort[[k]]$Age4.1<-Cohort[[k]]$Age4.2/(1-0.0184)
  Cohort[[k]]$Age3.12<-Cohort[[k]]$Age4.1/(1-0.0184)
  Cohort[[k]]$Age3.11<-(Cohort[[k]]$Age3.12/(1-0.0184))/(1-CWT[[k]]$Imp3_11)
  Cohort[[k]]$Age3.10<-(Cohort[[k]]$Age3.11/(1-0.0184))/(1-CWT[[k]]$Imp3_10)
  Cohort[[k]]$Age3.9<-(Cohort[[k]]$Age3.10/(1-0.0184))/(1-CWT[[k]]$Imp3_9)
  Cohort[[k]]$Age3.8<-(Cohort[[k]]$Age3.9/(1-0.0184))/(1-CWT[[k]]$Imp3_8)
  Cohort[[k]]$Age3.7<-(Cohort[[k]]$Age3.8/(1-0.0184))/(1-CWT[[k]]$Imp3_7)
  Cohort[[k]]$Age3.6<-(Cohort[[k]]$Age3.7/(1-0.0184))/(1-CWT[[k]]$Imp3_6)
  Cohort[[k]]$Age3.5<-(Cohort[[k]]$Age3.6/(1-0.0184))/(1-CWT[[k]]$Imp3_5)
  Cohort[[k]]$Age3.4<-(Cohort[[k]]$Age3.5/(1-0.0184))/(1-CWT[[k]]$Imp3_4)
  Cohort[[k]]$Age3.3<-Cohort[[k]]$Age3.4/(1-0.0184)
  #2 Year Old Spawners
  Cohort[[k]]$Age3.2<-(Cohort[[k]]$Age2Sp+Cohort[[k]]$Age2Hat+Cohort[[k]]$Age3.3)/(1-0.0561)
  
  #First Year at Sea
  Cohort[[k]]$Age3.1<-Cohort[[k]]$Age3.2/(1-0.0561)
  Cohort[[k]]$Age2.12<-Cohort[[k]]$Age3.1/(1-0.0561)
  Cohort[[k]]$Age2.11<-Cohort[[k]]$Age2.12/(1-0.0561)
  Cohort[[k]]$Age2.10<-Cohort[[k]]$Age2.11/(1-0.0561)
  Cohort[[k]]$Age2.9<-Cohort[[k]]$Age2.10/(1-0.0561)
  Cohort[[k]]$Age2.8<-Cohort[[k]]$Age2.9/(1-0.0561)
  Cohort[[k]]$Age2.7<-Cohort[[k]]$Age2.8/(1-0.0561)
  Cohort[[k]]$Age2.6<-Cohort[[k]]$Age2.7/(1-0.0561)
  Cohort[[k]]$Age2.5<-Cohort[[k]]$Age2.6/(1-0.0561)
  Cohort[[k]]$Age2.4<-Cohort[[k]]$Age2.5/(1-0.0561)
  Cohort[[k]]$Age2.3<-Cohort[[k]]$Age2.4/(1-0.0561)

  #Outmigration Survival
  Cohort[[k]]$Out_Survival<-Cohort[[k]]$Age2.3/Cohort[[k]]$Individuals_Released
  #Estimating Maturation
  Cohort[[k]]$Mat4<-(Cohort[[k]]$Age4Sp+Cohort[[k]]$Age4Hat+Cohort[[k]]$InRiver4)/(Cohort[[k]]$Age4Sp+Cohort[[k]]$Age4Hat+Cohort[[k]]$InRiver4+Cohort[[k]]$Age5.3)# Age 5.3 for females only
  Cohort[[k]]$Mat3<-(Cohort[[k]]$Age3Sp+Cohort[[k]]$Age3Hat+Cohort[[k]]$InRiver3)/(Cohort[[k]]$Age3Sp+Cohort[[k]]$Age3Hat+Cohort[[k]]$Age4.3+Cohort[[k]]$InRiver3)
  Cohort[[k]]$Mat2<-(Cohort[[k]]$Age2Sp+Cohort[[k]]$Age2Hat)/(Cohort[[k]]$Age2Sp+Cohort[[k]]$Age2Hat+Cohort[[k]]$Age3.3)
  Cohort[[k]]<-Cohort[[k]][1:16,]
}
saveRDS(Cohort, "CWT Cohort Reconstruction_Females.Rds")
test<-Cohort[[1]]
Maturation_Uncertainty_Bootstrap<-array(NA, c(16,3,1000))
for(i in 1:1000){
  Maturation_Uncertainty_Bootstrap[,1,i]<-Cohort[[i]]$Mat2
  Maturation_Uncertainty_Bootstrap[,2,i]<-Cohort[[i]]$Mat3
  Maturation_Uncertainty_Bootstrap[,3,i]<-Cohort[[i]]$Mat4
}
Maturation_Uncertainty<-matrix(nrow = 16, ncol=9)
for(i in 1:16){
  for(j in 1:3){
    Maturation_Uncertainty[i,c(1+(j-1)*3,2+(j-1)*3,3+(j-1)*3)]<-quantile(Maturation_Uncertainty_Bootstrap[i,j,c(1:1000)], probs=c(.025,.5,.975), na.rm = TRUE) #Mat 2
    Maturation_Uncertainty[i,2]<-mean(Maturation_Uncertainty_Bootstrap[i,1,c(1:1000)])
    Maturation_Uncertainty[i,5]<-mean(Maturation_Uncertainty_Bootstrap[i,2,c(1:1000)])
    Maturation_Uncertainty[i,8]<-mean(Maturation_Uncertainty_Bootstrap[i,3,c(1:1000)])
    
  }
}
brood_year<-c(2001:2016)
Maturation_Uncertainty[is.na(Maturation_Uncertainty)]<-1
Maturation_Uncertainty_F<-as.data.frame(cbind(brood_year,Maturation_Uncertainty))
names(Maturation_Uncertainty_F)<-c("brood_year","Mat2Lower","Mat2Mean",  "Mat2Upper", "Mat3Lower", "Mat3Mean","Mat3Upper", "Mat4Lower", "Mat4Mean","Mat4Upper")
Maturation_Uncertainty_F <- Maturation_Uncertainty_F %>%
  mutate(Sex = "Female")

Maturation_Uncertainty_Sex<-rbind(Maturation_Uncertainty_M, Maturation_Uncertainty_F)
# write.csv(Maturation_Uncertainty_Sex,"Maturation_Uncertainty_CWT_Sex.csv", row.names = FALSE)
