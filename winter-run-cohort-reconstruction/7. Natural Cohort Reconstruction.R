#####################################
### Natural Cohort Reconstruction ###
#####################################

#This script is used for both the general natural cohorts and sex-specific natural cohorts
#In order to have data output from all three (males, females, together), the code needs to be run 3 times
#making small tweaks depending on whether its males only, females only, or together
#The tweaks that need to made occur on lines
#Line 41: Indicate "Males", "Females" or "Together"
#Lines 209-211: uncomment and comment the lines depending on sex
#Lines 230-232:uncomment and comment the lines depending on sex
#Lines 253-255:uncomment and comment the lines depending on sex

library(tidyr)
library(purrr)
library(dplyr)
options(scipen=999)
setwd("~/winter-run-cohort-reconstruction")

#Vital Rates from Hatchery Fish
CWT<-readRDS("CWT Cohort Reconstruction.Rds")
Percent_Harvestable<-readRDS("Avg Harvestability.Rds") #Percent those encountered are harvestable based on location/fishery type of Cwt fish
Percent_ReleaseMort<-readRDS("Avg ReleaseMort.Rds")

#Juvenile Outmigrants
Recruits<-read.csv("Escapement Numbers.csv") #Natural-origin Fry produced
names(Recruits)[1]<-"brood_year"
Recruits<-Recruits %>% 
  map_df(rev) %>%
  filter(brood_year > 2001 & brood_year < 2018)

Male<-readRDS("Male Spawners Adjusted.Rds") 
Female<-readRDS("Female Spawners Adjusted.Rds") 
Together<-Male #Creating same structure to write over
for(i in 1:1000){
  Together[[i]]$Age2Sp<- Males[[i]]$Age2Sp+Females[[i]]$Age2Sp
  Together[[i]]$Age3Sp<- Males[[i]]$Age3Sp+Females[[i]]$Age3Sp
  Together[[i]]$Age4Sp<- Males[[i]]$Age4Sp+Females[[i]]$Age4Sp
}
#Choose based on whether doing males, females, or both
Spawners<-Female #Male, Female, or Together
years<-2002:2015

################################################
#bootstrapping
Cohort<-list()
for(k in 1:1000){
Cohort[[k]]<-Spawners[[k]]

Cohort[[k]]<-left_join(Cohort[[k]], Recruits, by="brood_year")
Cohort[[k]]<-Cohort[[k]] %>%
  select(brood_year,Fry, Age2Sp, Age3Sp, Age4Sp) %>%
  filter(brood_year %in% years) #Analyzing only 2002-2005 brood years, mostly complete cohorts
Percent_Harvestable[[k]]<-Percent_Harvestable[[k]] %>%
  filter(brood_year %in% years)
Percent_ReleaseMort[[k]]<-Percent_ReleaseMort[[k]] %>%
  filter(brood_year %in% years)
CWT[[k]]<-CWT[[k]] %>%
  filter(brood_year %in% years)

#4 Year Old Spawners
Cohort[[k]]$Age5.2<-(Cohort[[k]]$Age4Sp)/(1-0.0184)

#Third Year at Sea. 4 year olds are caught. 
Cohort[[k]]$Age5.1<-Cohort[[k]]$Age5.2/(1-0.0184)
Cohort[[k]]$Age4.12<-Cohort[[k]]$Age5.1/(1-0.0184)
Cohort[[k]]$Age4.11<-(Cohort[[k]]$Age4.12/(1-0.0184))/(1-CWT[[k]]$Cat4_11*Percent_Harvestable[[k]]$Nov4#1 - percent harvest 
                                                       -CWT[[k]]$Cat4_11*.05 #- percent drop mort 
                                                       -CWT[[k]]$Cat4_11*Percent_ReleaseMort[[k]]$Nov4)#- percent release mort
Cohort[[k]]$Age4.10<-(Cohort[[k]]$Age4.11/(1-0.0184))/(1-CWT[[k]]$Cat4_10*Percent_Harvestable[[k]]$Oct4
                                                       -CWT[[k]]$Cat4_10*.05
                                                       -CWT[[k]]$Cat4_10*Percent_ReleaseMort[[k]]$Oct4)
Cohort[[k]]$Age4.9<-(Cohort[[k]]$Age4.10/(1-0.0184))/(1-CWT[[k]]$Cat4_9*Percent_Harvestable[[k]]$Sept4
                                                      -CWT[[k]]$Cat4_9*.05
                                                      -CWT[[k]]$Cat4_9*Percent_ReleaseMort[[k]]$Sept4)
Cohort[[k]]$Age4.8<-(Cohort[[k]]$Age4.9/(1-0.0184))/(1-CWT[[k]]$Cat4_8*Percent_Harvestable[[k]]$Aug4
                                                     -CWT[[k]]$Cat4_8*.05
                                                     -CWT[[k]]$Cat4_8*Percent_ReleaseMort[[k]]$Aug4)
Cohort[[k]]$Age4.7<-(Cohort[[k]]$Age4.8/(1-0.0184))/(1-CWT[[k]]$Cat4_7*Percent_Harvestable[[k]]$Jul4
                                                     -CWT[[k]]$Cat4_7*.05
                                                     -CWT[[k]]$Cat4_7*Percent_ReleaseMort[[k]]$Jul4)
Cohort[[k]]$Age4.6<-(Cohort[[k]]$Age4.7/(1-0.0184))/(1-CWT[[k]]$Cat4_6*Percent_Harvestable[[k]]$Jun4
                                                     -CWT[[k]]$Cat4_6*.05
                                                     -CWT[[k]]$Cat4_6*Percent_ReleaseMort[[k]]$Jun4)
Cohort[[k]]$Age4.5<-(Cohort[[k]]$Age4.6/(1-0.0184))/(1-CWT[[k]]$Cat4_5*Percent_Harvestable[[k]]$May4
                                                     -CWT[[k]]$Cat4_5*.05
                                                     -CWT[[k]]$Cat4_5*Percent_ReleaseMort[[k]]$May4)
Cohort[[k]]$Age4.4<-(Cohort[[k]]$Age4.5/(1-0.0184))/(1-CWT[[k]]$Cat4_4*Percent_Harvestable[[k]]$Apr4
                                                     -CWT[[k]]$Cat4_4*.05
                                                     -CWT[[k]]$Cat4_4*Percent_ReleaseMort[[k]]$Apr4)
Cohort[[k]]$Age4.3<-(Cohort[[k]]$Age4.4/(1-0.0184))

#3 Year Old Spawners
Cohort[[k]]$Age4.2<-(Cohort[[k]]$Age3Sp+Cohort[[k]]$Age4.3)/(1-0.0184)

#Second Year at Sea. 3 year olds are caught. 
Cohort[[k]]$Age4.1<-Cohort[[k]]$Age4.2/(1-0.0184)
Cohort[[k]]$Age3.12<-Cohort[[k]]$Age4.1/(1-0.0184)
Cohort[[k]]$Age3.11<-(Cohort[[k]]$Age3.12/(1-0.0184))/(1-CWT[[k]]$Cat3_11*Percent_Harvestable[[k]]$Nov3 #1 - percent harvest 
                                                       -CWT[[k]]$Cat3_11*.05 #- percent drop mort 
                                                       -CWT[[k]]$Cat3_11*Percent_ReleaseMort[[k]]$Nov3)#- percent release mort
Cohort[[k]]$Age3.10<-(Cohort[[k]]$Age3.11/(1-0.0184))/(1-CWT[[k]]$Cat3_10*Percent_Harvestable[[k]]$Oct3
                                                       -CWT[[k]]$Cat3_10*.05
                                                       -CWT[[k]]$Cat3_10*Percent_ReleaseMort[[k]]$Oct3)
Cohort[[k]]$Age3.9<-(Cohort[[k]]$Age3.10/(1-0.0184))/(1-CWT[[k]]$Cat3_9*Percent_Harvestable[[k]]$Sept3
                                                      -CWT[[k]]$Cat3_9*.05
                                                      -CWT[[k]]$Cat3_9*Percent_ReleaseMort[[k]]$Sept3)
Cohort[[k]]$Age3.8<-(Cohort[[k]]$Age3.9/(1-0.0184))/(1-CWT[[k]]$Cat3_8*Percent_Harvestable[[k]]$Aug3
                                                     -CWT[[k]]$Cat3_8*.05
                                                     -CWT[[k]]$Cat3_8*Percent_ReleaseMort[[k]]$Aug3)
Cohort[[k]]$Age3.7<-(Cohort[[k]]$Age3.8/(1-0.0184))/(1-CWT[[k]]$Cat3_7*Percent_Harvestable[[k]]$Jul3
                                                     -CWT[[k]]$Cat3_7*.05
                                                     -CWT[[k]]$Cat3_7*Percent_ReleaseMort[[k]]$Jul3)
Cohort[[k]]$Age3.6<-(Cohort[[k]]$Age3.7/(1-0.0184))/(1-CWT[[k]]$Cat3_6*Percent_Harvestable[[k]]$Jun3
                                                     -CWT[[k]]$Cat3_6*.05
                                                     -CWT[[k]]$Cat3_6*Percent_ReleaseMort[[k]]$Jun3)
Cohort[[k]]$Age3.5<-(Cohort[[k]]$Age3.6/(1-0.0184))/(1-CWT[[k]]$Cat3_5*Percent_Harvestable[[k]]$May3
                                                     -CWT[[k]]$Cat3_5*.05
                                                     -CWT[[k]]$Cat3_5*Percent_ReleaseMort[[k]]$May3)
Cohort[[k]]$Age3.4<-(Cohort[[k]]$Age3.5/(1-0.0184))/(1-CWT[[k]]$Cat3_4*Percent_Harvestable[[k]]$Apr3
                                                     -CWT[[k]]$Cat3_4*.05
                                                     -CWT[[k]]$Cat3_4*Percent_ReleaseMort[[k]]$Apr3)
Cohort[[k]]$Age3.3<-Cohort[[k]]$Age3.4/(1-0.0184)
#2 Year Old Spawners
Cohort[[k]]$Age3.2<-(Cohort[[k]]$Age2Sp+Cohort[[k]]$Age3.3)/(1-0.0561)

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
#######################
#For obtaining the number of natural-origin individuals impacted
Cohort[[k]]$Imp4_11<-(Cohort[[k]]$Age4.11/(1-0.0184))*(CWT[[k]]$Cat4_11*Percent_Harvestable[[k]]$Nov4+
                                                          CWT[[k]]$Cat4_11*.05+
                                                          CWT[[k]]$Cat4_11*Percent_ReleaseMort[[k]]$Nov4)
Cohort[[k]]$Imp4_10<-(Cohort[[k]]$Age4.10/(1-0.0184))*(CWT[[k]]$Cat4_10*Percent_Harvestable[[k]]$Oct4+
                                                         CWT[[k]]$Cat4_10*.05+
                                                         CWT[[k]]$Cat4_10*Percent_ReleaseMort[[k]]$Oct4)
Cohort[[k]]$Imp4_9<-(Cohort[[k]]$Age4.9/(1-0.0184))*(CWT[[k]]$Cat4_9*Percent_Harvestable[[k]]$Sept4+
                                                        CWT[[k]]$Cat4_9*.05+
                                                        CWT[[k]]$Cat4_9*Percent_ReleaseMort[[k]]$Sept4)
Cohort[[k]]$Imp4_8<-(Cohort[[k]]$Age4.8/(1-0.0184))*(CWT[[k]]$Cat4_8*Percent_Harvestable[[k]]$Aug4+
                                                       CWT[[k]]$Cat4_8*.05+
                                                       CWT[[k]]$Cat4_8*Percent_ReleaseMort[[k]]$Aug4)
Cohort[[k]]$Imp4_7<-(Cohort[[k]]$Age4.7/(1-0.0184))*(CWT[[k]]$Cat4_7*Percent_Harvestable[[k]]$Jul4+
                                                       CWT[[k]]$Cat4_7*.05+
                                                       CWT[[k]]$Cat4_7*Percent_ReleaseMort[[k]]$Jul4)
Cohort[[k]]$Imp4_6<-(Cohort[[k]]$Age4.6/(1-0.0184))*(CWT[[k]]$Cat4_6*Percent_Harvestable[[k]]$Jun4+
                                                       CWT[[k]]$Cat4_6*.05+
                                                       CWT[[k]]$Cat4_6*Percent_ReleaseMort[[k]]$Jun4)
Cohort[[k]]$Imp4_5<-(Cohort[[k]]$Age4.5/(1-0.0184))*(CWT[[k]]$Cat4_5*Percent_Harvestable[[k]]$May4+
                                                       CWT[[k]]$Cat4_5*.05+
                                                       CWT[[k]]$Cat4_5*Percent_ReleaseMort[[k]]$May4)
Cohort[[k]]$Imp4_4<-(Cohort[[k]]$Age4.4/(1-0.0184))*(CWT[[k]]$Cat4_4*Percent_Harvestable[[k]]$Apr4+
                                                       CWT[[k]]$Cat4_4*.05+
                                                       CWT[[k]]$Cat4_4*Percent_ReleaseMort[[k]]$Apr4)
Cohort[[k]]$Imp4<-Cohort[[k]]$Imp4_11+Cohort[[k]]$Imp4_10+Cohort[[k]]$Imp4_9+Cohort[[k]]$Imp4_8+
  Cohort[[k]]$Imp4_7+Cohort[[k]]$Imp4_6+Cohort[[k]]$Imp4_5+Cohort[[k]]$Imp4_4

Cohort[[k]]$Imp4rate<-Cohort[[k]]$Imp4/Cohort[[k]]$Age4.3


Cohort[[k]]$Imp3_11<-(Cohort[[k]]$Age3.11/(1-0.0184))*(CWT[[k]]$Cat3_11*Percent_Harvestable[[k]]$Nov3+
                                                         CWT[[k]]$Cat3_11*.05+
                                                         CWT[[k]]$Cat3_11*Percent_ReleaseMort[[k]]$Nov3)
Cohort[[k]]$Imp3_10<-(Cohort[[k]]$Age3.10/(1-0.0184))*(CWT[[k]]$Cat3_10*Percent_Harvestable[[k]]$Oct3+
                                                         CWT[[k]]$Cat3_10*.05+
                                                         CWT[[k]]$Cat3_10*Percent_ReleaseMort[[k]]$Oct3)
Cohort[[k]]$Imp3_9<-(Cohort[[k]]$Age3.9/(1-0.0184))*(CWT[[k]]$Cat3_9*Percent_Harvestable[[k]]$Sept3+
                                                        CWT[[k]]$Cat3_9*.05+
                                                        CWT[[k]]$Cat3_9*Percent_ReleaseMort[[k]]$Sept3)
Cohort[[k]]$Imp3_8<-(Cohort[[k]]$Age3.8/(1-0.0184))*(CWT[[k]]$Cat3_8*Percent_Harvestable[[k]]$Aug3+
                                                       CWT[[k]]$Cat3_8*.05+
                                                       CWT[[k]]$Cat3_8*Percent_ReleaseMort[[k]]$Aug3)
Cohort[[k]]$Imp3_7<-(Cohort[[k]]$Age3.7/(1-0.0184))*(CWT[[k]]$Cat3_7*Percent_Harvestable[[k]]$Jul3+
                                                       CWT[[k]]$Cat3_7*.05+
                                                       CWT[[k]]$Cat3_7*Percent_ReleaseMort[[k]]$Jul3)
Cohort[[k]]$Imp3_6<-(Cohort[[k]]$Age3.6/(1-0.0184))*(CWT[[k]]$Cat3_6*Percent_Harvestable[[k]]$Jun3+
                                                       CWT[[k]]$Cat3_6*.05+
                                                       CWT[[k]]$Cat3_6*Percent_ReleaseMort[[k]]$Jun3)
Cohort[[k]]$Imp3_5<-(Cohort[[k]]$Age3.5/(1-0.0184))*(CWT[[k]]$Cat3_5*Percent_Harvestable[[k]]$May3+
                                                       CWT[[k]]$Cat3_5*.05+
                                                       CWT[[k]]$Cat3_5*Percent_ReleaseMort[[k]]$May3)
Cohort[[k]]$Imp3_4<-(Cohort[[k]]$Age3.4/(1-0.0184))*(CWT[[k]]$Cat3_4*Percent_Harvestable[[k]]$Apr3+
                                                       CWT[[k]]$Cat3_4*.05+
                                                       CWT[[k]]$Cat3_4*Percent_ReleaseMort[[k]]$Apr3)
Cohort[[k]]$Imp3<-Cohort[[k]]$Imp3_11+Cohort[[k]]$Imp3_10+Cohort[[k]]$Imp3_9+Cohort[[k]]$Imp3_8+
  Cohort[[k]]$Imp3_7+Cohort[[k]]$Imp3_6+Cohort[[k]]$Imp3_5+Cohort[[k]]$Imp3_4

Cohort[[k]]$Imp3rate<-Cohort[[k]]$Imp3/Cohort[[k]]$Age3.3

#Outmigration Survival
Cohort[[k]]$Out_Survival<-Cohort[[k]]$Age2.3/Cohort[[k]]$Fry
#Estimating Maturation
Cohort[[k]]$Mat4<-1
Cohort[[k]]$Mat3<-Cohort[[k]]$Age3Sp/(Cohort[[k]]$Age3Sp+Cohort[[k]]$Age4.3) #Sp includes both In-River and those taken to hatchery
Cohort[[k]]$Mat2<-Cohort[[k]]$Age2Sp/(Cohort[[k]]$Age2Sp+Cohort[[k]]$Age3.3) #Sp includes both In-River and those taken to hatchery
}
#####
test<-CWT[[1]]
# saveRDS(Cohort, "Natural Cohort Reconstruction.Rds")
# saveRDS(Cohort, "Natural Cohort Reconstruction_Male.Rds")
# saveRDS(Cohort, "Natural Cohort Reconstruction_Female.Rds")

Maturation_Uncertainty_Bootstrap<-array(NA, c(14,3,1000))
for(i in 1:1000){
  Maturation_Uncertainty_Bootstrap[,1,i]<-Cohort[[i]]$Mat2
  Maturation_Uncertainty_Bootstrap[,2,i]<-Cohort[[i]]$Mat3
  Maturation_Uncertainty_Bootstrap[,3,i]<-Cohort[[i]]$Mat4
}
Maturation_Uncertainty_Bootstrap[,,1]
Maturation_Uncertainty<-matrix(nrow = 14, ncol=9)
for(i in 1:14){
  for(j in 1:3){
    Maturation_Uncertainty[i,c(1+(j-1)*3,2+(j-1)*3,3+(j-1)*3)]<-quantile(Maturation_Uncertainty_Bootstrap[i,j,c(1:1000)], probs=c(.025,.5,.975), na.rm = TRUE) #Mat 2
    Maturation_Uncertainty[i, j*3-1]<-mean(Maturation_Uncertainty_Bootstrap[i,j,c(1:1000)])
  }
}
brood_year<-c(2002:2015)
Maturation_Uncertainty<-as.data.frame(cbind(brood_year,Maturation_Uncertainty))
names(Maturation_Uncertainty)<-c("brood_year","Mat2Lower","MeanMat2", "Mat2Upper", "Mat3Lower", "MeanMat3", "Mat3Upper", "Mat4Lower", "Mat4","Mat4Upper")
# write.csv(Maturation_Uncertainty,"Maturation_Uncertainty_Natural.csv", row.names = FALSE)
# write.csv(Maturation_Uncertainty,"Maturation_Uncertainty_Natural_Male.csv", row.names = FALSE)
# write.csv(Maturation_Uncertainty,"Maturation_Uncertainty_Natural_Female.csv", row.names = FALSE)

#Impacted Individuals
Impacted_Bootstrap<-array(NA, c(14,4,1000))
for(i in 1:1000){
  Impacted_Bootstrap[,1,i]<-Cohort[[i]]$Imp3
  Impacted_Bootstrap[,2,i]<-Cohort[[i]]$Imp4
  Impacted_Bootstrap[,3,i]<-Cohort[[i]]$Imp3rate
  Impacted_Bootstrap[,4,i]<-Cohort[[i]]$Imp4rate
}
Impacted<-matrix(nrow = 14, ncol=12)
for(i in 1:14){
  for(j in 1:4){
    Impacted[i,c(j+2*(j-1),j*2+1*(j-1),j*3)]<-quantile(Impacted_Bootstrap[i,j,c(1:1000)], probs=c(.025,.5,.975), na.rm = TRUE) 
    Impacted[i, j*3-1]<-mean(Impacted_Bootstrap[i,j,c(1:1000)])
    
     }
}
brood_year<-c(2002:2015)
Impacted<-as.data.frame(cbind(brood_year,Impacted))
names(Impacted)<-c("brood_year","Impact3Lower","Impact3Mid", "Impact3Upper", "Impact4Lower", "Impact4Mid","Impact4Upper","Impactrate3Lower","Impactrate3Mid", "Impactrate3Upper", "Impactrate4Lower", "Impactrate4Mid","Impactrate4Upper")
# write.csv(Impacted,"Impact_Uncertainty_Natural.csv", row.names = FALSE)
# write.csv(Impacted,"Impact_Uncertainty_Natural_Male.csv", row.names = FALSE)
# write.csv(Impacted,"Impact_Uncertainty_Natural_Female.csv", row.names = FALSE)
