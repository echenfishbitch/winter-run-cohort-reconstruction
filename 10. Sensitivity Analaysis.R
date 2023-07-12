###############################################
######     Sensitivity Analysis        ########
###############################################
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

setwd("~/winter-run-cohort-reconstruction-main")

#############################################
#####        Release data           #########
#############################################
CWT_Releases<-read.csv("CWTReleased.csv")
#turning NAs into 0 for addition
CWT_Releases$cwt_1st_mark_count[is.na(CWT_Releases$cwt_1st_mark_count)] <- 0 
CWT_Releases$cwt_2nd_mark_count[is.na(CWT_Releases$cwt_2nd_mark_count)] <- 0
CWT_Releases$non_cwt_1st_mark_count[is.na(CWT_Releases$non_cwt_1st_mark_count)] <- 0
CWT_Releases$non_cwt_2nd_mark_count[is.na(CWT_Releases$non_cwt_2nd_mark_count)] <- 0

#Phi is the proportion of released fish that have CWT and Ad Clips
#Fish with CWT and Ad Clip: cwt_1st_mark_count
#Fish with CWT and No clip: cwt_2nd_mark_count
#Fish with no CWT and Ad Clip: non_cwt_1st_mark_count
#Fish with no CWT and No Clip: non_cwt_2nd_mark_count
CWT_Releases$Total_Released<-(CWT_Releases$cwt_1st_mark_count+ CWT_Releases$cwt_2nd_mark_count+ CWT_Releases$non_cwt_1st_mark_count+ CWT_Releases$non_cwt_2nd_mark_count)
CWT_Releases$Phi <- CWT_Releases$cwt_1st_mark_count/CWT_Releases$Total_Released
CWT_Recoveries<-read.csv("CWTRecoveries.csv")
names(CWT_Releases)[7]<-"tag_code"
Phi_by_tag<-CWT_Releases %>% #phi for each batch
  select(tag_code, Phi)
CWT_Recoveries<-left_join(CWT_Recoveries, Phi_by_tag, by="tag_code")

#############################################
############## Ocean Harvest ################
#############################################
SiteCodes<-read.csv("sitearea.modified.csv")
SiteCodes<-SiteCodes %>%
  select(sampling_site,area.1)
colnames(SiteCodes)[2]<-"Location"
Size_Limits<-read.csv("Size_limits.csv")  
SizeAge<-read.csv("length.at.age.csv")
Release_mort<-read.csv("release.mort.rate.csv")
Release_mort<-Release_mort[,1:5] #removing comments. Most recent year is missing estimate. Commercial standard .26 during those years. Recreational standard .14 during those years. 
#Adjustments for varying release mortality
#Commercial fisheries +/- 2 percent
#Recreational fisheries +/- 6 percent
Release_mort<-Release_mort %>%
  mutate(upperMort = ifelse(fishery == 10, Release.mort.rate + .02, Release.mort.rate + .06)) %>%
  mutate(lowerMort = ifelse(fishery == 10, Release.mort.rate - .02, Release.mort.rate - .06)) 
#Adjustments for varying size at age by 1.8 inches
SizeAge$upper<-SizeAge$mean + 1.8 #increasing size at age each month by 1 SD reported in Satterthwaite et al. 2012
SizeAge$lower <-SizeAge$mean - 1.8 #decreasing size at age each month by 1 SD reported in Satterthwaite et al. 2012

######################
#Point estimates
CWT_Recoveries_Ocean<-CWT_Recoveries %>% #adding Month of record
  mutate(Month =month(ymd(CWT_Recoveries$recovery_date))) %>% 
  mutate(Age = run_year-brood_year) %>%
  filter(fishery == 10|fishery == 40) %>%
  mutate(Harvested = estimated_number/Phi) %>% #How many fish each CWT fish represents = 1/(estimated number (estimated sampling effort) * phi)
  left_join(SiteCodes) %>% #Appending region for each recovery location name. 
  left_join(Size_Limits) %>% #Appending size limit based on year, fishery type, region, and month
  left_join(Release_mort)#Appending release mortality based on year, fishery type, region, and month

#Function: Present Harvest requires Location (e.g. FB, SF, MO), Month, Brood Year, Run Year, and size-at-age scenario ("mean", "lower", "upper")
#produces the percentage of fish in that class that can be taken by the fishery. 

Percent_Harvest_sensitivity<-function(Month, Age, Size_Limit, scenario){
if(scenario == "mean"){
    1-pnorm(Size_Limit, mean = SizeAge$mean[which(SizeAge$age == Age+1 & SizeAge$month == Month)], sd = SizeAge$sd[which(SizeAge$age == Age+1 & SizeAge$month == Month)])
  }
else if(scenario == "lower"){
  1-pnorm(Size_Limit, mean = SizeAge$lower[which(SizeAge$age == Age+1 & SizeAge$month == Month)], sd = SizeAge$sd[which(SizeAge$age == Age+1 & SizeAge$month == Month)])
}
else if(scenario == "upper"){
  1-pnorm(Size_Limit, mean = SizeAge$upper[which(SizeAge$age == Age+1 & SizeAge$month == Month)], sd = SizeAge$sd[which(SizeAge$age == Age+1 & SizeAge$month == Month)])
}
else(NA)
}
#Calculating percent harvestable, catch, release and drop mortality and total impact for all three size-at-age scenarios
CWT_Recoveries_Ocean$Percent_Harvestable_mean<-as.numeric(as.character(mapply(Percent_Harvest_sensitivity, CWT_Recoveries_Ocean$Month, CWT_Recoveries_Ocean$Age, CWT_Recoveries_Ocean$limit, "mean")))
CWT_Recoveries_Ocean$Percent_Harvestable_lower<-as.numeric(as.character(mapply(Percent_Harvest_sensitivity, CWT_Recoveries_Ocean$Month, CWT_Recoveries_Ocean$Age, CWT_Recoveries_Ocean$limit, "lower")))
CWT_Recoveries_Ocean$Percent_Harvestable_upper<-as.numeric(as.character(mapply(Percent_Harvest_sensitivity, CWT_Recoveries_Ocean$Month, CWT_Recoveries_Ocean$Age, CWT_Recoveries_Ocean$limit, "upper")))
####### Impact numbers for all scenarios ###############
#Mean size-at-age, mean release mortality
Impact_mean<-CWT_Recoveries_Ocean %>%
  mutate(Percent_Harvestable_mean = ifelse(Percent_Harvestable_mean < .01, .01, Percent_Harvestable_mean)) %>%
  mutate(Catch=Harvested/Percent_Harvestable_mean) %>% #Catch is C=H/p
  mutate(Release_Mort = (Catch-Harvested)*Release.mort.rate) %>% #Percent of those caught but not harvested that die after release (Standard 26% catch and release mortality-commerical fishery)
  mutate(Drop_Mort = (Catch)*.05) %>% #Drop off mortality 5% of Catch
  mutate(Impact = Harvested+Release_Mort+Drop_Mort) %>% # I = H+S+D
  group_by(brood_year, run_year, Month, Age) %>%
  summarise(Tags_Collected = n(),Catch =sum (Catch),Harvested=sum(Harvested), Release_Mort=sum(Release_Mort), Drop_Mort=sum(Drop_Mort), Impact=sum(Impact)) %>%
  pivot_wider(names_from = c(Age,Month), values_from = Impact, names_sort=TRUE)%>%
  group_by(brood_year) %>%
  summarize(Apr3 = sum(`2_4`, na.rm = TRUE), May3 = sum(`2_5`, na.rm = TRUE), Jun3= sum(`2_6`, na.rm = TRUE)
            , Jul3 = sum(`2_7`, na.rm = TRUE), Aug3 = sum(`2_8`, na.rm = TRUE), Sept3 = sum(`2_9`, na.rm = TRUE),
            Oct3 = sum(`2_10`, na.rm = TRUE), Nov3 = sum(`2_11`, na.rm = TRUE), Apr4 = sum(`3_4`, na.rm = TRUE), 
            May4 = sum(`3_5`, na.rm = TRUE), Jun4= sum(`3_6`, na.rm = TRUE) , Jul4 = sum(`3_7`, na.rm = TRUE), 
            Aug4 = sum(`3_8`, na.rm = TRUE), Sept4 = sum(`3_9`, na.rm = TRUE), Oct4 = sum(`3_10`, na.rm = TRUE), 
            Nov4 = sum(`3_11`, na.rm = TRUE))
#Small size-at-age, mean release mortality
Impact_Sizelower<-CWT_Recoveries_Ocean %>%
  mutate(Percent_Harvestable_lower = ifelse(Percent_Harvestable_lower < .01, .01, Percent_Harvestable_lower)) %>%
  mutate(Catch=Harvested/Percent_Harvestable_lower) %>% #Catch is C=H/p
  mutate(Release_Mort = (Catch-Harvested)*Release.mort.rate) %>% #Percent of those caught but not harvested that die after release (Standard 26% catch and release mortality-commerical fishery)
  mutate(Drop_Mort = (Catch)*.05) %>% #Drop off mortality 5% of Catch
  mutate(Impact = Harvested+Release_Mort+Drop_Mort) %>% # I = H+S+D
  group_by(brood_year, run_year, Month, Age) %>%
  summarise(Tags_Collected = n(),Catch =sum (Catch),Harvested=sum(Harvested), Release_Mort=sum(Release_Mort), Drop_Mort=sum(Drop_Mort), Impact=sum(Impact)) %>%
  pivot_wider(names_from = c(Age,Month), values_from = Impact, names_sort=TRUE)%>%
  group_by(brood_year) %>%
  summarize(Apr3 = sum(`2_4`, na.rm = TRUE), May3 = sum(`2_5`, na.rm = TRUE), Jun3= sum(`2_6`, na.rm = TRUE)
            , Jul3 = sum(`2_7`, na.rm = TRUE), Aug3 = sum(`2_8`, na.rm = TRUE), Sept3 = sum(`2_9`, na.rm = TRUE),
            Oct3 = sum(`2_10`, na.rm = TRUE), Nov3 = sum(`2_11`, na.rm = TRUE), Apr4 = sum(`3_4`, na.rm = TRUE), 
            May4 = sum(`3_5`, na.rm = TRUE), Jun4= sum(`3_6`, na.rm = TRUE) , Jul4 = sum(`3_7`, na.rm = TRUE), 
            Aug4 = sum(`3_8`, na.rm = TRUE), Sept4 = sum(`3_9`, na.rm = TRUE), Oct4 = sum(`3_10`, na.rm = TRUE), 
            Nov4 = sum(`3_11`, na.rm = TRUE))
#Large size-at-age, mean release mortality
Impact_Sizeupper<-CWT_Recoveries_Ocean %>%
  mutate(Percent_Harvestable_upper = ifelse(Percent_Harvestable_upper < .01, .01, Percent_Harvestable_upper)) %>%
  mutate(Catch=Harvested/Percent_Harvestable_upper) %>% #Catch is C=H/p
  mutate(Release_Mort = (Catch-Harvested)*Release.mort.rate) %>% #Percent of those caught but not harvested that die after release (Standard 26% catch and release mortality-commerical fishery)
  mutate(Drop_Mort = (Catch)*.05) %>% #Drop off mortality 5% of Catch
  mutate(Impact = Harvested+Release_Mort+Drop_Mort) %>% # I = H+S+D
  group_by(brood_year, run_year, Month, Age) %>%
  summarise(Tags_Collected = n(),Catch =sum (Catch),Harvested=sum(Harvested), Release_Mort=sum(Release_Mort), Drop_Mort=sum(Drop_Mort), Impact=sum(Impact)) %>%
  pivot_wider(names_from = c(Age,Month), values_from = Impact, names_sort=TRUE) %>%
  group_by(brood_year) %>%
  summarize(Apr3 = sum(`2_4`, na.rm = TRUE), May3 = sum(`2_5`, na.rm = TRUE), Jun3= sum(`2_6`, na.rm = TRUE)
            , Jul3 = sum(`2_7`, na.rm = TRUE), Aug3 = sum(`2_8`, na.rm = TRUE), Sept3 = sum(`2_9`, na.rm = TRUE),
            Oct3 = sum(`2_10`, na.rm = TRUE), Nov3 = sum(`2_11`, na.rm = TRUE), Apr4 = sum(`3_4`, na.rm = TRUE), 
            May4 = sum(`3_5`, na.rm = TRUE), Jun4= sum(`3_6`, na.rm = TRUE) , Jul4 = sum(`3_7`, na.rm = TRUE), 
            Aug4 = sum(`3_8`, na.rm = TRUE), Sept4 = sum(`3_9`, na.rm = TRUE), Oct4 = sum(`3_10`, na.rm = TRUE), 
            Nov4 = sum(`3_11`, na.rm = TRUE))
#Mean size-at-age, higher release mortality
Impact_Mortupper<-CWT_Recoveries_Ocean %>%
  mutate(Percent_Harvestable_mean = ifelse(Percent_Harvestable_mean < .01, .01, Percent_Harvestable_mean)) %>%
  mutate(Catch=Harvested/Percent_Harvestable_mean) %>% #Catch is C=H/p
  mutate(Release_Mort = (Catch-Harvested)*upperMort) %>% #Percent of those caught but not harvested that die after release (Standard 26% catch and release mortality-commerical fishery)
  mutate(Drop_Mort = (Catch)*.05) %>% #Drop off mortality 5% of Catch
  mutate(Impact = Harvested+Release_Mort+Drop_Mort) %>% # I = H+S+D
  group_by(brood_year, run_year, Month, Age) %>%
  summarise(Tags_Collected = n(),Catch =sum (Catch),Harvested=sum(Harvested), Release_Mort=sum(Release_Mort), Drop_Mort=sum(Drop_Mort), Impact=sum(Impact)) %>%
  pivot_wider(names_from = c(Age,Month), values_from = Impact, names_sort=TRUE)%>%
  group_by(brood_year) %>%
  summarize(Apr3 = sum(`2_4`, na.rm = TRUE), May3 = sum(`2_5`, na.rm = TRUE), Jun3= sum(`2_6`, na.rm = TRUE)
            , Jul3 = sum(`2_7`, na.rm = TRUE), Aug3 = sum(`2_8`, na.rm = TRUE), Sept3 = sum(`2_9`, na.rm = TRUE),
            Oct3 = sum(`2_10`, na.rm = TRUE), Nov3 = sum(`2_11`, na.rm = TRUE), Apr4 = sum(`3_4`, na.rm = TRUE), 
            May4 = sum(`3_5`, na.rm = TRUE), Jun4= sum(`3_6`, na.rm = TRUE) , Jul4 = sum(`3_7`, na.rm = TRUE), 
            Aug4 = sum(`3_8`, na.rm = TRUE), Sept4 = sum(`3_9`, na.rm = TRUE), Oct4 = sum(`3_10`, na.rm = TRUE), 
            Nov4 = sum(`3_11`, na.rm = TRUE))
#Mean size-at-age, lower release mortality
Impact_Mortlower<-CWT_Recoveries_Ocean %>%
  mutate(Percent_Harvestable_mean = ifelse(Percent_Harvestable_mean < .01, .01, Percent_Harvestable_mean)) %>%
  mutate(Catch=Harvested/Percent_Harvestable_mean) %>% #Catch is C=H/p
  mutate(Release_Mort = (Catch-Harvested)*lowerMort) %>% #Percent of those caught but not harvested that die after release (Standard 26% catch and release mortality-commerical fishery)
  mutate(Drop_Mort = (Catch)*.05) %>% #Drop off mortality 5% of Catch
  mutate(Impact = Harvested+Release_Mort+Drop_Mort) %>% # I = H+S+D
  group_by(brood_year, run_year, Month, Age) %>%
  summarise(Tags_Collected = n(),Catch =sum (Catch),Harvested=sum(Harvested), Release_Mort=sum(Release_Mort), Drop_Mort=sum(Drop_Mort), Impact=sum(Impact)) %>%
  pivot_wider(names_from = c(Age,Month), values_from = Impact, names_sort=TRUE)%>%
  group_by(brood_year) %>%
  summarize(Apr3 = sum(`2_4`, na.rm = TRUE), May3 = sum(`2_5`, na.rm = TRUE), Jun3= sum(`2_6`, na.rm = TRUE)
            , Jul3 = sum(`2_7`, na.rm = TRUE), Aug3 = sum(`2_8`, na.rm = TRUE), Sept3 = sum(`2_9`, na.rm = TRUE),
            Oct3 = sum(`2_10`, na.rm = TRUE), Nov3 = sum(`2_11`, na.rm = TRUE), Apr4 = sum(`3_4`, na.rm = TRUE), 
            May4 = sum(`3_5`, na.rm = TRUE), Jun4= sum(`3_6`, na.rm = TRUE) , Jul4 = sum(`3_7`, na.rm = TRUE), 
            Aug4 = sum(`3_8`, na.rm = TRUE), Sept4 = sum(`3_9`, na.rm = TRUE), Oct4 = sum(`3_10`, na.rm = TRUE), 
            Nov4 = sum(`3_11`, na.rm = TRUE))
#Mean size-at-age, no release mortality
Impact_Mortnone<-CWT_Recoveries_Ocean %>%
  mutate(Percent_Harvestable_mean = ifelse(Percent_Harvestable_mean < .01, .01, Percent_Harvestable_mean)) %>%
  mutate(Catch=Harvested/Percent_Harvestable_mean) %>% #Catch is C=H/p
  mutate(Release_Mort = (Catch-Harvested)*0) %>% #Percent of those caught but not harvested that die after release (Standard 26% catch and release mortality-commerical fishery)
  mutate(Drop_Mort = (Catch)*.05) %>% #Drop off mortality 5% of Catch
  mutate(Impact = Harvested+Release_Mort+Drop_Mort) %>% # I = H+S+D
  group_by(brood_year, run_year, Month, Age) %>%
  summarise(Tags_Collected = n(),Catch =sum (Catch),Harvested=sum(Harvested), Release_Mort=sum(Release_Mort), Drop_Mort=sum(Drop_Mort), Impact=sum(Impact)) %>%
  pivot_wider(names_from = c(Age,Month), values_from = Impact, names_sort=TRUE)%>%
  group_by(brood_year) %>%
  summarize(Apr3 = sum(`2_4`, na.rm = TRUE), May3 = sum(`2_5`, na.rm = TRUE), Jun3= sum(`2_6`, na.rm = TRUE)
            , Jul3 = sum(`2_7`, na.rm = TRUE), Aug3 = sum(`2_8`, na.rm = TRUE), Sept3 = sum(`2_9`, na.rm = TRUE),
            Oct3 = sum(`2_10`, na.rm = TRUE), Nov3 = sum(`2_11`, na.rm = TRUE), Apr4 = sum(`3_4`, na.rm = TRUE), 
            May4 = sum(`3_5`, na.rm = TRUE), Jun4= sum(`3_6`, na.rm = TRUE) , Jul4 = sum(`3_7`, na.rm = TRUE), 
            Aug4 = sum(`3_8`, na.rm = TRUE), Sept4 = sum(`3_9`, na.rm = TRUE), Oct4 = sum(`3_10`, na.rm = TRUE), 
            Nov4 = sum(`3_11`, na.rm = TRUE))
###############################################
############## CWT Reconstruction #############
###############################################
To_SG<-read.csv("CWT to SG.csv") #Point estimate escapement by Year and Age using CWT expansions
To_Hatchery<-read.csv("Escapement to Hatchery.csv") #Hatchery Escapement
River_Harvest<-read.csv("River Harvest.csv")
## **The code below (Lines 203 - 291) needs to be repeated for every scenario** ##
#uncomment
#Line 210&211 for Mean size-at-age, mean mortality 
#Line 213&214 for Small size-at-age, mean mortality 
#Line 216&217 for Large size-at-age, mean mortality 
#Line 219&220 for Mean size-at-age, lower mortality 
#Line 222&223 for Mean size-at-age, higher mortality 
#Line 225&226 for Mean size-at-age, no mortality 
Cohort<-To_SG %>% 
  left_join(Recruits, by = "brood_year") %>%#merging with recruits
  select(brood_year, Age1Sp, Age2Sp,Age3Sp,Age4Sp,Age5Sp) %>% #Together
  filter(brood_year != 1998 & brood_year != 1999& brood_year != 2000) %>%
  left_join(To_Hatchery, by="brood_year") %>%
  left_join(River_Harvest,by="brood_year") %>%
  select(-run_year) %>%
  # left_join(Impact_mean,by="brood_year") %>%
  # mutate(scenario = "mean") %>%
#For exploring Size at age lower bound
  # left_join(Impact_Sizelower,by="brood_year") %>%
  # mutate(scenario = "-1 SD") %>%
#For exploring Size at age upper bound
  # left_join(Impact_Sizeupper,by="brood_year") %>%
  # mutate(scenario = "+1 SD") %>%
#For exploring release mortality lower bound
  # left_join(Impact_Mortlower,by="brood_year") %>%
  # mutate(scenario = "-%") %>%
# For exploring release mortality upper bound
# left_join(Impact_Mortupper,by="brood_year") %>%
#   mutate(scenario = "+%") %>%
#For exploring without release mortality 
  # left_join(Impact_Mortnone,by="brood_year") %>%
  # mutate(scenario = "no release mortality") %>%
  select(c("brood_year","Age1Sp","Age2Sp","Age3Sp","Age4Sp","Age5Sp","Age1Hat","Age2Hat","Age3Hat","Age4Hat", "InRiver3", "InRiver4", "scenario",
           "Apr3","May3","Jun3","Jul3","Aug3","Sept3","Oct3","Nov3","Apr4","May4","Jun4","Jul4","Aug4","Sept4","Oct4","Nov4"))
Cohort[is.na(Cohort)] <- 0

RunSize<-as.data.frame(cbind(2006:2016, rep(NA,11), rep(NA, 11), rep(NA, 11), rep(NA, 11), rep(NA, 11), rep(NA, 11), rep(NA, 11), rep(NA, 11)))
RunSize<-cbind(RunSize, rep(Cohort$scenario[1], 11))
#Cohort reconstruction
  # 5 Year Old Spawners 
Cohort$Age5.3<-Cohort$Age5Sp/(1-0.2)
  #4 Year Old Spawners 
  Cohort$Age5.2<-(Cohort$Age4Sp+Cohort$Age5.3+Cohort$Age4Hat+Cohort$InRiver4)/(1-0.0184)
  
  #Third Year at Sea. 4 year olds are caught. (monthly)
  Cohort$Age5.1<-Cohort$Age5.2/(1-0.0184)
  Cohort$Age4.12<-Cohort$Age5.1/(1-0.0184)
  Cohort$Age4.11<-Cohort$Age4.12/(1-0.0184)+Cohort$Nov4
  Cohort$Age4.10<-Cohort$Age4.11/(1-0.0184)+Cohort$Oct4
  Cohort$Age4.9<-Cohort$Age4.10/(1-0.0184)+Cohort$Sept4
  Cohort$Age4.8<-Cohort$Age4.9/(1-0.0184)+Cohort$Aug4
  Cohort$Age4.7<-Cohort$Age4.8/(1-0.0184)+Cohort$Jul4
  Cohort$Age4.6<-Cohort$Age4.7/(1-0.0184)+Cohort$Jun4
  Cohort$Age4.5<-Cohort$Age4.6/(1-0.0184)+Cohort$May4
  Cohort$Age4.4<-Cohort$Age4.5/(1-0.0184)+Cohort$Apr4
  Cohort$Age4.3<-Cohort$Age4.4/(1-0.0184)
  
  #3 Year Old Spawners and River Harvest(monthly)
  Cohort$Age4.2<-(Cohort$Age3Sp+Cohort$Age4.3+Cohort$Age3Hat+Cohort$InRiver3)/(1-0.0184)
  
  #Second Year at Sea. 3 year olds are caught.(monthly) 
  Cohort$Age4.1<-Cohort$Age4.2/(1-0.0184)
  Cohort$Age3.12<-Cohort$Age4.1/(1-0.0184)
  Cohort$Age3.11<-Cohort$Age3.12/(1-0.0184)+Cohort$Nov3
  Cohort$Age3.10<-Cohort$Age3.11/(1-0.0184)+Cohort$Oct3
  Cohort$Age3.9<-Cohort$Age3.10/(1-0.0184)+Cohort$Sept3
  Cohort$Age3.8<-Cohort$Age3.9/(1-0.0184)+Cohort$Aug3
  Cohort$Age3.7<-Cohort$Age3.8/(1-0.0184)+Cohort$Jul3
  Cohort$Age3.6<-Cohort$Age3.7/(1-0.0184)+Cohort$Jun3
  Cohort$Age3.5<-Cohort$Age3.6/(1-0.0184)+Cohort$May3
  Cohort$Age3.4<-Cohort$Age3.5/(1-0.0184)+Cohort$Apr3
  Cohort$Age3.3<-Cohort$Age3.4/(1-0.0184)
  #Estimating Maturation
  Cohort$Mat4<-(Cohort$Age4Sp+Cohort$Age4Hat+Cohort$InRiver4)/(Cohort$Age4Sp+Cohort$Age4Hat+Cohort$Age5.3+Cohort$InRiver4)
  Cohort$Mat3<-(Cohort$Age3Sp+Cohort$Age3Hat+Cohort$InRiver3)/(Cohort$Age3Sp+Cohort$Age3Hat+Cohort$Age4.3+Cohort$InRiver3)
  Cohort$Mat2<-(Cohort$Age2Sp+Cohort$Age2Hat)/(Cohort$Age2Sp+Cohort$Age2Hat+Cohort$Age3.3)
  #Estimating Impact Rate
  Cohort$Imp4<-rowSums(Cohort[,23:30])/Cohort$Age4.3
  Cohort$Imp3<-rowSums(Cohort[,15:22])/Cohort$Age3.3
  #Estimating SRR
  Cohort$HypoAge4.2<-Cohort$Age3.3*(.9816^11) #Natural mortality at age 3
  Cohort$HypoAge4.3<-Cohort$HypoAge4.2*(1-Cohort$Mat3)*(.9816) #Abundance after age-3 fish spawn
  Cohort$HypoAge3Sp<-Cohort$HypoAge4.2*(Cohort$Mat3)*(.9816) #Age 3 spawners
  Cohort$HypoAge4Sp<-Cohort$HypoAge4.3*(.9816^12) #Natural mortality at age 4. All spawn at age 4
  Cohort[is.na(Cohort)] <- 0
  #By run year
for(i in 1:11){
  RunSize[i,2]<-Cohort$Age2Sp[i+3]+Cohort$Age2Hat[i+3]
  RunSize[i,3]<-Cohort$HypoAge3Sp[i+2]
  RunSize[i,4]<-Cohort$HypoAge4Sp[i+1]
  RunSize[i,5]<-RunSize[i,2]+RunSize[i,3]+RunSize[i,4] #Hypothetical run size
  RunSize[i,6]<-Cohort$Age3Sp[i+2]+Cohort$Age3Hat[i+2]+Cohort$InRiver3[i+2]
  RunSize[i,7]<-Cohort$Age4Sp[i+1] +Cohort$Age4Hat[i+2]+Cohort$InRiver4[i+2]
  RunSize[i,8]<-RunSize[i,2]+RunSize[i,6]+RunSize[i,7] #Observed run size
  RunSize[i,9]<-1-RunSize[i,8]/RunSize[i,5] #SRR
}
colnames(RunSize)[c(1,9,10)]<-c("run_year", "SRR", "scenario")
##########################################
#Store each scenarios output input upper, lower, mean zero
############## Mean size-at-age and mean mortality
# Rates_mean<-Cohort %>%
# select(brood_year,scenario,Imp3, Imp4, Mat2, Mat3)
# SRR_mean<-RunSize %>%
#   select(run_year, SRR, scenario)
############## Lower size-at-age or lower mortality
# Rates_lower<-Cohort %>%
#   select(brood_year,scenario,Imp3, Imp4, Mat2, Mat3)
# SRR_lower<-RunSize %>%
#   select(run_year, SRR, scenario)
############## Higher size-at-age or higher mortality
# Rates_upper<-Cohort %>%
#   select(brood_year,scenario,Imp3, Imp4, Mat2, Mat3)
# SRR_upper<-RunSize %>%
#   select(run_year, SRR, scenario)
############## Mean size-at-age and no release mortality
# Rates_zero<-Cohort %>%
#   select(brood_year,scenario,Imp3, Imp4, Mat2, Mat3)
# SRR_zero<-RunSize %>%
#   select(run_year, SRR, scenario)

#After running all the scenarios for either size-at-age or release mortality, merge outputs to one data frame
############## for merging size-at-age output
# Rates<-rbind(Rates_lower, rbind(Rates_upper, Rates_mean))
# SRR<-rbind(SRR_lower, rbind(SRR_upper,SRR_mean))
# Rates$scenario <- factor(Rates$scenario, levels=c('+1 SD', 'mean', '-1 SD'))
# SRR$scenario <- factor(SRR$scenario, levels=c('+1 SD', 'mean', '-1 SD'))
############## for merging mortality output
Rates<-rbind(Rates_lower, rbind(Rates_upper, rbind(Rates_zero,Rates_mean)))
SRR<-rbind(SRR_lower, rbind(SRR_upper, rbind(SRR_zero,SRR_mean)))
Rates$scenario <- factor(Rates$scenario, levels=c('+%', 'mean', '-%', 'no release mortality'))
SRR$scenario <- factor(SRR$scenario, levels=c('+%', 'mean', '-%', 'no release mortality'))
Rates<-Rates %>%
  filter(brood_year > 2001 & brood_year < 2016)

blank_bg<-theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), axis.line = element_line(colour = "black"))
#Plot evaluating sensitivity of age-3 impact rate
ggplot(Rates, aes(x=brood_year, y=Imp3, group = scenario)) +
  geom_point(aes(shape = scenario), size = 3)+ 
  # scale_shape_manual(values = c(3, 16, 4)) + #for size at age with 3 scenarios
  scale_shape_manual(values = c(3, 16, 4, 1)) + #for release mortality with 4 scenarios
  geom_line()+ 
  ylab("Age-3 impact rate")+ 
  scale_x_continuous("Brood Year", labels = as.character(Rates$brood_year), breaks = Rates$brood_year) +
  ylim(0,1)+
  blank_bg
#Plot evaluating sensitivity of age-4 impact rate
ggplot(Rates, aes(x=brood_year, y=Imp4, group = scenario)) +
  geom_point(aes(shape = scenario), size = 3)+ 
  # scale_shape_manual(values = c(3, 16, 4)) + #for size at age with 3 scenarios
  scale_shape_manual(values = c(3, 16, 4, 1)) + #for release mortality with 4 scenarios
  geom_line()+ 
  ylab("Age-4 impact rate")+ 
  scale_x_continuous("Brood Year", labels = as.character(Rates$brood_year), breaks = Rates$brood_year) +
  ylim(0,1)+
  blank_bg

##Plot evaluating sensitivity of age-2 maturation rate
ggplot(Rates, aes(x=brood_year, y=Mat2, group = scenario)) +
  geom_point(aes(shape = scenario), size = 3)+ 
  scale_shape_manual(values = c(3, 16, 4, 1)) + #for release mortality with 4 scenarios
  # scale_shape_manual(values = c(3, 16, 4)) + #for size at age with 3 scenarios
  geom_line()+ 
  ylab("Age-2 maturation rate")+ 
  scale_x_continuous("Brood Year", labels = as.character(Rates$brood_year), breaks = Rates$brood_year) +
  ylim(0,1)+
  blank_bg
##Plot evaluating sensitivity of age-3 maturation rate
ggplot(Rates, aes(x=brood_year, y=Mat3, group = scenario)) +
  geom_point(aes(shape = scenario), size = 3)+ 
  scale_shape_manual(values = c(3, 16, 4, 1)) + #for release mortality with 4 scenarios
  # scale_shape_manual(values = c(3, 16, 4)) + #for size at age with 3 scenarios
  geom_line()+ 
  ylab("Age-3 maturation rate")+ 
  scale_x_continuous("Brood Year", labels = as.character(Rates$brood_year), breaks = Rates$brood_year) +
  ylim(0,1)+
  blank_bg

##Plot evaluating sensitivity of SRR
ggplot(SRR, aes(x=run_year, y=SRR, group = scenario)) +
  geom_point(aes(shape = scenario), size = 3)+ 
  scale_shape_manual(values = c(3, 16, 4, 1)) + #for release mortality with 4 scenarios
  # scale_shape_manual(values = c(3, 16, 4)) + #for size at age with 3 scenarios
  geom_line()+ 
  ylab("Spawner reduction rate")+ 
  scale_x_continuous("Run Year", labels = as.character(SRR$run_year), breaks = SRR$run_year) +
  ylim(0,1)+
  blank_bg
