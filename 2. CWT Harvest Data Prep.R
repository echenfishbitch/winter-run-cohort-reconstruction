###############################################
######     CWT Fishery Data Prep       ########
###############################################
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

setwd("~/winter-run-cohort-reconstruction")

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
CWT_Recoveries<- CWT_Recoveries %>%
  select(reporting_agency,run_year,recovery_date, fishery, tag_code, estimated_number, recovery_location_name,sampling_site , brood_year)

Phi_by_tag<-CWT_Releases %>% #phi for each batch
  select(tag_code, Phi)

CWT_Recoveries<-left_join(CWT_Recoveries, Phi_by_tag, by="tag_code")

CWT_Recoveries <- CWT_Recoveries %>%
  mutate(Value_Expanded = 1/Phi)
#############################################
#############  In-River Harvest #############
#############################################
#if in-river harvest happened in late fall (Nov&Dec), that belongs to the run
#year of the next year
CWT_Recoveries_River<-CWT_Recoveries %>%
  filter(fishery == 46) %>%
  mutate(run_year= ifelse(month(ymd(recovery_date))>= 11, run_year+1,run_year)) %>%
  mutate(Harvested = estimated_number/Phi) %>%
  mutate(Age = run_year-brood_year) %>%
  group_by(run_year, brood_year, Age) %>%
  summarise(Harvested = sum(Harvested)) %>%
  pivot_wider(values_from = "Harvested", names_from = "Age") %>%
  select(run_year,brood_year, `3`,`4`) %>%
  filter(brood_year != 2017)

CWT_Recoveries_River[is.na(CWT_Recoveries_River)]<-0
colnames(CWT_Recoveries_River)[c(3,4)]<-c("InRiver3", "InRiver4")

# write.csv(CWT_Recoveries_River, "River Harvest.csv", row.names = FALSE)
#In River by sex. Assumes the sex ratio of in-river harvest equals sex ratio on spawning grounds + hatchery
#Based on CWT Recoveries on the SG+Hatchery
#2006 BY, 2009 RY 74% female 26% male
#2007 BY, 2010 RY 69% female 31% male
#2011 BY, 2014 RY 76% female, 24% male
#2015 BY, 2019 RY 100% male

CWT_Recoveries_River_M<-CWT_Recoveries_River 
CWT_Recoveries_River_F<-CWT_Recoveries_River 

CWT_Recoveries_River_M$InRiver3<-CWT_Recoveries_River$InRiver3*c(.26,.31,.24,1)
CWT_Recoveries_River_M$InRiver4<-CWT_Recoveries_River$InRiver4*c(1,1,1,1)

CWT_Recoveries_River_F$InRiver3<-CWT_Recoveries_River$InRiver3*c(.74,.69,.76,1)
CWT_Recoveries_River_F$InRiver4<-CWT_Recoveries_River$InRiver4*c(1,1,1,0)

# write.csv(CWT_Recoveries_River_M, "River Harvest_M.csv", row.names = FALSE)
# write.csv(CWT_Recoveries_River_F, "River Harvest_F.csv", row.names = FALSE)


#############################################
############## Ocean Harvest ################
#############################################
SiteCodes<-read.csv("sitearea.modified.csv")
SiteCodes<-SiteCodes %>%
  select(sampling_site,area.1)
colnames(SiteCodes)[2]<-"Location"
Size_Limits<-read.csv("Size_limits.csv")  
Release_mort<-read.csv("release.mort.rate.csv")
Release_mort<-Release_mort[,1:5] #removing comments. Most recent year is missing estimate. Commercial standard .26 during those years. Recreational standard .14 during those years. 
SizeAge<-read.csv("length.at.age.csv")
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

#Commercial Fisheries
CWT_Recoveries.Com<-CWT_Recoveries %>%
  filter(fishery==10)
#Recreation Fisheries
CWT_Recoveries.Rec<-CWT_Recoveries %>%
  filter(fishery==40)

#Function: Present Harvest requires Location (e.g. FB, SF, MO), Month, Brood Year, Run Year, and 
#produces the percentage of fish in that class that can be taken by the fishery. 

Percent_Harvest<-function(Month, Age, Size_Limit){
  1-pnorm(Size_Limit, mean = SizeAge$mean[which(SizeAge$age == Age+1 & SizeAge$month == Month)], sd = SizeAge$sd[which(SizeAge$age == Age+1 & SizeAge$month == Month)])
}

#Calculating percent harvestable, catch, release and drop mortality and total impact
CWT_Recoveries_Ocean$Percent_Harvestable<-as.numeric(as.character(mapply(Percent_Harvest, CWT_Recoveries_Ocean$Month, CWT_Recoveries_Ocean$Age, CWT_Recoveries_Ocean$limit)))
CWT_Recoveries_Ocean<-CWT_Recoveries_Ocean %>%
  mutate(Catch=Harvested/Percent_Harvestable) %>% #Catch is C=H/p
  mutate(Release_Mort = (Catch-Harvested)*Release.mort.rate) %>% #Percent of those caught but not harvested that die after release (Standard 26% catch and release mortality-commerical fishery)
  mutate(Drop_Mort = (Catch)*.05) %>% #Drop off mortality 5% of Catch
  mutate(Impact = Harvested+Release_Mort+Drop_Mort) # I = H+S+D

#Stats by age*month for commercial fishery
Commercial_Harvest<-CWT_Recoveries_Ocean %>%
  filter(fishery == 10) %>%
  group_by(brood_year, run_year, Month) %>%
  summarise(Tags_Collected = n(),Catch =sum (Catch), Harvested=sum(Harvested), Release_Mort=sum(Release_Mort), Drop_Mort=sum(Drop_Mort), Impact=sum(Impact))
#Stats by age*month for recreational fishery
Recreational_Harvest<-CWT_Recoveries_Ocean %>%
  filter(fishery == 40) %>%
  group_by(brood_year, run_year, Month) %>%
  summarise(Tags_Collected = n(),Catch =sum (Catch),Harvested=sum(Harvested), Release_Mort=sum(Release_Mort), Drop_Mort=sum(Drop_Mort), Impact=sum(Impact))

Impact<-CWT_Recoveries_Ocean %>%
  group_by(brood_year, run_year, Month, Age) %>%
  summarise(Tags_Collected = n(),Catch =sum (Catch),Harvested=sum(Harvested), Release_Mort=sum(Release_Mort), Drop_Mort=sum(Drop_Mort), Impact=sum(Impact))

Catch<- Impact%>%
  pivot_wider(names_from = c(Age,Month), values_from = Catch, names_sort=TRUE) 
Catch<-Catch %>%
  group_by(brood_year) %>%
  summarize(Apr3 = sum(`2_4`, na.rm = TRUE), May3 = sum(`2_5`, na.rm = TRUE), Jun3= sum(`2_6`, na.rm = TRUE)
            , Jul3 = sum(`2_7`, na.rm = TRUE), Aug3 = sum(`2_8`, na.rm = TRUE), Sept3 = sum(`2_9`, na.rm = TRUE),
            Oct3 = sum(`2_10`, na.rm = TRUE), Nov3 = sum(`2_11`, na.rm = TRUE), Apr4 = sum(`3_4`, na.rm = TRUE), 
            May4 = sum(`3_5`, na.rm = TRUE), Jun4= sum(`3_6`, na.rm = TRUE) , Jul4 = sum(`3_7`, na.rm = TRUE), 
            Aug4 = sum(`3_8`, na.rm = TRUE), Sept4 = sum(`3_9`, na.rm = TRUE), Oct4 = sum(`3_10`, na.rm = TRUE), 
            Nov4 = sum(`3_11`, na.rm = TRUE))
Impact<- Impact%>%
  pivot_wider(names_from = c(Age,Month), values_from = Impact, names_sort=TRUE) 
Impact<-Impact %>%
  group_by(brood_year) %>%
  summarize(Apr3 = sum(`2_4`, na.rm = TRUE), May3 = sum(`2_5`, na.rm = TRUE), Jun3= sum(`2_6`, na.rm = TRUE)
            , Jul3 = sum(`2_7`, na.rm = TRUE), Aug3 = sum(`2_8`, na.rm = TRUE), Sept3 = sum(`2_9`, na.rm = TRUE),
            Oct3 = sum(`2_10`, na.rm = TRUE), Nov3 = sum(`2_11`, na.rm = TRUE), Apr4 = sum(`3_4`, na.rm = TRUE), 
            May4 = sum(`3_5`, na.rm = TRUE), Jun4= sum(`3_6`, na.rm = TRUE) , Jul4 = sum(`3_7`, na.rm = TRUE), 
            Aug4 = sum(`3_8`, na.rm = TRUE), Sept4 = sum(`3_9`, na.rm = TRUE), Oct4 = sum(`3_10`, na.rm = TRUE), 
            Nov4 = sum(`3_11`, na.rm = TRUE))
#####################
###Bootstrapping. 
#Recreational
CWT_Recoveries_Ocean$Harvested_Sample<-NA
CWT_Recoveries_Ocean$Catch_Sample<-NA
CWT_Recoveries_Ocean$Drop_Sample<-NA
CWT_Recoveries_Ocean$Release_Sample<-NA
CWT_Recoveries_Ocean$Impact_Sample<-NA

CWT_Recoveries.list<-list() #each item in the list will contain a resample of harvest, catch, release mort, impact, drop mort
Harvest.list<-list() #where we summarized Impact data by year, month, age
Catch_MakeUp<-list() #Calculating the make up of encounters along the coast by fishery type in region
#years and months in our analysis. For years and months with 0s
years<-read.csv("years with values.csv")

for(j in 1:1000){
  CWT_Recoveries.list[[j]]<-CWT_Recoveries_Ocean #Having point estimate info within template of bootstrapped sample
  for(i in 1:length(CWT_Recoveries_Ocean$estimated_number)){
#How many fish each tag represents harvested = (1 + unrecovered tags)/Phi 
    CWT_Recoveries.list[[j]]$Harvested_Sample[i]<-as.numeric(sum(c(1,rnbinom(1, 1,1/(CWT_Recoveries_Ocean$estimated_number[i]))),
                                                                     na.rm =TRUE)*CWT_Recoveries_Ocean$Value_Expanded[i])
    CWT_Recoveries.list[[j]]$Catch_Sample[i]<-CWT_Recoveries.list[[j]]$Harvested_Sample[i]/CWT_Recoveries.list[[j]]$Percent_Harvestable[i]
    CWT_Recoveries.list[[j]]$Drop_Sample[i]<-CWT_Recoveries.list[[j]]$Catch_Sample[i]*.05
    CWT_Recoveries.list[[j]]$Release_Sample[i]<-(CWT_Recoveries.list[[j]]$Catch_Sample[i]-CWT_Recoveries.list[[j]]$Harvested_Sample[i])*CWT_Recoveries.list[[j]]$Release.mort.rate[i]
    CWT_Recoveries.list[[j]]$Impact_Sample[i]<-CWT_Recoveries.list[[j]]$Drop_Sample[i]+CWT_Recoveries.list[[j]]$Harvested_Sample[i]+CWT_Recoveries.list[[j]]$Release_Sample[i]
       }
  Harvest.list[[j]]<-full_join(years, CWT_Recoveries.list[[j]], multiple= "all")
  Harvest.list[[j]]<-Harvest.list[[j]]%>%
    group_by(brood_year, run_year, Month, Age) %>%
    summarise(Tags_Collected = n(), Catch_Sample =sum(Catch_Sample), Harvested_Sample=sum(Harvested_Sample), Release_Sample=sum(Release_Sample), Drop_Sample=sum(Drop_Sample), Impact_Sample=sum(Impact_Sample)) %>%
    replace(is.na(.),0) %>%
    mutate(Tags_Collected = ifelse(Catch_Sample ==0, 0, Catch_Sample))
  Total_Catch<-CWT_Recoveries.list[[j]]%>%
    group_by(Month, run_year) %>%
    summarise(Catch_Sample_Total =sum (Catch_Sample))
  Catch_MakeUp[[j]]<-CWT_Recoveries.list[[j]]%>%
    group_by(Location, Month, run_year, fishery) %>%
    summarise(Catch_Sample_MakeUp =sum (Catch_Sample)) %>%
    left_join(Total_Catch) %>%
    mutate(Catch_Sample_Prop = Catch_Sample_MakeUp/Catch_Sample_Total) %>%
    pivot_wider(names_from = c(Location, fishery), values_from = Catch_Sample_Prop)%>%
    group_by(run_year, Month, Catch_Sample_Total) %>%
    summarize(CO.r = sum (CO_40,na.rm = TRUE), KC.r= sum(KC_40,na.rm = TRUE), KO.r=sum(KO_40,na.rm = TRUE),
            MO.r = sum (MO_40,na.rm = TRUE), SF.r= sum(SF_40,na.rm = TRUE), FB.r = sum(FB_40, na.rm = TRUE),
            FB.c = sum(FB_10, na.rm = TRUE), MO.c = sum (MO_10,na.rm = TRUE), SF.c= sum(SF_10,na.rm = TRUE))
  Harvest.list[[j]]<-left_join(Harvest.list[[j]], Catch_MakeUp[[j]])
}
test<-Harvest.list[[1]]
# saveRDS(Harvest.list, file = "Catch Representation.Rds")
#summarising Catches (Encounters) for each BY by month
Catch_Bootstrap<-list()
for (i in 1:1000){
  Catch_Bootstrap[[i]]<-Harvest.list[[i]] %>%
    pivot_wider(names_from = c(Age,Month),values_from= Catch_Sample, names_sort = TRUE) %>%
    group_by(brood_year) %>%
    summarize(Apr3 = sum(`2_4`, na.rm = TRUE), May3 = sum(`2_5`, na.rm = TRUE), Jun3= sum(`2_6`, na.rm = TRUE)
              , Jul3 = sum(`2_7`, na.rm = TRUE), Aug3 = sum(`2_8`, na.rm = TRUE), Sept3 = sum(`2_9`, na.rm = TRUE),
              Oct3 = sum(`2_10`, na.rm = TRUE), Nov3 = sum(`2_11`, na.rm = TRUE), Apr4 = sum(`3_4`, na.rm = TRUE), 
              May4 = sum(`3_5`, na.rm = TRUE), Jun4= sum(`3_6`, na.rm = TRUE) , Jul4 = sum(`3_7`, na.rm = TRUE), 
              Aug4 = sum(`3_8`, na.rm = TRUE), Sept4 = sum(`3_9`, na.rm = TRUE), Oct4 = sum(`3_10`, na.rm = TRUE), 
              Nov4 = sum(`3_11`, na.rm = TRUE))
}
# saveRDS(Catch_Bootstrap, file = "Catch Bootstrap.Rds")

Impact_Bootstrap<-list() #must be done together with Catch bootstrapping
for (i in 1:1000){
  Impact_Bootstrap[[i]]<-Harvest.list[[i]] %>%
    pivot_wider(names_from = c(Age,Month),values_from= Impact_Sample, names_sort = TRUE )%>%
    group_by(brood_year) %>%
    summarize(Apr3 = sum(`2_4`, na.rm = TRUE), May3 = sum(`2_5`, na.rm = TRUE), Jun3= sum(`2_6`, na.rm = TRUE)
              , Jul3 = sum(`2_7`, na.rm = TRUE), Aug3 = sum(`2_8`, na.rm = TRUE), Sept3 = sum(`2_9`, na.rm = TRUE),
              Oct3 = sum(`2_10`, na.rm = TRUE), Nov3 = sum(`2_11`, na.rm = TRUE), Apr4 = sum(`3_4`, na.rm = TRUE), 
              May4 = sum(`3_5`, na.rm = TRUE), Jun4= sum(`3_6`, na.rm = TRUE) , Jul4 = sum(`3_7`, na.rm = TRUE), 
              Aug4 = sum(`3_8`, na.rm = TRUE), Sept4 = sum(`3_9`, na.rm = TRUE), Oct4 = sum(`3_10`, na.rm = TRUE), 
              Nov4 = sum(`3_11`, na.rm = TRUE))
}
test<- Impact_Bootstrap[[1]]

# saveRDS(Impact_Bootstrap, file = "Impact Bootstrap.Rds")

################################
## Catch Representation#########
################################
rep<-readRDS("Catch Representation.Rds") #Or
# rep<-Harvest.list #if earlier script was run
years<-read.csv("years with values.csv") #with all rows for both 3/4 year olds
Release_mort<-read.csv("release.mort.rate.csv")
Release_mort<-Release_mort[,1:5] #removing comments. Most recent year is missing estimate. Commericial standard .26 during those years. Recreational standard .14 during those years.
Size_Limits<-read.csv("Size_limits.csv")  

#Size limit 
Size_Limits<-Size_Limits %>%
  pivot_wider(names_from = c(Location,fishery), values_from = limit) %>%
  select(run_year, Month, CO_40, KC_40, KO_40, MO_40, SF_40, FB_40, MO_10, SF_10, FB_10)
colnames(Size_Limits)<-c("run_year","Month","CO.r_lim","KC.r_lim","KO.r_lim","MO.r_lim","SF.r_lim", "FB.r_lim","MO.c_lim","SF.c_lim","FB.c_lim" )

Release_mort<-Release_mort %>%
  filter(fishery == 40) %>%
  pivot_wider(names_from = c(Location), values_from = Release.mort.rate, names_sort = TRUE) %>%
  select(run_year, Month, CO, FB, KC, KO, MO, SF) %>%
  replace(is.na(.),.14) #for missing ones, using default recreational
colnames(Release_mort)<-c("run_year","Month","CO_mort","FB_mort", "KC_mort","KO_mort","MO_mort","SF_mort")
#always .26 for commercial
Avg_Harvestability<-list()
Avg_ReleaseMort<-list()
BY<-as.data.frame((2001:2017))
colnames(BY)<-"brood_year"
#Calculating the average harvestability of every cohort each month based on the composition of catches spatially and by fishery type
#Calculating the average release mortality rate of every cohort each month based on the composition of catches spatially and by fishery type

#Average is the harvestability of each regioin*type multipled by its weight (Proportion _P)
#Avg_harvestable = Average harvestability of cohort during that month
# calculated by summing [harvestability of each region and type*proportion of that region and type]
#                       Sum(Area.type_P =  Area.type_h*Area.type)
#Avg_releasemort = Average release mortality of cohort during that month, because release mort varies by region and type
# calculated by summing [mortality rate of each region and type*proportion of that region and type]
#                       Sum(Area.type_M =  Area.mort*Area.type)
test<-rep[[1]]
for(i in 1:1000){
  rep[[i]]<-full_join(years,rep[[i]])
  rep[[i]]<-rep[[i]] %>%
    left_join(Size_Limits) %>%
    left_join(Release_mort)
#Area.type_h is the percent harvestable in the region, age, month
#harvestability by region and fishery type
rep[[i]]$CO.r_h<-as.numeric(as.character(mapply(Percent_Harvest, rep[[i]]$Month, rep[[i]]$Age, rep[[i]]$CO.r_lim)))
rep[[i]]$KC.r_h<-as.numeric(as.character(mapply(Percent_Harvest, rep[[i]]$Month, rep[[i]]$Age, rep[[i]]$KC.r_lim)))
rep[[i]]$KO.r_h<-as.numeric(as.character(mapply(Percent_Harvest, rep[[i]]$Month, rep[[i]]$Age, rep[[i]]$KO.r_lim)))
rep[[i]]$MO.r_h<-as.numeric(as.character(mapply(Percent_Harvest, rep[[i]]$Month, rep[[i]]$Age, rep[[i]]$MO.r_lim)))
rep[[i]]$SF.r_h<-as.numeric(as.character(mapply(Percent_Harvest, rep[[i]]$Month, rep[[i]]$Age, rep[[i]]$SF.r_lim)))
rep[[i]]$FB.r_h<-as.numeric(as.character(mapply(Percent_Harvest, rep[[i]]$Month, rep[[i]]$Age, rep[[i]]$FB.r_lim)))
rep[[i]]$MO.c_h<-as.numeric(as.character(mapply(Percent_Harvest, rep[[i]]$Month, rep[[i]]$Age, rep[[i]]$MO.c_lim)))
rep[[i]]$SF.c_h<-as.numeric(as.character(mapply(Percent_Harvest, rep[[i]]$Month, rep[[i]]$Age, rep[[i]]$SF.c_lim)))
rep[[i]]$FB.c_h<-as.numeric(as.character(mapply(Percent_Harvest, rep[[i]]$Month, rep[[i]]$Age, rep[[i]]$FB.c_lim)))

#proportion of harvest
rep[[i]]$CO.r_P<-rep[[i]]$CO.r*rep[[i]]$CO.r_h
rep[[i]]$KC.r_P<-rep[[i]]$KC.r*rep[[i]]$KC.r_h
rep[[i]]$KO.r_P<-rep[[i]]$KO.r*rep[[i]]$KO.r_h
rep[[i]]$MO.r_P<-rep[[i]]$MO.r*rep[[i]]$MO.r_h
rep[[i]]$SF.r_P<-rep[[i]]$SF.r*rep[[i]]$SF.r_h
rep[[i]]$FB.r_P<-rep[[i]]$FB.r*rep[[i]]$FB.r_h
rep[[i]]$MO.c_P<-rep[[i]]$MO.c*rep[[i]]$MO.c_h
rep[[i]]$SF.c_P<-rep[[i]]$SF.c*rep[[i]]$SF.c_h
rep[[i]]$FB.c_P<-rep[[i]]$FB.c*rep[[i]]$FB.c_h

#proportion of release that die
rep[[i]]$CO.r_M<-rep[[i]]$CO.r*(1-rep[[i]]$CO.r_h)*rep[[i]]$CO_mort 
rep[[i]]$KC.r_M<-rep[[i]]$KC.r*(1-rep[[i]]$KC.r_h)*rep[[i]]$KC_mort
rep[[i]]$KO.r_M<-rep[[i]]$KO.r*(1-rep[[i]]$KO.r_h)*rep[[i]]$KO_mort
rep[[i]]$MO.r_M<-rep[[i]]$MO.r*(1-rep[[i]]$MO.r_h)*rep[[i]]$MO_mort
rep[[i]]$SF.r_M<-rep[[i]]$SF.r*(1-rep[[i]]$SF.r_h)*rep[[i]]$SF_mort
rep[[i]]$FB.r_M<-rep[[i]]$FB.r*(1-rep[[i]]$FB.r_h)*rep[[i]]$FB_mort
rep[[i]]$MO.c_M<-rep[[i]]$MO.c*(1-rep[[i]]$MO.c_h)*.26
rep[[i]]$SF.c_M<-rep[[i]]$SF.c*(1-rep[[i]]$SF.c_h)*.26
rep[[i]]$FB.c_M<-rep[[i]]$FB.c*(1-rep[[i]]$FB.c_h)*.26

rep[[i]]<-rep[[i]] %>%
  replace(is.na(.),0) %>%
  mutate(Avg_harvestable = CO.r_P + KC.r_P + KO.r_P + MO.r_P + SF.r_P + FB.r_P + MO.c_P + SF.c_P + FB.c_P) %>%
  mutate(Avg_releasemort = CO.r_M + KC.r_M + KO.r_M + MO.r_M + SF.r_M + FB.r_M + MO.c_M + SF.c_M + FB.c_M) 

#reorganizing percent harvestable by brood year
  Avg_Harvestability[[i]]<-rep[[i]] %>%
    select(brood_year, run_year, Month, Avg_harvestable) %>%
    mutate(Age = run_year-brood_year) %>%
    pivot_wider(names_from = c(Age,Month),values_from= Avg_harvestable, names_sort = TRUE ) %>%
    group_by(brood_year) %>%
    summarize(Apr3 = sum(`2_4`, na.rm = TRUE), May3 = sum(`2_5`, na.rm = TRUE), Jun3= sum(`2_6`, na.rm = TRUE)
              , Jul3 = sum(`2_7`, na.rm = TRUE), Aug3 = sum(`2_8`, na.rm = TRUE), Sept3 = sum(`2_9`, na.rm = TRUE),
              Oct3 = sum(`2_10`, na.rm = TRUE), Nov3 = sum(`2_11`, na.rm = TRUE), Apr4 = sum(`3_4`, na.rm = TRUE), 
              May4 = sum(`3_5`, na.rm = TRUE), Jun4= sum(`3_6`, na.rm = TRUE) , Jul4 = sum(`3_7`, na.rm = TRUE), 
              Aug4 = sum(`3_8`, na.rm = TRUE), Sept4 = sum(`3_9`, na.rm = TRUE), Oct4 = sum(`3_10`, na.rm = TRUE), 
              Nov4 = sum(`3_11`, na.rm = TRUE)) %>%
    filter(brood_year != 2000)
  Avg_Harvestability[[i]]<-full_join(BY, as.data.frame(Avg_Harvestability[i]))
  Avg_Harvestability[[i]][is.na(Avg_Harvestability[[i]])]<-0
#reorganizing percent release mortality by brood year
  Avg_ReleaseMort[[i]]<-rep[[i]] %>%
    select(brood_year, run_year, Month, Avg_releasemort) %>%
    mutate(Age = run_year-brood_year) %>%
    pivot_wider(names_from = c(Age,Month),values_from= Avg_releasemort, names_sort = TRUE ) %>%
    group_by(brood_year) %>%
    summarize(Apr3 = sum(`2_4`, na.rm = TRUE), May3 = sum(`2_5`, na.rm = TRUE), Jun3= sum(`2_6`, na.rm = TRUE)
              , Jul3 = sum(`2_7`, na.rm = TRUE), Aug3 = sum(`2_8`, na.rm = TRUE), Sept3 = sum(`2_9`, na.rm = TRUE),
              Oct3 = sum(`2_10`, na.rm = TRUE), Nov3 = sum(`2_11`, na.rm = TRUE), Apr4 = sum(`3_4`, na.rm = TRUE), 
              May4 = sum(`3_5`, na.rm = TRUE), Jun4= sum(`3_6`, na.rm = TRUE) , Jul4 = sum(`3_7`, na.rm = TRUE), 
              Aug4 = sum(`3_8`, na.rm = TRUE), Sept4 = sum(`3_9`, na.rm = TRUE), Oct4 = sum(`3_10`, na.rm = TRUE), 
              Nov4 = sum(`3_11`, na.rm = TRUE))%>%
    replace(is.na(.),0)%>%
    filter(brood_year != 2000)
  Avg_ReleaseMort[[i]]<-full_join(BY, as.data.frame(Avg_ReleaseMort[i]))
  Avg_ReleaseMort[[i]][is.na(Avg_ReleaseMort[[i]])]<-0
}
# saveRDS(Avg_Harvestability, file = "Avg Harvestability.Rds")
# saveRDS(Avg_ReleaseMort, file = "Avg ReleaseMort.Rds")
