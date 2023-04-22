###############################################
############## CWT Reconstruction #############
#################CWT Data Prep#################
###############################################
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(lubridate)

setwd("~/winter-run-cohort-reconstruction")
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
names(CWT_Releases)[7]<-"tag_code"
Phi_by_tag<-CWT_Releases %>% #phi for each batch
  select(tag_code, Phi)
#############################################
#####      Reading recovery data    #########
#############################################
CWT_Recoveries<-read.csv("CWTRecoveries.csv")
CWT_Recoveries<- CWT_Recoveries %>%
  select(reporting_agency,run_year,period, recovery_date, fishery, sex, tag_code, estimated_number, recovery_location_name, brood_year)

Phi_by_tag<-CWT_Releases %>% #phi for each batch
  select(tag_code, Phi)
#merging Phi batch info with recovery data
CWT_Recoveries<-left_join(CWT_Recoveries, Phi_by_tag, by="tag_code")

CWT_Recoveries <- CWT_Recoveries %>%
  mutate(Value_Expanded = 1/Phi) 

#############################################
##########Escapement to Hatchery#############
#############################################
CWT_Hatchery<-CWT_Recoveries %>%
      filter(fishery ==50) %>%
      mutate(estimated_number = ifelse(is.na(estimated_number), 1, estimated_number))%>% #assuming NAs are 1 (aka 100% sampling of fish recovered are at the hatchery)
      mutate(Individuals = estimated_number*Value_Expanded)
EscapeToHatchery<-CWT_Hatchery %>%
  group_by(run_year, brood_year) %>%
  summarise(Escapement_to_Hatchery = sum(Value_Expanded, na.rm = TRUE)) %>%
  mutate(Age = run_year-brood_year)
#Natural Reconstruction needs long version for calculating number of escape to hatchery
EscapeToHatchery_long<-EscapeToHatchery

#For CWT Reconstruction
EscapeToHatchery<-EscapeToHatchery %>% #making each Age into its own column
  pivot_wider(names_from = Age, values_from = Escapement_to_Hatchery, names_sort=TRUE) %>%
  group_by(brood_year) %>%
  summarize(Age1Hat = sum(`1`, na.rm = TRUE), Age2Hat = sum(`2`, na.rm = TRUE), Age3Hat = sum(`3`, na.rm = TRUE) 
            , Age4Hat = sum(`4`, na.rm = TRUE))

#For males
EscapeToHatchery_M<-CWT_Hatchery %>%
  filter(sex == "M") %>%
  group_by(run_year, brood_year) %>%
  summarise(Escapement_to_Hatchery = sum(Value_Expanded, na.rm = TRUE)) %>%
  mutate(Age = run_year-brood_year)%>%
  pivot_wider(names_from = Age, values_from = Escapement_to_Hatchery, names_sort=TRUE) %>%
  group_by(brood_year) %>%
  summarize(Age2Hat = sum(`2`, na.rm = TRUE), Age3Hat = sum(`3`, na.rm = TRUE) 
            , Age4Hat = sum(`4`, na.rm = TRUE))
#reorganizing it by run year, for hatchery cohort reconstruction_sex
EscapeToHatchery_M_RY<-CWT_Hatchery %>%
  filter(sex == "M") %>%
  group_by(run_year, brood_year) %>%
  summarise(Escapement_to_Hatchery = sum(Value_Expanded, na.rm = TRUE)) %>%
  mutate(Age = run_year-brood_year)%>%
  pivot_wider(names_from = Age, values_from = Escapement_to_Hatchery, names_sort=TRUE) %>%
  group_by(run_year) %>%
  summarize(Age2Hat = sum(`2`, na.rm = TRUE), Age3Hat = sum(`3`, na.rm = TRUE) #For CWT Reconstruction
            , Age4Hat = sum(`4`, na.rm = TRUE))
#For females
EscapeToHatchery_F<-CWT_Hatchery %>%
  filter(sex == "F") %>%
  group_by(run_year, brood_year) %>%
  summarise(Escapement_to_Hatchery = sum(Value_Expanded, na.rm = TRUE)) %>%
  mutate(Age = run_year-brood_year)%>%
  pivot_wider(names_from = Age, values_from = Escapement_to_Hatchery, names_sort=TRUE) %>%
  group_by(brood_year) %>%
  summarize(Age2Hat = sum(`2`, na.rm = TRUE), Age3Hat = sum(`3`, na.rm = TRUE) #For CWT Reconstruction
            , Age4Hat = sum(`4`, na.rm = TRUE))
#reorganizing it by run year, for hatchery cohort reconstruction_sex
EscapeToHatchery_F_RY<-CWT_Hatchery %>%
  filter(sex == "F") %>%
  group_by(run_year, brood_year) %>%
  summarise(Escapement_to_Hatchery = sum(Value_Expanded, na.rm = TRUE)) %>%
  mutate(Age = run_year-brood_year)%>%
  pivot_wider(names_from = Age, values_from = Escapement_to_Hatchery, names_sort=TRUE) %>%
  group_by(run_year) %>%
  summarize(Age2Hat = sum(`2`, na.rm = TRUE), Age3Hat = sum(`3`, na.rm = TRUE) #For CWT Reconstruction
            , Age4Hat = sum(`4`, na.rm = TRUE))
 
# write.csv(EscapeToHatchery, file = "Escapement to Hatchery.csv", row.names = FALSE)
# write.csv(EscapeToHatchery_F, file = "Escapement to Hatchery_F.csv", row.names = FALSE)
# write.csv(EscapeToHatchery_M, file = "Escapement to Hatchery_M.csv", row.names = FALSE)

#Escapement to Hatchery_RY is organized by run year instead of brood year for age composition of runs for Hatchery Sex Specific Cohort Reconstructions
# write.csv(EscapeToHatchery_F_RY, file = "Escapement to Hatchery_F_RY.csv", row.names = FALSE)
# write.csv(EscapeToHatchery_M_RY, file = "Escapement to Hatchery_M_RY.csv", row.names = FALSE)

#Getting Fry released
CWT_Hatchery_Releases<- CWT_Releases %>%
  group_by(brood_year) %>%
  summarise(Individuals_Released = sum(Total_Released))
# write.csv(CWT_Hatchery_Releases, file="Hatchery Release.csv", row.names=FALSE)

#Subtracting hatchery fish from Total Hatchery fish escapement. Remainder is natural-origin
HatcheryEscapement_All<-read.csv("Total Escapement to Hatchery.csv")
EscapeToHatchery_long<- EscapeToHatchery_long %>%
  group_by(run_year)%>%
  summarize(Hatchery_to_Hatchery = sum(Escapement_to_Hatchery))
HatcheryEscapement_All<-left_join(HatcheryEscapement_All,EscapeToHatchery_long, by="run_year")
HatcheryEscapement_All$Hatchery_to_Hatchery[c(1,2,6,9)]<-c(149,163,0,0) #2010 and 2013 had no CWT fish taken to hatchery. Most up-to-date hatchery from Mike
HatcheryEscapement_All<-HatcheryEscapement_All %>% map_df(rev)
HatcheryEscapement_All$Natural_to_Hatchery<-HatcheryEscapement_All$Hatchery.Escapement-HatcheryEscapement_All$Hatchery_to_Hatchery
#Splitting up assuming 50/50 sex ratio of fish collected for broodstock. 
HatcheryEscapement_All$NaturalMales_to_Hatchery<-HatcheryEscapement_All$Natural_to_Hatchery*.5
HatcheryEscapement_All$NaturalFemales_to_Hatchery<-HatcheryEscapement_All$Natural_to_Hatchery*.5
# write.csv(HatcheryEscapement_All, "Natural Escapement to Hatchery.csv", row.names = FALSE)

#############################################
#######  Escapement to Spawning Grounds #####
#############################################
Escapement<-read.csv("CWT Recoveries SG.csv") 

Escapement<- Escapement %>%
  mutate(run_year = brood_year + Age) %>%
  mutate(Individuals = numtags*Value_Expanded) %>%
  group_by(run_year, brood_year, Age) %>%
  summarise(Individuals= sum(Individuals))

Escapement<-Escapement %>% #making each Age into its own column
  pivot_wider(names_from = Age, values_from = Individuals, names_sort=TRUE) 
Escapement_BY<-Escapement %>%
  group_by(brood_year) %>%
  summarize(Age1Sp = sum(`1`, na.rm = TRUE), Age2Sp = sum(`2`, na.rm = TRUE), Age3Sp = sum(`3`, na.rm = TRUE)
            , Age4Sp = sum(`4`, na.rm = TRUE), Age5Sp = sum(`5`, na.rm = TRUE))
Escapement_Run<-Escapement %>%
  group_by(run_year) %>%
  summarize(Age1Sp = sum(`1`, na.rm = TRUE), Age2Sp = sum(`2`, na.rm = TRUE), Age3Sp = sum(`3`, na.rm = TRUE)
            , Age4Sp = sum(`4`, na.rm = TRUE), Age5Sp = sum(`5`, na.rm = TRUE))
Escapement_Run$TotalCWT<-rowSums(Escapement_Run[,2:6])
Escapement_Run<-Escapement_Run %>%
  select(run_year, TotalCWT)
#For subtracting from total run in Natural-origin cohort reconstruction
# write.csv(Escapement_Run, "HatcheryRunSizeCWT.csv")
#BOOTSTRAP for uncertainty
SG_Escapement<-read.csv("CWT Recoveries SG.csv") 
SG_Escapement_Sum<-matrix(NA, nrow=18, ncol = 1000)
Cohort<-list() #Cohort will be a table of Spawners from each age from 20 years, sampled 1000 times
for(j in 1:1000){
  SG_Escapement$Resampled<-NA
  for(i in 1:length(SG_Escapement$cwtcode)){
    SG_Escapement$Resampled[i]<-sum(c(1,rnbinom(1, 1, 1/SG_Escapement$numtags[i])))*SG_Escapement$Value_Expanded[i]
  }
  SG_Escapement_Sum[,j]<-SG_Escapement %>%
    mutate(run_year = brood_year + Age) %>%
    group_by(run_year)%>%
    summarise(Run = sum(Resampled))%>%
    filter(run_year != 2001) %>%
    pull(Run)
  Cohort[[j]]<-SG_Escapement %>%
    mutate(run_year = brood_year + Age) %>%
    group_by(run_year, brood_year, Age) %>%
    summarise(Individuals = sum(Resampled))%>% #making each Age into its own column
    pivot_wider(names_from = Age, values_from = Individuals, names_sort=TRUE)%>%
    group_by(brood_year) %>%
    summarize(Age1Sp = sum(`1`, na.rm = TRUE), Age2Sp = sum(`2`, na.rm = TRUE), Age3Sp = sum(`3`, na.rm = TRUE)
              , Age4Sp = sum(`4`, na.rm = TRUE), Age5Sp = sum(`5`, na.rm = TRUE))
}
test<-Cohort[[1]]
#######################
## Run by Age and Sex #
#######################
Cohort_F<-list()
Cohort_M<-list()

#Run Size partitioned by Sex Ratio
SexRatio<-read.csv("SexRatioSG.csv")
SexRatio<-SexRatio %>%
  select(run_year, RatioF)
SexRatio<-rev(SexRatio$RatioF)
#Partitioning bootstrapped escapement by sex ratio
HatcheryFemaleRun<-SG_Escapement_Sum*SexRatio
HatcheryMaleRun<-SG_Escapement_Sum*(1-SexRatio)

CWT_Recoveries<-read.csv("CWTRecoveries.csv")

#Age Proportions of Females from CWT data
CWT_AgeProp_F<- left_join(CWT_Recoveries %>%
  mutate(Age = run_year-brood_year)%>%
  filter(fishery == 54) %>%
  group_by(run_year, Age, sex) %>%
  tally()%>%
  filter(sex=="F"),
  CWT_Recoveries %>%
    mutate(Age = run_year-brood_year)%>%
    filter(fishery == 54) %>%
    group_by(run_year,sex) %>%
    tally()%>%
    filter(sex=="F")%>%
    rename(total=n))
CWT_AgeProp_F$Age_Prop<-CWT_AgeProp_F$n/CWT_AgeProp_F$total
CWT_AgeProp_F<-pivot_wider(CWT_AgeProp_F, names_from = "Age", values_from = Age_Prop)
CWT_AgeProp_F<-CWT_AgeProp_F %>%
  group_by(run_year) %>%
  summarise(`2`=sum(`2`, na.rm = TRUE),`3`=sum(`3`, na.rm = TRUE),`4`=sum(`4`, na.rm = TRUE),`5`=sum(`5`, na.rm = TRUE))
#Age Proportions of males from CWT data
CWT_AgeProp_M<- left_join(CWT_Recoveries %>%
                            mutate(Age = run_year-brood_year)%>%
                            filter(fishery == 54) %>%
                            group_by(run_year, Age, sex) %>%
                            tally()%>%
                            filter(sex=="M"),
                          CWT_Recoveries %>%
                            mutate(Age = run_year-brood_year)%>%
                            filter(fishery == 54) %>%
                            group_by(run_year,sex) %>%
                            tally()%>%
                            filter(sex=="M")%>%
                            rename(total=n))
CWT_AgeProp_M$Age_Prop<-CWT_AgeProp_M$n/CWT_AgeProp_M$total
CWT_AgeProp_M<-pivot_wider(CWT_AgeProp_M, names_from = "Age", values_from = Age_Prop)
CWT_AgeProp_M<-CWT_AgeProp_M %>%
  group_by(run_year) %>%
  summarise(`2`=sum(`2`, na.rm = TRUE),`3`=sum(`3`, na.rm = TRUE),`4`=sum(`4`, na.rm = TRUE))
#Estimating the age and sex-specific escapement of each brood year. 
for(j in 1:1000){
  #Females
  Female_Age<-cbind(CWT_AgeProp_F[,1],HatcheryFemaleRun[,j]*CWT_AgeProp_F[,2:5])
  Female_Age<-pivot_longer(Female_Age, cols = c(`2`,`3`,`4`,`5`),values_to = "Individuals", names_to = "Age")
  Female_Age$Age<-as.numeric(Female_Age$Age)
  Female_Age$brood_year<-Female_Age$run_year-Female_Age$Age
  Female_Age<-pivot_wider(Female_Age, names_from = "Age", values_from = "Individuals")
  Cohort_F[[j]]<-Female_Age %>%
    group_by(brood_year) %>%
    summarize(Age2Sp = sum(`2`, na.rm = TRUE), Age3Sp = sum(`3`, na.rm = TRUE)
              , Age4Sp = sum(`4`, na.rm = TRUE), Age5Sp = sum(`5`, na.rm = TRUE))
  
  #Males
  Male_Age<-cbind(CWT_AgeProp_M[,1],HatcheryMaleRun[,1]*CWT_AgeProp_M[,2:4])
  Male_Age<-pivot_longer(Male_Age, cols = c(`2`,`3`,`4`),values_to = "Individuals", names_to = "Age")
  Male_Age$Age<-as.numeric(Male_Age$Age)
  Male_Age$brood_year<-Male_Age$run_year-Male_Age$Age
  Male_Age<-pivot_wider(Male_Age, names_from = "Age", values_from = "Individuals")
  Cohort_M[[j]]<-Male_Age %>%
    group_by(brood_year) %>%
    summarize(Age2Sp = sum(`2`, na.rm = TRUE), Age3Sp = sum(`3`, na.rm = TRUE)
              , Age4Sp = sum(`4`, na.rm = TRUE))
  
}
test<-Cohort_M[[2]]
# saveRDS(Cohort, file = "CWTBootstraps.Rds")
# saveRDS(Cohort_F, file = "CWTBootstraps_F.Rds")
# saveRDS(Cohort_M, file = "CWTBootstraps_M.Rds")

