#############################################
#######   Age Composition of Natural ########
############# origin spawners from scales ###
#############################################
#This script uses the scale aging data to estimate the age composition of returning runs
#This script is used for both the general natural cohorts and sex-specific natural cohorts
#In order to have data output from all three (males, females, together), the code needs to be run 3 times
#making small tweaks depending on whether its males only, females only, or together
#The tweaks that need to made occur on lines
#Line 61-63: switch between filter males or females or no filter if doing sexes together
#Lines 96-98, 100-102: uncomment and comment the lines depending on sex
#Lines 110-112:uncomment and comment the lines depending on sex
#Lines 179-181, 182-184:uncomment and comment the lines depending on sex
#Lines 193-195:uncomment and comment the lines depending on sex

setwd("~/winter-run-cohort-reconstruction")
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
options(scipen=999)
#######   Proportion total run by sex  ########
scaledata<-read.csv("Scale Readage.csv")
scaledata<-scaledata %>%
  mutate(Origin = ifelse(is.na(CWT.Code), "Natural", "Hatchery"))
escapement<-read.csv("Escapement Numbers.csv")
EscapeToHatchery<-read.csv("Natural Escapement to Hatchery.csv")
CWTRun<-read.csv("HatcheryRunSizeCWT.csv")
escapement<-escapement %>%
  left_join(EscapeToHatchery) %>%
  left_join(CWTRun) %>%
  filter(run_year > 2000)
#Total natural-origin escapement = Total escapement on SG - Hatchery escapement on SG + Natural escapement to Hatchery
escapement$Annual_Nat<-escapement$Annual-escapement$TotalCWT+escapement$Natural_to_Hatchery
escapement$Females_Nat<-escapement$Females-escapement$TotalCWT*escapement$RatioF+escapement$NaturalFemales_to_Hatchery
escapement$Males_Nat<-escapement$Males-escapement$TotalCWT*(1-escapement$RatioF)+escapement$NaturalMales_to_Hatchery
escapement_F<-escapement %>%
  select(run_year, Females_Nat)
escapement_M<-escapement %>%
  select(run_year, Males_Nat)
escapement<-escapement %>%
  select(run_year, Annual_Nat)

#Creating Validation matrix, only needs to be done once, for both sexes
#validation matrix
validated<-scaledata %>%
  filter(Age != 0) %>%
  filter(Readage != 0) %>%
  filter(Age != 1) %>%
  filter(Age != 5)
valid.mat<-as.matrix(table(validated$Readage,validated$Age)) #Header is Known Age, Side is Read Age

###### Resampling #############
#When calculating for just a sex, the line needs to be changed on
#Lines: 54
niter<-1000
scaledata<-scaledata%>%
  filter(Readage != 0) %>%
  filter(Age != 1) %>%
  filter(Age != 5) %>%
  filter(Origin == "Natural") #%>%
  # filter(Sex == "F") #comment out this line (and the pipe on the previous line) when not doing sex-specific data
  # filter(Sex == "M") #comment out this line when not doing sex-specific data

#Creating Space for Resamples
Age_Prop<-array(NA, c(4,length(unique(scaledata$Year)),niter))
#putting each year into its own item in a list
read_year<-split(scaledata, f=scaledata$Year)  
for(i in 1:niter){
for(y in 1:14){
#Resampling
resample<-sample(read_year[[y]]$Readage, length(read_year[[y]]$Readage), replace = TRUE)
#Calculating age proportions
Age_Prop[,y,i]<- if(length(tabulate(resample)/length(resample)) == 3)
            {c((tabulate(resample)/length(resample)), 0)} else if
                    (length(tabulate(resample)/length(resample)) == 2)#For in 2005, 2018 when they sometimes don't have 4 year olds
            {c((tabulate(resample)/length(resample)), 0,0)} else
            (tabulate(resample)/length(resample))
}}
Age_Prop<-Age_Prop[-1,,]
##############################
#### Age Composition   #######
###   by brood year  #########
#### without Kimura Chikuni ##
##############################
UnAdjusted.list<-list()
for(i in 1:1000){
  UnAdjusted.list[[i]]<-as.data.frame(Age_Prop[,,i])
  colnames(UnAdjusted.list[[i]])<-c("2005", "2006", "2007","2008","2009","2010","2011","2012","2013","2014",
                                  "2015","2016","2017","2018")
  UnAdjusted.list[[i]]<-pivot_longer(UnAdjusted.list[[i]], cols = starts_with("2"), names_to="run_year", values_to = "Age_Proportion")
  UnAdjusted.list[[i]]$Age<-rep(2:4,each=14)
  UnAdjusted.list[[i]]$run_year<-as.integer(UnAdjusted.list[[i]]$run_year)
  UnAdjusted.list[[i]]<-UnAdjusted.list[[i]] %>%
    #Uncomment one of the next three lines depending on whether its male, female, or both
    left_join(escapement)%>% #For sex combined
    # left_join(escapement_F) %>% #Females
    # left_join(escapement_M) %>% #Males
  #Uncomment one of the next three lines depending on whether its male, female, or both
    mutate(Annual = Age_Proportion*Annual_Nat) %>% #For sex combined
    # mutate(Annual = Age_Proportion*Females_Nat) %>% #Females
    # mutate(Annual = Age_Proportion*Males_Nat) %>% #Males
    mutate(brood_year = run_year-Age) %>% #making each Age into its own column
    pivot_wider(names_from = Age, values_from = Annual, names_sort = TRUE) %>%
    group_by(brood_year) %>% #if desired switch to run_year
    summarize(Age2Sp = sum(`2`, na.rm = TRUE), Age3Sp = sum(`3`, na.rm = TRUE)
              , Age4Sp = sum(`4`, na.rm = TRUE))
}
#Uncomment one of the next three lines depending on whether its male, female, or both
saveRDS(UnAdjusted.list, file = "Spawners Unadjusted.Rds")
# saveRDS(UnAdjusted.list, file = "Female Spawners Unadjusted.Rds")
# saveRDS(UnAdjusted.list, file = "Male Spawners Unadjusted.Rds")

###############################
#Kimura Chikuni
Adjusted<-array(NA, c(3,length(unique(scaledata$Year)),niter)) #where adjusted age proportions from resampled data will go
a=3#length(read.ages)
n=a #"length" categories are really scale-read ages

#observed distribution of "length" at age
q=array(NA,c(n,a))
for (i in 1:n)
{
  for (j in 1:a)
  {
    q[i,j]=as.numeric(valid.mat[i,j])
  }
}
for (j in 1:a) q[,j]=q[,j]/colSums(q)[j]
for(k in 1:14){
for (b in 1:1000){
#initialize p.hat - estimated proportion in each age class j
p.hat=array(1/a,a)
#observed "length" distribution
# f.bar<-as.numeric(unlist(read.ages[,k]))/as.numeric(unlist(samples[k]))
#IALK algorithm
max.iterations=500
p.chain=array(NA,c(max.iterations,a))
l.hat=array(NA,n) #placeholder, l.hat updated within loop
Pr.hat=array(NA,c(n,a)) #placeholder, Pr.hat updated within loop
iteration=1
bailout=0

while ((iteration+bailout)<(max.iterations+1))
{#iteration while loop
	p.chain[iteration,]=p.hat
	for (i in 1:n)
	{#loop over n
		l.hat[i]=sum(p.hat*q[i,])
	}#loop over n
	p.hat.new=p.hat
	p.hat.old=p.hat
	for (j in 1:a)
	{#loop over a
		cum.sum=0
		for (i in 1:n)
		{#loop over n
			cum.sum=cum.sum+Age_Prop[i,k,b]*p.hat[j]*q[i,j]/l.hat[i]
		}#loop over n
		p.hat.new[j]=cum.sum
	}#loop over a
	p.hat=p.hat.new
	iteration=iteration+1
	if (max(abs(p.hat.new-p.hat.old),na.rm=TRUE)<0.00005) bailout=max.iterations
}#iteration while loop
Adjusted[,k,b]<-p.hat
}
}
Adjusted.list<-list()
#Wrangling age proportion data to age composition by run year
for(i in 1:1000){
Adjusted.list[[i]]<-as.data.frame(Adjusted[,,i])
colnames(Adjusted.list[[i]])<-c("2005", "2006", "2007","2008","2009","2010","2011","2012","2013","2014",
                                "2015","2016","2017","2018")
Adjusted.list[[i]]<-pivot_longer(Adjusted.list[[i]], cols = starts_with("2"), names_to="run_year", values_to = "Age_Proportion")
Adjusted.list[[i]]$Age<-rep(2:4,each=14)
Adjusted.list[[i]]$run_year<-as.integer(Adjusted.list[[i]]$run_year)
Adjusted.list[[i]]<-Adjusted.list[[i]] %>%
  left_join(escapement)%>%
  # left_join(escapement_F) %>% #Change sex here
  # left_join(escapement_M) %>% #Change sex here
  mutate(Annual = Age_Proportion*Annual_Nat) %>%
  # mutate(Annual = Age_Proportion*Females_Nat) %>% #Change sex here
  # mutate(Annual = Age_Proportion*Males_Nat) %>% #Change sex here
  mutate(brood_year = run_year-Age ) %>% 
  pivot_wider(names_from = Age, values_from = Annual, names_sort = TRUE) %>% #making each Age into its own column
  group_by(brood_year) %>% #sub here for run_year if desired
  summarize(Age2Sp = sum(`2`, na.rm = TRUE), Age3Sp = sum(`3`, na.rm = TRUE)
            , Age4Sp = sum(`4`, na.rm = TRUE))
}

#########################
saveRDS(Adjusted.list, file = "Spawners Adjusted.Rds")
# saveRDS(Adjusted.list, file = "Female Spawners Adjusted.Rds")
# saveRDS(Adjusted.list, file = "Male Spawners Adjusted.Rds")

#############################
#Visualizing differences before and after corrections
Unadjusted_mean<-matrix(nrow=3,ncol=14)
for(i in 1:3){
  for(k in 1:14){
    Unadjusted_mean[i,k]<-mean(Age_Prop[i,k,])
  }}
Unadjusted_mean<-as.data.frame(t(Unadjusted_mean))
colnames(Unadjusted_mean)<-c("2","3","4")
Unadjusted_mean$Year<-c(2005:2018)
Unadjusted_mean<-pivot_longer(Unadjusted_mean, cols=c('2','3','4'), names_to = "Age" )

Adjusted_mean<-matrix(nrow=3,ncol=14)
for(i in 1:3){
  for(k in 1:14){
    Adjusted_mean[i,k]<-mean(Adjusted[i,k,])
  }}
Adjusted_mean<-as.data.frame(t(Adjusted_mean))
colnames(Adjusted_mean)<-c("2","3","4")
Adjusted_mean$Year<-c(2005:2018)
Adjusted_mean<-pivot_longer(Adjusted_mean, cols=c('2','3','4'), names_to = "Age" )

ggplot(Unadjusted_mean, aes(fill=fct_rev(as.factor(Age)), x=factor(Year), y=value)) +
  geom_bar(position="stack", stat="identity")+
  ylab("Individuals")+
  xlab("Run Year")+
  scale_fill_manual(values=c("#E69F00", "#56B4E9","#999999"),name = "Age")+
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90),axis.text.y= element_text(angle = 90))+
  ggtitle("Natural-origin Age Composition Raw")

ggplot(Adjusted_mean, aes(fill=fct_rev(as.factor(Age)), x=factor(Year), y=value)) +
  geom_bar(position="stack", stat="identity")+
  ylab("Individuals")+
  xlab("Run Year")+
  scale_fill_manual(values=c("#E69F00", "#56B4E9","#999999"),name = "Age")+
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90),axis.text.y= element_text(angle = 90))+
  ggtitle("Natural-origin Age Composition Kimura Chikuni")
