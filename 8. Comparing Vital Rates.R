########################################
##### Cohort Analysis Comparisons ######
########################################
library(ggplot2)
library(dplyr)
setwd("~/winter-run-cohort-reconstruction")
blank_bg<-theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

####### Hatchery vs Natural Maturation Rates #######
#only comparing 2002-2015 brood years
Uncertainty_CWT<-read.csv("Maturation_Uncertainty_CWT.csv")
years<-2002:2015
Uncertainty_CWT<-Uncertainty_CWT %>%
  filter(brood_year %in% years)
Uncertainty_Natural<-read.csv("Maturation_Uncertainty_Natural.csv")

Uncertainty_Natural<-Uncertainty_Natural %>%
      mutate(Source = "Natural") %>%
      mutate(Mat2Lower = ifelse(Mat2Lower == 0, NA, Mat2Lower)) %>%
      mutate(Mat2Upper = ifelse(Mat2Lower == 0, NA, Mat2Upper))

Maturation<-rbind(Uncertainty_CWT, Uncertainty_Natural)

ggplot(Maturation, aes(x=brood_year, y=Mat2Mean, color=Source)) +
  geom_point(size=3)+ 
  geom_errorbar(aes(ymin=Mat2Lower, ymax=Mat2Upper))+
  ggtitle("Age-2CK")+
  ylab("Maturation rate 2 Year Olds")+ 
  scale_x_continuous("Brood Year", labels = as.character(Maturation$brood_year), breaks = Maturation$brood_year) +
  scale_color_manual(values=c("#E69F00", "#56B4E9"))+
  blank_bg
ggplot(Maturation, aes(x=brood_year, y=Mat3Mean, color=Source)) +
  geom_point(size=3)+ 
  geom_errorbar(aes(ymin=Mat3Lower, ymax=Mat3Upper))+
  ggtitle("CK")+
  ylab("Maturation rate 3 Year Olds")+ 
  scale_x_continuous("Brood Year", labels = as.character(Maturation$brood_year), breaks = Maturation$brood_year) +
  scale_color_manual+
  blank_bg

####### Sex-specific Maturation Rates #######
#Hatchery
Maturation_Sex_CWT<-read.csv("Maturation_Uncertainty_CWT_Sex.csv")
Maturation_Sex_CWT$Source<-"Hatchery"
#Natural
Males<-read.csv("Maturation_Uncertainty_Natural_Male.csv")
Males<-Males %>%
  mutate(Sex="Male") %>%
  mutate(Source = "Natural")
Females<-read.csv("Maturation_Uncertainty_Natural_Female.csv")
Females<-Females %>%
  mutate(Sex="Female") %>%
  mutate(Source = "Natural")
Maturation_Sex<-rbind(Males, Females, Maturation_Sex_CWT)
Maturation_Sex <- Maturation_Sex %>%
  filter(brood_year %in% years)
#Males of both origin
ggplot(Maturation_Sex[which(Maturation_Sex$Sex =="Male"),], aes(x=brood_year, y=Mat2Mean, color=Source)) +
  geom_point(size=4, shape = 17)+ 
  geom_errorbar(aes(ymin=Mat2Lower, ymax=Mat2Upper),size = .8)+
  ggtitle("Males")+
  ylim(c(0,1))+
  ylab("Maturation rate")+
  scale_x_continuous("Brood Year", labels = as.character(Maturation_Sex$brood_year), breaks = Maturation_Sex$brood_year) +
  scale_color_manual(values=c("#E69F00", "#56B4E9"))+
  blank_bg
ggsave("MalesAge2.jpeg", plot=last_plot(), width=7, height = 4, device = "jpeg")

ggplot(Maturation_Sex[which(Maturation_Sex$Sex =="Male"),], aes(x=brood_year, y=Mat3Mean, color=Source)) +
  geom_point(size=4, shape =17)+ 
  geom_errorbar(aes(ymin=Mat3Lower, ymax=Mat3Upper),size = .8)+
  ylim(c(0,1))+
  ggtitle("")+
  ylab("Maturation rate")+ 
  scale_x_continuous("Brood Year", labels = as.character(Maturation_Sex$brood_year), breaks = Maturation_Sex$brood_year) +
  scale_color_manual(values=c("#E69F00", "#56B4E9"))+
  blank_bg
ggsave("MalesAge3.jpeg", plot=last_plot(), width=7, height = 4, device = "jpeg")

#Females of both origin
ggplot(Maturation_Sex[which(Maturation_Sex$Sex =="Female"),], aes(x=brood_year, y=Mat2Mean, color=Source)) +
  geom_point(size=4)+ 
  ylim(c(0,1))+
  geom_errorbar(aes(ymin=Mat2Lower, ymax=Mat2Upper),size = .8)+
  ggtitle("Females")+
  ylab("Maturation rate")+ 
  scale_x_continuous("Brood Year", labels = as.character(Maturation_Sex$brood_year), breaks = Maturation_Sex$brood_year) +
  scale_color_manual(values=c("#E69F00", "#56B4E9"))+
  blank_bg
ggsave("FemalesAge2.jpeg", plot=last_plot(), width=7, height = 4, device = "jpeg")

ggplot(Maturation_Sex[which(Maturation_Sex$Sex =="Female"),], aes(x=brood_year, y=Mat3Mean, color=Source)) +
  geom_point(size=4)+ 
  geom_errorbar(aes(ymin=Mat3Lower, ymax=Mat3Upper),size = .8)+
  ggtitle("")+
  ylim(c(0,1))+
  ylab("Maturation rate")+ 
  scale_x_continuous("Brood Year", labels = as.character(Maturation_Sex$brood_year), breaks = Maturation_Sex$brood_year) +
  scale_color_manual(values=c("#E69F00", "#56B4E9"))+
  blank_bg
ggsave("FemalesAge3.jpeg", plot=last_plot(), width=7, height = 4, device = "jpeg")

####### Impact with and without sharing data #########
Impact_shared<-read.csv("Impact hatchery_shared.csv")
Impact_independent<-read.csv("Impact hatchery_notshared.csv")
Impact_independent<-Impact_independent %>%
  mutate(Source = "Independent")
Impact_shared<-Impact_shared %>%
  mutate(Source = "Combined 3/4 data")
Impact<-rbind(Impact_independent, Impact_shared)
Impact[is.na(Impact)]<-0
Impact<-Impact[which(Impact$brood_year < 2016 & Impact$brood_year > 2001),]

ggplot(Impact, aes(x=brood_year, y=Imp3Mean, color=Source)) +
  geom_point(size=3)+ 
  geom_errorbar(aes(ymin=Imp3Lower, ymax=Imp3Upper))+
  ggtitle("3 Year Olds")+
  ylab("Impact rate")+ 
  scale_x_continuous("Brood Year", labels = as.character(Impact$brood_year), breaks = Impact$brood_year) +
  scale_color_manual(values=c("#404040","#cfcfcf"))+
  ylim(0,1)+
  blank_bg

ggplot(Impact[which(Impact$brood_year < 2016 & Impact$brood_year > 2001),], aes(x=brood_year, y=Imp4Mean, color=Source)) +
  geom_point(size=3)+ 
  geom_errorbar(aes(ymin=Imp4Lower, ymax=Imp4Upper))+
  ggtitle("4 Year Olds")+
  ylab("Impact rate")+ 
  scale_x_continuous("Brood Year", labels = as.character(Impact$brood_year), breaks = Impact$brood_year) +
  scale_color_manual(values=c("#404040","#cfcfcf"))+
  ylim(0,1)+
  blank_bg
  