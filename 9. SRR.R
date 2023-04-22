#####################################
###### Impact of Fishing on #########
### natural vs hatchery fish ########
#####################################
library(ggplot2)
library(dplyr)
setwd("~/winter-run-cohort-reconstruction")
options(scipen=999)
#This script is used for both the general natural cohorts and sex-specific natural cohorts
#In order to have data output from all three (males, females, together), the code needs to be run 3 times
#making small tweaks depending on whether its males only, females only, or together
#The tweaks that need to made occur on lines
#16-18, 77-79, 162-165

######## Hatchery SRR ############
# Reconstruct.H<-readRDS("CWT Cohort Reconstruction.Rds")
# Reconstruct.H<-readRDS("CWT Cohort Reconstruction_Females.Rds")
Reconstruct.H<-readRDS("CWT Cohort Reconstruction_Males.Rds")
#Reconstructing run without fishing
HypoTotal.H<-matrix(NA, nrow=length(Reconstruct.H[[1]]$brood_year), ncol=1000) 
for(j in 1:1000){
Hypo.H<-as.data.frame(cbind(Reconstruct.H[[1]]$brood_year, Reconstruct.H[[j]]$Age2Sp+Reconstruct.H[[j]]$Age2Hat, Reconstruct.H[[j]]$Age3.3))
colnames(Hypo.H)<-c("Brood Year", "Age2Sp","Age3.3")
Hypo.H$Age4.2<-Reconstruct.H[[j]]$Age3.3*(.9816^11) #Natural mortality at age 2
Hypo.H$Age4.3<-Hypo.H$Age4.2*(1-Reconstruct.H[[j]]$Mat3)*(.9816) #Abundance after age-3 fish spawn
Hypo.H$Age3Sp<-Hypo.H$Age4.2*(Reconstruct.H[[j]]$Mat3)*(.9816) #Age 3 spawners
Hypo.H$Age4Sp<-Hypo.H$Age4.3*(.9816^12) #Natural mortality at age 3. All spawn

#Summarizing hypothetical run size without fishing
HypoRun.H<-as.data.frame(Reconstruct.H[[1]]$brood_year+3)
colnames(HypoRun.H)<-"Run Year"
HypoRun.H$Age2<-NA
HypoRun.H$Age3<-NA
HypoRun.H$Age4<-NA
for(i in 1:length(Reconstruct.H[[1]]$brood_year)){
HypoRun.H$Age2[i]<-Hypo.H$Age2Sp[i+1]
HypoRun.H$Age3[i]<-Hypo.H$Age3Sp[i]
}
HypoRun.H$Age4[1]<-0
for(i in 2:length(Reconstruct.H[[1]]$brood_year)){
  HypoRun.H$Age4[i]<-Hypo.H$Age4Sp[i-1]
}
HypoRun.H<-HypoRun.H %>%
  mutate(Total = rowSums(HypoRun.H[,2:4], na.rm = TRUE))
HypoTotal.H[,j]<-HypoRun.H$Total
}
#Summarizing observed run size
ObsTotal.H<-matrix(NA, nrow=length(Reconstruct.H[[1]]$brood_year), ncol=1000)
for(j in 1:1000){
ObsRun.H<-as.data.frame(Reconstruct.H[[1]]$brood_year+3)
colnames(ObsRun.H)<-"Run Year"
ObsRun.H$Age2<-NA
ObsRun.H$Age3<-NA
ObsRun.H$Age4<-NA
for(i in 1:length(Reconstruct.H[[1]]$brood_year)){
  ObsRun.H$Age2[i]<-Reconstruct.H[[j]]$Age2Sp[i+1]+Reconstruct.H[[j]]$Age2Hat[i+1]
  ObsRun.H$Age3[i]<-Reconstruct.H[[j]]$Age3Sp[i]+Reconstruct.H[[j]]$Age3Hat[i]+Reconstruct.H[[j]]$InRiver3[i]
}
ObsRun.H$Age4[1]<-0
for(i in 2:length(Reconstruct.H[[1]]$brood_year)){
  ObsRun.H$Age4[i]<-Reconstruct.H[[j]]$Age4Sp[i-1]+Reconstruct.H[[j]]$Age4Hat[i-1]+Reconstruct.H[[j]]$InRiver4[i-1]
}
ObsRun.H<-ObsRun.H%>%
  mutate(Total = rowSums(ObsRun.H[,2:4], na.rm = TRUE))
ObsTotal.H[,j]<-ObsRun.H$Total
}
Reduction.matrix.H<-ObsTotal.H/HypoTotal.H
Reduction.H<-matrix(NA, nrow=16, ncol=3)
for(i in 1:16){
  Reduction.H[i,]<-quantile(Reduction.matrix.H[i,c(1:1000)], probs=c(.025,.5,.975), na.rm = TRUE) 
}

Reduction.H<-as.data.frame(cbind(2004:2019,Reduction.H, rep("Hatchery_wNoFuncertainty", 16)))
colnames(Reduction.H)<-c("Run_Year","Reduction_Lower", "Reduction", "Reduction_Upper", "Origin")

######## Natural Fish SRR ############
# Reconstruct<-readRDS("Natural Cohort Reconstruction.rds")
# Reconstruct<-readRDS("Natural Cohort Reconstruction_Female.rds")
Reconstruct<-readRDS("Natural Cohort Reconstruction_Male.rds")

#Reconstructing run without fishing
HypoTotal<-matrix(NA, nrow=14, ncol=1000)
for(j in 1:1000){
Hypo<-as.data.frame(cbind(Reconstruct[[1]]$brood_year, Reconstruct[[j]]$Age2Sp, Reconstruct[[j]]$Age3.3))
colnames(Hypo)<-c("Year", "Age2Sp","Age3.3")
Hypo$Age4.2<-Hypo$Age3.3*(.9816^11) #Natural mortality at age 2
Hypo$Age4.3<-Hypo$Age4.2*(1-Reconstruct[[j]]$Mat3)*(.9816) #Abundance after age-3 fish spawn
Hypo$Age3Sp<-Hypo$Age4.2*(Reconstruct[[j]]$Mat3)*(.9816) #Age 3 spawners
Hypo$Age4Sp<-Hypo$Age4.3*(.9816^12) #Natural mortality at age 3. All spawn

#Summarizing hypothetical run size without fishing
HypoRun<-as.data.frame(Reconstruct[[1]]$brood_year+3)
colnames(HypoRun)<-"Run Year"
HypoRun$Age2<-NA
HypoRun$Age3<-NA
HypoRun$Age4<-NA
for(i in 1:14){
  HypoRun$Age2[i]<-Hypo$Age2Sp[i+1]
  HypoRun$Age3[i]<-Hypo$Age3Sp[i]
}
HypoRun$Age4[1]<-0
for(i in 2:14){
  HypoRun$Age4[i]<-Hypo$Age4Sp[i-1]
}
HypoRun<-HypoRun %>%
  mutate(Total = Age2+Age3+Age4)
HypoTotal[,j]<-HypoRun$Total
}
#Summarizing observed run size
ObsTotal<-matrix(NA, nrow = 14, ncol = 1000)
for(j in 1:1000){
ObsRun<-as.data.frame(Reconstruct[[1]]$brood_year+3)
colnames(ObsRun)<-"Run Year"
ObsRun$Age2<-NA
ObsRun$Age3<-NA
ObsRun$Age4<-NA

for(i in 1:14){
  ObsRun$Age2[i]<-Reconstruct[[j]]$Age2Sp[i+1]
  ObsRun$Age3[i]<-Reconstruct[[j]]$Age3Sp[i]
}
ObsRun$Age4[1]<-0
for(i in 2:14){
  ObsRun$Age4[i]<-Reconstruct[[j]]$Age4Sp[i-1]
}
ObsRun<-ObsRun%>%
  mutate(Total = Age2+Age3+Age4)
ObsTotal[,j]<-ObsRun$Total
}
#########
#Combining
Reduction_Natural.matrix<-ObsTotal/HypoTotal
Reduction_Natural<-matrix(NA, nrow=13, ncol=3)
for(i in 1:13){
  Reduction_Natural[i,]<-quantile(Reduction_Natural.matrix[i,c(1:1000)], probs=c(.025,.5,.975), na.rm = TRUE) 
}

Reduction_Natural<-as.data.frame(cbind(2005:2017,Reduction_Natural, rep("Natural", 13)))
colnames(Reduction_Natural)<-c("Run_Year","Reduction_Lower", "Reduction","Reduction_Upper", "Origin")
Reduction<-rbind(Reduction.H, Reduction_Natural)
Reduction$Reduction<-as.numeric(as.character(Reduction$Reduction))
Reduction$Reduction_Lower<-as.numeric(as.character(Reduction$Reduction_Lower))
Reduction$Reduction_Upper<-as.numeric(as.character(Reduction$Reduction_Upper))
Reduction$Run_Year<-as.numeric(as.character(Reduction$Run_Year))

Reduction<-Reduction %>%
  filter(Run_Year > 2005)%>%
  filter(Run_Year < 2017)
# Reduction$Run_Year<-as.factor(Reduction$Run_Year)
# write.csv(Reduction, "SRR_HvW.csv")
ggplot(Reduction, aes(x=as.factor(Run_Year), y=1-Reduction, color=Origin)) +
  geom_point(size=3, shape =17)+ 
  ylim(c(0,1))+
  labs(color = "Origin") +
  # ggtitle("Impact of Fishing on Hatchery-origin Run Size")+
  ylab("Spawner Reduction Rate")+ 
  xlab("Run Year")+
  geom_errorbar(aes(ymin=1-Reduction_Lower, ymax=1-Reduction_Upper))+
  scale_color_manual(labels = c("Hatchery", "Natural"),values=c("#E69F00", "#56B4E9"))+
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
# ggsave("SRR.jpeg", plot=last_plot(), width=7, height = 3, device = "jpeg")
# ggsave("SRR_Female.jpeg", plot=last_plot(), width=7, height = 6, device = "jpeg")
# ggsave("SRR_Male.jpeg", plot=last_plot(), width=7, height = 6, device = "jpeg")
