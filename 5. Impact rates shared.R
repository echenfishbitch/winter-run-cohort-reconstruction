###################################################
########### Impact using shared data ##############
###################################################
setwd("~/winter-run-cohort-reconstruction")
CWT<-readRDS("CWT Cohort Reconstruction.Rds")
Percent_Harvestable<-readRDS("Avg Harvestability.Rds") #Percent those encountered are harvestable based on location/fishery type of Cwt fish
Percent_ReleaseMort<-readRDS("Avg ReleaseMort.Rds") #Percent those encountered are released and become mortalities based on location/fishery type of Cwt fish
test<-Percent_ReleaseMort[[2]]
#Calculating monthly impact rates from monthly contact rates
#Impact = Contact*Proportion harvest + Contact*Drop off mortality + Contact*Release Mort
#Release mort = (1-Proportion harvest)*Release mort rate
Impact_Shared<-Percent_ReleaseMort #just to copy its structure
for(i in 1:1000){
  for(k in 1:17){ #from 2001 to 2017
    for(j in 2:17){ #for each month, Apr-Nov Age 3 and Age 4 (16 months)
#CWT[[i]][k,j+28] is the encounter rates. CWT[[i]][k,30] starts with age-3 fish in april
      Impact_Shared[[i]][k,j]<- CWT[[i]][k,j+28]*Percent_Harvestable[[i]][k,j]+
      CWT[[i]][k,j+28]*.05+
      CWT[[i]][k,j+28]*Percent_ReleaseMort[[i]][k,j]
  }
  }
}
#Determining total age-3 impact from month impact rate
#total impact = Adding impact from April-November
#Impact rates later a disproportionately higher because few individuals are around
#Scaling impact rates later in year to account for natural mortalities and impacts that happened earlier
Imp3_shared<-matrix(NA,nrow=17, ncol=1000)
for(i in 1:1000){
    for(j in 1:17){
      AprJulImp<-(1-Impact_Shared[[i]]$Apr3[j])*(1-Impact_Shared[[i]]$May3[j])*(1-Impact_Shared[[i]]$Jun3[j])*(1-Impact_Shared[[i]]$Jul3[j])
      Imp3_shared[j,i]<-Impact_Shared[[i]]$Apr3[j]*(1-0.0184)+
        Impact_Shared[[i]]$May3[j]*(1-0.0184)^2*(1-Impact_Shared[[i]]$Apr3[j])+
        Impact_Shared[[i]]$Jun3[j]*(1-0.0184)^3*(1-Impact_Shared[[i]]$Apr3[j])*(1-Impact_Shared[[i]]$May3[j])+
        Impact_Shared[[i]]$Jul3[j]*(1-0.0184)^4*(1-Impact_Shared[[i]]$Apr3[j])*(1-Impact_Shared[[i]]$May3[j])*(1-Impact_Shared[[i]]$Jun3[j])+
        Impact_Shared[[i]]$Aug3[j]*(1-0.0184)^5*(1-Impact_Shared[[i]]$Apr3[j])*(1-Impact_Shared[[i]]$May3[j])*(1-Impact_Shared[[i]]$Jun3[j])*(1-Impact_Shared[[i]]$Jul3[j])+
        Impact_Shared[[i]]$Sept3[j]*(1-0.0184)^6*AprJulImp*(1-Impact_Shared[[i]]$Aug3[j])+
        Impact_Shared[[i]]$Oct3[j]*(1-0.0184)^7*AprJulImp*(1-Impact_Shared[[i]]$Aug3[j])*(1-Impact_Shared[[i]]$Sept3[j])+
        Impact_Shared[[i]]$Nov3[j]*(1-0.0184)^8*AprJulImp*(1-Impact_Shared[[i]]$Aug3[j])*(1-Impact_Shared[[i]]$Sept3[j])*(1-Impact_Shared[[i]]$Oct3[j])
  }
}
#Determining total age-4 impact
Imp4_shared<-matrix(NA,nrow=17, ncol=1000)
for(i in 1:1000){
  for(j in 1:17){
    AprJulImp<-(1-Impact_Shared[[i]]$Apr4[j])*(1-Impact_Shared[[i]]$May4[j])*(1-Impact_Shared[[i]]$Jun4[j])*(1-Impact_Shared[[i]]$Jul4[j])
    Imp4_shared[j,i]<-Impact_Shared[[i]]$Apr4[j]*(1-0.0184)+
      Impact_Shared[[i]]$May4[j]*(1-0.0184)^2*(1-Impact_Shared[[i]]$Apr4[j])+
      Impact_Shared[[i]]$Jun4[j]*(1-0.0184)^3*(1-Impact_Shared[[i]]$Apr4[j])*(1-Impact_Shared[[i]]$May4[j])+
      Impact_Shared[[i]]$Jul4[j]*(1-0.0184)^4*(1-Impact_Shared[[i]]$Apr4[j])*(1-Impact_Shared[[i]]$May4[j])*(1-Impact_Shared[[i]]$Jun4[j])+
      Impact_Shared[[i]]$Aug4[j]*(1-0.0184)^5*(1-Impact_Shared[[i]]$Apr4[j])*(1-Impact_Shared[[i]]$May4[j])*(1-Impact_Shared[[i]]$Jun4[j])*(1-Impact_Shared[[i]]$Jul4[j])+
      Impact_Shared[[i]]$Sept4[j]*(1-0.0184)^6*AprJulImp*(1-Impact_Shared[[i]]$Aug4[j])+
      Impact_Shared[[i]]$Oct4[j]*(1-0.0184)^7*AprJulImp*(1-Impact_Shared[[i]]$Aug4[j])*(1-Impact_Shared[[i]]$Sept4[j])+
      Impact_Shared[[i]]$Nov4[j]*(1-0.0184)^8*AprJulImp*(1-Impact_Shared[[i]]$Aug4[j])*(1-Impact_Shared[[i]]$Sept4[j])*(1-Impact_Shared[[i]]$Oct4[j])
  }
}
#summarizing boostrapped data
Impacted<-matrix(nrow = 17, ncol=6)
for(i in 1:17){
  for(j in 1:2){
    Impacted[i,1:3]<-quantile(Imp3_shared[i,c(1:1000)], probs=c(.025,.5,.975), na.rm = TRUE) 
    Impacted[i,4:6]<-quantile(Imp4_shared[i,c(1:1000)], probs=c(.025,.5,.975), na.rm = TRUE) 
    Impacted[i,2]<-mean(Imp3_shared[i,c(1:1000)])
    Impacted[i,5]<-mean(Imp4_shared[i,c(1:1000)])
  }
}
brood_year<-c(2001:2017)
Impacted<-as.data.frame(cbind(brood_year,Impacted))
names(Impacted)<-c("brood_year","Imp3Lower","Imp3Mean", "Imp3Upper", "Imp4Lower", "Imp4Mean","Imp4Upper")
# write.csv(Impacted,"Impact hatchery_shared.csv", row.names = FALSE)

###################################################
########### Impact with separated data#############
###################################################
#for independent data. Using cohort specific catch rates.
#This should be slightly different from simply calculating the
#total # of fish harvested/by the abundance in Feb
Impact_Indep<-Percent_ReleaseMort
test<-CWT[[1]]
for(i in 1:1000){
  for(k in 1:17){ #from 2001 to 2017
    for(j in 2:17){ #for each month, Apr-Nov Age 3 and Age 4 (16 months){
      Impact_Indep[[i]][k,j]<- CWT[[i]][k,j+44]*Percent_Harvestable[[i]][k,j]+
        CWT[[i]][k,j+44]*.05+
        CWT[[i]][k,j+44]*Percent_ReleaseMort[[i]][k,j]
      Impact_Indep[[i]][is.na(Impact_Indep[[i]])]<-0
    }
  }
}
#Age 3 impacts independently
Imp3_Indep<-matrix(NA,nrow=17, ncol=1000)
for(i in 1:1000){
  for(j in 1:17){
    AprJulImp<-(1-Impact_Indep[[i]]$Apr3[j])*(1-Impact_Indep[[i]]$May3[j])*(1-Impact_Indep[[i]]$Jun3[j])*(1-Impact_Indep[[i]]$Jul3[j])
    Imp3_Indep[j,i]<-Impact_Indep[[i]]$Apr3[j]*(1-0.0184)+
      Impact_Indep[[i]]$May3[j]*(1-0.0184)^2*(1-Impact_Indep[[i]]$Apr3[j])+
      Impact_Indep[[i]]$Jun3[j]*(1-0.0184)^3*(1-Impact_Indep[[i]]$Apr3[j])*(1-Impact_Indep[[i]]$May3[j])+
      Impact_Indep[[i]]$Jul3[j]*(1-0.0184)^4*(1-Impact_Indep[[i]]$Apr3[j])*(1-Impact_Indep[[i]]$May3[j])*(1-Impact_Indep[[i]]$Jun3[j])+
      Impact_Indep[[i]]$Aug3[j]*(1-0.0184)^5*(1-Impact_Indep[[i]]$Apr3[j])*(1-Impact_Indep[[i]]$May3[j])*(1-Impact_Indep[[i]]$Jun3[j])*(1-Impact_Indep[[i]]$Jul3[j])+
      Impact_Indep[[i]]$Sept3[j]*(1-0.0184)^6*AprJulImp*(1-Impact_Indep[[i]]$Aug3[j])+
      Impact_Indep[[i]]$Oct3[j]*(1-0.0184)^7*AprJulImp*(1-Impact_Indep[[i]]$Aug3[j])*(1-Impact_Indep[[i]]$Sept3[j])+
      Impact_Indep[[i]]$Nov3[j]*(1-0.0184)^8*AprJulImp*(1-Impact_Indep[[i]]$Aug3[j])*(1-Impact_Indep[[i]]$Sept3[j])*(1-Impact_Indep[[i]]$Oct3[j])
  }
}
#Age-4 impacts independently
Imp4_Indep<-matrix(NA,nrow=17, ncol=1000)
for(i in 1:1000){
  for(j in 1:17){
    AprJulImp<-(1-Impact_Indep[[i]]$Apr4[j])*(1-Impact_Indep[[i]]$May4[j])*(1-Impact_Indep[[i]]$Jun4[j])*(1-Impact_Indep[[i]]$Jul4[j])
    Imp4_Indep[j,i]<-Impact_Indep[[i]]$Apr4[j]*(1-0.0184)+
      Impact_Indep[[i]]$May4[j]*(1-0.0184)^2*(1-Impact_Indep[[i]]$Apr4[j])+
      Impact_Indep[[i]]$Jun4[j]*(1-0.0184)^3*(1-Impact_Indep[[i]]$Apr4[j])*(1-Impact_Indep[[i]]$May4[j])+
      Impact_Indep[[i]]$Jul4[j]*(1-0.0184)^4*(1-Impact_Indep[[i]]$Apr4[j])*(1-Impact_Indep[[i]]$May4[j])*(1-Impact_Indep[[i]]$Jun4[j])+
      Impact_Indep[[i]]$Aug4[j]*(1-0.0184)^5*(1-Impact_Indep[[i]]$Apr4[j])*(1-Impact_Indep[[i]]$May4[j])*(1-Impact_Indep[[i]]$Jun4[j])*(1-Impact_Indep[[i]]$Jul4[j])+
      Impact_Indep[[i]]$Sept4[j]*(1-0.0184)^6*AprJulImp*(1-Impact_Indep[[i]]$Aug4[j])+
      Impact_Indep[[i]]$Oct4[j]*(1-0.0184)^7*AprJulImp*(1-Impact_Indep[[i]]$Aug4[j])*(1-Impact_Indep[[i]]$Sept4[j])+
      Impact_Indep[[i]]$Nov4[j]*(1-0.0184)^8*AprJulImp*(1-Impact_Indep[[i]]$Aug4[j])*(1-Impact_Indep[[i]]$Sept4[j])*(1-Impact_Indep[[i]]$Oct4[j])
  }
}
#Impacted Individuals,
Impacted<-matrix(nrow = 17, ncol=6)
for(i in 1:17){
  for(j in 1:2){
    Impacted[i,1:3]<-quantile(Imp3_Indep[i,c(1:1000)], probs=c(.025,.5,.975), na.rm = TRUE)
    Impacted[i,4:6]<-quantile(Imp4_Indep[i,c(1:1000)], probs=c(.025,.5,.975), na.rm = TRUE)
    Impacted[i,2]<-mean(Imp3_Indep[i,c(1:1000)])
    Impacted[i,5]<-mean(Imp4_Indep[i,c(1:1000)])

  }
}
brood_year<-c(2001:2017)
Impacted<-as.data.frame(cbind(brood_year,Impacted))
names(Impacted)<-c("brood_year","Imp3Lower","Imp3Mean", "Imp3Upper", "Imp4Lower", "Imp4Mean","Imp4Upper")
write.csv(Impacted,"Impact hatchery_notshared.csv", row.names = FALSE)
