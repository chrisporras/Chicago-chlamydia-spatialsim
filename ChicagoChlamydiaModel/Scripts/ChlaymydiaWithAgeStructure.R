library(ggplot2)
library(ggmap)
library(rgdal)
library(rgeos)
library(maptools)
library(dplyr)
library(tidyr)
library(tmap)
library(sp)
library(gmapsdistance)
library(stringr)

setwd("~/Desktop/ChicagoChlamydiaModel/Data")
Chicago= readOGR(dsn = "chicomm", layer = "chicomm") ######Load GeoSpatial Data
ExtraData = read.csv(file="chicago-pop-data.csv", header=TRUE, sep=",") #####Load Population and Sq Area Data
ExtraData=ExtraData[as.numeric(Chicago@data[["DISTNAME"]]),] ####Reoorder dfs to same order
ClinicData=read.csv(file="chicago-clinics.csv", header=TRUE)
DistanceMat=spDists(Chicago, Chicago) #Euclidean Distance

ClinicTransitData=read.csv(file="ClinicDistances 2.csv", header = TRUE) ####Reads in Clinic Data
regexp = "[[:digit:]]+"
NumericData=str_extract_all(ClinicTransitData$Distance.Time, regexp, simplify = TRUE) ####Returns Matrix of Time and Distance Values
ClinicDistanceMat=0.000621371*matrix(as.numeric(NumericData[,1]), ncol=77) ####Codifies as Matrix of Distances in miles
ClinicTimeMat=matrix(as.numeric(NumericData[,2]), ncol=77)####Codifies as Matrix of Times

NumericData=matrix(c(ClinicData$lon,ClinicData$lat), ncol=2) ####DISTANCE GREAT CIRCLE FOR CLINICS
ClinicDistanceMat=spDists(NumericData,coordinates(Chicago))

###Free Clinics Only
ClinicDistanceMat=ClinicDistanceMat[22:24,]
ClinicTimeMat=ClinicTimeMat[22:24,]


ClinicDist=apply(ClinicDistanceMat, 2, FUN=min, na.rm=TRUE) ####Minimum Travel Distance, Removing NAs
ClinicTime=apply(ClinicTimeMat, 2, FUN=min, na.rm=TRUE) ####Minimum Travel Time, Removing NAs


TransitData=read.csv(file="TransitDistances 2.csv", header = TRUE) ####Reads in Neighborhood Data
regexp = "[[:digit:]]+"
NumericData=str_extract_all(TransitData$Distance.Time, regexp, simplify = TRUE) ####Returns Matrix of Time and Distance Values
DistanceMat=0.000621371*matrix(as.numeric(NumericData[,1]), nrow=sqrt(length(TransitData$Distance.Time)), byrow=TRUE) ####Codifies as Matrix of Distances in miles
TimeMat=matrix(as.numeric(NumericData[,2]), nrow=sqrt(length(TransitData$Distance.Time)), byrow=TRUE) ####Codifies as Matrix of Times

AgeStrucData=read.csv(file="Chicago_Age_Structure.csv", header=TRUE)
AgeStrucData=AgeStrucData[as.numeric(Chicago@data[["DISTNAME"]]),]


##Probability of Going to Other Neighborhoods
GTransProb=function(ExtraData, DistanceMat, alpha){
  ProbMat=matrix(NA, nrow=nrow(DistanceMat), ncol=ncol(DistanceMat))
  
  #######Prob of Individual in J going to K based on Transit Distance
  
  for (J in 1:nrow(DistanceMat)){
    for (K in 1:ncol(DistanceMat)){
      ProbMat[J,K]=exp(-((DistanceMat[J,K])/alpha)^2)
    }
  }
  
  ProbMat[which(is.na(ProbMat))]=0
  ######Normalizing Transition Probs#######
  
  for (J in 1:nrow(DistanceMat)){
    ProbMat[J,]=ProbMat[J,]/sum(ProbMat[J,])
  }
  
  return(ProbMat)
  ########Want to return matrix of prob from going to J to K
}

alpha=0.7
P=GTransProb(ExtraData, DistanceMat, alpha)

##########Demographics###########################
Population=ExtraData$X2010.Population
n= ceiling(.01*sum(Population))###########number of individuals total
NeighborhoodVec=sample(1:length(ExtraData$Neighborhood), n, replace=TRUE, prob = Population/sum(Population)) ####Who is in what neighborhood
AgeGroups = 6 ###Number of Age Classes
###Make age structure distributions
agedist_male = matrix(data = 0, ncol = AgeGroups, nrow  = length(Population)) 
agedist_female = matrix(data = 0, ncol = AgeGroups,nrow = length(Population))
totaldist = matrix(data = 0, ncol = AgeGroups, nrow = length(Population))
genderdist = matrix(data = 0, ncol = 2, nrow = length(Population))

for (i in 1:length(Population)){
  males = c(AgeStrucData$Male_under_15.[i],AgeStrucData$Male_15.17.[i],AgeStrucData$Male_18.24.[i],AgeStrucData$Male_25.34.[i],AgeStrucData$Male_35.49.[i],AgeStrucData$Male_50..[i])
  females = c(AgeStrucData$Female_Under_15.[i], AgeStrucData$Female_15.17.[i],AgeStrucData$Female_18.24.[i],AgeStrucData$Female_25.34.[i], AgeStrucData$Female_35.49.[i], AgeStrucData$Female_50..[i])
  
  agedist_male[i,] = males
  agedist_female[i,] = females
  totaldist = agedist_female + agedist_male
  
  genderdist[i,1] = AgeStrucData$Male.Population[i]/(AgeStrucData$Male.Population[i]+AgeStrucData$Female_Population[i])
  genderdist[i,2] = AgeStrucData$Female_Population[i]/(AgeStrucData$Male.Population[i]+AgeStrucData$Female_Population[i])
} 

####### Marriage distribution

marriagecats = 1:2 #1 married #2 single
agecats = 1:AgeGroups

#cohabitingvec = c(0,0,0.16,0.55,0.71,0.65)
#singlevec = c(1,1,0.84,0.45,0.29,0.35)
#marriagedist = matrix(data = c(cohabitingvec,singlevec),ncol = length(marriagecats), nrow = length(agecats))

MarriageProb= as.numeric(ExtraData$Marriage.Rate)
gender_vec = vector(length = n)
age_vec = vector(length = n)
marriage_vec = vector(length = n)

#### Partner Distribution
PartnerDist=matrix(c(.18, 0.39,.18, .1, .15, .18, .49, .18, .07, .08), ncol=2)
####### Assigning gender, age, and marriage status to individuals

for(i in 1:n){
  if(runif(1) < genderdist[NeighborhoodVec[i],1]){
    gender_vec[i] = 1 #male  
  }else{
    gender_vec[i] = 2 #female
  }
  
  if(gender_vec[i] == 1){ #if male
    age_vec[i] = sample(agecats,1,prob=agedist_male[NeighborhoodVec[i],])
  }else { #if female
    age_vec[i] = sample(agecats,1,prob=agedist_female[NeighborhoodVec[i],])
  }
  
  #Assign marriage status from marriage dist
  if (age_vec[i]>1){
  marriage_vec[i] = sample(marriagecats,1,prob = c(MarriageProb[NeighborhoodVec[i]], 1-MarriageProb[NeighborhoodVec[i]]))
  }else{
    marriage_vec[i]=2
  }
}

numpartners=numeric()
numpartners[which(gender_vec==1)]=sample(0:4,length(which(gender_vec==1)), prob=PartnerDist[,1], replace = TRUE) ###Number of Partners for Men
numpartners[which(gender_vec==2)]=sample(0:4,length(which(gender_vec==2)), prob=PartnerDist[,2], replace = TRUE) ###Number of Partners for Women
UpdatedTail=(numpartners[which(numpartners==4)] +rpois(which(numpartners==4),1))
numpartners[which(numpartners==4)]= UpdatedTail
numpartners[which(marriage_vec==1)]=1
numpartners[which(age_vec==1)]=0
Partners=rep(list(),n) #######List Containing Everyone's Partners
ActualConnects=rep(0,n) ######## List Containing Current Partners
ElegibleNeighborhoods=1:length(ExtraData$Neighborhood)
HealthInsure=numeric()

for (i in 1:n){
  HealthInsure[i]=sample(1:2, 1, prob = c(ExtraData$Percent_without_insurance[NeighborhoodVec[i]]/100, 1-ExtraData$Percent_without_insurance[NeighborhoodVec[i]]/100))
  
  if (marriage_vec[i]==1){
  ElegibleNeighborhoods=NeighborhoodVec[i]}else{
  ElegibleNeighborhoods=1:length(ExtraData$Neighborhood)
  }
  
  if (numpartners[i]>ActualConnects[i]){ #########If you have connects to Be filled
    if (length(ElegibleNeighborhoods)>1){ #####See if we can pick neighborhoods
    ContactNeigh=sample(ElegibleNeighborhoods, (numpartners[i]-ActualConnects[i]), replace=TRUE, prob=P[NeighborhoodVec[i],])}else{
      ContactNeigh=ElegibleNeighborhoods
    }
    
    #######Select Neighborhood Potential Partner Lives in
    for (j in 1:(numpartners[i]-ActualConnects[i])){ #######For every available partnership
      if (length(which(NeighborhoodVec==ContactNeigh[j] & age_vec!=1 & gender_vec!=gender_vec[i] & numpartners>ActualConnects))>0){#####Checks Neighborhood to see if anyone of opposite gender and same age group can be your partner
         if (length(which(NeighborhoodVec==ContactNeigh[j] & age_vec!=1 & gender_vec!=gender_vec[i] & numpartners>ActualConnects))>1){ ####if options pick one
          NewPartner=sample(which(NeighborhoodVec==ContactNeigh[j] & age_vec!=1 & gender_vec!=gender_vec[i] & numpartners>ActualConnects),1) ####Picks partner in that neighborhood
           }else{
            NewPartner=which(NeighborhoodVec==ContactNeigh[j] & age_vec!=1 & gender_vec!=gender_vec[i] & numpartners>ActualConnects)
           }
        
        ActualConnects[NewPartner]= ActualConnects[NewPartner]+1 ####Adds to your and partners number of connections
        ActualConnects[i]= ActualConnects[i]+1 ####Adds to your and partners number of connections
        Partners[i][[1]]=c(Partners[i][[1]],NewPartner) #####Updates Your List with Selected Partner
        Partners[NewPartner][[1]]=c(Partners[NewPartner][[1]], i) #######Updates Selected Partner with You
      }
    }
  }
  print(i)
}




#df=data_frame(numpartners=numpartners,gender=gender_vec, age=age_vec, ActualConnects=ActualConnects)
#ggplot(df,aes(x=numpartners)) +   geom_histogram(data= subset(df, gender==1), fill = "pink", alpha = 0.2) +
 # geom_histogram(data=subset(df, gender==2),fill = "blue", alpha = 0.2) 
#ggplot(df)  +   geom_bar(data= subset(df, age!=1), aes(x=numpartners), fill = "royalblue4", alpha = 0.6) +
#  geom_bar(data=subset(df, age!=1),aes(x=ActualConnects), fill = "pink", alpha = 0.6) + labs(x="Number of Partners (Men)")



########################Disease Dynamics #####################################################

Partner=numeric()
MaxTime=365
beta=c(0.3, 0.6)
delta=52/(365) ####maximim recovery rate
deltavec=matrix(c(delta*exp(0.5*(-(ClinicDist/max(ClinicDist))^2)), rep(delta, length(ExtraData$Neighborhood))), ncol=2)
meanencounterrate=30/365
S=matrix(NA,nrow= n, ncol=MaxTime+1) ####State Matrix######
S[,1]=rep(0,n)
for (I in 1:length(ExtraData$Neighborhood)){
  S[sample(which(NeighborhoodVec==I & ActualConnects>1),ceiling(0.01*length(which(NeighborhoodVec==I)))),1]=1
}

for (t in 1:MaxTime){
  for (i in 1:n){######### For each individual##############
    ################## Disease Dynamics ######################
    if (ActualConnects[i]>0){ #########If You Have Connections
      
      if (runif(1)<(meanencounterrate + 52/365*(ActualConnects[i]-1))){ #####If you Have a Contact
        Partner=sample(Partners[[i]], 1) ######Choose A Partner
        if (S[i,t]==0){ #### If susceptible, calculate probability of infection
          if (runif(1)<beta[gender_vec[i]]*S[Partner,t]){ #####If less than prob, you are infected
            S[i, t+1]=1
          }else{S[i,t+1]=0}### else you are not
        }
        
      if (S[i,t]==1){ #### If you are infected, calculate prob for infecting others
          if (runif(1)<beta[gender_vec[Partner]]){
            S[Partner, t+1]=1 #partner becomes infected
          }}}}
    
    
    if (S[i,t]==1){ ######Occurs Regardless of Number of Partners
      if (runif(1)< deltavec[NeighborhoodVec[i], HealthInsure[i]]){ #later will change as a function of distance from clinics * exp((-log(52)*ClinicDist[NeighborhoodVec[i]]/max(ClinicDist))) ##log52 ensures that min period is around 1 year
        S[i,t+1]=0
      }else{S[i,t+1]=1}
    }
    
    if (S[i,t]==0){#########If You Are Suspectible
      S[i,t+1]=S[i,t]
    }
  }
  print(t)
}

#FractionInfect=numeric()
#for (I in 1:length(ElegibleNeighborhoods)){
#  FractionInfect[I]= sum(S[which(NeighborhoodVec==I), MaxTime])/ ExtraData$X2010_Population[I] ####Divide Num Infected of that Neighborhood By Total Number
#} ########Corresponds to Order in DataFrame
#FractionInfect=100*FractionInfect



#Chicago@data$FRACT=FractionInfect #########Generates Heat Map
#Chicago2 = Chicago %>% fortify(region = 'DISTNAME')
#Chicago2  = merge(Chicago2, Chicago@data, by.x = 'id', by.y = 'DISTNAME')
#ggplot() + geom_polygon(data=Chicago2, aes(long, lat, group = group, fill = FRACT)) + coord_equal() +labs(x = "", y = "", fill = "Percent Infected Per Community") +  scale_fill_gradientn(breaks= seq(0, max(FractionInfect), max(FractionInfect)/3),  colours = c("lightblue", "steelblue3", 'royalblue4', "red") ) + geom_point(data=ClinicData, colour="yellow", size=1, shape=21, aes(x=lon, y=lat))
Cases=numeric()
for(t in 1:MaxTime){
  Cases[t]=100000*sum(S[,t])/sum(ExtraData$X2010.Population)
}

CasesPer=numeric()
for (I in 1:length(ElegibleNeighborhoods)){ ###Cases in 100,000
  CasesPer[I]= 100000*sum(S[which(NeighborhoodVec==I), MaxTime])/ ExtraData$X2010.Population[I] ####Divide Num Infected of that Neighborhood By Total Number
} 

Chicago@data$CASES=CasesPer #########Generates Heat Map
Chicago2 = Chicago %>% fortify(region = 'DISTNAME')
Chicago2  = merge(Chicago2, Chicago@data, by.x = 'id', by.y = 'DISTNAME')
ggplot() + geom_polygon(data=Chicago2, aes(long, lat, group = group, fill = CASES)) + coord_equal() +labs(x = "", y = "", fill = "Cases Per 100,000") +  scale_fill_gradientn(breaks= seq(0, max(CasesPer), max(CasesPer)/3),  colours = c("lightblue", "steelblue3", 'royalblue4', "red") ) + geom_point(data=ClinicData, colour="yellow", size=1, shape=21, aes(x=lon, y=lat))


