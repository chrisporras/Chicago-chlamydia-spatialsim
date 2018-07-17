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

setwd( "/Users/marlinfiggins/Desktop/ChicagoChlamydiaModel/")
Chicago= readOGR(dsn = "chicomm", layer = "chicomm") ######Load GeoSpatial Data
ExtraData = read.csv(file="Chicago Pop Data.csv", header=TRUE, sep=",") #####Load Population and Sq Area Data
ExtraData=ExtraData[as.numeric(Chicago@data[["DISTNAME"]]),] ####Reoorder dfs to same order
ClinicData=read.csv(file="chicago-clinics.csv", header=TRUE)
DistanceMat=spDists(Chicago, Chicago) #Euclidean Distance
alpha=1

# set.api.key("AIzaSyAWCSuIvKzjWJNEBeDO5VoK_TrTHl3qTmk") ########GoogleMaps API
# 
# #########Puts Coordinates in Appropriate Format
# CoordinateVec=character()
# for (i in 1:length(coordinates(Chicago)[,1])){
#   CoordinateVec[i]=paste(coordinates(Chicago)[i,2:1],collapse="+")
# }
# GoogleData=gmapsdistance(CoordinateVec,CoordinateVec , mode="transit", combinations = "all")
# DistanceMat=GoogleData$Distance 
# 
# for (j in 1:length(ExtraData$Neighborhood)){ ######Finds Minimum Distance Between a Clinic and Neighborhood
#   ClinicVec=numeric()
#   for (k in 1:length(ClinicData$Address)){
#     GMAP=gmapsdistance(CoordinateVec[j], paste(ClinicData$Address[k], collapse = ""), mode="transit")
#     ClinicVec[k]= GMAP$Distance
#   }
#   ClinicDist[j]=min(ClinicVec)
# }

##########Switch to Area to Area###############
GTransProb=function(ExtraData, DistanceMat, alpha){
  ProbMat=matrix(NA, nrow=nrow(DistanceMat), ncol=ncol(DistanceMat))
  
  #######Prob of Individual in J going to K based on Transit Distance
  
  for (J in 1:nrow(DistanceMat)){
    for (K in 1:ncol(DistanceMat)){
      ProbMat[J,K]=exp(-((DistanceMat[J,K])/alpha)^2)
    }
  }
  
  ######Normalizing Transition Probs#######
  
  for (J in 1:nrow(DistanceMat)){
    ProbMat[J,]=ProbMat[J,]/sum(ProbMat[J,])
  }
  
  return(ProbMat)
  ########Want to return matrix of prob from going to J to K
}

P=GTransProb(ExtraData, DistanceMat, alpha)
Population=ExtraData$X2010.Population

########################Disease Dynamics #####################################################

n= ceiling(0.01*sum(Population))###########number of individuals total
NeighborhoodVec=sample(1:length(ExtraData$Neighborhood), n, replace=TRUE, prob = Population/sum(Population)) ####Who is in what neighborhood
Partner=numeric()
PI=numeric()
MaxTime=356
beta=0.4
delta=1/(365) 
meanencounters=120
S=matrix(NA,nrow= n, ncol=MaxTime+1) ####State Matrix######
S[,1]=rep(0,n)
S[sample(1:n, ceiling(0.03*n)),1]=1

PP=c(21.5,59.6,10.6/3, 10.6/3, 10.6/3, 5)/sum(c(21.5,59.6,10.6/3, 10.6/3, 10.6/3, 5))
numpartners=sample(0:5,n, prob=PP, replace = TRUE)
numpartners[which(numpartners==5)]=numpartners[which(numpartners==5)]+rpois(length(which(numpartners==5)),1)
Partners=rep(list(),n) #######List Containing Everyone's Partners
ActualConnects=rep(0,n) ######## List Containing Current Partners
ElegibleNeighborhoods=1:length(ExtraData$Neighborhood)

for (i in 1:n){
  if (numpartners[i]>ActualConnects[i]){ #########If you Have Connects to Be filled
    ContactNeigh=sample(ElegibleNeighborhoods, numpartners[i], replace=TRUE, prob=P[NeighborhoodVec[i],])
    for (j in 1:(numpartners[i]-ActualConnects[i])){
      
      if (length(which(NeighborhoodVec==ContactNeigh[j] & numpartners>ActualConnects))!=0){
        NewPartner=sample(which(NeighborhoodVec==ContactNeigh[j] & numpartners>ActualConnects),1)
        ActualConnects[c(NewPartner, i)]= ActualConnects[c(NewPartner, i)]+1
        Partners[i][[1]]=c( Partners[i][[1]],NewPartner) #####Updates Your List with Selected Partner
        Partners[NewPartner][[1]]=c( Partners[NewPartner][[1]], i) #######Updates Selected Partner with You
      }
    }
  }
}


for (t in 1:MaxTime){
  for (i in 1:n){######### For each individual##############
    ################## Disease Dynamics ######################
    if (ActualConnects[i]>0){ #########If You Have Connections
      
    if (runif(1)<meanencounters/365){ #####If you Have a Contact
    Partner=sample(Partners[[i]], 1) ######Choose A Partner
        if (S[i,t]==0){ #### If susceptible, calculate probability of infection
            if (runif(1)<beta*S[Partner,t]){ #####If less than prob, you are infected
             S[i, t+1]=1
        }else{S[i,t+1]=0}### else you are not
       }
    
     if (S[i,t]==1){ #### If you are infected, calculate prob for infecting others
       if (runif(1)<beta){
        S[j, t+1]=1 #partner j becomes infected
      }}}}
      
    
    if (S[i,t]==1){ ######Occurs Regardless of Number of Partners
      if (runif(1)< delta){ #later will change as a function of distance from clinics * exp((-ClinicDist[NeighborhoodVec[i]]/max(ClinicDist))^2)
        S[i,t+1]=0
      }else{S[i,t+1]=1}
    }
    
    if (S[i,t]==0){#########If You Are Suspectible
        S[i,t+1]=S[i,t]
      }
    }
  print(t)
}

FractionInfect=numeric()
for (I in 1:length(ElegibleNeighborhoods)){
  FractionInfect[I]= sum(S[which(NeighborhoodVec==I), MaxTime])/ ExtraData$X2010.Population[I] ####Divide Num Infected of that Neighborhood By Total Number
} ########Corresponds to Order in DataFrame


Chicago@data$FRACT=FractionInfect #########Generates Heat Map
Chicago2 = Chicago %>% fortify(region = 'DISTNAME')
Chicago2  = merge(Chicago2, Chicago@data, by.x = 'id', by.y = 'DISTNAME')
FractInf = Chicago2 %>% select(id) %>% distinct()
ggplot(Chicago2, aes(long, lat, group = group, fill = FRACT)) + geom_polygon() + coord_equal() +labs(x = "", y = "", fill = "Fraction Infected Per Community") +  scale_fill_gradient(low = "dodgerblue", high = "red")


