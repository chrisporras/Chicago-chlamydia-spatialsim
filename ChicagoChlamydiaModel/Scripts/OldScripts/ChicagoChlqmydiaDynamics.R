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

setwd( "/Users/marlinfiggins/Desktop/ChicagoChlamydiaModel/")
Chicago= readOGR(dsn = "chicomm", layer = "chicomm") ######Load GeoSpatial Data
ExtraData = read.csv(file="chicago-pop-data.csv", header=TRUE, sep=",") #####Load Population and Sq Area Data
ExtraData=ExtraData[as.numeric(Chicago@data[["DISTNAME"]]),] ####Reoorder dfs to same order
ClinicData=read.csv(file="chicago-clinics.csv", header=TRUE)
DistanceMat=spDists(Chicago, Chicago) #Euclidean Distance

ClinicTransitData=read.csv(file="ClinicDistances 2.csv", header = TRUE) ####Reads in Clinic Data
regexp = "[[:digit:]]+"
NumericData=str_extract_all(ClinicTransitData$Distance.Time, regexp, simplify = TRUE) ####Returns Matrix of Time and Distance Values
ClinicDistanceMat=0.000621371*matrix(as.numeric(NumericData[,1]), nrow=77) ####Codifies as Matrix of Distances in miles
ClinicTimeMat=matrix(as.numeric(NumericData[,2]), nrow=77)####Codifies as Matrix of Times

ClinicDist=apply(ClinicDistanceMat, 1, FUN=mean, na.rm=TRUE) ####Minimum Travel Distance, Removing NAs
ClinicTime=apply(ClinicTimeMat, 1, FUN=mean, na.rm=TRUE) ####Minimum Travel Time, Removing NAs


TransitData=read.csv(file="TransitDistances 2.csv", header = TRUE) ####Reads in Neighborhood Data
regexp = "[[:digit:]]+"
NumericData=str_extract_all(TransitData$Distance.Time, regexp, simplify = TRUE) ####Returns Matrix of Time and Distance Values
DistanceMat=0.000621371*matrix(as.numeric(NumericData[,1]), nrow=sqrt(length(TransitData$Distance.Time))) ####Codifies as Matrix of Distances in miles
TimeMat=matrix(as.numeric(NumericData[,2]), nrow=sqrt(length(TransitData$Distance.Time))) ####Codifies as Matrix of Times


alpha=1


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

P=GTransProb(ExtraData, DistanceMat, alpha)
Population=ExtraData$X2010_Population

########################Disease Dynamics #####################################################

n= ceiling(.01*sum(Population))###########number of individuals total
NeighborhoodVec=sample(1:length(ExtraData$Neighborhood), n, replace=TRUE, prob = Population/sum(Population)) ####Who is in what neighborhood
Partner=numeric()
PI=numeric()
MaxTime=365
beta=0.6
delta=52/(365) ####maximim recovery rate
deltavec=delta*exp((-log(52)*ClinicDist/max(ClinicDist)))
meanencounters=120/365
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
  if (numpartners[i]>ActualConnects[i]){ #########If you have connects to Be filled
    ContactNeigh=sample(ElegibleNeighborhoods, (numpartners[i]-ActualConnects[i]), replace=TRUE, prob=P[NeighborhoodVec[i],])
    #######Select Neighborhood Potential Partner Lives in
    for (j in 1:(numpartners[i]-ActualConnects[i])){ #######For every available partnership
      
      if (length(which(NeighborhoodVec==ContactNeigh[j] & numpartners>ActualConnects))!=0){ #####Checks Neighborhood to see if anyone wants to be your partner
        NewPartner=sample(which(NeighborhoodVec==ContactNeigh[j] & numpartners>ActualConnects),1) ####Picks partner in that neighborhood
        ActualConnects[c(NewPartner, i)]= ActualConnects[c(NewPartner, i)]+1 ####Adds to your and partners number of connections
        Partners[i][[1]]=c( Partners[i][[1]],NewPartner) #####Updates Your List with Selected Partner
        Partners[NewPartner][[1]]=c( Partners[NewPartner][[1]], i) #######Updates Selected Partner with You
      }
    }
  }
  print(i)
}


for (t in 1:MaxTime){
  for (i in 1:n){######### For each individual##############
    ################## Disease Dynamics ######################
    if (ActualConnects[i]>0){ #########If You Have Connections
      
    if (runif(1)<meanencounterrate){ #####If you Have a Contact
    Partner=sample(Partners[[i]], 1) ######Choose A Partner
        if (S[i,t]==0){ #### If susceptible, calculate probability of infection
            if (runif(1)<beta*S[Partner,t]){ #####If less than prob, you are infected
             S[i, t+1]=1
        }else{S[i,t+1]=0}### else you are not
       }
    
     if (S[i,t]==1){ #### If you are infected, calculate prob for infecting others
       if (runif(1)<beta){
        S[Partner, t+1]=1 #partner becomes infected
      }}}}
      
    
    if (S[i,t]==1){ ######Occurs Regardless of Number of Partners
      if (runif(1)< deltavec[NeighborhoodVec[i]]){ #later will change as a function of distance from clinics * exp((-log(52)*ClinicDist[NeighborhoodVec[i]]/max(ClinicDist))) ##log52 ensures that min period is around 1 year
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
  FractionInfect[I]= sum(S[which(NeighborhoodVec==I), MaxTime])/ ExtraData$X2010_Population[I] ####Divide Num Infected of that Neighborhood By Total Number
} ########Corresponds to Order in DataFrame

FractionInfect=100*FractionInfect

Chicago@data$FRACT=FractionInfect #########Generates Heat Map
Chicago2 = Chicago %>% fortify(region = 'DISTNAME')
Chicago2  = merge(Chicago2, Chicago@data, by.x = 'id', by.y = 'DISTNAME')
ggplot(Chicago2, aes(long, lat, group = group, fill = FRACT)) + geom_polygon() + coord_equal() +labs(x = "", y = "", fill = "Percent Infected Per Community") +  scale_fill_gradientn(breaks= seq(0, max(FractionInfect), max(FractionInfect)/3), colours = c("lightblue", "steelblue3", 'royalblue4', "red") ) 


