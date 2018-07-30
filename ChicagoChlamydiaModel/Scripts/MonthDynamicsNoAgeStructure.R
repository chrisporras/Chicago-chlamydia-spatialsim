library(ggplot2)
library(ggmap)
library(rgdal)
library(rgeos)
library(maptools)
library(dplyr)
library(tidyr)
library(tmap)
library(sp)
library(stringr)

setwd("/Users/marlinfiggins/Desktop/ChicagoChlamydiaModel/Data")
Chicago = readOGR(dsn = "chicomm", layer = "chicomm")  ######Load GeoSpatial Data
ExtraData = read.csv(file = "chicago-pop-data.csv", header = TRUE, sep = ",")  #####Load Population, Marriage Rate, .etc
ExtraData = ExtraData[as.numeric(Chicago@data[["DISTNAME"]]), ]  ####Reoorder dfs to same order
DistanceMat = spDists(Chicago, Chicago)  #Great Circle Distance between neighborhoods
ClinicData = read.csv(file = "ChicagoClinics.csv", header = TRUE)

######Clinic Transit Data
#ClinicTransitData = read.csv(file = "ClinicDistances 2.csv", header = TRUE)  ####Reads in Clinic Transit Distance Data
#regexp = "[[:digit:]]+"
#NumericData = str_extract_all(ClinicTransitData$Distance.Time, regexp, simplify = TRUE)  ####Returns Matrix of Time and Distance Values
#ClinicDistanceMat = 0.000621371 * matrix(as.numeric(NumericData[, 1]), ncol = 77)  ####Codifies as Matrix of Distances in miles
#ClinicTimeMat = matrix(as.numeric(NumericData[, 2]), ncol = 77)  ####Codifies as Matrix of Times

########Clinic Great Circle Distance
NumericData=matrix(c(ClinicData$lon,ClinicData$lat), ncol=2)
ClinicDistanceMat=spDists(NumericData,coordinates(Chicago))

### Free Clinics Only
ClinicDistanceMat = ClinicDistanceMat[22:24, ]
#ClinicTimeMat = ClinicTimeMat[22:24, ]

ClinicDistanceMat[which(is.na(ClinicDistanceMat))] = mean(ClinicDistanceMat)
#ClinicTimeMat[which(is.na(ClinicTimeMat))] = mean(ClinicTimeMat)

ClinicDist = apply(ClinicDistanceMat, 2, FUN = min, na.rm = TRUE)  ####Minimum Travel Distance, Removing NAs
#ClinicTime = apply(ClinicTimeMat, 2, FUN = min, na.rm = TRUE)  ####Minimum Travel Time, Removing NAs


#TransitData = read.csv(file = "TransitDistances 2.csv", header = TRUE)  ####Reads in Neighborhood Data
#regexp = "[[:digit:]]+"
#NumericData = str_extract_all(TransitData$Distance.Time, regexp, simplify = TRUE)  ####Returns Matrix of Time and Distance Values
#DistanceMat = 0.000621371 * matrix(as.numeric(NumericData[, 1]), nrow = sqrt(length(TransitData$Distance.Time)),  byrow = TRUE)  ####Codifies as Matrix of Distances in miles
#TimeMat = matrix(as.numeric(NumericData[, 2]), nrow = sqrt(length(TransitData$Distance.Time)),              byrow = TRUE)  ####Codifies as Matrix of Times

####Read in AGE STRUCTURE
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
alpha=0.1 ###Interaction Parameter.
P=GTransProb(ExtraData, DistanceMat, alpha)

###Demographics
Population=ExtraData$X2010.Population
n= ceiling(.01*sum(Population))###########number of individuals total
NeighborhoodVec=sample(1:length(ExtraData$Neighborhood), n, replace=TRUE, prob = Population/sum(Population)) ####Who is in what neighborhood
PP=c(21.5,59.6,10.6/3, 10.6/3, 10.6/3, 5)/sum(c(21.5,59.6,10.6/3, 10.6/3, 10.6/3, 5)) ####Probability distribution for number of partners
numpartners=sample(0:5,n, prob=PP, replace = TRUE)
numpartners[which(numpartners==5)]=numpartners[which(numpartners==5)]+rpois(length(which(numpartners==5)),1) ###Upades Tail of Number of Partners
Partners=rep(list(),n) #######List Containing Everyone's Partners
ActualConnects=rep(0,n) ######## List Containing Current Partners
ElegibleNeighborhoods=1:length(ExtraData$Neighborhood)

PovPer=numeric() ####Holds Poverty Status of Each Individual

for (i in 1:n){ ####For every Person
  #####Give them Poverty Status Based on Neighborhood
  PovPer[i]=sample(1:2, 1, prob = c(ExtraData$Percent_Poverty[NeighborhoodVec[i]]/100, 1-ExtraData$Percent_Poverty[NeighborhoodVec[i]]/100))
  if (numpartners[i]>ActualConnects[i]){ #########If you have connects to be filled
    if (length(ElegibleNeighborhoods)>1){ #####See if we can pick neighborhoods
      ContactNeigh=sample(ElegibleNeighborhoods, (numpartners[i]-ActualConnects[i]), replace=TRUE, prob=P[NeighborhoodVec[i],])}else{
        ContactNeigh=ElegibleNeighborhoods
      }

    #######Select Neighborhood Potential Partner Lives in
    for (j in 1:(numpartners[i]-ActualConnects[i])){ #######For every available partnership
      if (length(which(NeighborhoodVec==ContactNeigh[j] & numpartners>ActualConnects))>0){#####Checks Neighborhood to see if anyone can be your partner
        if (length(which(NeighborhoodVec==ContactNeigh[j] & numpartners>ActualConnects))>1){ ####if you have severak options, pick one
          NewPartner=sample(which(NeighborhoodVec==ContactNeigh[j] & numpartners>ActualConnects),1) ####Picks partner in that neighborhood
        }else{ ### you have only one choice, so pick them
          NewPartner=which(NeighborhoodVec==ContactNeigh[j] & numpartners>ActualConnects)
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

#df=data_frame(numpartners=numpartners, ActualConnects=ActualConnects)
#ggplot(df) + geom_bar( aes(x=numpartners), fill = 'royalblue4', alpha = 0.6) +
 #geom_bar(aes(x=ActualConnects), fill = 'pink', alpha = 0.6) + labs(x='Number of Partners')


######################## Disease Dynamics #####################################################

MaxTime = 144
# MaxTime=500
beta = 0.4 ###percent chance of becoming infected by one encounter
PI=numeric() ####Probability you are infected after one time step
delta = 0.15  ####Percent that go for testing
deltavec = matrix(c(delta * exp(1 * (-(ClinicDist/max(ClinicDist))^2)), rep(delta, length(ExtraData$Neighborhood))), ncol = 2)
S = matrix(NA, nrow = n, ncol = MaxTime + 1)  ####State Matrix with 0 suscpetible, 1 infected######
S[, 1] = rep(0, n) ####Everyone is susceptible
for (I in 1:length(ExtraData$Neighborhood)) { ######Some fraction of each neighborhood is infected.
  S[sample(which(NeighborhoodVec == I & ActualConnects > 0), ceiling(0.02 * length(which(NeighborhoodVec ==
    I)))), 1] = 1
}

for (t in 1:MaxTime) { ####For each time step
  for (i in 1:n) { ####For each individual in our population

    if (ActualConnects[i] > 0) { ###People with contacts draw their encounters for the timestep

      if (S[i,t]==0){ ####For susceptibles,
        if (length(Partners[[i]])>1){
      contacts = sample(Partners[[i]], 5, replace = TRUE) ###Encounters for time step
    }else{
      contacts=rep(Partners[[i]],5)
    }
      PI[i] = 1 - prod(1 - beta * S[contacts, t])

      if (runif(1)< PI[i]) { ######### Draw random number
        S[i, t + 1] = 1 ###You contract.
      } else {
        S[i, t + 1] = 0 ####You're safe.
      }
    }} else { ###If you have no contacts, you're in the same state as before
      S[i, t + 1] = S[i, t]
    }

    if (S[i, t] == 1) {
      TestChance = deltavec[NeighborhoodVec[i], PovPer[i]]
      #### Chance you get tested. Interviduals in poverty depend on free clinics for access.
      if (runif(1) < TestChance) {
        S[i, t + 1] = 0  #####Test and Chance Recover ##sample(0:1,)
      } else {
        S[i, t + 1] = 1  ##########No test, no recovery
      }

    }

  }
  print(t)
}


# FractionInfect=numeric() for (I in 1:length(ElegibleNeighborhoods)){
# FractionInfect[I]= sum(S[which(NeighborhoodVec==I), MaxTime])/
# ExtraData$X2010_Population[I] ####Divide Num Infected of that Neighborhood By
# Total Number } ########Corresponds to Order in DataFrame
# FractionInfect=100*FractionInfect



# Chicago@data$FRACT=FractionInfect #########Generates Heat Map Chicago2 =
# Chicago %>% fortify(region = 'DISTNAME') Chicago2 = merge(Chicago2,
# Chicago@data, by.x = 'id', by.y = 'DISTNAME') ggplot() +
# geom_polygon(data=Chicago2, aes(long, lat, group = group, fill = FRACT)) +
# coord_equal() +labs(x = '', y = '', fill = 'Percent Infected Per Community') +
# scale_fill_gradientn(breaks= seq(0, max(FractionInfect),
# max(FractionInfect)/3), colours = c('lightblue', 'steelblue3', 'royalblue4',
# 'red') ) + geom_point(data=ClinicData, colour='yellow', size=1, shape=21,
# aes(x=lon, y=lat))

# Chicago@data$Insurance=ExtraData$Percent_without_insurance #########Generates
# Heat Map Chicago2 = Chicago %>% fortify(region = 'DISTNAME') Chicago2 =
# merge(Chicago2, Chicago@data, by.x = 'id', by.y = 'DISTNAME') ggplot() +
# geom_polygon(data=Chicago2, aes(long, lat, group = group, fill = Insurance)) +
# coord_equal() +labs(x = '', y = '', fill = 'Percent without Insurance') +
# scale_fill_gradientn(breaks=
# seq(0,max(as.numeric(ExtraData$Percent_without_insurance)),
# max(as.numeric(ExtraData$Percent_without_insurance))/3), colours =
# c('lightblue', 'steelblue3', 'royalblue4', 'red') ) +
# geom_point(data=ClinicData, colour='yellow', size=1, shape=21, aes(x=lon,
# y=lat))


Cases = numeric()
for (t in 1:MaxTime) {
  Cases[t] = 1e+05 * sum(S[, t])/n
}

CasesPer = numeric()
for (I in 1:length(ElegibleNeighborhoods)) {
  ### Cases in 100,000
  CasesPer[I] = 1e+05 * sum(S[which(NeighborhoodVec == I), MaxTime])/length(which(NeighborhoodVec==I))  ####Divide Num Infected of that Neighborhood By Total Number
}

Chicago@data$CASES = CasesPer  #########Generates Heat Map
Chicago2 = Chicago %>% fortify(region = "DISTNAME")
Chicago2 = merge(Chicago2, Chicago@data, by.x = "id", by.y = "DISTNAME")
ggplot() + geom_polygon(data = Chicago2, aes(long, lat, group = group, fill = CASES)) +
  coord_equal() + labs(x = "", y = "", fill = "Cases Per 100,000") + scale_fill_gradientn(breaks = seq(0,
                                                                                                       max(CasesPer), max(CasesPer)/3), colours = c("lightblue", "steelblue3", "royalblue4",
                                                                                                                                                    "red")) + geom_point(data = ClinicData, colour = "yellow", size = 1, shape = 21,
                                                                                                                                                                         aes(x = lon, y = lat))
