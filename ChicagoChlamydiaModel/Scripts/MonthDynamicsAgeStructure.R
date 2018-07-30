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

WhichClinics="Free"
WhichDistance='Great Circle'


###############LOAD IN DATA######################
setwd("/Users/marlinfiggins/Desktop/ChicagoChlamydiaModel/Data")
Chicago = readOGR(dsn = "chicomm", layer = "chicomm")  ######Load GeoSpatial Data
ExtraData = read.csv(file = "chicago-pop-data.csv", header = TRUE, sep = ",")  #####Load Population, Marriage Rate, .etc
ExtraData = ExtraData[as.numeric(Chicago@data[["DISTNAME"]]), ]  ####Reoorder dfs to same order
ClinicData = read.csv(file = "ChicagoClinics.csv", header = TRUE)
AgeStrucData=read.csv(file="Chicago_Age_Structure.csv", header=TRUE)
AgeStrucData=AgeStrucData[as.numeric(Chicago@data[["DISTNAME"]]),] ####Read in AGE STRUCTURE

if(WhichDistance=="Great Circle"){
  
  #Great Circle Distance between neighborhoods
  DistanceMat = spDists(Chicago, Chicago) 
  
  ########Clinic Great Circle Distance
  NumericData=matrix(c(ClinicData$lon,ClinicData$lat), ncol=2)
  ClinicDistanceMat=spDists(NumericData,coordinates(Chicago))
  
}else{
  
  ######Neighborhood Transit Data
  TransitData = read.csv(file = "TransitDistances 2.csv", header = TRUE)  ####Reads in Neighborhood Data
  regexp = "[[:digit:]]+"
  NumericData = str_extract_all(TransitData$Distance.Time, regexp, simplify = TRUE)  ####Returns Matrix of Time and Distance Values
  DistanceMat = 0.000621371 * matrix(as.numeric(NumericData[, 1]), nrow = sqrt(length(TransitData$Distance.Time)),  byrow = TRUE)  ####Codifies as Matrix of Distances in miles
  #TimeMat = matrix(as.numeric(NumericData[, 2]), nrow = sqrt(length(TransitData$Distance.Time)),              byrow = TRUE)  ####Codifies as Matrix of Times
  
  ######Clinic Transit Data
  ClinicTransitData = read.csv(file = "ClinicDistances 2.csv", header = TRUE)  ####Reads in Clinic Transit Distance Data
  regexp = "[[:digit:]]+"
  NumericData = str_extract_all(ClinicTransitData$Distance.Time, regexp, simplify = TRUE)  ####Returns Matrix of Time and Distance Values
  ClinicDistanceMat = 0.000621371 * matrix(as.numeric(NumericData[, 1]), ncol = 77)  ####Codifies as Matrix of Distances in miles
  #ClinicTimeMat = matrix(as.numeric(NumericData[, 2]), ncol = 77)  ####Codifies as Matrix of Times
  
}


if(WhichClinics=="Free"){
### Free Clinics Only
ClinicData=ClinicData[22:24,]
ClinicDistanceMat = ClinicDistanceMat[22:24, ]
#ClinicTimeMat = ClinicTimeMat[22:24, ]

#ClinicDistanceMat[which(is.na(ClinicDistanceMat))] = mean(ClinicDistanceMat)
#ClinicTimeMat[which(is.na(ClinicTimeMat))] = mean(ClinicTimeMat)
}

ClinicDist = apply(ClinicDistanceMat, 2, FUN = min, na.rm = TRUE)  ####Minimum Travel Distance, Removing NAs
#ClinicTime = apply(ClinicTimeMat, 2, FUN = min, na.rm = TRUE)  ####Minimum Travel Time, Removing NAs

#############PROBABILITY OF TRAVELING BETWEEN NEIGHBORHOODS 

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

alpha=0.8 ###Interaction Parameter. ###HIGHER MEANS MORE INTERACTION
P=GTransProb(ExtraData, DistanceMat, alpha)

##########Demographics###########################
Population=ExtraData$X2010.Population
Neighborhood=ExtraData$Neighborhood
n= ceiling(.01*sum(Population))###########number of individuals total
NeighborhoodVec=sample(1:length(Neighborhood), n, replace=TRUE, prob = Population/sum(Population)) ####Who is in what neighborhood
AgeGroups = 6 ###Number of Age Classes
###Make age/gender structure distributions
agedist_male = matrix(data = 0, ncol = AgeGroups, nrow  = length(Population))
agedist_female = matrix(data = 0, ncol = AgeGroups,nrow = length(Population))
totaldist = matrix(data = 0, ncol = AgeGroups, nrow = length(Population))
genderdist = matrix(data = 0, ncol = 2, nrow = length(Population))

##Assign Gender and age calss by distributions
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
cohabitingvec = c(0,0,0.16,0.55,0.71,0.65) ####Percent of Each age class cohabit/married
#singlevec = c(1,1,0.84,0.45,0.29,0.35)
#marriagedist = matrix(data = c(cohabitingvec,singlevec),ncol = length(marriagecats), nrow = length(agecats))

MarriageProb= as.numeric(ExtraData$Marriage.Rate) ####Probability of marriage in each neighborhood
gender_vec = vector(length = n) ####Stores gender of each individual
age_vec = vector(length = n)     ####Stores age of each individual
marriage_vec = rep(2, n) ####Stores marriage status of each individual

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
}

#Assign marriage status from marriage dist
for(I in 1:length(Neighborhood)){ ###Each Neighborhood wants a certain number of married folks
 marriage_vec[sample(which(NeighborhoodVec==I), floor(MarriageProb[I]*length(which(NeighborhoodVec==I))), prob= cohabitingvec[age_vec[which(NeighborhoodVec==I)]])]=1 ###Draws marriage status accroding to distribution and age structure
}

numpartners=numeric()
numpartners[which(gender_vec==1 & age_vec!=1 & marriage_vec!=1)]=sample(0:4,length(which(gender_vec==1 & age_vec!=1& marriage_vec!=1)), prob=PartnerDist[,1], replace = TRUE) ###Number of Partners for Single Adult Men
numpartners[which(gender_vec==2 & age_vec!=1 & marriage_vec!=1)]=sample(0:4,length(which(gender_vec==2 & age_vec!=1& marriage_vec!=1)), prob=PartnerDist[,2], replace = TRUE) ###Number of Partners for Single Adult Women
UpdatedTail=(numpartners[which(numpartners==4)] +rpois(which(numpartners==4),1))
numpartners[which(numpartners==4)]= UpdatedTail ###Updates Tail of Distribution with poisson
numpartners[which(marriage_vec==1)]=1 #########Sets married people to have oen partner
numpartners[which(age_vec==1)]=0 ###sets children to have none.

Partners=rep(list(),n) #######List Containing Everyone's Partners
ActualConnects=rep(0,n) ######## List Containing Current Partners
ElegibleNeighborhoods=1:length(Neighborhood)
PovPer=numeric()

for (i in 1:n){ ####For every person
  PovPer[i]=sample(1:2, 1, prob = c(ExtraData$Percent_Poverty[NeighborhoodVec[i]]/100, 1-ExtraData$Percent_Poverty[NeighborhoodVec[i]]/100)) ##Assign poverty status based on neighborhood

  if (marriage_vec[i]==1){ ####Married people select within neighborhood.
    ElegibleNeighborhoods=NeighborhoodVec[i]}else{
      ElegibleNeighborhoods=1:length(Neighborhood)
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
        }else{### You have one option and choose them
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
#Plot1=ggplot(df)  +   geom_bar(data= subset(df, age!=1), aes(x=numpartners), fill = "royalblue4", alpha = 0.6) +
#  geom_bar(data=subset(df, age!=1),aes(x=ActualConnects), fill = "pink", alpha = 0.6) + labs(x="Number of Partners (Men)")

#Plot2=ggplot(df)  +   geom_hist(data= subset(df, gender_vec=1), aes(x=numpartners), fill = "royalblue4", alpha = 0.6) +
  #geom_bar(data=subset(df, gender_vec=1),aes(x=ActualConnects), fill = "pink", alpha = 0.6) + labs(x="Number of Partners (Men)")



######################## Disease Dynamics #####################################################

MaxTime = 400
beta = c(0.2, 0.2) ####Percent chance of contraction per encounter men, wome.
PI=numeric()
delta = 0.15  ####Percent that go for testing

deltavec = matrix(c(delta * exp( -3*(ClinicDist/max(ClinicDist))^2), rep(delta, length(Neighborhood))), ncol = 2)

#deltavec = matrix(c(delta * exp( -(ClinicDist/alpha)^2), rep(delta, length(Neighborhood))), ncol = 2)
S = matrix(NA, nrow = n, ncol = MaxTime + 1)  ####State Matrix######
S[, 1] = rep(0, n)
for (I in 1:length(Neighborhood)) {
  S[sample(which(NeighborhoodVec == I & ActualConnects > 0), ceiling(0.005 * length(which(NeighborhoodVec == I& ActualConnects > 0)))), 1] = 1
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
      PI[i] = 1 - prod(1 - beta[gender_vec[contacts]] * S[contacts, t])

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

CasesPer=matrix(nrow = length(Neighborhood), ncol=MaxTime)
for (t in MaxTime:MaxTime){
  for (I in 1:length(Neighborhood)) {   ### Cases per 100,000 by Neighborhood
    CasesPer[I,t] = 1e+05 * sum(S[which(NeighborhoodVec == I), t])/length(which(NeighborhoodVec==I))  ####Divide Num Infected of that Neighborhood By Total Number
  }}

Chicago@data$CASES = CasesPer[,MaxTime]  #########Generates Heat Map
Chicago2 = Chicago %>% fortify(region = "DISTNAME")
Chicago2 = merge(Chicago2, Chicago@data, by.x = "id", by.y = "DISTNAME")
ggplot() + geom_polygon(data = Chicago2, aes(long, lat, group = group, fill = CASES)) +
  coord_equal() + labs(x = "", y = "", fill = "Cases Per 100,000") + scale_fill_gradientn(breaks = seq(0,
                                                                                                       max(CasesPer[,MaxTime]), max(CasesPer[,MaxTime])/3), colours = c("lightblue", "steelblue3", "royalblue4",
                                                                                                                                                    "red")) + geom_point(data = ClinicData, colour = "yellow", size = 1, shape = 21,
                                                                                                                                                                         aes(x = lon, y = lat))



