library(pracma)
library(ggplot2)
library(MASS)
library(compositions)
library(ggmap)
library(rgdal)
library(rgeos)
library(maptools)
library(dplyr)
library(tidyr)
library(tmap)
setwd( "/Users/marlinfiggins/Desktop/ChicagoChlamydiaModel/")
Chicago= readOGR(dsn = "chicomm", layer = "chicomm")
ExtraData = read.csv(file="Chicago Pop Data.csv", header=TRUE, sep=",")
ExtraData=ExtraData[as.numeric(Chicago@data[["DISTNAME"]]),]
DistanceMat=spDists(Chicago, Chicago)
alpha=0.015


##########Switch to Area to Area
GTransProb=function(GridPositions,ExtraData, DistanceMat, alpha){
  ProbMat=matrix(NA, nrow=nrow(GridPositions), ncol=nrow(GridPositions))
  GP=GridPositions 
  
  ##from J to K####### 
  
  #####Find Distance Classes for each neighborhood, find the probability (summed for each neighborhood in that class) and weigh it by the size of the neighborhood
  DistanceClasses= seq(0, max(DistanceMat+0.001), length.out = 20)
  
  for (J in 1:nrow(GridPositions)){
    
    for (i in 2:length(DistanceClasses)){
      DistanceClassProb=0
      NeighClass=which(DistanceMat[J,]<=DistanceClasses[i] & DistanceMat[J,]>=DistanceClasses[i-1])
      for (K in NeighClass){
        DistanceClassProb=exp(-sum(((GridPositions[K,]-GridPositions[J,])/alpha)^2))+DistanceClassProb
      }
      
      for (K in NeighClass){
        ProbMat[J,K]=DistanceClassProb*ExtraData$Land.Area..Acres.[K]/sum(ExtraData$Land.Area..Acres.[NeighClass])
      }
      
    }
  }

  ######Normalizing Transition Probs#######
  
  for (J in 1:nrow(GridPositions)){
    ProbMat[J,]=ProbMat[J,]/sum(ProbMat[J,])
  }
  
  return(ProbMat)
  ########Want to return matrix of prob from going to J to K
}

P=GTransProb(coordinates(Chicago), ExtraData, DistanceMat, alpha)
