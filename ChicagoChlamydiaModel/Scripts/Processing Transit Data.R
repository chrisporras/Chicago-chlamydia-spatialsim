library(stringr)

TransitData=read.csv(file="TransitDistances 2.csv", header = TRUE) ####Reads in Neighborhood Data
regexp = "[[:digit:]]+"
NumericData=str_extract_all(TransitData$Distance.Time, regexp, simplify = TRUE) ####Returns Matrix of Time and Distance Values
DistanceMat=0.000621371*matrix(as.numeric(NumericData[,1]), nrow=sqrt(length(TransitData$Distance.Time))) ####Codifies as Matrix of Distances
TimeMat=matrix(as.numeric(NumericData[,2]), nrow=sqrt(length(TransitData$Distance.Time))) ####Codifies as Matrix of Times


ClinicTransitData=read.csv(file="ClinicDistances 2.csv", header = TRUE) ####Reads in Clinic Data
regexp = "[[:digit:]]+"
NumericData=str_extract_all(ClinicTransitData$Distance.Time, regexp, simplify = TRUE) ####Returns Matrix of Time and Distance Values
ClinicDistanceMat=0.000621371*matrix(as.numeric(NumericData[,1]), nrow=77) ####Codifies as Matrix of Distances in miles
ClinicTimeMat=matrix(as.numeric(NumericData[,2]), nrow=77)####Codifies as Matrix of Times

ClinicDistanceMat[which(ClinicDistanceMat==0)]=NA
ClinicTimeMat[which(ClinicTimeMat==0)]=NA

ClinicDist=apply(ClinicDistanceMat, 1, FUN=mean, na.rm=TRUE) ####Minimum Travel Distance, Removing NAs
ClinicTime=apply(ClinicTimeMat, 1, FUN=mean, na.rm=TRUE) ####Minimum Travel Time, Removing NAs

plot(sort(ClinicDist))

ClinicDistanceClasses