
###################Sum Squared Error computations##########################

percent_simulated=0.01 #percent of population that was simulated

#compute the actual number of individuals infected in each neighboorhood
NumInfec_actual=numeric()
for (I in 1:length(ElegibleNeighborhoods)){
  NumInfec_actual[I]=ceiling(ExtraData[I,9]/100000*ExtraData[I,4]) #get the actual number infected by taking rate per 100,000 divided by 100,000 and multiply by neighboorhood pop
} 

#compute the simulated population in each nieghborhood
Neighborhood_Pop=numeric()
for (i in 1:length(ElegibleNeighborhoods)) {
  Neighborhood_Pop[i]=length(which(NeighborhoodVec==i))*(1/percent_simulated) #create a vector containing the simulated population in each of the 77 neighboorhoods
}

#compute the number of individuals infected in each neighboorhood as simulated by the model
NumInfec_predicted=numeric()
for (I in 1:length(ElegibleNeighborhoods)){
  FractionInfect[I]= sum(S[which(NeighborhoodVec==I), MaxTime])/(100000*percent_simulated) ####Divide Number infected of that Neighborhood by 100,000 times the percent simulated to get rate
  NumInfec_predicted[I]=ceiling(FractionInfect[I]*Neighborhood_Pop[I]) #multiply rate by simulated population of the neighboorhood
} ########Corresponds to Order in DataFrame

#Now can preform SSE with NumInfec_actual and NumInfec_predicted
