# Google distance matrix script for large list of addresses.
#Version 1.0, 02/01/2016
#Author: Luis Herskovic 

#Notes: This script saves progress, so if it's stopped by any reason, it should resume without losing what it had done
#Load required packages
library(RCurl)
library(stringr)
library(jsonlite)

#API KEY
#Using the google distance matrix requires an API key, request it here (https://developers.google.com/maps/documentation/distance-matrix/get-api-key) and fill out the value below:
API_KEY<-"AIzaSyAWCSuIvKzjWJNEBeDO5VoK_TrTHl3qTmk" ###Paste your API key in this line inside quotation marks####
#API_KEY<-"AIzaSyA1k7brw-Df6IBzKBIwlS4YchfWXBdziLw"
#API_KEY<-"AIzaSyD17KnQItd4Ia86mjE3_yVybqYYfBWQf-k"

#Set the working directory
infile <- "/Users/marlinfiggins/Desktop/ChicagoChlamydiaModel/" ####Set working directory in this line####

#Load CSV file that contains addresses to calculate distances
#The file must contain 2 sets of addresses in separate columns: address_from, address_to
#and a departure_time variable, calculated in seconds from Jan 1st, 1970 
#More information on departure_time can be found here: https://developers.google.com/maps/documentation/distance-matrix/intro
addresses<-read.csv(paste0("", infile ,"ClinicData.csv"),sep=",") ####Paste the name of your csv file here between quotations after infile, using the correct delimiter####

#Define mode of transportation to be used when calculating distances (driving, walking, transit, etc)
transport_mode<-"transit" ###Define transportation mode in this line###

#Trip duration will be in seconds, and trip distance will be in meters

#Create data frames to hold results 
results <- data.frame(index=NA, destination=NA,  origin=NA,	distance=NA,	duration=NA, status=NA)
geocoded <- data.frame(index=NA, destination=NA,  origin=NA,	distance=NA,	duration=NA, status=NA)

address_total<-as.character(addresses$address_from)

#if a previous save file exists we load it and count how far along it is in geocoding
startindex <- 1
tempfilename <- paste0('/', infile, 'temp_geocoded.rds')


if (file.exists(tempfilename)){
  print("Found save file - will resume from index:")
  geocoded <- readRDS(tempfilename)
  startindex <- nrow(geocoded)
  print(startindex)
}

#Now we request the distances between address from the server with a for loop
for (ii in seq(startindex, length(address_total))){
  print(paste("Working on address", ii, "of", length(address_total)))
  add_string<-(sprintf("https://maps.googleapis.com/maps/api/distancematrix/json?origins=%s&destinations=%s&mode=%s&departure_time=%s&key=%s", addresses$address_from[ii], addresses$address_to[ii], transport_mode, addresses$departure_time[ii], API_KEY))
  add_string<-URLencode(add_string, reserved=FALSE) #this avoids problems with spaces in addresses
  answer<-getURL(add_string)
  temp_address_str<-fromJSON(answer)
  status<-as.character(temp_address_str$status[[1]])
  while (status=="OVER_QUERY_LIMIT"|status=="MAX_ELEMENTS_EXCEEDED"){ ##check if we are under the 2500 daily query limit, or under 100 elements for depature_time
    print("OVER QUERY LIMIT - Pausing for 24 hours at:") ##if we are over the limit, pause for 24 hours
    time <- Sys.time()
    print(as.character(time))
    Sys.sleep(60*60)    
    answer<-getURL(add_string)
    temp_address_str<-fromJSON(answer)
    status<-as.character(temp_address_str$status[[1]])
  }
  
  # Execute this in all other cases
  print("Request is valid")  
  address_str<-fromJSON(answer)
  address_str<-as.data.frame(address_str)  
  results$index<-ii
  if(((address_str$elements)[[1]]["status"]=="ZERO_RESULTS")){
    results$destination<-as.character(address_str$destination_addresses[[1]])
    results$origin<-as.character(address_str$origin_addresses[[1]])
    results$distance<-"ZERO_RESULTS"
    results$duration<-"ZERO_RESULTS"
    results$status<-"ZERO_RESULTS"
    geocoded[ii,] <- rbind(results)
  }
  else if(((address_str$elements)[[1]]["status"]=="NOT_FOUND")){
    results$destination<-as.character(address_str$destination_addresses[[1]])
    results$origin<-"NOT_FOUND"
    results$distance<-"NOT_FOUND"
    results$duration<-"NOT_FOUND"
    results$status<-"NOT_FOUND"
    geocoded[ii,] <- rbind(results)  
  }
  else{
    results$destination<-as.character(address_str$destination_addresses[[1]])
    results$origin<-as.character(address_str$origin_addresses[[1]])
    results$distance<-data.frame(address_str[["elements"]][1])[["distance"]]["value"]
    results$duration<-data.frame(address_str[["elements"]][1])[["duration"]]["value"]
    results$status<-as.character(address_str$status[[1]])
    geocoded[ii,] <- rbind(results)
  }
  #save temporary progress in case it fails midway
  saveRDS(geocoded, tempfilename)
  Sys.sleep(0.1*1) #pause for half a second to comply with query limits  
}

#finally write it all to the output files
saveRDS(geocoded, paste0("", infile ,"distances.rds"))
geocoded_for_export <- data.frame(lapply(geocoded, as.character), stringsAsFactors=FALSE)
write.table(geocoded_for_export, file=paste0("",infile, "ClinicDistances.csv"),sep="|") ####Set the filename for your completed file here####
