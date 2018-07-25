library(ggmap)
geocoded <- data.frame(stringsAsFactors = FALSE)

# Loop through the addresses to get the latitude and longitude of each address and add it to the
# origAddress data frame in new columns lat and lon


key <- "AIzaSyAWCSuIvKzjWJNEBeDO5VoK_TrTHl3qTmk"
register_google(key=key)

  result=geocode(as.character(ClinicData$Address), output = "latlona",override_limit=TRUE, source = "google")
  ClinicData$lon=as.numeric(result[[1]])
  ClinicData$lat=as.numeric(result[[2]])
write.csv(ClinicData,file='chicago-clinics2.csv')

