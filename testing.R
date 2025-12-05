#testing/running code
#load libraries 
library(tidyverse)
#for haversine:
library(pracma)

#read in data
ridership_df <- read.csv("/Users/juliadubnoff/Downloads/ridership_simulated.csv")
otp_df <- read.csv("/Users/juliadubnoff/Downloads/otp_simulated.csv")

#test type of date 
#str(otp_df$Date)
#Date is a character, so occurrences should be too

#weekly occurances starting from first of may as a starting point 
occurrences <- as.character(c("2024-05-01", "2024-05-08", "2024-05-15", "2024-05-22", "2024-05-29"))

#this works, check in :
routes_df <- otp_df %>%
  filter(Date %in% occurrences)

otp_df$Date %in% occurrences



#test haversine
event_lat<-41.79829
event_long<--71.41475
distance <- haversine(c(stops$StopLat[1], stops$StopLng[1]), c(event_lat, event_long))
#this works!



# CURRENT RUN THROUGH OF PROGRAM
ridership_df <- read.csv("/Users/juliadubnoff/Downloads/ridership_simulated.csv")
otp_df <- read.csv("/Users/juliadubnoff/Downloads/otp_simulated.csv")

#run through event_analaysis
occurrences <- as.character(c("2024-05-01", "2024-05-08", "2024-05-15", "2024-05-22", "2024-05-29"))
cleaned_ridership_df <- ride_data_cleaning(ridership_df, occurrences)
#extract routes
routes_df <- route_extraction(otp_df, occurrences)
#find which routes are viable to the event through location_analysis function
#returns a list 

#set values event_lat, event_long, max_distance

#coordinates of hope street farmers market
event_lat <- 41.85480685417614
event_long <- -71.39157664680116

max_distance <- 0.8
viable_routes <- location_analysis(routes_df, event_lat, event_long, max_distance)

is.na(stops$StopLat[23])


#note: this stop SHOULD work 
#hoch: 41.85631 -71.39157
#break down location_analysis function
distance_to_event <- haversine(c(stops$StopLat[19], stops$StopLng[19]),
                               c(event_lat, event_long))
if (distance_to_event <= max_distance){
  nearby_stops_test <- nearby_stops_test %>%
    rbind(stops[19, ])
  
  #nearby_routes_vector <- c(nearby_routes_vector, stops$Route[i])
}


#get a more concise list of stops
stops<-routes_df %>% 
  group_by(Route, Stop) %>%
  summarize(StopLat=mean(StopLat), StopLng=mean(StopLng))

#create dataframe of nearby_stops
nearby_stops<-data.frame("Route" = "temp", "Stop" = "temp", 
                         "StopLat" = "temp", "StopLng" ="temp")
#initialize vector with route number
nearby_routes_vector <- c()
#cycle through all stops with haversine, 
#find all that are within the distance from the event
nrow(stops)
for (i in nrow(stops)){
  distance_to_event <- haversine(c(stops$StopLat[i], stops$StopLng[i]),
                                 c(event_lat, event_long))
  
  #evaluate distance                                                                                                                             event_long))
  if (distance_to_event <= max_distance){
    nearby_stops <- nearby_stops %>%
      rbind(stops[i, ])
    
    #nearby_routes_vector <- c(nearby_routes_vector, stops$Route[i])
  }
  
}

distance_vector<-c()
for (i in 1:nrow(stops)){
  distance_to_event <- haversine(c(stops$StopLat[i], stops$StopLng[i]),
                                 c(event_lat, event_long))
  
  distance_vector<-c(distance_vector, distance_to_event)
  
}


vec<-c()
for (i in 1:nrow(stops)){ 
  vec<-c(vec, i)
}

i<-19
distance_to_event <- haversine(c(stops$StopLat[i], stops$StopLng[i]),
                               c(event_lat, event_long))

#evaluate distance                                                                                                                             event_long))
if (distance_to_event <= max_distance){
  nearby_stops <- nearby_stops %>%
    rbind(stops[i, ])
  }



#remove temp line
nearby_stops <- nearby_stops %>%
  filter(Route!="temp")


#find all routes associates with those stops
routes_to_event <- routes_df %>%
  filter(Route %in% nearby_routes_vector)

return(as.list(routes_to_event, nearby_stops))





