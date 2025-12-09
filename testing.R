#testing/running code
#load libraries 
library(tidyverse)
#for haversine:
library(pracma)
library(ggplot2)
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



#test

# CURRENT RUN THROUGH OF PROGRAM
ridership_df <- read.csv("/Users/juliadubnoff/Downloads/ridership_simulated.csv")
otp_df <- read.csv("/Users/juliadubnoff/Downloads/otp_simulated.csv")

#run through event_analaysis
occurrences <- as.character(c("2024-05-01", "2024-05-08", "2024-05-15", "2024-05-22", "2024-05-29"))
cleaned_ridership_df <- ride_data_cleaning(ridership_df)
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
viable_stops<-find_nearby_stops(routes_df, event_lat, event_long, max_distance)


#EXTRA CODE: 
#filter to make the data set only include routes on the days of occurrence
#filter(Date %in% occurrences) %>%

#test change_ridership

route_usage <- cleaned_ridership_df %>% 
  group_by(Date, Hour, Route) %>%
  summarize(sum_riders=n()) %>%
  ungroup()%>%
  #categorize whether the events are in the occurances
  mutate(event_day=case_when(Date %in% occurrences ~ 1, TRUE ~ 0)) %>%
  group_by(Route, event_day) %>%
  summarize(mean_riders=mean(sum_riders)) %>%
  pivot_wider(names_from=event_day, values_from = mean_riders, names_prefix = "mean_riders_") %>%
  #evaluate how much ridership changes between event day and non event day for route
  mutate(difference_of_mean_riders=mean_riders_1-mean_riders_0) %>%
  #remove NA entries 
  filter(!is.na(difference_of_mean_riders)) %>%
  #create entry to designate if nearby route 
  mutate(is_viable_route = case_when( Route %in% viable_routes ~ 1, TRUE ~ 0))


t.test(data=route_usage, difference_of_mean_riders~is_viable_route, alternative="two.sided")
#this works !




#extract routes
routes_df <- route_extraction(otp_df, occurrences)

#find which stops are viable to the event through location_analysis function
nearby_stops <- find_nearby_stops(routes_df, event_lat, event_long, max_distance)
#find which routes are viable
viable_routes<-unique(nearby_stops$Route)

#change in route usage during event:
route_usage_df <- route_usage(cleaned_ridership_df, viable_routes, occurances)


route_usage2 <- cleaned_ridership_df %>% 
  group_by(Date, Hour, Route) %>%
  summarize(sum_riders=n()) %>%
  ungroup()%>%
  #categorize whether the events are in the occurrences
  mutate(event_day=case_when(Date %in% occurrences ~ "event_day", TRUE ~ "not_event")) %>%
  pivot_wider(names_from=event_day, values_from = sum_riders, names_prefix = "sum_riders_") 

group_by(Route, event_day) %>%
  mutate(difference_sum_riders = sum_riders_event_day - sum_riders_not_event)%>%
  filter(!is.na(difference_sum_riders)) %>%
  mutate(is_viable_route = case_when( Route %in% viable_routes ~ 1, TRUE ~ 0))


test_general<-t.test(data=route_usage_df, difference_of_mean_riders~is_viable_route, alternative="two.sided")
#PRINT TEST
print(test_general)
#' @param occurrences vector listing dates (in standard character form) that the recurring event recurs
#' 
#' 
#'   
#create additional data sets that could be used for analyses of demographics if a user chose
usage_demographic_college <- usage_demographics(ridership_df, College, viable_routes, occurances)
usage_demographic_highschool <- usage_demographics(ridership_df, High.School, viable_routes, occurances)
usage_demographic_lowinc <- usage_demographics(ridership_df, Low.Income, viable_routes, occurances)
