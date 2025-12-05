#location analysis

#default max_distance value =0.8 km that designates the radius from an event that a
#stop could be
find_nearby_stops <- function(routes_df, event_lat, event_long, max_distance){
  
  #get a more concise list of stops
  stops<-routes_df %>% 
    group_by(Route, Stop) %>%
    summarize(StopLat=mean(StopLat), StopLng=mean(StopLng))
  
  #create dataframe of nearby_stops
  nearby_stops<-data.frame("Route" = "temp", "Stop" = "temp", 
                           "StopLat" = "temp", "StopLng" ="temp")

  #cycle through all stops with haversine, 
  
  #find all that are within the distance from the event
  for(i in 1:nrow(stops)){
    #if not NA values
    if(!is.na(stops$StopLat[i]) & !is.na(stops$StopLng[i])){
        distance_to_event <- haversine(c(stops$StopLat[i], stops$StopLng[i]),
                                   c(event_lat, event_long))
    
    #evaluate distance        
        if (distance_to_event<=max_distance){
          nearby_stops <- nearby_stops %>%
            rbind(stops[i, ])
          nearby_routes_vector <- c(nearby_routes_vector, stops$Route[i])
        }                                                                                                                        
    }

    
  }
  
  #remove temp line
  nearby_stops <- nearby_stops %>%
    filter(Route!="temp")

  return(nearby_stops)
  

}
