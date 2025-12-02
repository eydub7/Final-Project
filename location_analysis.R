#location analysis

#default max_distance value =0.8 km that designates the radius from an event that a
#stop could be
location_analysis <- function(routes_df, event_lat, event_long, max_distance){
  
  #get a more concise list of stops
  stops<-routes_df %>% 
    group_by(Route, Stop) %>%
    summarize(StopLat=mean(StopLat), StopLng=mean(StopLng))
  
  #create dataframe of nearby_stops
  nearby_stops<-data.frame("Route" = "temp", "Stop" = "temp", 
                           "StopLat" = "temp", "StopLng" ="temp")
  
  #cycle through all stops with haversine, 
  #find all that are within the distance from the event
  for(i in nrow(stops)){
    distance_to_event <- haversine(c(stops$StopLat[i], stops$StopLng[i]),
                                   c(event_lat, event_long))
    
    #evaluate distance                                                                                                                             event_long))
    if (distance_to_event<=max_distance){
      nearby_stops <- nearby_stops %>%
        rbind(stops[i, ])
    }
    
  }
  
  #remove temp line
  nearby_stops <- nearby_stops %>%
    filter(Route!="temp")
  
  return(nearby_stops)
  
  
}
