#' find nearby stops 
#' 
#' @description finds a data frame of stops and their associated details that 
#' are near to the event in question
#' @param routes_df data frame of routes that occur on dates of the occurrences 
#'  @param event_lat latitude of event location
#' @param event_long longitude of event location
#' @param max_distance cutoff distance in kilometers for a stop to be considered
#' close to an event. 
#' 
#' @return data frame of nearby stops 
find_nearby_stops <- function(routes_df, event_lat, event_long, max_distance){
  
  #get a more concise list of stops
  stops<-routes_df %>% 
    group_by(Route, Stop) %>%
    summarize(StopLat=mean(StopLat), StopLng=mean(StopLng))
  
  #create data frame of nearby_stops
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
        }                                                                                                                        
    }

    
  }
  
  #remove temp line
  nearby_stops <- nearby_stops %>%
    filter(Route!="temp")

  return(nearby_stops)
  

}
