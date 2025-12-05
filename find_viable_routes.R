find_viable_routes <- function(routes_df, stops){
  
#get a more concise list of stops
  stops<-routes_df %>% 
    group_by(Route, Stop) %>%
    summarize(StopLat=mean(StopLat), StopLng=mean(StopLng))
  
  #initialize vector with route number
  nearby_routes_vector <- c()
    for(i in 1:nrow(stops)){
      #if not NA values
      if(!is.na(stops$StopLat[i]) & !is.na(stops$StopLng[i])){
        distance_to_event <- haversine(c(stops$StopLat[i], stops$StopLng[i]),
                                       c(event_lat, event_long))
        
        #evaluate distance        
        if (distance_to_event<=max_distance){
          nearby_routes_vector <- c(nearby_routes_vector, stops$Route[i])
        }                                                                                                                        
      }
      
      
    }
return(nearby_routes_vector)

}