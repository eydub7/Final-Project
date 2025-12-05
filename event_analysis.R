# i want this project to be contained in a singular function. nest all functions within this.


event_analysis <- function (otp_df, ridership_df, occurrences, 
                            start_time, end_time, event_lat, event_long, max_distance=0.8) {
  
  # clean ridership data
  ridership_df <- ride_data_cleaning(ridership_df, occurrences)
  
  #extract routes
  routes_df <- route_extraction(otp_df, occurrences)
  
  #find which routes ands stops are viable to the event through location_analysis function
  nearby_stops <- find_nearby_stops(routes_df, event_lat, event_long, max_distance)
  viable_routes <- find_viable_routes(routes_df, stops)
  
  
  #viable_routes_bytime <- time_analysis(viable_routes)
  
   
  
  
}