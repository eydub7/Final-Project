event_analysis <- function (routes_df, ridership_df, occurrences, 
                            start_time, end_time, event_lat, event_long, max_distance=0.8) {
  #find which routes are viable to the event through location_analysis function
  viable_routes <- location_analysis(routes_df, event_lat, event_long, max_distance)
}