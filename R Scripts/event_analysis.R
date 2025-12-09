setwd("/Users/juliadubnoff/Documents/GitHub/Final-Project/R Scripts")


list_files <- c("data_cleaning.R", "route_extraction.R", "find_nearby_stops.R", 
                "route_usage.R", "usage_demographics.R")
sapply(list_files, source)


#' event analysis
#' 
#' @description runs through complete analysis of event that a user wants to 
#' analyze in relation to bus ridership. Analysis broadly whether the event 
#' itself may be related to a change in ridership for routes near the event.
#' 
#' @param otp_df data frame of stops' scheduled and arrival times
#' @param ridership_df data frame of ridership
#' @param occurrences list of days in which the event in question occurs
#' @param event_lat latitude of event location
#' @param event_long longitude of event location
#' @param max_distance cutoff distance in kilometers for a stop to be considered
#' close to an event. Default 0.8 kilometers to approximate a 10 minute walk.
#' 
#' @return a list containing: 1. test = a t-test evaluating whether the routes
#' sufficiently close to events days display a greater change in ridership on
#' such days than farther routes. 2. general_boxplot = a ggplot object 
#' displaying how change in ridership on event days varies by nearness of route.
#' 3. boxplot_by_type = a ggplot object similar to general_boxplot, but showing
#' how results vary by type of rider (i.e. adult, child, disabled, etc.)

event_analysis <- function (otp_df, ridership_df, occurrences, 
                            event_lat, event_long, max_distance=0.8) {
  
  #check to see format of occurrences
  if (!is.character(occurrences)){
    break("Occurrences must be a character")
  }
  
  # clean ridership data
  cleaned_ridership_df <- ride_data_cleaning(ridership_df)
  
  #extract routes that have somen occurrences on event days
  routes_df <- route_extraction(otp_df, occurrences)
  
  #find which stops are viable to the event through location_analysis function
  nearby_stops <- find_nearby_stops(routes_df, event_lat, event_long, 
                                                                  max_distance)
  #find which routes are viable
  viable_routes<-unique(nearby_stops$Route)

  #change in route usage during event:
  route_usage_df <- route_usage(cleaned_ridership_df, viable_routes, occurrences)
  
  #calculate the general t-test
  test_general<-t.test(data=route_usage_df, 
                       difference_of_mean_riders~is_viable_route, 
                       alternative="two.sided")
  
  
  #next step: visuals
  
  general_boxplot<-ggplot(data=route_usage_df) +
    geom_boxplot(aes(group=is_viable_route, y=difference_of_mean_riders, 
                     fill=is_viable_route)) +
    labs(y="Increase of Riders on Days of Event",
         title="Routes' Usage Change On Event Days" ) +
    theme_minimal() +
    theme(axis.text.x = element_blank(), axis.ticks.x=element_blank())
  
  #demographic analysis of type of rider. 
  usage_demographic_type <- usage_demographics(cleaned_ridership_df, 
                                               viable_routes, occurrences)
  
  boxplot_by_type<-ggplot(data=usage_demographic_type) + 
    geom_boxplot(aes(x=is_viable_route, 
                                    y=difference_of_mean_riders, fill=Type))+ 
    labs(y="Increase of Riders on Days of Event",
         title="Routes' Usage Change On Event Days, By Type of Rider" ) +
    theme_minimal() +
    theme(axis.ticks.x=element_blank()) +
    scale_y_continuous(limits = c(-50, 340))
  
  return(list(test = test_general, general_boxplot= general_boxplot, 
              boxplot_by_type = boxplot_by_type))
  
}
