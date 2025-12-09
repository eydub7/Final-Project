#' route usage
#' 
#' @description finds a data frame of stops and their associated details that 
#' are near to the event in question
#' @param cleaned_ridership_df ridership_df as cleaned by data cleaning function
#' @param viable_routes vector containing all routes that have stops near enough 
#' to event
#' @param occurrences list of days in which the event in question occurs
#' 
#' @return data frame that can be used in tests and graphs evaluating how much 
#' ridership changes on event days versus non event days.
route_usage <- function(cleaned_ridership_df, viable_routes, occurrences) {
  
  route_usage <- cleaned_ridership_df %>% 
    
    #convert route to character
    mutate(Route = as.character(Route))%>%
    
    #sum ridership over hour
    group_by(Date, Hour, Route) %>%
    ungroup()%>%
    
    #categorize whether the events are in the occurrences
    mutate(event_day=case_when(Date %in% occurrences ~ 1, TRUE ~ 0)) %>%
    group_by(Route, event_day) %>%
    summarize(mean_riders=mean(sum_riders)) %>%
    pivot_wider(names_from=event_day, values_from = mean_riders, names_prefix = 
                  "mean_riders_") %>%
    
    #evaluate how much ridership changes between event day and non event day 
    #for route
    mutate(difference_of_mean_riders=mean_riders_1-mean_riders_0) %>%
    
    #remove NA entries 
    filter(!is.na(difference_of_mean_riders)) %>%
    
    #create entry to designate if nearby route 
    mutate(is_viable_route = case_when( !(Route %in% viable_routes) ~ "No", 
                                        TRUE ~ "Yes")) %>%
    #convert category is_viable_route to factor
    mutate(is_viable_route=as.factor(is_viable_route))  
    
  
  
   return(route_usage)
}

