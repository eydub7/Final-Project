#' usage by demographics
#' @description finds a data frame of stops and their associated details that 
#' are near to the event in question, sorted by a demographic column of choice
#' @param cleaned_ridership_df ridership_df as cleaned by data cleaning function
#' @param col demographic variable of choice, should be a column in the data 
#' frame
#' @param viable_routes vector containing all routes that have stops near enough 
#' to event
#' @param occurrences list of days in which the event in question occurs
#' 
#' @return data frame that can be used in tests and graphs evaluating how much 
#' ridership changes on event days versus non event days, divided by the 
#' demographic column. 
usage_demographics <- function (cleaned_ridership_df, col, viable_routes, 
                                                                    occurances){
  # this function is very similar to route_usage. 
  
  #quote column
  col <- enquo(col)
  
  #demographic_variable should be a string corresponding to a column name
  #sum ridership over hour, then take the mean
  
  route_usage <- ridership_df %>% 
    
    #quote column
    
    #sum ridership over hour and column
    group_by(Date, Hour, Route, !!col) %>%
    summarize(sum_riders=n()) %>%
    ungroup()%>%
    
    #categorize whether the events are in the occurrences
    mutate(event_day=case_when(Date %in% occurrences ~ 1, TRUE ~ 0)) %>%
    group_by(Route, event_day, !!col) %>%
    summarize(mean_riders=mean(sum_riders)) %>%
    pivot_wider(names_from=event_day, values_from = mean_riders, names_prefix = 
                                                            "mean_riders_") %>%
    
    #evaluate how much ridership changes between event day and non event day for
    #route
    mutate(difference_of_mean_riders=mean_riders_1-mean_riders_0) %>%
    
    #remove NA entries 
    filter(!is.na(difference_of_mean_riders)) %>%
    
    #create entry to designate if nearby route 
    mutate(is_viable_route = case_when( Route %in% viable_routes ~ "Yes", 
                                        TRUE ~ "No"))%>%
    mutate(is_viable_route=as.factor(is_viable_route)) 
    

return(route_usage)
}

