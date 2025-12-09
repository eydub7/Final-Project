#' route extraction
#' 
#' @description takes otp_df (on time performance) and occurrences of events and
#' outputs a df of routes that occur on the dates of those occurrences
#' @param otp_df data frame of stops' scheduled and arrival times
#' @param occurrences list of days in which the event in question occurs
#' 
#' @return data frame of routes that occur on dates of the occurrences 


route_extraction <- function(otp_df, occurrences){

  #filter to make the data set only include routes that run on the days of 
  #occurrence. we are concerned about how usage of routes that apply to those
  #days change. 
  
  routes_df <- otp_df %>%
    filter(Date %in% occurrences) %>%
  #select only necessary columns for the scheduled routes of a day
    select(c(Date, Route, Trip, Stop, Stop.Sequence, Scheduled.Time, StopLat, 
             StopLng))
  
  return(routes_df)
} 

