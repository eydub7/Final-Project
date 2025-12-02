#route extraction
#given otp like data set

#description: takes otp_df (on time performance) and occurrences of events and
#outputs a df of routes that occur on the dates of those occurrences

route_extraction <- function(otp_df, occurrences){
  
  #check to see format of occurrences
  
  if (!as.character(occurrences)){
    break("Occurrences must be a character")
  }

  #filter to make the data set only include routes on the days of occurrence
  routes_df <- otp_df %>%
    filter(Date %in% occurrences) %>%
  #select only necessary columns for the scheduleed routes of a day
    select(c(Date, Route, Trip, Stop, Stop.Sequence, Scheduled.Time, StopLat, StopLng))
  
  return(routes_df)
} 

