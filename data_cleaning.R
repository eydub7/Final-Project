#' ride data cleaning
#' 
#' @description prepares ridership data for use in analysis 
#' @param ridership_df read in a suitable data frame of ridership data for the
#'  transit system
#' @return an organized data frame with Date, Hour, and Time variables. 
ride_data_cleaning <- function(ridership_df){
  
  cleaned_ridership_df <- ridership_df %>%
    #find the date
    mutate(Time = mdy_hm(Time)) %>%
    mutate(Date = date(Time), Hour = hour(Time)) %>%
    
    #select only necessary columns for analysis, for example Transfer is
    #irrelevant here
    select(c(Date, Hour, Time, Route, Trip, Stop.Number, Trip, Source, Type, 
             College, High.School, Low.Income, Eco.Pass, Card.Number))
  
  return(cleaned_ridership_df)
}
 