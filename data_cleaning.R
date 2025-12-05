#' data cleaning
#' 
#' 
ride_data_cleaning <- function(ridership_df, occurrences){
  
  #check to see format of occurrences
  
  if (!is.character(occurrences)){
    break("Occurrences must be a character")
  }
  
  cleaned_ridership_df <- ridership_df %>%
    #find the date
    mutate(Time = mdy_hm(Time)) %>%
    mutate(Date = date(Time)) %>%
    #filter to make the data set only include routes on the days of occurrence
    filter(Date %in% occurrences) %>%
    #select only necessary columns for analysis: we don't need the pass type
    #for this
    select(c(Date, Route, Trip, Stop.Number, Trip, Source, Type, College, 
             High.School, Low.Income, Eco.Pass, Card.Number))
  
  return(cleaned_ridership_df)
}
 