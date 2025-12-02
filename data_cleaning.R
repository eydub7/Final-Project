#' data cleaning
#' 
#' 
ride_data_cleaning <- function(ridership_df, occurrences){
  
  #check to see format of occurrences
  
  if (!as.character(occurrences)){
    break("Occurrences must be a character")
  }
  
  #filter to make the data set only include routes on the days of occurrence
  cleaned_ridership_df <- ridership_df %>%
    filter(Date %in% occurrences) %>%
    #select only necessary columns for analysis: we don't need the pass type
    #for this
    select(c(Time, Route, Trip, Stop.Number, Trip, Source, Key, Type, College, 
             High.School, Low.Income, Eco.Pass, Card.Number))
  
  return(cleaned_ridership_df)
}