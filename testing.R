#testing/running code
#load libraries 
library(tidyverse)

#for haversine:
library(pracma)

#read in data
ridership_df <- read.csv("/Users/juliadubnoff/Downloads/ridership_simulated.csv")
otp_df <- read.csv("/Users/juliadubnoff/Downloads/otp_simulated.csv")

#test type of date 
str(otp_df$Date)
#Date is a character, so occurrences should be too
#weekly occurances starting from first of may as a starting point 
occurrences <- as.character(c("2024-05-01", "2024-05-08", "2024-05-15", "2024-05-22", "2024-05-29"))

#this works, check in :
routes_df <- otp_df %>%
  filter(Date %in% occurrences)

otp_df$Date %in% occurrences


#test haversine
event_lat<-41.79829
event_long<--71.41475
distance <- haversine(c(stops$StopLat[1], stops$StopLng[1]), c(event_lat, event_long))
#this works!


