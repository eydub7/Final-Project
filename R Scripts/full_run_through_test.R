

#full_run_through_test
setwd("/Users/juliadubnoff/Documents/GitHub/Final-Project/R Scripts")
source("event_analysis.R")

#load libraries 
library(tidyverse)
#for haversine:
library(pracma)
library(ggplot2)
library(dplyr)




#read in data
ridership_df <- read.csv("/Users/juliadubnoff/Downloads/ridership_simulated.csv")
otp_df <- read.csv("/Users/juliadubnoff/Downloads/otp_simulated.csv")

#weekly occurrences starting from first of may as a starting point 
occurrences <- as.character(c("2024-05-01", "2024-05-08", "2024-05-15", 
                              "2024-05-22", "2024-05-29"))

#set event latitude and longitude based on Lippitt Memorial
#Park, where the farmers market is

event_lat <- 41.85480685417614
event_long <- -71.39157664680116



outputs<-event_analysis(otp_df, ridership_df, occurrences, 
                event_lat, event_long, max_distance=0.8)

outputs$general_boxplot
ggsave("/Users/juliadubnoff/Documents/GitHub/Final-Project/Results/general_plot.png",
       plot = outputs$general_boxplot,
       width = 7, height = 7)

ggsave("/Users/juliadubnoff/Documents/GitHub/Final-Project/Results/boxplot_by_type.png",
       plot = outputs$boxplot_by_type,
       width = 7, height = 7)










