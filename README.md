# Final-Project

The key question under investigation in this project is in regard to recurring events—how do they affect RIPTA usage?

I went about creating a function to investigate this called event_analysis. This function runs through complete analysis of event that a user wants to analyze in relation to bus ridership. It cleans the data, finds which routes contain stops near the event's location, and determine whether these routes experience a greater change in ridership on days of these events than other routes do. This accounts for usual usage spikes—for example, if RIPTA broader experiences a spike in usage on a Saturday, this project compares whether routes nearer the event have a more significant increase that could be attributed to the event itself. 

This project is all housed within the function event_analysis. This function calls the following functions, also built for this project, within it: data_cleaning, route_extraction, find_nearby_stops, route_usage, usage_demographics. The output of event_analysis is a list containing: 
1. test = a t-test evaluating whether the routes sufficiently close to events days display a greater change in ridership on such days than farther routes. 
2. general_boxplot = a ggplot object displaying how change in ridership on event days varies by nearness of route.
3. boxplot_by_type = a ggplot object similar to general_boxplot, but showing how results vary by type of rider (i.e. adult, child, disabled, etc.)

Libraries needed for this event are tidyverse, pracma, ggplot2, and dplyr.