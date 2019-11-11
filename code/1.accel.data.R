#'##########################################################################
#' ame1 project - studying the relation between home range size and activity
#' 2019/10/04
#' Adam Kane, Enrico Pirotta & Barry McMahon
#' https://mecoco.github.io/ame1.html
#' Accelerometry data exploration 
#'##########################################################################
#' Load the required packages
library(tidyverse)

#' Section 1: Load the data ----
mydata <- read_csv("data/actdata_mecoco.csv" , col_names = TRUE)
#' take a look at what we've loaded
glimpse(mydata)
head(mydata)
tail(mydata)


levels(as.factor(mydata$activity_sensor_mode_code))

#' 1 - vectronics	
#' ---act1 = x number of forward-backward moves
#' ---act2 = y number side-to-side moves
#' ---act3 = NA
#'  
#' 3 - lotek 3300
#' ---act1 = y number of side-to-side moves
#' ---act2 = z number of up-down moves
#' ---act3 = % of time in head down
#' 
#' 5 - e-obs
#' ---act1 = x number of forward-backward moves
#' ---act2 = y number of side-to-side moves
#' ---act3 = z number of up-down moves

#' how many data points per accelerometer type
mydata %>% 
  group_by(activity_sensor_mode_code) %>%
  summarise(no_rows = length(activity_sensor_mode_code))

#' how many data points per study
mydata %>% 
  group_by(study_areas_id) %>%
  summarise(no_rows = length(study_areas_id))
