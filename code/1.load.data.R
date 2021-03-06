#'##########################################################################
#' ame1 project - studying the relation between home range size and activity
#' 2019/10/04
#' Adam Kane, Enrico Pirotta & Barry McMahon
#' https://mecoco.github.io/ame1.html
#' loading and cleaning the GPS data
############################################################################
#' Load the required packages
library(tidyverse)

#' Section 1: Load the data ----
mydata <- read_csv("data/gpsdata_mecoco.csv" , col_names = TRUE)
#' take a look at what we've loaded
glimpse(mydata)
head(mydata)
tail(mydata)
summary(mydata)
str(mydata)
levels(as.factor(mydata$animals_id))
#' 1,892,424 observations of 15 variables
#' looks like there are some NAs in the data
#' let's drop them
mydata <- mydata %>% drop_na()
#' reduces the data set down to 1,788,743 observations
#' how many data points are in each of the studies?
mydata %>% group_by(study_areas_id) %>% summarise(duration = length(study_areas_id))



