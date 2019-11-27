#'##########################################################################
#' ame1 project - studying the relation between home range size and activity
#' 2019/10/04
#' Adam Kane, Enrico Pirotta & Barry McMahon
#' https://mecoco.github.io/ame1.html
#' extracting day length 
############################################################################

#' day length from study sites
#' CH - Swiss Alps (Bernese, study areas id = 25) = (46.56 N, 7.51 E)
#' IT - Italian Alps (Bondone, study areas id = 1) = (45.99 N, 11.03 E)
#' FR - South France (Aurignac, study areas id = 8) = (43.25 N, 0.87 E) 
#' SE-DE - Southeast Germany (Bavarian Forest, study areas id = 2) = (48.94 N, 13.42 E)
#' SW-DE - Southern Germany (Upper Rhine valley, study areas id = 15) = (48.67 N, 8.00 E) 
#' PL - Northeastern Poland (Bialowieza National Park, study areas id = 16) = (52.76 N, 23.74 E)
#' latitude is the first value in North 

#' Load the required packages
library(tidyverse)
library(geosphere)

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

#' add day length
#' Compute daylength (photoperiod) for a latitude and date.
#' You'll need to install geosphere
#' date format is 'yyyy-mm-dd'
#' this function called daylength calculates the daylength (photoperiod) for a latitude and date.
mydata$time <-
  parse_date_time(x = mydata$acquisition_time, c("%Y-%m-%d %H:%M:%S"))
mydata$time <- strftime(mydata$time, format = "%Y-%m-%d")
head(mydata$time)

mydata$day_length <-
  daylength(mydata$latitude, mydata$time) 

#' pull out the data we need and make sure the time is formatted properly
newdata <- mydata %>% select(animals_id, time, day_length)
newdata$time <- parse_date_time(x = newdata$time, c("%Y-%m-%d"))
class(newdata$time)
head(newdata)

#' we want to work with weekly averages 
#' some animals were tracked for over a year
#' so we must include a year-month-id grouping variable
#' first combine year and month
newdata$yr_month <- format(newdata$time, format = "%Y/%m")
newdata$yr_month  <- as.factor(newdata$yr_month)

newdata$yr <- format(newdata$time, format = "%Y")
newdata$yr <- as.factor(newdata$yr)

newdata$week <- week(newdata$time)
newdata$week <- as.factor(newdata$week)

newdata$yr_week <- paste(newdata$yr,newdata$week, sep = "_")
newdata

#' we also need a year-week-day variable to see what the coverage is like over the course of
#' a week for each individual
newdata$day <- day(newdata$time)
newdata$day <- as.factor(newdata$day)
newdata$yr_week_day <- paste(newdata$yr_week, newdata$day, sep = "_")

#' count the number of unique days when grouped by id and and year
short_weeks <- newdata %>%
  group_by(animals_id, yr_week) %>%
  summarise(count = n_distinct(yr_week_day)) %>% filter(count < 5) %>% droplevels()
short_weeks
short_weeks$yr_week

#' we merge the two and force all = TRUE so even the values that don't have a count
#' are included, this allows us to extract the tracks that have ~ a month
#' of coverage 
newdata <- merge(short_weeks, newdata, all = TRUE)
length(newdata$animals_id)
length(newdata$animals_id)

#' keep only the rows with the NAs which are the counts > 5 i.e. data with ~ a week
#' of coverage
newdata<- newdata %>% filter_all(any_vars(is.na(.))) 
head(newdata)

#' now create a unique identifier that has the ID, year and week
#' We will use these to summarise the covariates 
newdata$identifier <- paste(newdata$animals_id, newdata$yr_week, sep = "_")
newdata$identifier <- as.factor(newdata$identifier)
head(newdata)

#' calculate summary statistics for day length, really only need the mean here
summary_data <- newdata %>% group_by(identifier) %>% 
  summarise_at(.vars = vars(day_length),
               .funs = c(Mean="mean"))
summary_data

#' export the results
write.csv(summary_data, "results/day_length_byWeek.csv", row.names = F)
