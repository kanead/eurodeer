#'##########################################################################
#' ame1 project - studying the relation between home range size and activity
#' 2019/11/04
#' Adam Kane, Enrico Pirotta & Barry McMahon
#' https://mecoco.github.io/ame1.html
#' merging data for week
############################################################################
library(tidyverse)
library(lubridate)
#' library(stringr)

#' load in HR data
hr <- read_csv("results/home_range_week.csv" , col_names = TRUE)

#' load in environmental data 
env <- read_csv("results/env_data_week.csv" , col_names = TRUE)

#' load in the day length data
day_length <- read_csv("results/day_length_byWeek.csv" , col_names = TRUE)
head(day_length)

#' load in activity data
#' new 5 min interpolated activity data by week
activity <- read_csv("results/Activity_data_byWeek_5min.csv" , col_names = TRUE)
head(activity)

#' merge the first home range and environmental data
merge1 <- merge(hr,env,c("identifier")) %>% select(-id.x, -id.y, -week.y)
head(merge1)

#' merge with activity data
#' make an identifier so it matches the other output
activity$identifier <-  gsub('([[:punct:]])|\\s+','_',activity$group)
activity <- activity %>% select(-group)
head(activity)

merge2 <- merge(merge1,activity,c("identifier")) 
head(merge2)

#' merge with day length
merge3 <- merge(merge2,day_length,c("identifier")) 
head(merge3)

#' merge with individual descriptions 
animal <- read_csv("data/animalsensordata_mecoco.csv" , col_names = TRUE)
head(animal)

#' need a year column
animal$year <- format(as.Date(animal$gps_start_time, format="%Y/%m/%d"),"%Y")
head(animal)

#' select only relevant columns 
animal <- animal %>% select(animals_id, sex, year_birth, year)

#' we need to append individual descriptions using aniamls_id 
all_data <- merge(animal, merge3, by = "animals_id")

#' now calculate age
all_data$year_birth <- as.numeric(all_data$year_birth)
all_data$year <- as.numeric(all_data$year)
head(all_data)
all_data$age <- all_data$year - all_data$year_birth
head(all_data)

#' add predator species counts 
#' CH - Swiss Alps (Bernese, study areas id = 25) -> 2
#' IT - Italian Alps (Bondone, study areas id = 1) -> 2
#' FR - South France (Aurignac, study areas id = 8) -> 1
#' SE-DE - Southeast Germany (Bavarian Forest, study areas id = 2) -> 2
#' SW-DE - Southern Germany (Upper Rhine valley, study areas id = 15) -> 1
#' PL - Northeastern Poland (Bialowieza National Park, study areas id = 16) -> 3

all_data$predator <- ifelse(all_data$study_areas_id == 25 | all_data$study_areas_id == 1 | all_data$study_areas_id == 2, 2, 1)
all_data$predator <- ifelse(all_data$study_areas_id == 16, 3, all_data$predator)
head(all_data)
tail(all_data)
#' check that study area 16 has 3 predators 
filter(all_data, study_areas_id == 16)
#' check that study area 25 has 2 predators 
filter(all_data, study_areas_id == 25)
#' check that study area 8 has 1 predator
filter(all_data, study_areas_id == 8)

names(all_data)

#' new name is first
all_data <- all_data %>% rename(week = week.x, mean_day_length = Mean)

#' export the results
write.csv(all_data, "results/combined_data_week.csv", row.names = F)

