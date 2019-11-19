#'##########################################################################
#' ame1 project - studying the relation between home range size and activity
#' 2019/11/04
#' Adam Kane, Enrico Pirotta & Barry McMahon
#' https://mecoco.github.io/ame1.html
#' merging data
############################################################################
library(tidyverse)
library(lubridate)
#' load in HR data
hr <- read_csv("results/home_range_month.csv" , col_names = TRUE)

#' load in environmental data 
env <- read_csv("results/env_data.csv" , col_names = TRUE)

#' load in activity data
activity <- read_csv("results/Activity_data_byMonth.csv" , col_names = TRUE)

#' merge the first 2
merge1 <- merge(hr,env,c("identifier")) %>% select(-id.x, -id.y)
head(merge1)

#' merge with activity data

#' make an identifier so it matches the other output
#' add a leading zero 
activity$month <- sprintf("%02d",activity$month) # fix to 2 characters 
activity$yr_month <- paste(activity$year, activity$month, sep = "/")
activity$identifier <- paste(activity$animals_id, activity$yr_month, sep = "_")
activity <- activity %>% select(-yr_month)
head(activity)

merge2 <- merge(merge1,activity,c("identifier")) 
head(merge2)

#' merge with individual descriptions 
animal <- read_csv("data/animalsensordata_mecoco.csv" , col_names = TRUE)
head(animal)

#' select only relevant columns 
animal <- animal %>% select(animals_id, sex, year_birth)

#' we need to append individual descriptions using aniamls_id 
all_data <- merge(animal, merge2, by = "animals_id")

#' now calculate age
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

#' export the results
write.csv(all_data, "results/combined_data.csv", row.names = F)
