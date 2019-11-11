#'##########################################################################
#' ame1 project - studying the relation between home range size and activity
#' 2019/10/04
#' Adam Kane, Enrico Pirotta & Barry McMahon
#' https://mecoco.github.io/ame1.html
#' applying the amt package on one study to calculate home ranges (MCP & KDE)
############################################################################

# Packages 
library(tidyverse)
library(lubridate)

# Activity data----
# Notes from data providers:
#   | 1 - vectronics             | act_1: number of forward-backward moves                   |
#   |                            | act_2: number side-to-side moves                          |
#   |                            | act_3: not used   

#   | 3 - lotek 3300             | act_1: number of side-to-side moves                       |
#   |                            | act_2: number of up-down moves                            |
#   |                            | act_3: percentage of time in head down position (0 to 100)|

#   | 5 - e-obs                  | act_1: number of forward-backward moves                   |
#   |                            | act_2: number of side-to-side moves                       |
#   |                            | act_3: number of up-down moves                            |


# Load and inspect activity data
adat <- read_csv("data/actdata_mecoco.csv", col_names=T)
head(adat)
names(adat)

adat$act_sel <- adat$act_1 #select activity channel of interest (f-b for sensors 1 and 5, s-s for sensor 3; all are in column act_1)

adat$animals_id <- as.factor(adat$animals_id)
adat <- arrange(adat, animals_id, acquisition_time)

# Time variables
adat$acquisition_time <- strptime(adat$acquisition_time, format="%Y-%m-%d %H:%M:%S", tz="UTC")
adat$year <- adat$acquisition_time$year+1900
adat$month <- month(adat$acquisition_time)

# Summarise activity by month
adat_m <- adat %>% 
  group_by(animals_id, year, month) %>% 
  summarise(mean_act = mean(act_sel), sd_act=sd(act_sel), max_act=max(act_sel), min_act=min(act_sel), n_obs=n(), activity_sensors_id=unique(activity_sensors_id), sensor_type=unique(activity_sensor_mode_code),study_areas_id=unique(study_areas_id),gps_sensors_id=unique(gps_sensors_id)) %>% 
  ungroup() %>% 
  group_by(animals_id) %>% 
  mutate(id = row_number()) %>% 
  ungroup()

dim(adat_m)
names(adat_m)

#plot mean activity:
ggplot(adat_m) +
  geom_path(aes(x=id, y=mean_act, group=animals_id))

#plot sd activity:
ggplot(adat_m) +
  geom_path(aes(x=id, y=sd_act, group=animals_id))

#plot metrics against each other 
ggplot(adat_m) +
  geom_point(aes(x=mean_act, y=sd_act))
ggplot(adat_m) +
  geom_point(aes(x=mean_act, y=max_act))
#there seems to be good correlation between mean and sd of activity, and, to some extent, mean and max 

#' write.csv(adat_m, "results/Activity_data_byMonth.csv", row.names=F)
