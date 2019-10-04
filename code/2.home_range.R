#'##########################################################################
#' ame1 project - studying the relation between home range size and activity
#' 2019/10/04
#' Adam Kane, Enrico Pirotta & Barry McMahon
#' https://mecoco.github.io/ame1.html
#' applying the amt package on one study to calculate home ranges (MCP & KDE)
############################################################################

# study_areas_id number 1 Tracking Dataset

library(lubridate)
library(SDLfilter)
library(amt)
library(sp)

# select andre data which is the data we tracked in andreland
data_study1 <- filter(mydata, study_areas_id == "1")
data_study1

#' Check for duplicated observations
ind2 <-
  data_study1 %>% select(acquisition_time, longitude, latitude, animals_id) %>%
  duplicated
sum(ind2)
# remove them
data_study1$dups <- ind2
data_study1 <- filter(data_study1, dups == "FALSE")
data_study1


#' rename some of the columns for data manipulation
#' order is new name old name
data_study1 <- rename(data_study1, time = acquisition_time)
data_study1 <- rename(data_study1, long = longitude)
data_study1 <- rename(data_study1, lat = latitude)
data_study1 <- rename(data_study1, id = animals_id)
data_study1 <- rename(data_study1, study = study_areas_id)
data_study1

#' look at the IDs
levels(factor(data_study1$id))
# can look at an individual level with
(filter(data_study1, id == "768"))

# all of the data is in the format of day-month-year
data_study1$New_time <-
  parse_date_time(x = data_study1$time, c("%Y-%m-%d %H:%M:%S"))

# keep only the new time data
data_study1 <- select(data_study1, New_time, long, lat, id, study)
data_study1 <- rename(data_study1, time = New_time)
data_study1

#' filter extreme data based on a speed threshold
#' based on vmax which is km/hr
#' time needs to be labelled DateTime for these functions to work
names(data_study1)[names(data_study1) == 'time'] <- 'DateTime'
SDLfilterData <-
  ddfilter.speed(data.frame(data_study1), vmax = 70, method = 1)
length(SDLfilterData$DateTime)

#' rename everything as before
data_study1 <- SDLfilterData
names(data_study1)[names(data_study1) == 'DateTime'] <- 'time'

# check the minimum time and the maximum time
min_time <- data_study1 %>% group_by(id) %>% slice(which.min(time))
data.frame(min_time)

max_time <- data_study1 %>% group_by(id) %>% slice(which.max(time))
data.frame(max_time)

#' determine the length of time each bird was tracked for
duration <-
  difftime(max_time$time, min_time$time, units = "days")
duration

#' try the amt package
trk <-
  mk_track(
    data_study1,
    .x = long,
    .y = lat,
    .t = time,
    id = id,
    crs = CRS("+init=epsg:4326")
  )  %>%
  transform_coords(
    sp::CRS(
      #' we can transform the CRS of the data to an equal area projection
      #' https://epsg.io/102022
      "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
    )
  )

#' summarise the sampling rate
data_summary <-
  trk %>% nest(-id) %>% mutate(sr = map(data, summarize_sampling_rate)) %>%
  dplyr::select(id, sr) %>% unnest %>% arrange(id)
data_summary

#' Calculate home range size for data that is not regularised
mcps <- trk %>% nest(-id) %>%
  mutate(mcparea = map(data, ~ hr_mcp(., levels = c(0.95)) %>% hr_area)) %>%
  select(id, mcparea) %>% unnest()

mcps$area <- mcps$area / 1000000
mcp_95 <- mcps %>% arrange(id)

#' Same for KDE
kde <- trk %>% nest(-id) %>%
  mutate(kdearea = map(data, ~ hr_kde(., levels = c(0.95)) %>% hr_area)) %>%
  select(id, kdearea) %>% unnest()

kde$kdearea <-  kde$kdearea / 1000000
kde_95 <- kde %>% arrange(id)

#' combine the summary stats
data_summary$duration <- duration
data_summary$min_time <- min_time$time
data_summary$max_time <- max_time$time
data_summary$kde <- kde_95$kdearea
data_summary$mcps <- mcp_95$area
data_summary$study <- "study1"
data_summary

#' export the results
write.csv(data_summary, "results/data_summary_study1.csv", row.names = F)
