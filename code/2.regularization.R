#'##########################################################################
#' ame1 project - studying the relation between home range size and activity
#' 2019/11/04
#' Adam Kane, Enrico Pirotta & Barry McMahon
#' https://mecoco.github.io/ame1.html
#' regularise the data
############################################################################

# 1. break up tracks if they exceed a day (no need to remove rare data)
# 2. redis at a period of 4 hours
# 3. eliminate months without sufficient coverage
# 4. glue them back together

library(adehabitatLT)
library(lubridate)
#' library(SDLfilter)
library(amt)
library(sp)

#' can filter by study for testing
#'  mydata <- filter(mydata, study_areas_id == "1")

#' Check for duplicated observations
#' ind2 <-
#'  mydata %>% select(acquisition_time, longitude, latitude, animals_id) %>%
#'  duplicated
#' sum(ind2)
#' remove them
#' mydata$dups <- ind2
#' mydata <- filter(mydata, dups == "FALSE")
#' mydata


#' rename some of the columns for data manipulation
#' order is new name old name
mydata <- rename(mydata, time = acquisition_time)
mydata <- rename(mydata, long = longitude)
mydata <- rename(mydata, lat = latitude)
mydata <- rename(mydata, id = animals_id)
mydata <- rename(mydata, study = study_areas_id)
mydata

#' look at the IDs
levels(factor(mydata$id))
# can look at an individual level with
(filter(mydata, id == "768"))



#' all of the data is in the format of day-month-year
#' note that time is in UTC
mydata$New_time <-
  parse_date_time(x = mydata$time, c("%Y-%m-%d %H:%M:%S"))

# keep only the new time data
mydata <- select(mydata, New_time, long, lat, id, study)
mydata <- rename(mydata, time = New_time)
mydata

#' filter extreme data based on a speed threshold
#' based on vmax which is km/hr
#' time needs to be labelled DateTime for these functions to work
#' not necessary as nothing exceeds this value 
#' names(mydata)[names(mydata) == 'time'] <- 'DateTime'
#' SDLfilterData <-
#'  ddfilter.speed(data.frame(mydata), vmax = 70, method = 1)
#' length(SDLfilterData$DateTime)

#' rename everything as before
#' mydata <- SDLfilterData
#' names(mydata)[names(mydata) == 'DateTime'] <- 'time'

# check the minimum time and the maximum time
min_time <- mydata %>% group_by(id) %>% slice(which.min(time))
data.frame(min_time)

max_time <- mydata %>% group_by(id) %>% slice(which.max(time))
data.frame(max_time)

#' determine the length of time each deer was tracked for
duration <-
  difftime(max_time$time, min_time$time, units = "days")
duration

#' try the amt package
trk <-
  mk_track(
    mydata,
    .x = long,
    .y = lat,
    .t = time,
    id = id,
    crs = CRS("+init=epsg:4326")
  )  %>%
  transform_coords(
    sp::CRS(
      "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
    )  # Lambert Azimuthal Equal Area
  )

#' summarise the sampling rate
data_summary <-
  trk %>% nest(-id) %>% mutate(sr = map(data, summarize_sampling_rate)) %>%
  dplyr::select(id, sr) %>% unnest %>% arrange(id)
data_summary

#' measure the time difference between points for each bird ID using dplyr
#' - Group your data by ID
#' - Compute time diffs between each timestamp in your group (the 1st time diff is NA)
#' - Create a new ID that counts no. of prior time gaps that are large (e.g. > 24 hours)
#' - Split the ID into newID by using an underscore separator

length(levels(as.factor(trk$id)))

trk2 <- trk %>%
  group_by(id) %>%
  mutate(timeDiff = c(NA, difftime(tail(t_,-1), head(t_,-1), units = "hours"))) %>%
  mutate(newID = paste(id, cumsum(!is.na(timeDiff) &
                                    timeDiff > 24), sep = "_")) %>%
  ungroup()
head(trk2)


#' check the number of newIDs
levels(as.factor(trk2$newID))
length(levels(as.factor(trk2$newID)))

#' how long are the tracks now that some of them have been split
sapply(split(trk2$x_, trk2$newID), length)

#' create a trajectory object using adehabitatLT
trk_ltraj <-
  as.ltraj(xy = trk2[, c("x_", "y_")],
           date = trk2$t_,
           id = trk2$newID)

#' rediscretization of the trajectory
tstep <-
  14400 # time step we want for the rediscretization, in seconds, 14400 secs = 4 hours
newtr <- redisltraj(trk_ltraj, u = tstep, type = "time")
head(newtr[1])
head(newtr[2])
class(newtr)

#' convert to class data frame
trk3 <- ld(newtr)
head(trk3)
class(trk3$date)

#' we should group the IDs that were split if they had big gaps back together into their original ID structure
#' this involves accessing the name of the new ID that occurs before the underscore
trk3 <- separate(trk3,
                 col = id,
                 sep = "_",
                 into = c("ID", "NA"))
head(trk3)
levels(as.factor(trk3$ID))
length(levels(as.factor(trk3$ID)))

#' remove the resultant NA column that occurs after the split
trk3 <- select(trk3, x, y, date, ID)
head(trk3)

#' turn it back into a trk
trk4 <-
  mk_track(
    trk3,
    .x = x,
    .y = y,
    .t = date,
    id = ID,
    crs = CRS(
      "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
    )
  )


#' we need to extract monthly home ranges, some animals were tracked for over a year
#' so we must include a year-month-id grouping variable
#' first combine year and month
trk4$yr_month <- format(trk4$t_, format = "%Y/%m")
trk4$yr_month <- as.factor(trk4$yr_month)

#' we also need a year-month-day variable to see what the coverage is like over the course of
#' a month for each individual
trk4$yr_month_day <- format(trk4$t_, format = "%Y/%m/%d")
trk4$yr_month_day <- as.factor(trk4$yr_month_day)
head(trk4)

#' count the number of unique days when grouped by id and and month
short_months <- trk4 %>%
  group_by(id, yr_month) %>%
  summarise(count = n_distinct(yr_month_day)) %>% filter(count < 28) %>% droplevels()
short_months
short_months$yr_month

#' we merge the two and force all = TRUE so even the values that don't have a count
#' are included, this allows us to extract the tracks that have ~ a month
#' of coverage 
test <- merge(short_months, trk4, all = TRUE)
length(test$id)
length(trk4$id)

#' keep only the rows with the NAs which are the counts > 28 i.e. data with ~ a month
#' of coverage
trk5<- test %>% filter_all(any_vars(is.na(.))) 

#' now create a unique identifier that has the ID, year and month
#' We will use these to build home ranges
trk5$identifier <- paste(trk5$id, trk5$yr_month, sep = "_")
trk5$identifier <- as.factor(trk5$identifier)
head(trk5)

#' turn it back into a trk
trk5 <-
  mk_track(
    trk5,
    .x = x_,
    .y = y_,
    .t = t_,
    id = id,
    identifier = identifier,
    crs = CRS(
      "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
    )
  )


#' run some summary stats on the data to make sure everything looks okay
length(levels(as.factor(trk5$id)))
length(levels(as.factor(trk5$id)))
sapply(split(trk5$x_, trk5$identifier), length)

#' Calculate home range size
#' first for MCP
mcps <- trk5 %>% group_by(identifier) %>%  nest(-identifier) %>%
  mutate(mcparea = map(data, ~ hr_mcp(., levels = c(0.95)) %>% hr_area)) %>%
  select(identifier, mcparea) %>% unnest()

mcps$area <- mcps$area / 1000000
mcp_95 <- mcps %>% arrange(identifier)
mcp_95

#' Same for KDE 95
kde <- trk5 %>% group_by(identifier) %>% nest(-identifier) %>%
  mutate(kdearea = map(data, ~ hr_kde(., level = c(0.95)) %>% hr_area)) %>%
  select(identifier, kdearea) %>% unnest()

kde$kdearea <-  kde$kdearea / 1000000
kde_95 <- kde %>% arrange(identifier)
kde_95

#' combine the data
kde_95$mcp <- mcp_95$area

#' rename
home_range_month <- kde_95
head(home_range_month)
tail(home_range_month)

#' add an id column
home_range_month <-
  separate(
    home_range_month,
    col = identifier,
    into = c("id", "NA"),
    sep = "_",
    remove = "FALSE"
  ) %>%  select(-"NA")

#' check the data with short_months to make sure no home ranges were collected
#' for month long intervals where less than 28 days had data
#' e.g. id 768   for year/month 2005/10 had only 13 days
#' this does not show up in the home_range_month data
#' filter(home_range_month, id == 768)

#' export the results
write.csv(home_range_month, "results/home_range_month.csv", row.names = F)
