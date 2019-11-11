#'##########################################################################
#' ame1 project - studying the relation between home range size and activity
#' 2019/11/04
#' Adam Kane, Enrico Pirotta & Barry McMahon
#' https://mecoco.github.io/ame1.html
#' summarise the covariates
############################################################################

library(adehabitatLT)
library(lubridate)
library(amt)
library(sp)

#' can filter by study for testing
#'  mydata <- filter(mydata, study_areas_id == "1")

#' Check for duplicated observations
#' ind2 <-
#'   mydata %>% select(acquisition_time, longitude, latitude, animals_id) %>%
#'   duplicated
#' sum(ind2)
#' remove them
#' mydata$dups <- ind2
#' mydata <- filter(mydata, dups == "FALSE")
#' mydata

rm(ind2)

#' rename some of the columns for data manipulation
#' order is new name old name
mydata <- rename(mydata, time = acquisition_time)
mydata <- rename(mydata, long = longitude)
mydata <- rename(mydata, lat = latitude)
mydata <- rename(mydata, id = animals_id)
mydata <- rename(mydata, study = study_areas_id)
mydata <- rename(mydata, ndvi = ndvi_modis_boku)
mydata <- rename(mydata, altitude = altitude_copernicus)
mydata <- rename(mydata, slope = slope_copernicus)
mydata <- rename(mydata, aspect = aspect_copernicus)
mydata <- rename(mydata, tree = treecover_copernicus)

#' all of the data is in the format of day-month-year
#' note that time is in UTC
mydata$New_time <-
  parse_date_time(x = mydata$time, c("%Y-%m-%d %H:%M:%S"))

# keep only the new time data
mydata <- select(mydata, New_time, long, lat, id, study, ndvi, altitude, slope, aspect, tree, corine_2006, corine_2012)
mydata <- rename(mydata, time = New_time)
mydata

#' we want to work with monthly averages 
#' some animals were tracked for over a year
#' so we must include a year-month-id grouping variable
#' first combine year and month
mydata$yr_month <- format(mydata$time, format = "%Y/%m")
mydata$yr_month  <- as.factor(mydata$yr_month)

#' we also need a year-month-day variable to see what the coverage is like over the course of
#' a month for each individual
mydata$yr_month_day <- format(mydata$time, format = "%Y/%m/%d")
mydata$yr_month_day <- as.factor(mydata$yr_month_day)
head(mydata)

#' count the number of unique days when grouped by id and and month
short_months <- mydata %>%
  group_by(id, yr_month) %>%
  summarise(count = n_distinct(yr_month_day)) %>% filter(count < 28) %>% droplevels()
short_months
short_months$yr_month

#' we merge the two and force all = TRUE so even the values that don't have a count
#' are included, this allows us to extract the tracks we won't that have ~ a month
#' of coverage 
mydata2 <- merge(short_months, mydata, all = TRUE)
length(mydata$id)
length(mydata2$id)

#' keep only the rows with the NAs which are the counts > 28
mydata2<- mydata2 %>% filter_all(any_vars(is.na(.))) 
head(mydata2)

#' Corine data switches in 2012, so they're not the same value 
#' mydata2$corine_2006 == mydata2$corine_2012
#' We can create a new column with year and provide Corine conditional on the year
mydata2$year <- format(mydata2$time, format = "%Y")
mydata2$year <- as.numeric(mydata2$year)
head(mydata2)
mydata2$corine <- if_else(mydata2$year >= 2012, mydata2$corine_2012, mydata2$corine_2006)
#' check that it worked 
mydata2 %>% filter(id == 1453)

#' now create a unique identifier that has the ID, year and month
#' We will use these to summarise the covariates 
mydata2$identifier <- paste(mydata2$id, mydata2$yr_month, sep = "_")
mydata2$identifier <- as.factor(mydata2$identifier)
head(mydata2)

#' Create the function for mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#' calculate summary statistics for each covariate
summary_data <- mydata2 %>% group_by(identifier) %>% 
  summarise_at(.vars = vars(ndvi, altitude, slope, aspect, tree, corine),
               .funs = c(Mean="mean", Sd="sd", Median = "median", Mode = "getmode"))
#' remove the summary statistics for corine that don't make sense i.e. everything bar the mode
summary_data <- summary_data %>% select(-corine_Mean, -corine_Sd, -corine_Median)
summary_data

#' add an id column
summary_data <-
  separate(
    summary_data,
    col = identifier,
    into = c("id", "NA"),
    sep = "_",
    remove = "FALSE"
  ) %>%  select(-"NA")

#' export the results
write.csv(summary_data, "results/env_data.csv", row.names = F)

