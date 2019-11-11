#'##########################################################################
#' ame1 project - studying the relation between home range size and activity
#' 2019/11/04
#' Adam Kane, Enrico Pirotta & Barry McMahon
#' https://mecoco.github.io/ame1.html
#' merging data
############################################################################

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

all_data <- merge(merge1,activity,c("identifier")) 
head(all_data)

#' export the results
write.csv(all_data, "results/combined_data.csv", row.names = F)
